// Road Map:
// [X] - Write to a file named vk.rs directly. Ask for permission before overwriting an existing
//       one.
// [ ] - Emit Vulkan commands
// [ ] - Emit Vulkan extensions
//     [ ] - Emit constants
//     [ ] - Emit enumerations
//     [ ] - Emit structures
// [ ] - Generate function pointer loading library

const BASIC_C_TYPE: [&str; 12] = [
    "void", "char", "float", "double", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int32_t",
    "int64_t", "size_t", "int",
];
const BASIC_RUST_TYPE: [&str; 12] = [
    "c_void", "c_char", "f32", "f64", "u8", "u16", "u32", "u64", "i32", "i64", "usize", "i32",
];

const OUTPUT_FILE_NAME: &str = "vk.rs";

fn main() -> std::io::Result<()> {
    let file = {
        let mut f = std::fs::OpenOptions::new();
        f.write(true).create_new(true);

        f
    };

    let output = match file.open(OUTPUT_FILE_NAME) {
        Ok(f) => Some(f),
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
            eprint!(
                "{} alread exists. Would you like to overwrite? (Y/N): ",
                OUTPUT_FILE_NAME
            );
            // Get input
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer)?;
            let input = buffer.trim_end().to_lowercase();

            if input == "y" {
                Some(std::fs::File::create(OUTPUT_FILE_NAME)?)
            } else if input == "n" {
                None
            } else {
                eprintln!("Unknown input. Exiting...");
                None
            }
        }
        Err(e) => return Err(e),
    };

    if let Some(output) = output {
        let path = std::env::args()
            .nth(1)
            .unwrap_or_else(|| String::from("vk.xml"));
        match std::fs::read_to_string(&path) {
            Ok(source) => match roxmltree::Document::parse(&source) {
                Ok(registry) => {
                    let mut writer = std::io::BufWriter::new(output);
                    process(registry, &mut writer)?
                }
                Err(e) => eprintln!("Failed to parse Vulkan XML registry!\n{}", e),
            },
            Err(e) => eprintln!(
                "Could not read Vulkan XML registry at path `{}`!\n{}",
                path, e
            ),
        };
    }

    Ok(())
}

// TODO: Clean up all node filtering instances into a single function
fn process<W>(registry: roxmltree::Document, output: &mut W) -> std::io::Result<()>
where
    W: std::io::Write,
{
    let root = registry.root_element();
    generate_types(&root, output)?;
    generate_commands(root);

    Ok(())
}

fn generate_commands(root: roxmltree::Node) {
    let mut buffer = Vec::with_capacity(1024);

    let commands = root
        .children()
        .filter(|child| child.is_element() && (child.tag_name().name() == "commands"))
        .flat_map(|commands| {
            commands
                .children()
                .filter(|node| node.is_element() && (node.tag_name().name() == "command"))
        });

    // There are two forms of the command tag:
    // 1. Defines a command alias with the only attributes being `name` and `alias`.
    // 2. Full definition of a command.
    for command in commands {
        let alias = command.attribute("name").zip(command.attribute("alias"));
        if let Some((name, alias)) = alias {
            eprintln!("pub type {} = {};", name, alias);
        } else {
            let prototype = command
                .children()
                .find(|child| child.is_element() && (child.tag_name().name() == "proto"))
                .and_then(Prototype::extract);

            let definition = command.children().filter_map(|node| {
                if node.is_element() && (node.tag_name().name() == "param") {
                    Parameter::extract(node)
                } else {
                    None
                }
            });
            buffer.extend(definition);

            if let Some(p) = prototype {
                eprint!("pub type {} = unsafe extern \"system\" fn(", p.name);
                buffer.drain(..).enumerate().for_each(|(i, parameter)| {
                    if i != 0 {
                        eprint!(", ");
                    }
                    eprint!("{}: {}", parameter.name, parameter.type_name);
                });
                eprintln!(") -> {}", p.return_value);
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Prototype<'p> {
    pub name: &'p str,
    pub return_value: &'p str,
}

impl<'p> Prototype<'p> {
    /// Extract a Vulkan function prototype.
    ///
    /// The function prototype includes the name and return value of the function that is being
    /// declared.
    pub fn extract(node: roxmltree::Node<'p, 'p>) -> Option<Self> {
        let name = node
            .children()
            .find(|child| child.is_element() && (child.tag_name().name() == "name"))
            .as_ref()
            .and_then(roxmltree::Node::text);

        let return_value = node
            .children()
            .find(|child| child.is_element() && (child.tag_name().name() == "type"))
            .as_ref()
            .and_then(roxmltree::Node::text);

        name.zip(return_value)
            .map(|(name, return_value)| Self { name, return_value })
    }
}

pub struct Parameter<'p> {
    pub name: &'p str,
    pub type_name: &'p str,
}

impl<'p> Parameter<'p> {
    /// Extract a Vulkan command parameter.
    ///
    /// A parameter can have attributes which provide additional information about the parameter as
    /// well as the definition itself.
    pub fn extract(node: roxmltree::Node<'p, 'p>) -> Option<Self> {
        // In terms of generation of function prototypes, the attributes do not have an impact on
        // the parameter, thus we ignore them.
        let name = node
            .children()
            .find(|child| child.is_element() && (child.tag_name().name() == "name"))
            .as_ref()
            .and_then(roxmltree::Node::text);

        let type_name = node
            .children()
            .find(|child| child.is_element() && (child.tag_name().name() == "type"))
            .as_ref()
            .and_then(roxmltree::Node::text);

        name.zip(type_name)
            .map(|(name, type_name)| Self { name, type_name })
    }
}

fn generate_types<W>(root: &roxmltree::Node, writer: &mut W) -> std::io::Result<()>
where
    W: std::io::Write,
{
    let mut buffer = Vec::with_capacity(4096);

    let mut enumerants = Vec::with_capacity(128);
    let enums = root
        .children()
        .filter(|child| child.is_element() && (child.tag_name().name() == "enums"))
        .filter_map(|e| Some(e).zip(e.attribute("name")));
    for (node, name) in enums {
        if name == "API Constants" {
            generate_constants(&node, &mut enumerants, &mut buffer);
        } else {
            generate_enum(&node, name, &mut enumerants, &mut buffer);
            enumerants.clear();
        }

        buffer.push(b'\n');
        writer.write_all(&buffer)?;
        buffer.clear();
    }

    let elements = root
        .children()
        .filter(|child| child.is_element() && (child.tag_name().name() == "types"))
        .flat_map(|t| t.children().filter(|t| t.is_element()))
        .filter_map(|e| Some(e).zip(e.attribute("category").zip(e.attribute("name"))));

    for (node, (category, name)) in elements {
        if let Some(alias) = node.attribute("alias") {
            buffer.extend_from_slice(b"pub type ");
            vk2rt(name, &mut buffer);
            buffer.extend_from_slice(b" = ");
            vk2rt(alias, &mut buffer);
            buffer.extend_from_slice(b";\n\n");

            writer.write_all(&buffer)?;
            buffer.clear();
        } else if (category == "struct") && generate_struct(&mut buffer, name, &node) {
            buffer.push(b'\n');
            writer.write_all(&buffer)?;
            buffer.clear();
        }
    }

    Ok(())
}

fn generate_constants<'c>(
    node: &roxmltree::Node<'c, 'c>,
    enums: &mut Vec<Enumerant<'c>>,
    buf: &mut Vec<u8>,
) {
    // Set up a buffer to hold previous constants in order to account for proper types in aliases
    let mut constants: Vec<(&str, &str)> = Vec::with_capacity(64);

    generate_variants(node, enums);
    enums.drain(..).for_each(|e| {
        buf.extend_from_slice(b"pub const ");
        let name = e.name.get(3..).unwrap_or(e.name);
        buf.extend_from_slice(name.as_bytes());
        match e.value {
            EnumerantValue::Alias(alias) => {
                buf.extend_from_slice(b": ");
                let t = constants
                    .iter()
                    .find(|c| c.1 == alias)
                    .map(|c| c.0)
                    .unwrap_or("u32");
                buf.extend_from_slice(t.as_bytes());
                buf.extend_from_slice(b" = ");
                let value = if alias.starts_with("VK_") {
                    alias.get(3..).unwrap_or(alias)
                } else {
                    alias
                };
                buf.extend_from_slice(value.as_bytes());
            }
            EnumerantValue::BitPos(bpos) => {
                buf.extend_from_slice(b": u32 = ");
                let value = format!("{}", bpos);
                buf.extend_from_slice(value.as_bytes());
                constants.push(("u32", name));
            }
            EnumerantValue::Integer(i) => {
                let i = if i.bytes().any(|b| b == b'~') {
                    i.replace('~', "!")
                } else {
                    i.to_string()
                };

                let (t, value) = if i.contains("ULL") {
                    ("u64", i.replace("ULL", ""))
                } else if i.contains('f') {
                    ("f32", i.replace("f", ""))
                } else if i.bytes().any(|b| b == b'U') {
                    ("u32", i.replace("U", ""))
                } else {
                    ("u32", i)
                };

                buf.extend_from_slice(b": ");
                buf.extend_from_slice(t.as_bytes());
                buf.extend_from_slice(b" = ");
                buf.extend_from_slice(value.as_bytes());
                constants.push((t, name));
            }
        }
        buf.extend_from_slice(b";\n");
    });
}

fn generate_enum<'e>(
    node: &roxmltree::Node<'e, 'e>,
    name: &'e str,
    enumerants: &mut Vec<Enumerant<'e>>,
    buffer: &mut Vec<u8>,
) {
    generate_variants(node, enumerants);

    buffer.extend_from_slice(b"#[derive(Clone, Copy, Debug)]\n");
    buffer.extend_from_slice(b"#[repr(C)]\n");
    buffer.extend_from_slice(b"pub enum ");
    vk2rt(name, buffer);
    buffer.extend_from_slice(b" {\n");
    vk2rv(name, enumerants, buffer);
    buffer.extend_from_slice(b"}\n");
}

fn generate_struct(buffer: &mut Vec<u8>, name: &str, structure: &roxmltree::Node) -> bool {
    // Do not emit these placeholder structures
    if (name == "VkBaseInStructure") || (name == "VkBaseOutStructure") {
        return false;
    }

    buffer.extend_from_slice(b"#[derive(Clone, Copy)]\n");
    buffer.extend_from_slice(b"#[repr(C)]\n");
    buffer.extend_from_slice(b"pub struct ");
    vk2rt(name, buffer);
    buffer.extend_from_slice(b" {");

    let mut type_buffer = String::with_capacity(128);

    let members = structure
        .children()
        .filter(|c| c.is_element() && (c.tag_name().name() == "member"));
    for member in members {
        buffer.extend_from_slice(b"\n   ");

        let children = member.children().filter(|c| c.is_element() || c.is_text());
        for child in children {
            let name = child.tag_name().name();
            if name != "comment" {
                if let Some(text) = child.text() {
                    let text = text.trim();
                    if !text.is_empty() {
                        if name == "name" {
                            buffer.extend_from_slice(b" pub ");
                            vk2rm(buffer, text);
                        } else {
                            type_buffer.push(' ');
                            type_buffer.push_str(text);
                        }
                    }
                }
            }
        }

        let tokens = tokenize_c_type(&type_buffer);
        // Do not generate the struct if we have a member that is a bit-field.
        if tokens.iter().any(|t| *t == CToken::Colon) {
            buffer.clear();
            return false;
        }

        buffer.extend_from_slice(b": ");
        ct2rt(buffer, tokens);
        buffer.push(b',');
        type_buffer.clear();
    }

    buffer.extend_from_slice(b"\n}\n");
    true
}

fn generate_variants<'e>(node: &roxmltree::Node<'e, 'e>, enumerants: &mut Vec<Enumerant<'e>>) {
    let children = node
        .children()
        .filter(|c| c.is_element() && (c.tag_name().name() == "enum"));
    for child in children {
        let name = child.attribute("name").unwrap_or("UnknownEnumerant");

        if let Some(value) = child.attribute("value") {
            let enumerant = Enumerant {
                name,
                value: EnumerantValue::Integer(value),
            };
            enumerants.push(enumerant);
        } else if let Some(bitpos) = child.attribute("bitpos") {
            if let Ok(bitpos) = bitpos.parse::<u32>() {
                let enumerant = Enumerant {
                    name,
                    value: EnumerantValue::BitPos(bitpos),
                };
                enumerants.push(enumerant);
            }
        } else if let Some(alias) = child.attribute("alias") {
            let enumerant = Enumerant {
                name,
                value: EnumerantValue::Alias(alias),
            };
            enumerants.push(enumerant);
        } else if let Some(_offset) = child.attribute("offset") {
            eprintln!("Enumerant {} has offset!", name);
        }
    }
}

/// Convert a list of C tokens to a Rust type.
fn ct2rt(buffer: &mut Vec<u8>, tokens: Vec<CToken>) {
    let search = tokens
        .iter()
        .filter_map(|t| {
            if let CToken::Identifier(s) = t {
                Some(s)
            } else {
                None
            }
        })
        .next();

    if let Some(identifier) = search {
        if tokens.len() == 1 {
            vk2rt(identifier, buffer);
        } else if tokens.iter().any(|t| *t == CToken::Colon) {
            todo!("Bitfields!");
        } else if tokens.iter().any(|t| *t == CToken::BracketLeft) {
            // Array
            let mut stack = Vec::with_capacity(8);
            for t in tokens.iter() {
                if let CToken::Literal(l) = t {
                    stack.push(l);
                }
            }

            for _ in 0..stack.len() {
                buffer.push(b'[');
            }
            vk2rt(identifier, buffer);
            for length in stack.into_iter().rev() {
                buffer.extend_from_slice(b"; ");
                buffer.extend_from_slice(length.as_bytes());
                buffer.push(b']');
            }
        } else {
            // Pointers
            let mut seen_pointer = false;
            for (i, t) in tokens.iter().enumerate().rev() {
                if seen_pointer {
                    if *t == CToken::Qualifier(CQualifier::Const) {
                        buffer.extend_from_slice(b"*const ");
                        seen_pointer = false;
                    } else if (*t == CToken::Pointer) || (i == 0) {
                        // `**` or <identifier> * so we must append mut to the pointer we have
                        // already seen before
                        buffer.extend_from_slice(b"*mut ");
                    }
                } else {
                    seen_pointer = *t == CToken::Pointer;
                }
            }

            vk2rt(identifier, buffer);
        }
    }
}

fn tokenize_c_type(mut t: &str) -> Vec<CToken> {
    let mut tokens = Vec::with_capacity(64);
    t = t.trim();

    for split in t.split_whitespace() {
        if let CToken::TokenList(_) = CToken::from(split) {
            if !split.is_empty() {
                let mut start = 0;
                for (end, c) in split.chars().enumerate() {
                    if CToken::is_basic(c) {
                        // We were tracking prior token before encountering this basic one so add
                        // that first.
                        if start < end {
                            if let Some(s) = split.get(start..end) {
                                tokens.push(CToken::from(s));
                            }
                        }

                        // Add the basic token we just encountered
                        tokens.push(CToken::from(c));
                        start = end + 1;
                    } else if end + 1 == split.len() {
                        if let Some(s) = split.get(start..end + 1) {
                            tokens.push(CToken::from(s));
                        }
                    }
                }
            }
        } else {
            tokens.push(CToken::from(split));
        }
    }

    tokens
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CToken<'t> {
    BracketLeft,
    BracketRight,
    Colon,
    Identifier(&'t str),
    Literal(&'t str),
    Pointer,
    Qualifier(CQualifier),
    TokenList(&'t str),
}

impl<'t> CToken<'t> {
    pub fn is_basic(c: char) -> bool {
        (c == '[') || (c == ']') || (c == ':') || (c == '*')
    }
}

impl From<char> for CToken<'_> {
    fn from(c: char) -> Self {
        match c {
            '[' => CToken::BracketLeft,
            ']' => CToken::BracketRight,
            ':' => CToken::Colon,
            '*' => CToken::Pointer,
            _ => panic!("Unknown basic token '{}'", c),
        }
    }
}

impl<'s> From<&'s str> for CToken<'s> {
    fn from(s: &'s str) -> Self {
        match s {
            "[" => CToken::BracketLeft,
            "]" => CToken::BracketRight,
            ":" => CToken::Colon,
            "*" => CToken::Pointer,
            "const" => CToken::Qualifier(CQualifier::Const),
            "volatile" => CToken::Qualifier(CQualifier::Volatile),
            i => {
                // NOTE: We assume that the tokens have been split on whitespace so the only
                // thing a token tree can contain is a mix of identifier and pointers which were
                // not space-separated.
                if i.contains(|c| (c == '[') || (c == ']') || (c == ':') || (c == '*')) {
                    CToken::TokenList(i)
                } else if i.bytes().all(|b| b.is_ascii_digit()) {
                    CToken::Literal(i)
                } else {
                    CToken::Identifier(i)
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CQualifier {
    Const,
    Volatile,
}

/// Convert a Vulkan structure member name to a Rust structure member name
fn vk2rm(buffer: &mut Vec<u8>, mut name: &str) {
    if name == "sType" {
        // Special case for `sType`. We cannot truncate to `type` since it is a Rust keyword so we
        // keep the prefix and just change it to snake_case.
        buffer.extend_from_slice(b"s_type");
    } else {
        // Do we have a Hungarian-notation prefix?
        let mut lowers = 0;
        for b in name.bytes() {
            if b.is_ascii_lowercase() {
                lowers += 1;
            } else {
                break;
            }
        }

        if lowers > 0 {
            if let Some(s) = name.get(lowers..) {
                let prefixes = ["pp", "pfn"];
                if !s.is_empty()
                    && ((lowers == 1) || prefixes.iter().any(|&p| p == &name[..lowers]))
                {
                    name = s;
                }
            }
        }

        cc2sc(buffer, name);
    }
}

// TODO: Handle case for types beginning with `PFN_vk`
/// Convert a Vulkan type to a Rust type
fn vk2rt(vk_name: &str, buffer: &mut Vec<u8>) {
    if let Some(i) = BASIC_C_TYPE.iter().position(|c| *c == vk_name) {
        if let Some(rtype) = BASIC_RUST_TYPE.get(i) {
            buffer.extend(rtype.bytes());
        }
    } else if vk_name.starts_with("Vk") {
        // Find consecutive capitals at the end of the type name, e.g. `EXT`, `KHR`, `NV`, etc.
        // and convert them to PascalCase
        let mut ncaps = 0;
        for b in vk_name.bytes().rev() {
            if b.is_ascii_uppercase() {
                ncaps += 1;
            } else {
                break;
            }
        }

        let len = vk_name.len();
        if (ncaps > 1) && (ncaps <= len - 2) {
            buffer.extend(vk_name.bytes().skip(2).take(len - ncaps - 2));
            c2pc(buffer, vk_name.bytes().skip(len - ncaps))
        } else {
            buffer.extend(vk_name.bytes().skip(2));
        }
    } else {
        buffer.extend_from_slice(vk_name.as_bytes());
    }
}

/// Convert a Vulkan enumeration variant to a Rust-style enumeration variant.
fn vk2rv(name: &str, variants: &[Enumerant], w: &mut Vec<u8>) {
    if variants.is_empty() {
        return;
    }

    let mut prefix = Vec::with_capacity(128);
    prefix.extend_from_slice(b"VK");
    pc2ssc(&mut prefix, name.bytes().skip(2));

    // Append / so we pick up the end of the name and stop matching
    prefix.push(b'/');

    // Working buffer
    let mut buffer = Vec::with_capacity(128);
    for v in variants {
        buffer.extend_from_slice(b"    ");

        if let EnumerantValue::Alias(_) = v.value {
            // Enums cannot have duplicate discriminants so we comment out aliases
            buffer.extend_from_slice(b"// ");
        }

        // TODO: Individual variants could have the suffix but it could be the case that it is not
        // shared across all variants. This means that we cannot strip it.
        let name = strip_prefix(v.name, &prefix).unwrap_or(v.name);
        ssc2cc(&mut buffer, name.bytes());

        buffer.extend_from_slice(b" = ");
        if let EnumerantValue::Alias(alias) = v.value {
            let alias = strip_prefix(alias, &prefix).unwrap_or(alias);
            ssc2cc(&mut buffer, alias.bytes());
        } else {
            let value = v.value.sanitize();
            buffer.extend_from_slice(value.as_bytes());
        }

        buffer.extend_from_slice(b",\n");
        w.extend_from_slice(&buffer);

        buffer.clear();
    }
}

fn strip_prefix<'i>(name: &'i str, prefix: &'i [u8]) -> Option<&'i str> {
    let prefix_index = prefix.iter().zip(name.bytes()).position(|(&l, r)| l != r);

    match prefix_index {
        Some(0) | None => None,
        Some(mut x) => {
            // Must limit match to a word boundary. In this case, a word boundary is `_` since
            // we're still working with a SCREAMING_SNAKE_CASE name.
            if !name.get(x..)?.starts_with('_') {
                if let Some(boundary) = name.get(..x)?.rfind('_') {
                    x = boundary;
                }
            }

            name.get(x..)
        }
    }
}

#[derive(Clone, Debug)]
struct Enumerant<'e> {
    name: &'e str,
    value: EnumerantValue<'e>,
}

#[derive(Clone, Copy, Debug)]
enum EnumerantValue<'v> {
    Alias(&'v str),
    BitPos(u32),
    Integer(&'v str),
}

impl<'v> EnumerantValue<'v> {
    pub fn sanitize(&self) -> std::borrow::Cow<'v, str> {
        use std::borrow::Cow;

        match self {
            Self::Alias(a) => Cow::Borrowed(a),
            Self::BitPos(b) => Cow::Owned(format!("0x{:X}", 0x1 << b)),
            Self::Integer(i) => {
                if i.contains("ULL") {
                    Cow::Owned(i.replace("ULL", ""))
                } else if i.bytes().any(|b| b == b'f') {
                    Cow::Owned(i.replace("f", ""))
                } else if i.bytes().any(|b| b == b'U') {
                    Cow::Owned(i.replace("U", ""))
                } else if i.bytes().any(|b| b == b'~') {
                    Cow::Owned(i.replace('~', "!"))
                } else {
                    Cow::Borrowed(i)
                }
            }
        }
    }
}

/// Convert an iterator over chars, B, from CAPS to PascalCase, storing it in `buffer`.
fn c2pc<B>(buffer: &mut Vec<u8>, mut name: B)
where
    B: Iterator<Item = u8>,
{
    if let Some(b) = name.next() {
        buffer.push(b);
        buffer.extend(name.map(|b| b.to_ascii_lowercase()))
    }
}

/// Convert an iterator over bytes, `B`, from PamelCase to SCREAMING_SNAKE_CASE storing it in
/// `buffer`.
fn pc2ssc<B>(buffer: &mut Vec<u8>, name: B)
where
    B: Iterator<Item = u8>,
{
    for c in name {
        if c.is_ascii_uppercase() {
            let s = [b'_', c];
            buffer.extend_from_slice(&s);
        } else {
            buffer.push(c.to_ascii_uppercase() as u8);
        }
    }
}

/// Convert a string from camelCase to snake_case.
fn cc2sc(buffer: &mut Vec<u8>, name: &str) {
    let mut was_upper = false;
    for (i, c) in name.bytes().enumerate() {
        if c.is_ascii_uppercase() {
            if !was_upper && (i != 0) {
                buffer.push(b'_');
            }
            was_upper = true;
        } else {
            was_upper = false;
        }
        buffer.push(c.to_ascii_lowercase());
    }
}

/// Convert an iterator over bytes, `B`, from SCREAMING_SNAKE_CASE to CamelCase storing it in
/// `buffer`.
fn ssc2cc<B>(buffer: &mut Vec<u8>, bytes: B)
where
    B: Iterator<Item = u8>,
{
    let mut capitalize = true;
    for c in bytes {
        if c == b'_' {
            capitalize = true;
        } else if capitalize {
            buffer.push(c);
            capitalize = false;
        } else {
            buffer.push(c.to_ascii_lowercase());
        }
    }
}

fn _trim_suffix<'t>(s: &'t str, suffixes: &'t [&str]) -> &'t str {
    if let Some(suffix) = suffixes.iter().find(|&&suffix| s.ends_with(suffix)) {
        if suffix.len() < s.len() {
            let y = s.len() - suffix.len();
            &s[..y]
        } else {
            s
        }
    } else {
        s
    }
}
