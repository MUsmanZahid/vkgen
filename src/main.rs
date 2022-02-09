const BASIC_C_TYPE: [&str; 12] = [
    "void", "char", "float", "double", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int32_t",
    "int64_t", "size_t", "int",
];
const BASIC_RUST_TYPE: [&str; 12] = [
    "c_void", "c_char", "f32", "f64", "u8", "u16", "u32", "u64", "i32", "i64", "usize", "i32",
];

// TODO: Should just write to a `vk.rs` and `vk_loader.rs` directly and ask for overwrite if needed
fn main() -> std::io::Result<()> {
    let path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| String::from("vk.xml"));
    match std::fs::read_to_string(&path) {
        Ok(source) => match roxmltree::Document::parse(&source) {
            Ok(registry) => process(registry)?,
            Err(e) => eprintln!("Failed to parse Vulkan XML registry!\n{}", e),
        },
        Err(e) => eprintln!(
            "Could not read Vulkan XML registry at path `{}`!\n{}",
            path, e
        ),
    };

    Ok(())
}

fn process(registry: roxmltree::Document) -> std::io::Result<()> {
    let root = registry.root_element();
    let mut enums = generate_enums(&root);

    for child in root.children().filter(|c| c.is_element()) {
        let name = child.tag_name().name();
        if name == "enums" {
            if let Some(name) = child.attribute("name") {
                if let Some(e) = enums.iter_mut().find(|e| e.name == name) {
                    generate_variants(e, &child);
                } else {
                    let mut e = Enum {
                        name,
                        enumerants: Vec::new(),
                    };
                    generate_variants(&mut e, &child);
                    enums.push(e);
                }
            } else {
                eprintln!("Unnamed enum found!");
            }
        } else if name == "extensions" {
            for item in child.children() {
                if item.is_element() && (item.tag_name().name() == "extension") {
                    // generate_extension(&item);
                }
            }
        }
    }

    let mut writer = std::io::BufWriter::new(std::io::stdout());
    write_enums(enums, &mut writer)
}

fn _generate_extension(ext: &roxmltree::Node) {
    if let Some(name) = ext.attribute("name") {
        eprint!("{} ", name);
    } else {
        eprint!("Unknown extension ");
    }

    for e in ext.children() {
        if e.is_element() {
            let name = e.tag_name().name();
            if name == "require" {
                eprint!("requires ");
            } else if name == "remove" {
                eprint!("removes ");
            }

            for item in e.children() {
                eprint!("{} ", item.tag_name().name());
            }
            eprintln!();
        }
    }
}

fn write_enums<W: std::io::Write>(enums: Vec<Enum>, w: &mut W) -> std::io::Result<()> {
    let mut buffer = Vec::with_capacity(256);

    writeln!(w, "use std::{{ffi::c_void, os::raw::c_char}};\n")?;
    for e in enums {
        vk2rt(e.name, &mut buffer);

        writeln!(w, "#[derive(Clone, Copy, Debug)]")?;
        writeln!(w, "#[repr(C)]")?;
        write!(w, "pub enum ")?;
        w.write_all(&buffer)?;
        writeln!(w, "{{")?;
        vk2rv(e.name, &e.enumerants, w)?;
        writeln!(w, "}}\n")?;

        buffer.clear();
    }

    Ok(())
}

fn generate_enums<'r, 's>(root: &'r roxmltree::Node<'s, 's>) -> Vec<Enum<'s>> {
    use std::io::Write;

    let stdout = std::io::stdout();
    let mut stdout_locked = stdout.lock();

    let mut enums = Vec::new();
    let mut structure = Vec::with_capacity(4096);

    let elements = root
        .children()
        .filter(|child| child.is_element() && (child.tag_name().name() == "types"))
        .flat_map(|t| t.descendants().filter(|t| t.is_element()))
        .filter_map(|e| Some(e).zip(e.attribute("category").zip(e.attribute("name"))));

    for (node, (category, name)) in elements {
        if let Some(alias) = node.attribute("alias") {
            structure.extend_from_slice(b"pub type ");
            vk2rt(name, &mut structure);
            structure.extend_from_slice(b" = ");
            vk2rt(alias, &mut structure);
            structure.extend_from_slice(b";\n\n");
        } else {
            match category {
                "enum" => {
                    let e = Enum {
                        name,
                        enumerants: Vec::new(),
                    };
                    enums.push(e);
                }
                "struct" => {
                    if generate_struct(&mut structure, name, &node) {
                        structure.push(b'\n');
                        if let Err(e) = stdout_locked.write_all(&structure) {
                            eprintln!("Failed to write {}:\n{}", name, e);
                        }
                    }
                    structure.clear();
                }
                _ => {}
            }
        }
    }

    enums
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

fn generate_variants<'n>(e: &mut Enum<'n>, node: &roxmltree::Node<'n, 'n>) {
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
            e.enumerants.push(enumerant);
        } else if let Some(bitpos) = child.attribute("bitpos") {
            if let Ok(bitpos) = bitpos.parse::<u32>() {
                let enumerant = Enumerant {
                    name,
                    value: EnumerantValue::BitPos(bitpos),
                };
                e.enumerants.push(enumerant);
            }
        } else if let Some(alias) = child.attribute("alias") {
            let enumerant = Enumerant {
                name,
                value: EnumerantValue::Alias(alias),
            };
            e.enumerants.push(enumerant);
        } else if let Some(_offset) = child.attribute("offset") {
            eprintln!("Enumerant with offset found!");
        } else {
            eprintln!("Enumerant {} has no value!", name);
        }
    }
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
        // and convert them to CamelCase
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
            c2cc(buffer, vk_name.bytes().skip(len - ncaps))
        } else {
            buffer.extend(vk_name.bytes().skip(2));
        }
    } else {
        buffer.extend_from_slice(vk_name.as_bytes());
    }
}

/// Convert a Vulkan enumeration variant to a Rust-style enumeration variant.
fn vk2rv<W: std::io::Write>(name: &str, variants: &[Enumerant], w: &mut W) -> std::io::Result<()> {
    if variants.is_empty() {
        return Ok(());
    }

    let mut prefix = Vec::with_capacity(128);
    prefix.extend_from_slice(b"VK");
    cc2ssc(&mut prefix, name.bytes().skip(2));

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
            use std::io::Write;
            // NOTE: Need to write formatted output since we do a conversion for bit-position based
            // enums.
            write!(&mut buffer, "{}", v.value)?;
        }

        buffer.extend_from_slice(b",\n");
        w.write_all(&buffer)?;

        buffer.clear();
    }

    Ok(())
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

struct Enum<'e> {
    name: &'e str,
    enumerants: Vec<Enumerant<'e>>,
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

impl std::fmt::Display for EnumerantValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Alias(a) => write!(f, "{}", a),
            Self::BitPos(b) => write!(f, "0x{:X}", 0x1 << b),
            Self::Integer(i) => write!(f, "{}", i),
        }
    }
}

/// Convert an iterator over chars, B, from CAPS to CamelCase, storing it in `buffer`.
fn c2cc<B>(buffer: &mut Vec<u8>, mut name: B)
where
    B: Iterator<Item = u8>,
{
    if let Some(b) = name.next() {
        buffer.push(b);
        buffer.extend(name.map(|b| b.to_ascii_lowercase()))
    }
}

/// Convert an iterator over bytes, `B`, from CamelCase to SCREAMING_SNAKE_CASE storing it in
/// `buffer`.
fn cc2ssc<B>(buffer: &mut Vec<u8>, name: B)
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
