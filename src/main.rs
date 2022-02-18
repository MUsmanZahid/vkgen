// Road Map:
// [X] - Write to a file named vk.rs directly. Ask for permission before overwriting an existing
//       one.
// [X] - Emit Vulkan commands
// [X] - Emit Vulkan handles
// [ ] - Emit proper type in structures for handles.
// [ ] - Store all identifiers in memory and emit in an organised format.
// [ ] - Emit extensions
//     [ ] - Constants
//     [X] - Enumerations
//     [ ] - Structures
// [ ] - Emit features
// [ ] - Generate function pointer loading library
// [ ] - Remove extra indirection - we first write to buffer, then we copy from buffer into output

const BASIC_C_TYPE: [&str; 12] = [
    "void", "char", "float", "double", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int32_t",
    "int64_t", "size_t", "int",
];
const BASIC_RUST_TYPE: [&str; 12] = [
    "c_void", "c_char", "f32", "f64", "u8", "u16", "u32", "u64", "i32", "i64", "usize", "i32",
];

const OUTPUT_FILE_NAME: &str = "vk.rs";

fn main() -> std::io::Result<()> {
    let output = match std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(OUTPUT_FILE_NAME)
    {
        Ok(f) => Some(f),
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
            print!(
                "{} already exists. Would you like to overwrite? (Y/N): ",
                OUTPUT_FILE_NAME
            );
            <_ as std::io::Write>::flush(&mut std::io::stdout())?;

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
                Ok(registry) => process(registry, std::io::BufWriter::new(output))?,
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

pub fn process<W>(registry: roxmltree::Document, output: W) -> std::io::Result<()>
where
    W: std::io::Write,
{
    let generator = Generator {
        buffer: Vec::with_capacity(4 * 4096),
        output,
        root: registry.root_element(),
    };
    generator.emit()
}

pub struct Generator<'i, W> {
    buffer: Vec<u8>,
    output: W,
    root: roxmltree::Node<'i, 'i>,
}

impl<'i, W: std::io::Write> Generator<'i, W> {
    fn drain_write_all(&mut self) -> std::io::Result<()> {
        self.output.write_all(&self.buffer)?;
        self.buffer.clear();
        Ok(())
    }

    pub fn emit(mut self) -> std::io::Result<()> {
        // Add imports at the very top
        self.buffer
            .extend_from_slice(b"use std::{ffi::c_void, os::raw::c_char};\n\n");

        self.emit_constants()?;
        self.emit_handles()?;

        self.emit_structs()?;
        self.emit_enums()?;
        self.emit_commands()?;
        self.emit_extensions()?;
        self.emit_aliases()
    }

    pub fn emit_aliases(&mut self) -> std::io::Result<()> {
        let aliases = filter(self.root, "types")
            .flat_map(|t| filter(t, "type"))
            .filter_map(|t| t.attribute("name").zip(t.attribute("alias")));

        self.buffer.extend_from_slice(b"// Aliases\n");
        aliases.for_each(|(name, alias)| {
            self.buffer.extend_from_slice(b"pub type ");
            vk2rt(name, &mut self.buffer);
            self.buffer.extend_from_slice(b" = ");
            vk2rt(alias, &mut self.buffer);
            self.buffer.extend_from_slice(b";\n");
        });

        self.drain_write_all()
    }

    pub fn emit_commands(&mut self) -> std::io::Result<()> {
        let mut params = Vec::with_capacity(128);

        self.buffer.extend_from_slice(b"// Commands\n");
        filter(self.root, "commands")
            .flat_map(|commands| filter(commands, "command"))
            .for_each(|command| generate_command(command, &mut params, &mut self.buffer));

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    pub fn emit_constants(&mut self) -> std::io::Result<()> {
        let constants = filter(self.root, "enums")
            .find(|e| matches!(e.attribute("name"), Some("API Constants")));
        let node = if let Some(n) = constants {
            n
        } else {
            return Ok(());
        };

        let mut enums = Vec::new();
        // Set up a buffer to hold previous constants in order to account for proper types in
        // aliases
        let mut constants = Vec::with_capacity(64);

        enums.extend(filter(node, "enum").filter_map(|e| generate_variant(e, None)));

        self.buffer.extend_from_slice(b"// Constants\n");
        enums.drain(..).for_each(|e| {
            self.buffer.extend_from_slice(b"pub const ");

            let name = if e.name.starts_with("VK_") {
                e.name.get(3..).unwrap_or(e.name)
            } else {
                e.name
            };
            self.buffer.extend_from_slice(name.as_bytes());

            match e.value {
                EnumerantValue::Alias(alias) => {
                    self.buffer.extend_from_slice(b": ");
                    let t = constants
                        .iter()
                        .find_map(|&(t, name)| if name == alias { Some(t) } else { None })
                        .unwrap_or("u32");
                    self.buffer.extend_from_slice(t.as_bytes());
                    self.buffer.extend_from_slice(b" = ");
                    let value = if alias.starts_with("VK_") {
                        alias.get(3..).unwrap_or(alias)
                    } else {
                        alias
                    };
                    self.buffer.extend_from_slice(value.as_bytes());
                }
                EnumerantValue::BitPos(bpos) => {
                    self.buffer.extend_from_slice(b": u32 = ");
                    let value = format!("{}", bpos);
                    self.buffer.extend_from_slice(value.as_bytes());
                    constants.push(("u32", name));
                }
                EnumerantValue::Extension { .. } => {
                    eprintln!("ERROR: Constant with extension found!")
                }
                EnumerantValue::Integer(i) => {
                    let mut value = if i.bytes().any(|b| b == b'~') {
                        i.replace('~', "!")
                    } else {
                        i.to_string()
                    };

                    let mut t = "u32";
                    if value.contains("ULL") {
                        t = "u64";
                        value = value.replace("ULL", "");
                    }

                    if name.contains("SIZE") {
                        t = "usize";
                    }

                    if value.bytes().any(|b| b == b'f') {
                        t = "f32";
                        value = value.replace("f", "");
                    }

                    if value.bytes().any(|b| b == b'U') {
                        value = value.replace("U", "");
                    }

                    self.buffer.extend_from_slice(b": ");
                    self.buffer.extend_from_slice(t.as_bytes());
                    self.buffer.extend_from_slice(b" = ");
                    self.buffer.extend_from_slice(value.as_bytes());
                    constants.push((t, e.name));
                }
            }
            self.buffer.extend_from_slice(b";\n");
        });

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    pub fn emit_enums(&mut self) -> std::io::Result<()> {
        // TODO: Maybe count how many enums and their corresponding variants we have.
        let mut map = std::collections::HashMap::with_capacity(512);

        let enums = filter(self.root, "enums").filter_map(|e| {
            let mut result = None;

            if let Some(name) = e.attribute("name") {
                if name != "API Constants" {
                    result = Some((e, name));
                }
            }

            result
        });

        for (node, name) in enums {
            let mut enumerants = Vec::with_capacity(256);
            enumerants.extend(filter(node, "enum").filter_map(|e| generate_variant(e, None)));
            map.insert(name, enumerants);
        }

        generate_extended_enums(self.root, &mut map);

        self.buffer.extend(b"// Enumerations\n");
        for (name, variants) in map {
            if !variants.is_empty() {
                emit_enum(name, variants, &mut self.buffer);
                self.buffer.push(b'\n');
            }
        }

        self.drain_write_all()
    }

    // TODO: Store commands, constants, and structs in memory and emit them in an organized format
    pub fn emit_extensions(&mut self) -> std::io::Result<()> {
        let extensions = filter(self.root, "extensions")
            .flat_map(|ext| filter(ext, "extension"))
            .filter_map(|ext| {
                if !matches!(ext.attribute("supported"), Some("disabled")) {
                    ext.attribute("name").map(|n| (ext, n))
                } else {
                    None
                }
            });

        for (ext, name) in extensions {
            eprintln!("// {}", name);

            let elements = filter(ext, "require").flat_map(|req| {
                req.children().filter(|c| {
                    let name = c.tag_name().name();
                    c.is_element() && ((name == "type") || (name == "enum") || (name == "command"))
                })
            });

            for element in elements {
                let name = element.tag_name().name();
                if name == "enum" && element.attribute("extends").is_none() {
                    if let Some(name) = element.attribute("name") {
                        if name.ends_with("EXTENSION_NAME") {
                            if let Some(value) = element.attribute("value") {
                                let len = name.len() - "_EXTENSION_NAME".len();
                                eprintln!(
                                    "pub const {}: &str = {}\\u{{0}}\";",
                                    &name[3..len],
                                    &value[..value.len() - 1]
                                );
                            }
                        }
                    }
                }
            }

            eprintln!();
        }

        Ok(())
    }

    pub fn emit_handles(&mut self) -> std::io::Result<()> {
        let handles = filter(self.root, "types")
            .flat_map(|t| filter(t, "type"))
            .filter_map(|t| {
                let mut result = None;

                if let Some(category) = t.attribute("category") {
                    if category == "handle" {
                        result = t.attribute("name").or_else(|| {
                            t.children()
                                .find(|c| c.tag_name().name() == "name")
                                .and_then(|n| n.text())
                        });
                    }
                }

                result
            });

        self.buffer.extend_from_slice(b"// Handles\n");
        for name in handles {
            self.buffer.extend_from_slice(b"#[repr(C)]\n");
            self.buffer.extend_from_slice(b"#[derive(Clone, Copy)]\n");
            self.buffer.extend_from_slice(b"pub struct ");
            vk2rt(name, &mut self.buffer);
            self.buffer.extend_from_slice(b" {\n");
            self.buffer.extend_from_slice(b"    _data: [u8; 0],\n");
            self.buffer.extend_from_slice(
                b"    _marker: core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,\n",
            );
            self.buffer.extend_from_slice(b"}\n\n");
        }

        self.drain_write_all()
    }

    pub fn emit_structs(&mut self) -> std::io::Result<()> {
        let structs = filter(self.root, "types")
            .flat_map(|t| t.children().filter(|c| c.is_element()))
            .filter(|t| {
                let mut result = false;

                if let Some(category) = t.attribute("category") {
                    if (category == "struct") && t.attribute("alias").is_none() {
                        result = true;
                    }
                }

                result
            })
            .filter_map(|s| s.attribute("name").map(|name| (s, name)));

        let mut type_buffer = String::with_capacity(256);
        self.buffer.extend_from_slice(b"// Structures\n");
        for (s, name) in structs {
            if generate_struct(&mut self.buffer, &mut type_buffer, name, s) {
                self.buffer.push(b'\n');
            }
        }

        self.drain_write_all()
    }

    pub fn new(capacity: usize, output: W, root: roxmltree::Node<'i, 'i>) -> Self {
        Self {
            buffer: Vec::with_capacity(capacity),
            output,
            root,
        }
    }
}

pub fn generate_command<'n>(
    command: roxmltree::Node<'n, 'n>,
    params: &mut Vec<FunctionSection<'n>>,
    buffer: &mut Vec<u8>,
) {
    let alias = command.attribute("name").zip(command.attribute("alias"));

    // There are two forms of the command tag:
    // 1. Defines a command alias with the only attributes being `name` and `alias`.
    // 2. Full definition of a command.
    if let Some((name, alias)) = alias {
        buffer.extend_from_slice(b"pub type ");
        vk2rt(name, buffer);
        buffer.extend_from_slice(b" = ");
        vk2rt(alias, buffer);
        buffer.extend_from_slice(b";\n");
    } else {
        let prototype = command
            .children()
            .find(|child| child.is_element() && (child.tag_name().name() == "proto"))
            .and_then(FunctionSection::extract);

        if let Some(p) = prototype {
            let definition = command.children().filter_map(|node| {
                if node.is_element() && (node.tag_name().name() == "param") {
                    FunctionSection::extract(node)
                } else {
                    None
                }
            });
            params.extend(definition);

            buffer.extend_from_slice(b"pub type ");
            vk2rt(p.name, buffer);
            buffer.extend_from_slice(b" = unsafe extern \"system\" fn(");

            params.drain(..).enumerate().for_each(|(i, parameter)| {
                if i != 0 {
                    buffer.extend_from_slice(b", ");
                }
                vk2rm(buffer, parameter.name);
                buffer.extend_from_slice(b": ");
                vk2rt(parameter.type_name, buffer);
            });

            buffer.push(b')');
            if p.type_name != "void" {
                buffer.extend_from_slice(b" -> ");
                vk2rt(p.type_name, buffer);
            }
            buffer.extend_from_slice(b";\n");
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FunctionSection<'s> {
    pub name: &'s str,
    pub type_name: &'s str,
}

impl<'s> FunctionSection<'s> {
    /// Extract a Vulkan function section.
    ///
    /// A function section can be a pair of either:
    /// 1. The function name and its return value, or
    /// 2. The name of one of the function's parameters and its corresponding type.
    pub fn extract(node: roxmltree::Node<'s, 's>) -> Option<Self> {
        let name = find(node, "name").as_ref().and_then(roxmltree::Node::text);
        let type_name = find(node, "type").as_ref().and_then(roxmltree::Node::text);

        name.zip(type_name)
            .map(|(name, type_name)| Self { name, type_name })
    }
}

pub fn generate_extended_enums<'n>(
    root: roxmltree::Node<'n, 'n>,
    map: &mut std::collections::HashMap<&'n str, Vec<Enumerant<'n>>>,
) {
    let extensions = filter(root, "extensions")
        .flat_map(|ext| filter(ext, "extension"))
        .filter(|ext| !matches!(ext.attribute("supported"), Some("disabled")));

    for ext in extensions {
        let extnum = ext.attribute("number");
        let variants = filter(ext, "require")
            .flat_map(|req| filter(req, "enum"))
            .filter(|e| {
                let name = e
                    .attribute("name")
                    .map(|n| !n.ends_with("SPEC_VERSION") && !n.ends_with("EXTENSION_NAME"));
                matches!(name, Some(true))
            })
            .filter_map(|e| e.attribute("extends").zip(generate_variant(e, extnum)));

        for (extends, variant) in variants {
            if let Some(value) = map.get_mut(extends) {
                value.push(variant);
            } else {
                let mut value = Vec::with_capacity(256);
                value.push(variant);
                map.insert(extends, value);
            }
        }
    }
}

// TODO: Keep a list of the handles!
//
// If any of the member types have a pointer to a handle or the name of the handle itself we need to
// add a pointer to the type since we don't hide the fact that the handles are pointers in our Rust
// API.
fn generate_struct(
    buffer: &mut Vec<u8>,
    type_buffer: &mut String,
    name: &str,
    node: roxmltree::Node,
) -> bool {
    // Do not emit these placeholder structures
    if (name == "VkBaseInStructure") || (name == "VkBaseOutStructure") {
        return false;
    }

    // Need to keep the original length of the buffer around because we might need to bail in the
    // middle of generating the struct since we don't support emitting structs with bitfields right
    // now.
    //
    // With this length, we can `truncate` the buffer later on when we need to bail.
    let length = buffer.len();

    // Insert the structure header
    buffer.extend_from_slice(b"#[derive(Clone, Copy)]\n");
    buffer.extend_from_slice(b"#[repr(C)]\n");
    buffer.extend_from_slice(b"pub struct ");
    vk2rt(name, buffer);
    buffer.extend_from_slice(b" {");

    for member in filter(node, "member") {
        buffer.extend_from_slice(b"\n   ");

        let children = member.children().filter_map(|c| {
            if (c.is_element() || c.is_text()) && (c.tag_name().name() != "comment") {
                c.text().map(|s| (c, s))
            } else {
                None
            }
        });
        for (child, mut text) in children {
            let name = child.tag_name().name();
            text = text.trim();
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

        let tokens = tokenize_c_type(type_buffer);

        // Need to bail. Do not generate the struct if we have a member that is a bit-field.
        if tokens.iter().any(|t| *t == CToken::Colon) {
            buffer.truncate(length);
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

fn generate_variant<'v>(
    node: roxmltree::Node<'v, 'v>,
    extension_number: Option<&'v str>,
) -> Option<Enumerant<'v>> {
    let name = node.attribute("name")?;

    let value = if let Some(value) = node.attribute("value") {
        Some(EnumerantValue::Integer(value))
    } else if let Some(bitpos) = node.attribute("bitpos") {
        bitpos.parse().ok().map(EnumerantValue::BitPos)
    } else if let Some(alias) = node.attribute("alias") {
        Some(EnumerantValue::Alias(alias))
    } else if let Some(offset) = node.attribute("offset") {
        let number = extension_number.or_else(|| node.attribute("extnumber"))?;
        let direction = node.attribute("dir");

        let ext = EnumerantValue::Extension {
            number,
            offset,
            direction,
        };
        Some(ext)
    } else {
        None
    };

    value.map(|value| Enumerant { name, value })
}

pub fn emit_enum(name: &str, variants: Vec<Enumerant>, buffer: &mut Vec<u8>) {
    buffer.extend_from_slice(b"#[derive(Clone, Copy, Debug)]\n");
    buffer.extend_from_slice(b"#[repr(C)]\n");
    buffer.extend_from_slice(b"pub enum ");
    vk2rt(name, buffer);
    buffer.extend_from_slice(b" {\n");
    vk2rv(name, &variants, buffer);
    buffer.extend_from_slice(b"}\n");
}

pub fn filter<'b, 'n>(
    node: roxmltree::Node<'b, 'n>,
    name: &'b str,
) -> impl Iterator<Item = roxmltree::Node<'b, 'n>> {
    node.children()
        .filter(move |child| child.is_element() && (name == child.tag_name().name()))
}

pub fn find<'n>(node: roxmltree::Node<'n, 'n>, name: &str) -> Option<roxmltree::Node<'n, 'n>> {
    node.children()
        .find(|child| child.is_element() && (child.tag_name().name() == name))
}

/// Convert a list of C tokens to a Rust type.
fn ct2rt(buffer: &mut Vec<u8>, tokens: Vec<CToken>) {
    let search = tokens.iter().find_map(|t| {
        if let CToken::Identifier(s) = t {
            Some(s)
        } else {
            None
        }
    });

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
        // keep the prefix.
        buffer.extend_from_slice(b"stype");
    } else {
        // Do we have a Hungarian-notation prefix?
        let prefixes = ["pp", "pfn"];

        // Count how many lowercase letters we start with in the camelCase member name
        let lowers = name
            .bytes()
            .position(|b| b.is_ascii_uppercase())
            .unwrap_or(name.len());

        let prefix = name.get(..lowers);
        let rem = name.get(lowers..);
        if let Some((prefix, rem)) = prefix.zip(rem) {
            if !rem.is_empty() && ((lowers == 1) || prefixes.iter().any(|&p| p == prefix)) {
                name = rem;
            }
        }

        cc2sc(buffer, name);
    }
}
/// Convert a Vulkan type to a Rust type
fn vk2rt(name: &str, buffer: &mut Vec<u8>) {
    let prefixes = ["Vk", "vk", "PFN_vk"];
    if let Some(i) = BASIC_C_TYPE.iter().position(|c| *c == name) {
        if let Some(rtype) = BASIC_RUST_TYPE.get(i) {
            buffer.extend(rtype.bytes());
        }
    } else if let Some(prefix) = prefixes.iter().find(|&&prefix| name.starts_with(prefix)) {
        let plen = prefix.len();

        // Find consecutive capitals at the end of the type name, e.g. `EXT`, `KHR`, `NV`, etc.
        // and convert them to PascalCase
        let caps = name
            .bytes()
            .rev()
            .position(|b| !b.is_ascii_uppercase())
            .unwrap_or(name.len());

        let len = name.len();
        if (caps > 1) && (caps <= len - plen) {
            buffer.extend(name.bytes().skip(plen).take(len - caps - plen));
            c2pc(buffer, name.bytes().skip(len - caps))
        } else {
            buffer.extend(name.bytes().skip(plen));
        }
    } else {
        buffer.extend_from_slice(name.as_bytes());
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

        // TODO: Better suffix removal logic.
        //
        // Individual variants could have the suffix but it could be the case that it is not shared
        // across all variants. This means that we cannot strip it.
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
pub struct Enumerant<'e> {
    pub name: &'e str,
    pub value: EnumerantValue<'e>,
}

#[derive(Clone, Copy, Debug)]
pub enum EnumerantValue<'v> {
    Alias(&'v str),
    BitPos(u32),
    Extension {
        number: &'v str,
        offset: &'v str,
        direction: Option<&'v str>,
    },
    Integer(&'v str),
}

impl<'v> EnumerantValue<'v> {
    pub fn sanitize(&self) -> std::borrow::Cow<'v, str> {
        use std::borrow::Cow;

        match self {
            Self::Alias(a) => Cow::Borrowed(a),
            Self::BitPos(b) => Cow::Owned(format!("0x{:X}", 0x1 << b)),
            Self::Extension {
                number,
                offset,
                direction,
            } => {
                // Initial offset for any extension
                let mut value = 1_000_000_000;
                if let Ok(number) = number.parse::<i32>() {
                    value += (number - 1) * 1_000;
                }
                if let Ok(offset) = offset.parse::<i32>() {
                    value += offset;
                }
                if let Some("-") = direction {
                    value = -value;
                }

                Cow::Owned(value.to_string())
            }
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
pub fn c2pc<B>(buffer: &mut Vec<u8>, mut name: B)
where
    B: Iterator<Item = u8>,
{
    if let Some(b) = name.next() {
        buffer.push(b);
        buffer.extend(name.map(|b| b.to_ascii_lowercase()))
    }
}

/// Convert an iterator over bytes, `B`, from PascalCase to SCREAMING_SNAKE_CASE storing it in
/// `buffer`.
pub fn pc2ssc<B>(buffer: &mut Vec<u8>, name: B)
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
pub fn cc2sc(buffer: &mut Vec<u8>, name: &str) {
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
pub fn ssc2cc<B>(buffer: &mut Vec<u8>, bytes: B)
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

pub fn _trim_suffix<'t>(s: &'t str, suffixes: &'t [&str]) -> &'t str {
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
