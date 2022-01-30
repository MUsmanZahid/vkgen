const _BASIC_C_TYPE: [&'static str; 12] = [
    "void", "char", "float", "double", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int32_t",
    "int64_t", "size_t", "int",
];
const _BASIC_RUST_TYPE: [&'static str; 12] = [
    "c_void", "c_char", "f32", "f64", "u8", "u16", "u32", "u64", "i32", "i64", "usize", "i32",
];

fn main() -> std::io::Result<()> {
    let path = std::env::args().nth(1).unwrap_or(String::from("vk.xml"));
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
    let mut enums = generate_enums(&root).unwrap();

    for child in root.children() {
        if child.is_element() {
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
                for e in child.children() {
                    if e.is_element() {
                        if let Some(name) = e.attribute("name") {
                            eprintln!("{} extension found!", name);
                            for ext in e.children() {
                                if ext.is_element() {
                                    let name = ext.tag_name().name();
                                    if name == "require" {
                                        for req in ext.children() {
                                            let name = req.tag_name().name();
                                            eprintln!("\t{}", name);
                                        }
                                    } else if name == "remove" {
                                        eprintln!("\tRemove request");
                                    }
                                }
                            }
                        } else {
                            eprintln!("Unnamed extension found!");
                        }
                    }
                }
            }
        }
    }

    let mut writer = std::io::BufWriter::new(std::io::stdout());
    write_enums(enums, &mut writer)
}

fn write_enums<W: std::io::Write>(enums: Vec<Enum>, w: &mut W) -> std::io::Result<()> {
    let mut buffer = String::with_capacity(256);

    writeln!(w, "use std::{{ffi::c_void, os::raw::c_char}};\n")?;
    for e in enums {
        vk2rt(e.name, &mut buffer);

        writeln!(w, "#[derive(Clone, Copy, Debug)]")?;
        writeln!(w, "#[repr(C)]")?;
        writeln!(w, "pub enum {} {{", buffer)?;
        vk2rv(e.name, &e.enumerants, w)?;
        writeln!(w, "}}\n")?;

        buffer.clear();
    }

    Ok(())
}

fn generate_enums<'r, 's>(root: &'r roxmltree::Node<'s, 's>) -> Option<Vec<Enum<'s>>> {
    let mut result = None;

    for child in root.children() {
        if child.is_element() {
            let name = child.tag_name().name();
            if name == "types" {
                result = Some(types_path(&child));
                break;
            }
        }
    }

    result
}

fn generate_variants<'b, 'n>(e: &'b mut Enum<'n>, node: &'b roxmltree::Node<'n, 'n>) {
    for child in node.children() {
        let name = child.tag_name().name();
        if child.is_element() && (name == "enum") {
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
}

/// Convert a Vulkan type to a Rust type
fn vk2rt(vk_name: &str, buffer: &mut String) {
    buffer.extend(vk_name.chars().skip(2))
}

/// Convert a Vulkan enumeration variant to a Rust-style enumeration variant.
fn vk2rv<W: std::io::Write>(name: &str, variants: &[Enumerant], w: &mut W) -> std::io::Result<()> {
    if variants.len() == 0 {
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

fn types_path<'b, 'i>(types: &'b roxmltree::Node<'i, 'i>) -> Vec<Enum<'i>> {
    let mut enums = Vec::with_capacity(256);

    if let Some(comment) = types.attribute("comment") {
        eprintln!("{}", comment);
    }

    for vk_type in types.descendants() {
        if vk_type.is_element() {
            if let Some(category) = vk_type.attribute("category") {
                if (category == "include") || (category == "define") {
                    continue;
                }
            }

            if let Some(name) = vk_type.attribute("name") {
                if let Some("enum") = vk_type.attribute("category") {
                    let e = Enum {
                        name,
                        enumerants: Vec::new(),
                    };
                    enums.push(e);
                } else {
                    eprint!("{}: ", name);
                    for attribute in vk_type.attributes() {
                        eprint!("({}, {}), ", attribute.name(), attribute.value());
                    }

                    if let Some(text) = vk_type.text() {
                        if !text.chars().all(char::is_whitespace) {
                            eprintln!("{}", text);
                        }
                    } else {
                        eprintln!();
                    }
                }
            }
        }
    }

    enums
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
            Self::BitPos(b) => write!(f, "{:X}", 0x1 << b),
            Self::Integer(i) => write!(f, "{}", i),
        }
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

fn trim_suffix<'t>(s: &'t str, suffixes: &'t [&str]) -> &'t str {
    if let Some(suffix) = suffixes.iter().find(|&&suffix| s.ends_with(suffix)) {
        if suffix.len() < s.len() {
            let y = s.len() - suffix.len();
            &s[..y]
        } else {
            &s
        }
    } else {
        &s
    }
}
