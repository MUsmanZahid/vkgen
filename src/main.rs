const BASIC_C_TYPE: [&'static str; 12] = [
    "void", "char", "float", "double", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int32_t",
    "int64_t", "size_t", "int",
];
const BASIC_RUST_TYPE: [&'static str; 12] = [
    "c_void", "c_char", "f32", "f64", "u8", "u16", "u32", "u64", "i32", "i64", "usize", "i32",
];

fn main() {
    let path = std::env::args().nth(1).unwrap_or(String::from("vk.xml"));
    match std::fs::read_to_string(&path) {
        Ok(source) => match roxmltree::Document::parse(&source) {
            Ok(registry) => process(registry),
            Err(e) => eprintln!("Failed to parse Vulkan XML registry!\n{}", e),
        },
        Err(e) => eprintln!(
            "Could not read Vulkan XML registry at path `{}`!\n{}",
            path, e
        ),
    }
}

fn process(registry: roxmltree::Document) {
    let root = registry.root_element();
    let mut enums = generate_enums(&root);

    for child in root.children() {
        if child.is_element() {
            let name = child.tag_name().name();

            if name == "enums" {
                if let Some(name) = child.attribute("name") {
                    println!("Enums: {} has {} children", name, child.children().count());
                    if let Some(e) = enums.iter_mut().find(|e| e.name == name) {
                        generate_variants(e, &child);
                    } else {
                        eprintln!("Unseen enum {} encountered!", name);
                    }
                } else {
                    eprintln!("Unnamed enum found!");
                }
            }
        }
    }

    for e in enums {
        eprintln!("#[derive(Clone, Copy, Debug)]");
        eprintln!("#[repr(C)]");
        eprintln!("pub enum {} {{", vk_type_to_rust_type(e.name));
        for v in vk2rv(e.name, &e.enumerants).iter() {
            eprintln!("    {} = {},", v.name, v.value);
        }
        eprintln!("}}\n");
    }
}

fn generate_enums<'r, 's>(root: &'r roxmltree::Node<'s, 's>) -> Vec<Enum<'s>> {
    for child in root.children() {
        if child.is_element() {
            let name = child.tag_name().name();
            if name == "types" {
                return types_path(&child);
            }
        }
    }

    panic!("No types found!");
}

fn generate_variants<'b, 'n>(e: &'b mut Enum<'n>, node: &'b roxmltree::Node<'n, 'n>) {
    for child in node.children() {
        if child.is_element() {
            let name = child
                .attribute("name")
                .map(|s| String::from(s).into_boxed_str())
                .unwrap_or(String::from("UnknownEnumerant").into_boxed_str());
            if let Some(value) = child.attribute("value") {
                let enumerant = Enumerant {
                    name,
                    value: String::from(value).into_boxed_str(),
                };
                e.enumerants.push(enumerant);
            } else if let Some(bitpos) = child.attribute("bitpos") {
                if let Ok(bitpos) = bitpos.parse::<u32>() {
                    let enumerant = Enumerant {
                        name,
                        value: format!("{:X}", 0x1 << bitpos).into_boxed_str(),
                    };
                    e.enumerants.push(enumerant);
                }
            } else if let Some(alias) = child.attribute("alias") {
                let enumerant = Enumerant {
                    name,
                    value: String::from(alias).into_boxed_str(),
                };
                e.enumerants.push(enumerant);
            } else if let Some(_offset) = child.attribute("offset") {
                println!("Enumerant with offset found!");
            } else {
                println!("Enumerant with no value found!");
            }
        }
    }
}

fn vk_type_to_rust_type(vk_name: &str) -> Box<str> {
    // Skip past `Vk`
    vk_name.chars().skip(2).collect::<String>().into_boxed_str()
}

fn vk2rv(name: &str, variants: &[Enumerant]) -> Box<[Enumerant]> {
    if variants.len() == 0 {
        return Box::new([]);
    }

    let mut prefix = Vec::with_capacity(128);

    // Copy `Vk` and turn it into 'VK'
    prefix.extend(
        name.bytes()
            .take(2)
            .map(|b| (b as char).to_ascii_uppercase() as u8),
    );

    // Make the rest of the type name SCREAMING_SNAKE_CASE
    cc2ssc(&mut prefix, name.bytes().skip(2));

    // Append / so we pick up the end of the name and stop matching
    prefix.push(b'/');

    // Working buffer
    let mut buffer = Vec::with_capacity(128);

    variants
        .iter()
        .map(|v| {
            let prefix_index = prefix.iter().zip(v.name.bytes()).position(|(&l, r)| l != r);

            match prefix_index {
                Some(0) | None => {
                    println!(
                        "Unable to find common prefix in {} -> {:?}",
                        name,
                        std::str::from_utf8(&prefix)
                    );

                    v.clone()
                }
                Some(x) => {
                    let suffixes = ["_BIT", "_NV", "_KHR"];
                    let trimmed = trim_suffix(&v.name[x..], &suffixes);

                    // Make the trimmed variant name CamelCase
                    ssc2cc(&mut buffer, trimmed.bytes());

                    let name = std::str::from_utf8(&buffer)
                        .expect("Enum variant conversion resulted in non-UTF-8 output!")
                        .to_string()
                        .into_boxed_str();

                    buffer.clear();
                    Enumerant {
                        name,
                        value: v.value.clone(),
                    }
                }
            }
        })
        .collect()
}

fn types_path<'b, 'i>(types: &'b roxmltree::Node<'i, 'i>) -> Vec<Enum<'i>> {
    let mut enums = Vec::with_capacity(256);

    eprintln!("use std::{{ffi::c_void, os::raw::c_char}};\n");

    if let Some(comment) = types.attribute("comment") {
        println!("{}", comment);
    }

    for vk_type in types.descendants() {
        if vk_type.is_element() {
            if let Some(category) = vk_type.attribute("category") {
                if (category == "include") || (category == "define") {
                    continue;
                }
            }

            if let Some(name) = vk_type.attribute("name") {
                if let Some(index) = BASIC_C_TYPE.iter().position(|s| s == &name) {
                    println!("{} -> {}", name, BASIC_RUST_TYPE[index]);
                } else if let Some("enum") = vk_type.attribute("category") {
                    let e = Enum {
                        name,
                        enumerants: Vec::new(),
                    };
                    enums.push(e);

                    println!("Added enum {}", name);
                } else {
                    print!("{}: ", name);
                    for attribute in vk_type.attributes() {
                        print!("({}, {}), ", attribute.name(), attribute.value());
                    }

                    if let Some(text) = vk_type.text() {
                        if !text.chars().all(char::is_whitespace) {
                            println!("{}", text);
                        }
                    } else {
                        println!();
                    }
                }
            }
        }
    }

    enums
}

struct Enum<'e> {
    name: &'e str,
    enumerants: Vec<Enumerant>,
}

#[derive(Clone, Debug)]
struct Enumerant {
    name: Box<str>,
    value: Box<str>,
}

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

fn ssc2cc<B>(buffer: &mut Vec<u8>, bytes: B)
where
    B: Iterator<Item = u8>
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
    if let Some(suffix) = suffixes.iter().find(|&&s| s.ends_with(s)) {
        let y = s.len() - suffix.len();
        &s[..y]
    } else {
        &s
    }
}
