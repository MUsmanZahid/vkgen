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
    let _enums = generate_enums(&root);
}

fn generate_enums<'n, 's>(root: &'n roxmltree::Node<'n, 's>) -> Vec<Enum<'s>> {
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

fn types_path<'n, 'p>(types: &'p roxmltree::Node<'n, 'p>) -> Vec<Enum<'p>> {
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
    enumerants: Vec<Enumerant<'e>>,
}

struct Enumerant<'e> {
    name: &'e str,
    value: &'e str,
}

