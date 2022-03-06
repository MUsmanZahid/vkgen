// Road Map:
// [X] - Write to a file named vk.rs directly. Ask for permission before overwriting an existing
//       one.
// [X] - Emit Vulkan commands
// [X] - Emit Vulkan handles
// [X] - Emit proper type in structures and commands for handles.
// [X] - Emit extensions
//     [X] - Constants
//     [X] - Enumerations
//     [X] - Structures
// [X] - Store all identifiers in memory and emit in an organised format.
// [X] - Bitmask types
//     [X] - Emit
//     [X] - Resolve
// [ ] - Emit `PFN_vk` types.
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
    let result = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(OUTPUT_FILE_NAME);
    let output = match result {
        Ok(f) => Some(f),
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
            // Ask to over-write
            print!(
                "{} already exists. Would you like to over-write? (Y/N): ",
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
        buffer: Vec::with_capacity(1024 * 1024),
        output,
        root: registry.root_element(),
    };

    let registry = Registry::build(registry.root_element());
    generator.emit(registry)
}

pub struct Enum<'e> {
    pub name: &'e str,
    pub variants: Vec<Enumerant<'e>>,
}

impl Enum<'_> {
    pub fn emit(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(b"#[derive(Clone, Copy, Debug)]\n#[repr(C)]\npub enum ");
        vk2rt(self.name, buffer);
        buffer.extend_from_slice(b" {\n");
        vk2rv(self.name, &self.variants, buffer);
        buffer.extend_from_slice(b"}\n");
    }
}

pub struct Registry<'r> {
    pub aliases: Box<[Category<Alias<'r>>]>,
    pub bitmasks: Box<[Category<&'r str>]>,
    pub commands: Box<[Category<Command<'r>>]>,
    pub constants: Box<[Constant<'r>]>,
    pub enums: Box<[Enum<'r>]>,
    pub handles: Box<[&'r str]>,
    pub structures: Box<[Category<Struct<'r>>]>,
}

impl<'r> Registry<'r> {
    pub fn build(root: roxmltree::Node<'r, 'r>) -> Self {
        let extensions = filter(root, "extensions").next().unwrap_or(root);

        macro_rules! categorize {
            ( $iter:expr ) => {
                $iter
                    .map(|t| {
                        let sub_category = ApiCategory::resolve(extensions, t.name);
                        Category {
                            base: t,
                            sub_category,
                        }
                    })
                    .collect()
            };
        }

        let bitmasks = Self::load_bitmasks(root)
            .map(|b| {
                let sub_category = ApiCategory::resolve(extensions, b);
                Category {
                    base: b,
                    sub_category,
                }
            })
            .collect();

        Self {
            aliases: categorize!(Self::load_aliases(root)),
            bitmasks,
            commands: categorize!(Self::load_commands(root)),
            constants: Self::load_constants(root),
            enums: Self::load_enums(root),
            handles: Self::load_handles(root),
            structures: categorize!(Self::load_structures(root)),
        }
    }

    fn load_aliases(root: roxmltree::Node<'r, 'r>) -> impl Iterator<Item = Alias<'r>> {
        // Aliases in the `types` tags
        let types = filter(root, "types").flat_map(|t| filter(t, "type"));

        // Command aliases
        let commands = filter(root, "commands").flat_map(|c| filter(c, "command"));

        types
            .filter_map(Alias::from_node)
            .chain(commands.filter_map(Alias::from_node))
    }

    fn load_bitmasks(root: roxmltree::Node<'r, 'r>) -> impl Iterator<Item = &'r str> {
        filter(root, "types")
            .flat_map(|types| {
                types.children().filter(|child| {
                    (child.tag_name().name() == "type")
                        && matches!(child.attribute("category"), Some("bitmask"))
                })
            })
            .filter_map(|t| {
                t.children()
                    .find(|child| child.tag_name().name() == "name")
                    .and_then(|name| name.text())
            })
    }

    fn load_commands(root: roxmltree::Node<'r, 'r>) -> impl Iterator<Item = Command<'r>> {
        filter(root, "commands")
            .flat_map(|commands| filter(commands, "command"))
            .filter_map(Command::from_node)
    }

    fn load_constants(root: roxmltree::Node<'r, 'r>) -> Box<[Constant<'r>]> {
        let mut constants: Box<[_]> = filter(root, "enums")
            .filter(|e| matches!(e.attribute("name"), Some("API Constants")))
            .flat_map(|constant| filter(constant, "enum").filter_map(Constant::from_node))
            .collect();

        Constant::resolve_types(&mut constants);
        constants
    }

    fn load_enums(root: roxmltree::Node<'r, 'r>) -> Box<[Enum<'r>]> {
        let mut enums: Vec<_> = filter(root, "enums")
            .filter_map(|e| match e.attribute("name") {
                Some(name) if name != "API Constants" => {
                    let variants = filter(e, "enum")
                        .filter_map(Enumerant::from_core_node)
                        .collect();
                    let e = Enum { name, variants };

                    Some(e)
                }
                _ => None,
            })
            .collect();

        // TODO: Insert variants from features

        // Insert variants from extensions
        let extensions = filter(root, "extensions")
            .flat_map(|ext| filter(ext, "extension"))
            .filter(|ext| matches!(ext.attribute("supported"), Some("disabled")))
            .filter_map(|ext| ext.attribute("number").map(|n| (ext, n)));

        for (ext, extnum) in extensions {
            let variants = filter(ext, "require")
                .flat_map(|req| filter(req, "enum"))
                .filter(|e| {
                    let name = e
                        .attribute("name")
                        .map(|n| !(n.ends_with("SPEC_VERSION") || n.ends_with("EXTENSION_NAME")));
                    matches!(name, Some(true))
                })
                .filter_map(|e| {
                    e.attribute("extends")
                        .zip(Enumerant::from_extension_node(e, extnum))
                });

            for (extends, variant) in variants {
                if let Some(i) = enums.iter().position(|e| e.name == extends) {
                    if let Some(e) = enums.get_mut(i) {
                        if e.variants.iter().all(|&v| v != variant) {
                            e.variants.push(variant);
                        }
                    }
                } else {
                    let e = Enum {
                        name: extends,
                        variants: vec![variant],
                    };
                    enums.push(e);
                }
            }
        }

        enums.into_boxed_slice()
    }

    fn load_handles(root: roxmltree::Node<'r, 'r>) -> Box<[&'r str]> {
        filter(root, "types")
            .flat_map(|t| filter(t, "type"))
            .filter_map(|t| match (t.attribute("category"), t.attribute("alias")) {
                (Some(category), None) if category == "handle" => {
                    // Get the nodes' `name` attribute if it exists or find its name from its
                    // children
                    t.attribute("name").or_else(|| {
                        t.children()
                            .find(|c| c.tag_name().name() == "name")
                            .and_then(|n| n.text())
                    })
                }
                _ => None,
            })
            .collect()
    }

    fn load_structures(root: roxmltree::Node<'r, 'r>) -> impl Iterator<Item = Struct<'r>> {
        filter(root, "types")
            .flat_map(|type_node| filter(type_node, "type"))
            .filter(|vk_struct| {
                let category = vk_struct.attribute("category");
                let alias = vk_struct.attribute("alias");

                matches!((category, alias), (Some("struct"), None))
            })
            .filter_map(Struct::from_node)
    }
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

    pub fn emit(mut self, registry: Registry<'i>) -> std::io::Result<()> {
        // Emit imports
        self.buffer
            .extend_from_slice(b"use std::{ffi::c_void, os::raw::c_char};\n\n");

        self.emit_constants(&registry.constants)?;
        self.emit_handles(&registry.handles)?;

        self.emit_structs(&registry.structures, &registry.handles)?;
        self.emit_enums(&registry.enums)?;
        self.emit_bitmasks(&registry.bitmasks)?;
        self.emit_function_pointers()?;
        self.emit_commands(&registry.commands, &registry.handles)?;
        self.emit_aliases(&registry.aliases)?;

        self.emit_extensions(&registry, &registry.handles)?;
        Ok(())
    }

    fn emit_aliases(&mut self, aliases: &[Category<Alias<'i>>]) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Aliases\n");
        aliases.iter().for_each(|alias| {
            if let Category {
                base,
                sub_category: ApiCategory::Core,
            } = alias
            {
                self.buffer.extend_from_slice(b"pub type ");
                vk2rt(base.name, &mut self.buffer);
                self.buffer.extend_from_slice(b" = ");
                vk2rt(base.original_name, &mut self.buffer);
                self.buffer.extend_from_slice(b";\n");
            }
        });

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_bitmasks(&mut self, bitmasks: &[Category<&str>]) -> std::io::Result<()> {
        self.buffer
            .extend_from_slice(b"// Bitmasks\npub type Flags = u32;\n");
        bitmasks
            .iter()
            .filter(|category| matches!(category.sub_category, ApiCategory::Core))
            .for_each(|category| {
                self.buffer.extend_from_slice(b"pub type ");
                vk2rt(category.base, &mut self.buffer);
                self.buffer.extend_from_slice(b" = Flags;\n");
            });

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_commands(
        &mut self,
        commands: &[Category<Command<'i>>],
        handles: &[&str],
    ) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Commands\n");
        commands
            .iter()
            .filter(|category| matches!(category.sub_category, ApiCategory::Core))
            .for_each(|category| category.base.emit(handles, &mut self.buffer));

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_constants(&mut self, constants: &[Constant]) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Constants\n");
        constants.iter().for_each(|c| c.emit(&mut self.buffer));
        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_enums(&mut self, enums: &[Enum]) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Enumerations\n");
        enums.iter().for_each(|e| e.emit(&mut self.buffer));
        self.drain_write_all()
    }

    fn emit_extensions(&mut self, registry: &Registry, handles: &[&str]) -> std::io::Result<()> {
        let extensions = filter(self.root, "extensions")
            .flat_map(|ext| filter(ext, "extension"))
            .filter_map(|ext| {
                if !matches!(ext.attribute("supported"), Some("disabled")) {
                    ext.attribute("name").map(|n| (ext, n))
                } else {
                    None
                }
            });

        for (ext, ext_name) in extensions {
            self.buffer.extend_from_slice(b"// ");
            vk2rt(ext_name, &mut self.buffer);
            self.buffer.push(b'\n');

            let elements = filter(ext, "require").flat_map(|req| {
                req.children().filter(|c| {
                    let name = c.tag_name().name();
                    c.is_element() && ((name == "type") || (name == "enum") || (name == "command"))
                })
            });

            for element in elements {
                let tag_name = element.tag_name().name();
                if let Some(name) = element.attribute("name") {
                    if tag_name == "enum" {
                        if element.attribute("extends").is_none()
                            && name.ends_with("EXTENSION_NAME")
                        {
                            if let Some(value) = element.attribute("value") {
                                let len = name.len() - "_EXTENSION_NAME".len();
                                let trimmed = name.get(3..len).zip(value.get(..value.len() - 1));
                                if let Some((name, value)) = trimmed {
                                    self.buffer.extend_from_slice(b"pub const ");
                                    self.buffer.extend_from_slice(name.as_bytes());
                                    self.buffer.extend_from_slice(b": &str = ");
                                    self.buffer.extend_from_slice(value.as_bytes());
                                    self.buffer.extend(b"\\u{0}\";\n");
                                }
                            }
                        }
                    } else if (tag_name == "type") || (tag_name == "command") {
                        if let Some(Category {
                            base: alias,
                            sub_category: ApiCategory::Extension,
                        }) = registry
                            .aliases
                            .iter()
                            .find(|alias| alias.base.name == name)
                        {
                            self.buffer.extend_from_slice(b"pub type ");
                            vk2rt(alias.name, &mut self.buffer);
                            self.buffer.extend_from_slice(b" = ");
                            vk2rt(alias.original_name, &mut self.buffer);
                            self.buffer.extend_from_slice(b";\n");
                        } else if tag_name == "type" {
                            if let Some(Category {
                                base: s,
                                sub_category: ApiCategory::Extension,
                            }) = registry.structures.iter().find(|s| s.base.name == name)
                            {
                                // These structures are duplicated in the Vulkan XML registry in
                                // the `VK_KHR_swapchain` extension so do not emit them.
                                let exclusions = [
                                    "VkImageSwapchainCreateInfoKHR",
                                    "VkBindImageMemorySwapchainInfoKHR",
                                    "VkAcquireNextImageInfoKHR",
                                    "VkDeviceGroupPresentCapabilitiesKHR",
                                    "VkDeviceGroupPresentInfoKHR",
                                    "VkDeviceGroupSwapchainCreateInfoKHR",
                                ];

                                if !((ext_name == "VK_KHR_swapchain")
                                    && exclusions.iter().any(|&ex| ex == name))
                                {
                                    s.emit(handles, &mut self.buffer);
                                }
                            } else if let Some(Category {
                                base: bitmask,
                                sub_category: ApiCategory::Extension,
                            }) = registry
                                .bitmasks
                                .iter()
                                .find(|category| category.base == name)
                            {
                                self.buffer.extend_from_slice(b"pub type ");
                                vk2rt(bitmask, &mut self.buffer);
                                self.buffer.extend_from_slice(b" = Flags;\n");
                            }
                        } else if tag_name == "command" {
                            if let Some(Category {
                                base: command,
                                sub_category: ApiCategory::Extension,
                            }) = registry.commands.iter().find(|c| c.base.name == name)
                            {
                                command.emit(handles, &mut self.buffer);
                            }
                        }
                    }
                }
            }

            if !self.buffer.ends_with(b"\n\n") {
                self.buffer.push(b'\n');
            }
            self.drain_write_all()?;
        }

        Ok(())
    }

    fn emit_function_pointers(&self) -> std::io::Result<()> {
        let pointers: Vec<String> = self
            .root
            .descendants()
            .filter(|d| {
                (d.tag_name().name() == "type")
                    && matches!(d.attribute("category"), Some("funcpointer"))
            })
            .map(|t| t.children().filter_map(|c| c.text()).collect())
            .collect();

        for pointer in pointers {
            eprintln!("{}", pointer);
            dbg!(CToken::tokenize(&pointer));
        }

        Ok(())
    }

    fn emit_handles(&mut self, handles: &[&str]) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Handles\n");
        for name in handles.iter() {
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

    fn emit_structs(
        &mut self,
        structs: &[Category<Struct>],
        handles: &[&str],
    ) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Structures\n");

        structs
            .iter()
            .filter_map(|c| match c.sub_category {
                ApiCategory::Core => Some(&c.base),
                _ => None,
            })
            .for_each(|s| s.emit(handles, &mut self.buffer));

        self.drain_write_all()
    }
}

pub struct Alias<'a> {
    pub name: &'a str,
    pub original_name: &'a str,
}

impl<'a> Alias<'a> {
    pub fn from_node(node: roxmltree::Node<'a, 'a>) -> Option<Self> {
        let alias = node.attribute("alias")?;
        let name = node.attribute("name")?;

        let a = Alias {
            name,
            original_name: alias,
        };
        Some(a)
    }
}

pub struct Category<T> {
    base: T,
    sub_category: ApiCategory,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ApiCategory {
    Core,
    Extension,
    Feature,
}

impl ApiCategory {
    pub fn resolve(node: roxmltree::Node, name: &str) -> Self {
        let is_extension = node
            .descendants()
            .filter(|d| {
                let name = d.tag_name().name();
                d.is_element() && ((name == "type") || (name == "enum") || (name == "command"))
            })
            .any(|child| match child.attribute("name") {
                Some(child_name) => child_name == name,
                None => false,
            });

        if is_extension {
            Self::Extension
        } else {
            Self::Core
        }
    }
}

#[derive(Clone, Debug)]
pub struct Command<'c> {
    name: &'c str,
    return_type: String,
    parameters: Vec<FunctionSection<'c>>,
}

impl<'c> Command<'c> {
    pub fn emit(&self, handles: &[&str], buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(b"pub type ");
        vk2rt(self.name, buffer);

        buffer.extend_from_slice(b" = unsafe extern \"system\" fn(");
        self.parameters.iter().enumerate().for_each(|(i, section)| {
            if i != 0 {
                buffer.extend_from_slice(b", ");
            }

            vk2rm(buffer, section.name);
            buffer.extend_from_slice(b": ");
            let tokens = CToken::tokenize_and_resolve_handles(&section.type_name, handles);
            ct2rt(buffer, tokens);
        });

        buffer.push(b')');

        if self.return_type.trim() != "void" {
            buffer.extend_from_slice(b" -> ");
            let tokens = CToken::tokenize_and_resolve_handles(&self.return_type, handles);
            ct2rt(buffer, tokens);
        }
        buffer.extend_from_slice(b";\n");
    }

    pub fn from_node(node: roxmltree::Node<'c, 'c>) -> Option<Self> {
        // There are two forms of the command tag:
        // 1. Defines a command alias with the only attributes being `name` and `alias`.
        // 2. Full definition of a command with its name, return type, and list of parameters.

        // Do not emit a command if it is an alias.
        if node.attribute("alias").is_some() {
            return None;
        }

        let FunctionSection {
            name,
            type_name: return_type,
        } = node
            .children()
            .find(|child| child.tag_name().name() == "proto")
            .and_then(FunctionSection::extract)?;

        let parameters = node
            .children()
            .filter(|child| child.tag_name().name() == "param")
            .filter_map(FunctionSection::extract)
            .collect();

        let command = Self {
            name,
            return_type,
            parameters,
        };
        Some(command)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Constant<'c> {
    name: &'c str,
    rtype: &'static str,
    value: NonExtensionVariant<'c>,
}

impl<'c> Constant<'c> {
    pub fn emit(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(b"pub const ");
        let name = if self.name.starts_with("VK_") {
            self.name.get(3..).unwrap_or(self.name)
        } else {
            self.name
        };
        buffer.extend_from_slice(name.as_bytes());
        buffer.extend_from_slice(b": ");
        buffer.extend_from_slice(self.rtype.as_bytes());
        buffer.extend_from_slice(b" = ");

        if let NonExtensionVariant::Alias(a) = self.value {
            let value = if a.starts_with("VK_") {
                a.get(3..).unwrap_or(a)
            } else {
                a
            };
            buffer.extend_from_slice(value.as_bytes());
        } else {
            let value = self.value.sanitize();
            buffer.extend_from_slice(value.as_bytes());
        }
        buffer.extend_from_slice(b";\n");
    }

    pub fn from_node(node: roxmltree::Node<'c, 'c>) -> Option<Self> {
        node.attribute("name")
            .zip(NonExtensionVariant::from_node(node))
            .map(|(name, value)| Self {
                name,
                rtype: "u32",
                value,
            })
    }

    pub fn resolve_types(c: &mut [Self]) {
        for constant in c.iter_mut() {
            let mut ctype = "u32";

            if let NonExtensionVariant::Integer(i) = constant.value {
                if i.contains("ULL") {
                    ctype = "u64";
                } else if i.bytes().any(|b| b == b'f') {
                    ctype = "f32";
                }

                if constant.name.contains("SIZE") {
                    ctype = "usize";
                }
            }

            constant.rtype = ctype;
        }

        let aliases: Vec<_> = c
            .iter()
            .enumerate()
            .filter_map(|(i, constant)| match constant.value {
                NonExtensionVariant::Alias(alias) => {
                    c.iter().find(|c| c.name == alias).map(|c| (i, c.rtype))
                }
                _ => None,
            })
            .collect();

        for (i, ctype) in aliases {
            if let Some(a) = c.get_mut(i) {
                a.rtype = ctype;
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum NonExtensionVariant<'v> {
    Alias(&'v str),
    BitPosition(u32),
    Integer(&'v str),
}

impl<'v> NonExtensionVariant<'v> {
    pub fn from_node(node: roxmltree::Node<'v, 'v>) -> Option<Self> {
        let mut result = None;

        if node.tag_name().name() == "enum" {
            if let Some(value) = node.attribute("value") {
                result = Some(Self::Integer(value));
            } else if let Some(b) = node.attribute("bitpos") {
                result = b.parse().ok().map(Self::BitPosition);
            } else if let Some(alias) = node.attribute("alias") {
                result = Some(Self::Alias(alias));
            }
        }

        result
    }

    pub fn sanitize(&self) -> std::borrow::Cow<'v, str> {
        use std::borrow::Cow;

        match self {
            Self::Alias(a) => Cow::Borrowed(a),
            Self::BitPosition(b) => Cow::Owned(format!("0x{:X}", 0x1 << b)),
            Self::Integer(i) => {
                let mut result = Cow::Borrowed(*i);

                if result.contains("ULL") {
                    result = Cow::Owned(result.replace("ULL", ""));
                }

                if result.bytes().any(|b| b == b'f') {
                    result = Cow::Owned(result.replace('f', ""));
                }

                if result.bytes().any(|b| b == b'U') {
                    result = Cow::Owned(result.replace('U', ""));
                }

                if i.bytes().any(|b| b == b'~') {
                    result = Cow::Owned(result.replace('~', "!"));
                }

                result
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Struct<'s> {
    name: &'s str,
    members: Vec<Member<'s>>,
}

impl<'s> Struct<'s> {
    pub fn emit(&self, handles: &[&str], buffer: &mut Vec<u8>) {
        // Make a note of the length of the buffer. We might need to bail later.
        let len = buffer.len();

        buffer.extend_from_slice(b"#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ");
        vk2rt(self.name, buffer);
        buffer.extend_from_slice(b" {\n");

        for member in self.members.iter() {
            buffer.extend_from_slice(b"    pub ");
            vk2rm(buffer, member.name);
            buffer.extend_from_slice(b": ");

            let tokens = CToken::tokenize_and_resolve_handles(&member.ctype, handles);
            if tokens.iter().any(|&token| token == CToken::Colon) {
                buffer.truncate(len);
                return;
            }

            ct2rt(buffer, tokens);
            buffer.extend_from_slice(b",\n");
        }

        buffer.extend_from_slice(b"}\n\n");
    }

    pub fn from_node(node: roxmltree::Node<'s, 's>) -> Option<Self> {
        let name = node.attribute("name")?;

        // Do not emit these placeholder structures
        if (name == "VkBaseInStructure") || (name == "VkBaseOutStructure") {
            return None;
        }

        let members = filter(node, "member")
            .filter_map(Member::from_node)
            .collect();

        let s = Self { name, members };
        Some(s)
    }
}

#[derive(Clone, Debug)]
pub struct Member<'m> {
    name: &'m str,
    ctype: String,
}

impl<'m> Member<'m> {
    pub fn from_node(node: roxmltree::Node<'m, 'm>) -> Option<Self> {
        // Find all text and type nodes that constitute the full type of the member
        let type_nodes = node
            .children()
            .filter(|c| {
                let name = c.tag_name().name();
                c.is_text() || ((name != "name") && (name != "comment"))
            })
            .filter_map(|t| t.text());

        // Find the name of the member
        let name = node
            .children()
            .find(|c| c.tag_name().name() == "name")
            .map(|m| m.text())
            .flatten();

        name.map(|name| Self {
            name,
            ctype: type_nodes.collect(),
        })
    }
}

/// A function section is a name associated with a type.
///
/// A function section ties two sets of things together:
/// 1. the function name and its return type, and
/// 2. a function member name and its type.
#[derive(Clone, Debug)]
pub struct FunctionSection<'s> {
    pub name: &'s str,
    pub type_name: String,
}

impl<'s> FunctionSection<'s> {
    /// Extract a Vulkan function section.
    ///
    /// A function section can be a pair of either:
    /// 1. The function name and its return value, or
    /// 2. The name of one of the function's parameters and its corresponding type.
    ///
    /// The type is allowed to be arbitrary C code which is why the full return type of the
    /// function can be contained in XML text node siblings of the `name` and `type` nodes.
    pub fn extract(node: roxmltree::Node<'s, 's>) -> Option<Self> {
        let name = node
            .children()
            .find(|child| child.is_element() && (child.tag_name().name() == "name"))
            .and_then(|name| name.text())?;
        let type_name = node
            .children()
            .filter(|c| c.is_text() || (c.tag_name().name() == "type"))
            .filter_map(|t| t.text())
            .collect();

        let section = Self { name, type_name };
        Some(section)
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
        if let Some(extnum) = ext.attribute("number") {
            let variants = filter(ext, "require")
                .flat_map(|req| filter(req, "enum"))
                .filter(|e| {
                    let name = e
                        .attribute("name")
                        .map(|n| !n.ends_with("SPEC_VERSION") && !n.ends_with("EXTENSION_NAME"));
                    matches!(name, Some(true))
                })
                .filter_map(|e| {
                    e.attribute("extends")
                        .zip(Enumerant::from_extension_node(e, extnum))
                });

            for (extends, variant) in variants {
                if let Some(value) = map.get_mut(extends) {
                    if value.iter().all(|&v| v != variant) {
                        value.push(variant);
                    }
                } else {
                    let mut value = Vec::with_capacity(256);
                    value.push(variant);
                    map.insert(extends, value);
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Enumerant<'e> {
    pub name: &'e str,
    pub value: Variant<'e>,
}

impl<'e> Enumerant<'e> {
    pub fn from_core_node(node: roxmltree::Node<'e, 'e>) -> Option<Self> {
        node.attribute("name")
            .zip(Variant::from_core_node(node))
            .map(|(name, value)| Self { name, value })
    }

    pub fn from_extension_node(node: roxmltree::Node<'e, 'e>, ext_num: &'e str) -> Option<Self> {
        Self::from_core_node(node).or_else(|| {
            node.attribute("name")
                .zip(Variant::from_extension_node(node, ext_num))
                .map(|(name, value)| Self { name, value })
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Variant<'v> {
    Alias(&'v str),
    BitPos(u32),
    Extension {
        number: &'v str,
        offset: &'v str,
        direction: Option<&'v str>,
    },
    Integer(&'v str),
}

impl<'v> Variant<'v> {
    pub fn from_core_node(node: roxmltree::Node<'v, 'v>) -> Option<Self> {
        if let Some(value) = node.attribute("value") {
            Some(Self::Integer(value))
        } else if let Some(b) = node.attribute("bitpos") {
            b.parse().ok().map(Self::BitPos)
        } else {
            node.attribute("alias").map(Self::Alias)
        }
    }

    pub fn from_extension_node(node: roxmltree::Node<'v, 'v>, ext_num: &'v str) -> Option<Self> {
        Self::from_core_node(node).or_else(|| {
            node.attribute("offset").map(|offset| {
                let number = node.attribute("extnumber").unwrap_or(ext_num);
                let direction = node.attribute("dir");

                Self::Extension {
                    number,
                    offset,
                    direction,
                }
            })
        })
    }

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
                let mut result = Cow::Borrowed(*i);

                if result.contains("ULL") {
                    result = Cow::Owned(result.replace("ULL", ""));
                }

                if result.bytes().any(|b| b == b'f') {
                    result = Cow::Owned(result.replace('f', ""));
                }

                if result.bytes().any(|b| b == b'U') {
                    result = Cow::Owned(result.replace('U', ""));
                }

                if i.bytes().any(|b| b == b'~') {
                    result = Cow::Owned(result.replace('~', "!"));
                }

                result
            }
        }
    }
}

fn filter<'b, 'n>(
    node: roxmltree::Node<'b, 'n>,
    name: &'b str,
) -> impl Iterator<Item = roxmltree::Node<'b, 'n>> {
    node.children()
        .filter(move |child| child.is_element() && (name == child.tag_name().name()))
}

/// Convert a list of C tokens to a Rust type.
fn ct2rt(buffer: &mut Vec<u8>, mut tokens: Vec<CToken>) {
    if (tokens.len() == 1)
        || tokens
            .iter()
            .any(|&t| (t == CToken::BracketLeft) && (t == CToken::Colon))
    {
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
            }
        }
    } else if let Some(i) = tokens
        .iter()
        .position(|&t| matches!(t, CToken::Identifier(_)))
    {
        // Moving the identifier to the end turns the C pointer type into a Rust pointer type.
        let last = tokens.len() - 1;
        tokens.swap(i, last);

        // Pointers
        let mut is_const = false;
        for t in tokens.iter() {
            if *t == CToken::Pointer {
                if is_const {
                    buffer.extend_from_slice(b"*const ");
                    is_const = false;
                } else {
                    buffer.extend_from_slice(b"*mut ");
                }
            } else {
                is_const = *t == CToken::Const;
            }
        }

        if let Some(CToken::Identifier(identifier)) = tokens.last() {
            vk2rt(identifier, buffer);
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CToken<'t> {
    BracketLeft,
    BracketRight,
    Colon,
    Comma,
    Const,
    Enum,
    Identifier(&'t str),
    Literal(&'t str),
    ParenLeft,
    ParenRight,
    Pointer,
    SemiColon,
    Struct,
    TypeDef,
    Union,
    Void,
    Volatile,
}

impl<'t> CToken<'t> {
    pub fn tokenize(mut t: &'t str) -> Vec<Self> {
        t = t.trim();
        let mut tokens = Vec::with_capacity(8);

        // Keeping track of sub-slices of our string for identifiers
        let mut start = 0;
        let mut end = start;

        for &byte in t.as_bytes() {
            if let Some(token) = Self::parse_byte(byte) {
                if start < end {
                    tokens.push(Self::parse(&t[start..end]));
                }

                tokens.push(token);
                end += 1;
                start = end;
            } else if byte.is_ascii_whitespace() {
                if start < end {
                    tokens.push(Self::parse(&t[start..end]));
                }

                end += 1;
                start = end;
            } else {
                end += 1;
            }
        }

        // Handle potentially left-over token at the end of string
        if start < end {
            tokens.push(Self::parse(&t[start..end]));
        }

        tokens
    }

    pub fn parse_byte(byte: u8) -> Option<Self> {
        match byte {
            b'[' => Some(Self::BracketLeft),
            b']' => Some(Self::BracketRight),
            b',' => Some(Self::Comma),
            b':' => Some(Self::Colon),
            b'(' => Some(Self::ParenLeft),
            b')' => Some(Self::ParenRight),
            b'*' => Some(Self::Pointer),
            b';' => Some(Self::SemiColon),
            _ => None,
        }
    }

    pub fn parse(bytes: &'t str) -> Self {
        match bytes {
            "const" => Self::Const,
            "enum" => Self::Enum,
            "struct" => Self::Struct,
            "typedef" => Self::TypeDef,
            "union" => Self::Union,
            "void" => Self::Void,
            "volatile" => Self::Volatile,
            s if s.as_bytes().iter().all(|b| b.is_ascii_digit()) => Self::Literal(bytes),
            _ => Self::Identifier(bytes),
        }
    }

    pub fn tokenize_and_resolve_handles(t: &'t str, handles: &[&str]) -> Vec<Self> {
        let mut tokens = CToken::tokenize(t);

        // If we have an identifier that resolves to a handle, we have to add a pointer to it since
        // the original Vulkan api hides the fact that handles are pointers and we do not.
        let handle_position = tokens.iter().position(|token| {
            handles.iter().any(|handle| match token {
                CToken::Identifier(i) => i == handle,
                _ => false,
            })
        });

        // Found a handle
        if let Some(i) = handle_position {
            // `i + 1` since C adds pointers after the identifier
            tokens.insert(i + 1, CToken::Pointer);
        }

        tokens
    }
}

/// Convert a Vulkan structure member name to a Rust structure member name
fn vk2rm(buffer: &mut Vec<u8>, mut name: &str) {
    if name == "sType" {
        // Special case for `sType`. We cannot truncate to `type` since it is a Rust keyword so we
        // keep the prefix.
        buffer.extend_from_slice(b"stype");
    } else if name == "type" {
        buffer.extend_from_slice(b"mtype");
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
            let one_letter_prefix = (lowers == 1) && (prefix != "x") && (prefix != "y");
            if !rem.is_empty() && (one_letter_prefix || prefixes.iter().any(|&p| p == prefix)) {
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

        if let Variant::Alias(_) = v.value {
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
        if let Variant::Alias(alias) = v.value {
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
