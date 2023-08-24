pub mod cmd;
pub mod cnst;
pub mod enm;
pub mod strct;

use crate::filter;

pub struct Registry<'r> {
    pub aliases: Box<[Category<Alias<'r>>]>,
    pub bitmasks: Box<[Category<&'r str>]>,
    pub commands: Box<[Category<cmd::Command<'r>>]>,
    pub constants: Box<[cnst::Constant<'r>]>,
    pub enums: Box<[enm::Enum<'r>]>,
    pub handles: Box<[&'r str]>,
    pub structures: Box<[Category<strct::Struct<'r>>]>,
    pub extensions: Box<[Extension<'r>]>,
}

impl<'r> Registry<'r> {
    pub fn build(root: roxmltree::Node<'r, 'r>) -> Self {
        // NOTE: Used in the macro below
        #[allow(unused_variables)]
        let extensions = filter(root, "extensions").next().unwrap_or(root);

        macro_rules! categorize {
            ( $iter:expr $(, $field:ident)? ) => {
                $iter
                    .map(|t| {
                        let sub_category = ApiCategory::resolve(extensions, t$(.$field)?);
                        Category {
                            base: t,
                            sub_category,
                        }
                    })
                    .collect()
            };
        }

        let aliases: Box<[_]> = categorize!(Self::load_aliases(root), name);
        let bitmasks: Box<[_]> = categorize!(Self::load_bitmasks(root));
        let commands: Box<[_]> = categorize!(Self::load_commands(root), name);
        let structures: Box<[_]> = categorize!(Self::load_structures(root), name);

        let extensions = Self::load_extensions(root, &aliases, &structures, &bitmasks, &commands);
        Self {
            aliases,
            bitmasks,
            commands,
            constants: Self::load_constants(root),
            enums: Self::load_enums(root),
            handles: Self::load_handles(root),
            structures,
            extensions,
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

    fn load_commands(root: roxmltree::Node<'r, 'r>) -> impl Iterator<Item = cmd::Command<'r>> {
        filter(root, "commands")
            .flat_map(|commands| filter(commands, "command"))
            .filter_map(cmd::Command::from_node)
    }

    fn load_constants(root: roxmltree::Node<'r, 'r>) -> Box<[cnst::Constant<'r>]> {
        let mut constants: Box<[_]> = filter(root, "enums")
            .filter(|e| matches!(e.attribute("name"), Some("API Constants")))
            .flat_map(|constant| filter(constant, "enum").filter_map(cnst::Constant::from_node))
            .collect();

        cnst::Constant::resolve_types(&mut constants);
        constants
    }

    fn load_enums(root: roxmltree::Node<'r, 'r>) -> Box<[enm::Enum<'r>]> {
        let mut enums: Vec<_> = filter(root, "enums")
            .filter_map(|e| match e.attribute("name") {
                Some(name) if name != "API Constants" => {
                    let variants = filter(e, "enum")
                        .filter_map(enm::Enumerant::from_core_node)
                        .collect();
                    let e = enm::Enum { name, variants };

                    Some(e)
                }
                _ => None,
            })
            .collect();

        macro_rules! insert {
            ( $iter:expr ) => {
                for (extends, variant) in $iter {
                    if let Some(i) = enums.iter().position(|e| e.name == extends) {
                        if let Some(e) = enums.get_mut(i) {
                            if e.variants.iter().all(|&v| v != variant) {
                                e.variants.push(variant);
                            }
                        }
                    } else {
                        let e = enm::Enum {
                            name: extends,
                            variants: vec![variant],
                        };
                        enums.push(e);
                    }
                }
            };
        }

        // Insert variants from features
        let features = filter(root, "feature")
            .flat_map(|feature| {
                feature
                    .descendants()
                    .filter(|d| d.tag_name().name() == "enum")
            })
            .filter_map(|e| {
                e.attribute("extends")
                    .zip(enm::Enumerant::from_extension_node(e, "0"))
            });
        insert!(features);

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
                        .zip(enm::Enumerant::from_extension_node(e, extnum))
                });

            insert!(variants);
        }

        enums.into_boxed_slice()
    }

    fn load_extensions(
        root: roxmltree::Node<'r, 'r>,
        aliases: &[Category<Alias<'r>>],
        structures: &[Category<strct::Struct<'r>>],
        bitmasks: &[Category<&'r str>],
        commands: &[Category<cmd::Command<'r>>],
    ) -> Box<[Extension<'r>]> {
        let extensions = filter(root, "extensions")
            .flat_map(|ext| filter(ext, "extension"))
            .filter_map(|ext| {
                if !matches!(ext.attribute("supported"), Some("disabled")) {
                    ext.attribute("name").map(|n| (ext, n))
                } else {
                    None
                }
            });

        extensions
            .map(|(ext, ext_name)| {
                let elements = filter(ext, "require")
                    .flat_map(|req| {
                        req.children().filter(|c| {
                            let name = c.tag_name().name();
                            c.is_element()
                                && ((name == "type") || (name == "enum") || (name == "command"))
                        })
                    })
                    .filter_map(|element| element.attribute("name").map(|name| (element, name)));

                let mut ext_id = None;
                let mut ext_aliases = Vec::new();
                let mut ext_flags = Vec::new();
                let mut ext_structs = Vec::new();
                let mut ext_cmds = Vec::new();

                for (element, name) in elements {
                    let tag_name = element.tag_name().name();
                    if tag_name == "enum" && name.ends_with("EXTENSION_NAME") {
                        let name = {
                            let len = name.len() - "_EXTENSION_NAME".len();
                            name.get(3..len)
                        };

                        // Do not set ext_id if this is an aliased extension
                        if !element.has_attribute("alias") {
                            let value = element.attribute("value");
                            ext_id = name
                                .zip(value)
                                .map(|(name, value)| ExtensionId { name, value });
                        }
                    } else if (tag_name == "type") || (tag_name == "command") {
                        if let Some(Category {
                            base: alias,
                            sub_category: ApiCategory::Extension,
                        }) = aliases.iter().find(|alias| alias.base.name == name)
                        {
                            ext_aliases.push(*alias);
                        } else if tag_name == "type" {
                            if let Some(Category {
                                base: s,
                                sub_category: ApiCategory::Extension,
                            }) = structures.iter().find(|s| s.base.name == name)
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
                                    ext_structs.push(s.clone());
                                }
                            } else if let Some(Category {
                                base: bitmask,
                                sub_category: ApiCategory::Extension,
                            }) = bitmasks.iter().find(|category| category.base == name)
                            {
                                ext_flags.push(*bitmask);
                            }
                        } else if tag_name == "command" {
                            if let Some(Category {
                                base: command,
                                sub_category: ApiCategory::Extension,
                            }) = commands.iter().find(|c| c.base.name == name)
                            {
                                ext_cmds.push(command.clone());
                            }
                        }
                    }
                }

                Extension {
                    name: ext_name,
                    id: ext_id.expect("No identifier for extension found!"),
                    flags: ext_flags,
                    structs: ext_structs,
                    cmds: ext_cmds,
                    aliases: ext_aliases,
                }
            })
            .collect()
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

    fn load_structures(root: roxmltree::Node<'r, 'r>) -> impl Iterator<Item = strct::Struct<'r>> {
        filter(root, "types")
            .flat_map(|type_node| filter(type_node, "type"))
            .filter(|vk_struct| {
                let category = vk_struct.attribute("category");
                let alias = vk_struct.attribute("alias");

                matches!((category, alias), (Some("struct"), None))
            })
            .filter_map(strct::Struct::from_node)
    }
}

#[derive(Clone, Copy)]
pub struct Alias<'a> {
    pub name: &'a str,
    pub original_name: &'a str,
}

impl<'a> Alias<'a> {
    pub fn emit(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(b"pub type ");
        crate::conv::vk2rt(self.name, buffer);
        buffer.extend_from_slice(b" = ");
        crate::conv::vk2rt(self.original_name, buffer);
        buffer.extend_from_slice(b";\n");
    }

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

#[derive(Debug)]
pub struct Category<T> {
    pub base: T,
    pub sub_category: ApiCategory,
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

pub struct Extension<'e> {
    name: &'e str,
    id: ExtensionId<'e>,
    flags: Vec<&'e str>,
    structs: Vec<strct::Struct<'e>>,
    cmds: Vec<cmd::Command<'e>>,
    aliases: Vec<Alias<'e>>,
}

impl Extension<'_> {
    pub fn emit(&self, handles: &[&str], buffer: &mut Vec<u8>) {
        let name_and_identifier = [
            "// ",
            self.name,
            "\npub const ",
            self.id.name,
            ": &str = ",
            self.id.value,
            ";\n\n",
        ];
        name_and_identifier
            .iter()
            .for_each(|item| buffer.extend_from_slice(item.as_bytes()));

        if !self.flags.is_empty() {
            self.flags.iter().for_each(|flag| {
                let items = ["pub type ", flag, " = Flags;\n"];
                items
                    .iter()
                    .for_each(|item| buffer.extend_from_slice(item.as_bytes()));
            });
            buffer.push(b'\n');
        }

        self.structs.iter().for_each(|s| s.emit(handles, buffer));

        if !self.cmds.is_empty() {
            self.cmds.iter().for_each(|cmd| cmd.emit(handles, buffer));
            buffer.push(b'\n');
        }

        if !self.aliases.is_empty() {
            self.aliases.iter().for_each(|alias| alias.emit(buffer));
            buffer.push(b'\n');
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ExtensionId<'i> {
    name: &'i str,
    value: &'i str,
}
