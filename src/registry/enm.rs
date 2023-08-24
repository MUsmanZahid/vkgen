pub struct Enum<'e> {
    pub name: &'e str,
    pub variants: Vec<Enumerant<'e>>,
}

impl Enum<'_> {
    pub fn emit(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(b"#[derive(Clone, Copy, Debug)]\n#[repr(C)]\npub enum ");
        crate::conv::vk2rt(self.name, buffer);
        buffer.extend_from_slice(b" {\n");
        crate::conv::vk2rv(self.name, &self.variants, buffer);
        buffer.extend_from_slice(b"}\n\n");
    }
}

fn _generate_extended_enums<'n>(
    root: roxmltree::Node<'n, 'n>,
    map: &mut std::collections::HashMap<&'n str, Vec<Enumerant<'n>>>,
) {
    let extensions = crate::filter(root, "extensions")
        .flat_map(|ext| crate::filter(ext, "extension"))
        .filter(|ext| !matches!(ext.attribute("supported"), Some("disabled")));

    for ext in extensions {
        if let Some(extnum) = ext.attribute("number") {
            let variants = crate::filter(ext, "require")
                .flat_map(|req| crate::filter(req, "enum"))
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
            Self::BitPos(b) => Cow::Owned(format!("0x{:X}", 0x1u64 << b)),
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
