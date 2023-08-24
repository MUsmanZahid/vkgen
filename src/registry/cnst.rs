#[derive(Clone, Copy, Debug)]
pub struct Constant<'c> {
    pub name: &'c str,
    pub rtype: &'static str,
    pub value: NonExtensionVariant<'c>,
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
