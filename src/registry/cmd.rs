const DEVICE_CHILD: [&str; 3] = ["VkDevice", "VkQueue", "VkCommandBuffer"];

#[derive(Clone, Debug)]
pub struct Command<'c> {
    pub name: &'c str,
    pub return_type: String,
    pub parameters: Vec<FunctionSection<'c>>,
}

impl<'c> Command<'c> {
    pub fn is_device_level(&self) -> bool {
        match self.parameters.first() {
            Some(first) => DEVICE_CHILD.iter().any(|&c| c == first.type_name.trim()),
            None => false,
        }
    }

    pub fn emit(&self, handles: &[&str], buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(b"pub type ");
        crate::conv::vk2rt(self.name, buffer);

        buffer.extend_from_slice(b" = unsafe extern \"system\" fn(");
        self.parameters.iter().enumerate().for_each(|(i, section)| {
            if i != 0 {
                buffer.extend_from_slice(b", ");
            }

            crate::conv::vk2rm(buffer, section.name);
            buffer.extend_from_slice(b": ");
            let tokens = crate::c::Token::tokenize_and_resolve_handles(&section.type_name, handles);
            crate::conv::ct2rt(buffer, tokens);
        });

        buffer.push(b')');

        if self.return_type.trim() != "void" {
            buffer.extend_from_slice(b" -> ");
            let tokens = crate::c::Token::tokenize_and_resolve_handles(&self.return_type, handles);
            crate::conv::ct2rt(buffer, tokens);
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
