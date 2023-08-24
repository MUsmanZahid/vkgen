#[derive(Clone, Debug)]
pub struct Struct<'s> {
    pub name: &'s str,
    pub members: Vec<Member<'s>>,
}

impl<'s> Struct<'s> {
    pub fn emit(&self, handles: &[&str], buffer: &mut Vec<u8>) {
        // Make a note of the length of the buffer. We might need to bail later.
        let len = buffer.len();

        buffer.extend_from_slice(b"#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ");
        crate::conv::vk2rt(self.name, buffer);
        buffer.extend_from_slice(b" {\n");

        for member in self.members.iter() {
            buffer.extend_from_slice(b"    pub ");
            crate::conv::vk2rm(buffer, member.name);
            buffer.extend_from_slice(b": ");

            let tokens = crate::c::Token::tokenize_and_resolve_handles(&member.ctype, handles);

            // TODO: Add bitfield support
            // Need to bail as we do not support emitting bitfield types from C
            if tokens.iter().any(|&token| token == crate::c::Token::Colon) {
                return buffer.truncate(len);
            } else {
                crate::conv::ct2rt(buffer, tokens);
                buffer.extend_from_slice(b",\n");
            }
        }

        buffer.extend_from_slice(b"}\n\n");
    }

    pub fn from_node(node: roxmltree::Node<'s, 's>) -> Option<Self> {
        let name = node.attribute("name")?;

        // Do not emit these placeholder structures
        if (name == "VkBaseInStructure") || (name == "VkBaseOutStructure") {
            None
        } else {
            let members = crate::filter(node, "member")
                .filter_map(Member::from_node)
                .collect();

            let s = Self { name, members };
            Some(s)
        }
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
            .and_then(|m| m.text());

        name.map(|name| Self {
            name,
            ctype: type_nodes.collect(),
        })
    }
}
