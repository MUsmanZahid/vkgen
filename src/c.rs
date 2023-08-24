#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Token<'t> {
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
    Volatile,
}

impl<'t> Token<'t> {
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
            "volatile" => Self::Volatile,
            s if s.as_bytes().iter().all(|b| b.is_ascii_digit()) => Self::Literal(bytes),
            _ => Self::Identifier(bytes),
        }
    }

    pub fn tokenize_and_resolve_handles(t: &'t str, handles: &[&str]) -> Vec<Self> {
        let mut tokens = Self::tokenize(t);

        // If we have an identifier that resolves to a handle, we have to add a pointer to it since
        // the original Vulkan api hides the fact that handles are pointers and we do not.
        let handle_position = tokens.iter().position(|token| {
            handles.iter().any(|handle| match token {
                Self::Identifier(i) => i == handle,
                _ => false,
            })
        });

        // Found a handle
        if let Some(i) = handle_position {
            // `i + 1` since C adds pointers after the identifier
            tokens.insert(i + 1, Self::Pointer);
        }

        tokens
    }
}
