const BASIC_C_TYPE: [&str; 12] = [
    "void", "char", "float", "double", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int32_t",
    "int64_t", "size_t", "int",
];
const BASIC_RUST_TYPE: [&str; 12] = [
    "c_void", "c_char", "f32", "f64", "u8", "u16", "u32", "u64", "i32", "i64", "usize", "i32",
];

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

/// Convert a string from camelCase to snake_case.
pub fn cc2sc<B>(buffer: &mut Vec<u8>, name: B)
where
    B: Iterator<Item = u8>,
{
    let mut was_upper = false;
    for (i, c) in name.enumerate() {
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

/// Convert a list of C tokens to a Rust type.
pub fn ct2rt(buffer: &mut Vec<u8>, mut tokens: Vec<crate::c::Token>) {
    if (tokens.len() == 1)
        || tokens
            .iter()
            .any(|&t| (t == crate::c::Token::BracketLeft) || (t == crate::c::Token::Colon))
    {
        let search = tokens.iter().find_map(|t| {
            if let crate::c::Token::Identifier(s) = t {
                Some(s)
            } else {
                None
            }
        });

        if let Some(identifier) = search {
            if tokens.len() == 1 {
                vk2rt(identifier, buffer);
            } else if tokens.iter().any(|t| *t == crate::c::Token::Colon) {
                todo!("Bitfields!");
            } else if tokens.iter().any(|t| *t == crate::c::Token::BracketLeft) {
                // Array
                let mut stack = Vec::with_capacity(8);
                for t in tokens.iter() {
                    if let crate::c::Token::Literal(l) = t {
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
        .position(|&t| matches!(t, crate::c::Token::Identifier(_)))
    {
        // Moving the identifier to the end turns the C pointer type into a Rust pointer type.
        let last = tokens.len() - 1;
        tokens.swap(i, last);

        // Pointers
        let mut is_const = false;
        for t in tokens.iter() {
            if *t == crate::c::Token::Pointer {
                if is_const {
                    buffer.extend_from_slice(b"*const ");
                    is_const = false;
                } else {
                    buffer.extend_from_slice(b"*mut ");
                }
            } else {
                is_const = *t == crate::c::Token::Const;
            }
        }

        if let Some(crate::c::Token::Identifier(identifier)) = tokens.last() {
            vk2rt(identifier, buffer);
        }
    }
}

/// Convert digits to words.
///
/// Takes the string representation of a numeral and converts it into words. Output is written into
/// buffer directly.
///
/// # Example
/// d2w(&mut buffer, 128) # OneHundredTwentyEight
pub fn d2w(buffer: &mut Vec<u8>, digits: &[u8]) {
    let units = [
        "", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
    ];
    let tens = [
        "", "Ten", "Twenty", "Thirty", "Fourty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety",
    ];
    let teens = [
        "Ten",
        "Eleven",
        "Twelve",
        "Thirteen",
        "Fourteen",
        "Fifteen",
        "Sixteen",
        "Seventeen",
        "Eighteen",
        "Nineteen",
    ];
    let suffix = ["", "", "Hundred", "Thousand"];

    let len = digits.len();
    if len > 4 {
        panic!("[d2w]: Cannot yet convert numbers greater than 4 digits in size!");
    } else {
        for (i, byte) in digits.iter().enumerate() {
            let digit = (byte - b'0') as usize;
            let digit_num = len - i - 1;
            let prefix = if digit_num == 1 {
                if digit == 1 {
                    // In the 10-19 range
                    buffer.extend_from_slice(teens[(digits[len - 1] - b'0') as usize].as_bytes());
                    break;
                } else {
                    tens
                }
            } else {
                units
            };

            buffer.extend_from_slice(prefix[digit].as_bytes());
            buffer.extend_from_slice(suffix[digit_num].as_bytes());
        }
    }
}

/// Convert an iterator over bytes, `B`, from PascalCase to SCREAMING_SNAKE_CASE storing it in
/// `buffer`.
pub fn pc2ssc<B>(buffer: &mut Vec<u8>, name: B)
where
    B: Iterator<Item = u8>,
{
    for (i, c) in name.enumerate() {
        if c.is_ascii_uppercase() {
            if i > 0 {
                buffer.push(b'_');
            }
            buffer.push(c);
        } else {
            buffer.push(c.to_ascii_uppercase());
        }
    }
}

/// Convert an iterator over bytes, `B`, from SCREAMING_SNAKE_CASE to PascalCase storing it in
/// `buffer`.
pub fn ssc2pc<B>(buffer: &mut Vec<u8>, bytes: B)
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

fn strip_prefix<'i>(name: &'i str, prefix: &'i [u8]) -> Option<&'i str> {
    let prefix_index = prefix.iter().zip(name.bytes()).position(|(&l, r)| l != r);

    match prefix_index {
        Some(mut x) if x > 0 => {
            // Must limit match to a word boundary. In this case, a word boundary is `_` since
            // we're still working with a SCREAMING_SNAKE_CASE name.
            if !name.get(x..)?.starts_with('_') {
                if let Some(boundary) = name.get(..x)?.rfind('_') {
                    x = boundary;
                }
            }

            name.get(x..)
        }
        _ => None,
    }
}

/// Convert a Vulkan structure member name to a Rust structure member name
pub fn vk2rm(buffer: &mut Vec<u8>, mut name: &str) {
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

        cc2sc(buffer, name.bytes());
    }
}

/// Convert a Vulkan type to a Rust type
pub fn vk2rt(name: &str, buffer: &mut Vec<u8>) {
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
pub fn vk2rv(name: &str, variants: &[crate::registry::enm::Enumerant], w: &mut Vec<u8>) {
    if variants.is_empty() {
        return;
    }

    let mut prefix = Vec::with_capacity(128);
    prefix.extend_from_slice(b"VK_");
    pc2ssc(&mut prefix, name.bytes().skip(2));

    // Append / so we pick up the end of the name and stop matching
    prefix.push(b'/');

    // Working buffer
    let mut buffer = Vec::with_capacity(128);
    for v in variants {
        buffer.extend_from_slice(b"    ");

        if let crate::registry::enm::Variant::Alias(_) = v.value {
            // Enums cannot have duplicate discriminants so we comment out aliases
            buffer.extend_from_slice(b"// ");
        }

        // TODO: Better suffix removal logic.
        //
        // Individual variants could have the suffix but it could be the case that it is not shared
        // across all variants. This means that we cannot strip it.
        let name = {
            // If we are successful in stripping a prefix, we will have an `_` as the first
            // character.
            let variant = strip_prefix(v.name, &prefix).unwrap_or(v.name);

            // In order to perform the upcoming check for digits, we need to look past the
            // underscore and see whether the variant name contains digits.
            let variant = if variant.starts_with('_')
                && variant[1..].starts_with(|c: char| c.is_ascii_digit())
            {
                &variant[1..]
            } else {
                variant
            };

            // Find position of first non-digit character
            match variant.as_bytes().iter().position(|b| !b.is_ascii_digit()) {
                Some(x) if x > 0 => {
                    d2w(&mut buffer, variant[..x].as_bytes());
                    &variant[x..]
                }
                _ => variant,
            }
        };
        ssc2pc(&mut buffer, name.bytes());

        buffer.extend_from_slice(b" = ");
        if let crate::registry::enm::Variant::Alias(alias) = v.value {
            let alias = strip_prefix(alias, &prefix).unwrap_or(alias);
            ssc2pc(&mut buffer, alias.bytes());
        } else {
            let value = v.value.sanitize();
            buffer.extend_from_slice(value.as_bytes());
        }

        buffer.extend_from_slice(b",\n");
        w.extend_from_slice(&buffer);

        buffer.clear();
    }
}
