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
// [X] - Emit `PFN_vk` types.
// [X] - Emit features
// [X] - Rename variants starting with numbers to the numerals written out
// [X] - Refactor extension emission code:
//     [X] - Load extension
//     [X] - Emit extension
// [ ] - Generate function pointer loading library
// [ ] - Testing
//     [X] - Basic conversion functions
// [ ] - Remove extra indirection - we first write to buffer, then we copy from buffer into output

mod c;
mod conv;
mod registry;

#[cfg(test)]
mod tests;

const BINDING_FILE_NAME: &str = "vk/bindings.rs";
const LOADER_FILE_NAME: &str = "vk/loader.rs";
const MODULE_FILE_NAME: &str = "vk/mod.rs";
const OUTPUT_FOLDER_NAME: &str = "vk";

fn main() -> std::io::Result<()> {
    let path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| String::from("vk.xml"));
    let xml = std::fs::read_to_string(path)?;
    let document = match roxmltree::Document::parse(&xml) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Failed to parse Vulkan XML registry!\n{e}");
            return Ok(());
        }
    };

    let registry = registry::Registry::build(document.root_element());
    let mut working_buffer = Vec::with_capacity(1024 * 1024);

    // Create the `vk` folder if it doesn't exist
    if !std::path::Path::new(OUTPUT_FOLDER_NAME).exists() {
        std::fs::create_dir(OUTPUT_FOLDER_NAME)?;
    }

    // Emit Rust module file
    if let Some(mut module) = ask_to_overwrite(MODULE_FILE_NAME)? {
        use std::io::Write;
        module.write_all(b"mod bindings;\npub mod loader;\n\npub use bindings::*;\n\n")?;
    }

    // Emit Vulkan bindings
    if let Some(bindings) = ask_to_overwrite(BINDING_FILE_NAME)? {
        working_buffer.clear();
        Generator {
            buffer: &mut working_buffer,
            output: bindings,
            root: document.root_element(),
        }
        .emit(&registry)?;
    }

    // Emit Vulkan meta-loader
    if let Some(loader) = ask_to_overwrite(LOADER_FILE_NAME)? {
        working_buffer.clear();
        MetaLoader {
            buffer: &mut working_buffer,
            output: loader,
            registry: &registry,
        }
        .emit()?;
    }

    Ok(())
}

fn ask_to_overwrite(path: &str) -> std::io::Result<Option<std::fs::File>> {
    use std::io::Write;

    match std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)
    {
        Ok(f) => Ok(Some(f)),
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
            // Ask to over-write
            print!("{path} already exists. Would you like to over-write? (Y/N): ");
            std::io::stdout().flush()?;

            // Get input
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer)?;
            let input = buffer.trim_end().to_lowercase();

            let answer = if input == "y" {
                Some(std::fs::File::create(path)?)
            } else if input == "n" {
                None
            } else {
                eprintln!("Unknown input. Exiting...");
                None
            };
            Ok(answer)
        }
        Err(e) => Err(e),
    }
}

pub struct MetaLoader<'m, 'r, O> {
    buffer: &'m mut Vec<u8>,
    output: O,
    registry: &'m registry::Registry<'r>,
}

impl<'m, 'r, O: std::io::Write> MetaLoader<'m, 'r, O> {
    pub fn emit(mut self) -> std::io::Result<()> {
        let cast_raw = "macro_rules! cast_raw {
    ( $p:expr ) => {
        *((&($p) as *const _) as *const Option<_>)
    };
}

";
        let load = "macro_rules! load_table {
    ( $f:ident, $src:ident, $($field:expr : $name:literal $(,)? )+ ) => {
        (
            $field: cast_raw!(f($src, $name)),
        )+
    };
}

";
        self.buffer.extend_from_slice(cast_raw.as_bytes());
        self.buffer.extend_from_slice(load.as_bytes());

        // Global functions
        self.emit_global_table();

        // Instance-level functions
        self.emit_instance_table();

        // Device-level functions
        self.emit_device_table();

        // TODO: Extensions

        self.output.write_all(self.buffer)?;
        self.buffer.clear();

        Ok(())
    }

    fn emit_device_table(&mut self) {
        self.buffer
            .extend_from_slice(b"pub struct DeviceTable<'d> {\n");
        for command in self.registry.commands.iter() {
            if command.base.is_device_level() {
                self.buffer.extend_from_slice(b"    pub ");
                conv::vk2rm(self.buffer, command.base.name);
                self.buffer.extend_from_slice(b": Option<super::");
                conv::vk2rt(command.base.name, self.buffer);
                self.buffer.extend_from_slice(b">,\n");
            }
        }
        self.buffer
            .extend_from_slice(b"    _marker: core::marker::PhantomData<&'d usize>\n}\n\n");

        // TODO: Loading
        let block = b"impl<'d> DeviceTable<'d> {\n";
        let load = b"    pub unsafe fn load(device: &'d mut super::Device, \
            f: super::GetDeviceProcAddr) -> Self {\n";
        let table = b"        Self {
    load_table!(
        f,
        device,
";

        let prelude = [block.as_slice(), load.as_slice(), table.as_slice()];
        prelude
            .iter()
            .for_each(|item| self.buffer.extend_from_slice(item));

        for command in self.registry.commands.iter() {
            if command.base.is_device_level() {
                self.buffer.extend_from_slice(b"                ");
                conv::vk2rm(self.buffer, command.base.name);
                self.buffer.extend_from_slice(b": \"");
                self.buffer.extend_from_slice(command.base.name.as_bytes());
                self.buffer.extend_from_slice(b"\\u{0}\",\n");
            }
        }

        self.buffer.extend_from_slice(
            b"\n            ),\\n            _marker: core::marker::PhantomData,\n        }\n    }
            \n}\n\n",
        );
    }

    fn emit_global_table(&mut self) {
        // Global functions
        let global = [
            "vkEnumerateInstanceVersion",
            "vkEnumerateInstanceExtensionProperties",
            "vkEnumerateInstanceLayerProperties",
            "vkCreateInstance",
        ];

        self.buffer.extend_from_slice(b"pub struct GlobalTable {\n");
        for g in global {
            self.buffer.extend_from_slice(b"    pub ");
            conv::vk2rm(self.buffer, &g[2..]);
            self.buffer.extend_from_slice(b": Option<super::");
            conv::vk2rt(g, self.buffer);
            self.buffer.extend_from_slice(b">,\n");
        }
        self.buffer.extend_from_slice(b"}\n\n");

        // Loading
        let block = b"impl GlobalTable {\n";
        let load = b"    pub unsafe fn load(f: super::GetInstanceProcAddr) -> Self {\n";
        let instance = b"        let instance = std::ptr::null_mut();\n\n";
        let table = b"        Self {
    load_table!(
        f,
        instance,
";

        let prelude = [
            block.as_slice(),
            load.as_slice(),
            instance.as_slice(),
            table.as_slice(),
        ];
        prelude
            .iter()
            .for_each(|item| self.buffer.extend_from_slice(item));

        for g in global {
            self.buffer.extend_from_slice(b"                ");
            conv::vk2rm(self.buffer, &g[2..]);
            self.buffer.extend_from_slice(b": \"");
            self.buffer.extend_from_slice(g.as_bytes());
            self.buffer.extend_from_slice(b"\\u{0}\",\n");
        }
        self.buffer
            .extend_from_slice(b"            )\n        }\n    }\n}\n\n");
    }

    fn emit_instance_table(&mut self) {
        let exclusions = ["vkGetInstanceProcAddr", "vkCreateInstance"];
        self.buffer
            .extend_from_slice(b"pub struct InstanceTable<'i> {\n");
        for command in self.registry.commands.iter() {
            if !exclusions.iter().any(|&e| e == command.base.name.trim())
                && !command.base.is_device_level()
            {
                self.buffer.extend_from_slice(b"    pub ");
                conv::vk2rm(self.buffer, command.base.name);
                self.buffer.extend_from_slice(b": Option<super::");
                conv::vk2rt(command.base.name, self.buffer);
                self.buffer.extend_from_slice(b">,\n");
            }
        }
        self.buffer
            .extend_from_slice(b"    _marker: core::marker::PhantomData<&'i usize>\n}\n\n");

        let block = b"impl<'i> InstanceTable<'i> {\n";
        let load = b"    pub unsafe fn load(instance: &'i mut super::Instance, \
            f: super::GetInstanceProcAddr) -> Self {\n";
        let table = b"        Self {
    load_table!(
        f,
        instance,
";

        let prelude = [block.as_slice(), load.as_slice(), table.as_slice()];
        prelude
            .iter()
            .for_each(|item| self.buffer.extend_from_slice(item));

        for command in self.registry.commands.iter() {
            if !exclusions.iter().any(|&e| e == command.base.name.trim())
                && !command.base.is_device_level()
            {
                self.buffer.extend_from_slice(b"                ");
                conv::vk2rm(self.buffer, command.base.name);
                self.buffer.extend_from_slice(b": \"");
                self.buffer.extend_from_slice(command.base.name.as_bytes());
                self.buffer.extend_from_slice(b"\\u{0}\",\n");
            }
        }
        self.buffer.extend_from_slice(
            b"
            ),
            _marker: core::marker::PhantomData,
        }
    }
}

",
        );
    }
}

pub struct Generator<'i, W> {
    buffer: &'i mut Vec<u8>,
    output: W,
    root: roxmltree::Node<'i, 'i>,
}

impl<'i, W: std::io::Write> Generator<'i, W> {
    fn drain_write_all(&mut self) -> std::io::Result<()> {
        self.output.write_all(self.buffer)?;
        self.buffer.clear();
        Ok(())
    }

    pub fn emit(mut self, registry: &registry::Registry<'i>) -> std::io::Result<()> {
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

        self.emit_extensions(&registry.extensions, &registry.handles)?;
        Ok(())
    }

    fn emit_aliases(
        &mut self,
        aliases: &[registry::Category<registry::Alias<'i>>],
    ) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Aliases\n");
        aliases.iter().for_each(|alias| {
            if let registry::Category {
                base,
                sub_category: registry::ApiCategory::Core,
            } = alias
            {
                self.buffer.extend_from_slice(b"pub type ");
                conv::vk2rt(base.name, self.buffer);
                self.buffer.extend_from_slice(b" = ");
                conv::vk2rt(base.original_name, self.buffer);
                self.buffer.extend_from_slice(b";\n");
            }
        });

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_bitmasks(&mut self, bitmasks: &[registry::Category<&str>]) -> std::io::Result<()> {
        self.buffer
            .extend_from_slice(b"// Bitmasks\npub type Flags = u32;\n");
        bitmasks
            .iter()
            .filter(|category| matches!(category.sub_category, registry::ApiCategory::Core))
            .for_each(|category| {
                self.buffer.extend_from_slice(b"pub type ");
                conv::vk2rt(category.base, self.buffer);
                self.buffer.extend_from_slice(b" = Flags;\n");
            });

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_commands(
        &mut self,
        commands: &[registry::Category<registry::cmd::Command<'i>>],
        handles: &[&str],
    ) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Commands\n");
        commands
            .iter()
            .filter(|category| matches!(category.sub_category, registry::ApiCategory::Core))
            .for_each(|category| category.base.emit(handles, self.buffer));

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_constants(&mut self, constants: &[registry::cnst::Constant]) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Constants\n");
        constants.iter().for_each(|c| c.emit(self.buffer));
        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_enums(&mut self, enums: &[registry::enm::Enum]) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Enumerations\n");
        enums.iter().for_each(|e| e.emit(self.buffer));
        self.drain_write_all()
    }

    fn emit_extensions(
        &mut self,
        extensions: &[registry::Extension],
        handles: &[&str],
    ) -> std::io::Result<()> {
        extensions
            .iter()
            .for_each(|ext| ext.emit(handles, self.buffer));
        self.drain_write_all()
    }

    fn emit_function_pointers(&mut self) -> std::io::Result<()> {
        let pointers: Vec<String> = self
            .root
            .descendants()
            .filter(|d| {
                (d.tag_name().name() == "type")
                    && matches!(d.attribute("category"), Some("funcpointer"))
            })
            .map(|t| t.children().filter_map(|c| c.text()).collect())
            .collect();

        self.buffer.extend_from_slice(b"// Function pointers\n");
        for pointer in pointers {
            let tokens = c::Token::tokenize(&pointer);

            // Retrieving the return type - token list: typedef `XXXX` (
            // Find the first left parenthesis
            let return_type = tokens
                .iter()
                .position(|&t| t == c::Token::ParenLeft)
                .and_then(|i| tokens.get(1..i));

            // Retrieving function pointer name - token list:
            // typedef XXXX (<Identifier> *`<Identifier>`)
            let name = tokens
                .iter()
                .position(|&t| t == c::Token::ParenRight)
                .and_then(|i| match tokens.get(i - 1) {
                    Some(c::Token::Identifier(i)) => Some(i),
                    _ => None,
                });

            // Retrieving parameter types and names. Retrieve the second opening parenthesis in the
            // function pointer definition: typedef XXXX (<Identifier> *<Identifier>)`(`...);
            //
            // This leads us to the start of the functions' argument list.
            let arguments = {
                let find_second = |token: c::Token, offset: usize| {
                    tokens
                        .iter()
                        .enumerate()
                        .filter(|(_, &t)| t == token)
                        .nth(1)
                        .map(|(position, _)| position + offset)
                };

                let list_start = find_second(c::Token::ParenLeft, 1);
                let list_end = find_second(c::Token::ParenRight, 0);

                list_start
                    .zip(list_end)
                    .and_then(|(start, end)| tokens.get(start..end))
                    .map(|mut list| {
                        list = match list.first() {
                            Some(c::Token::Identifier("void")) if list.len() == 1 => &[],
                            _ => list,
                        };
                        list.split(|&c| c == c::Token::Comma)
                    })
            };

            if let Some(((name, return_type), arguments)) = name.zip(return_type).zip(arguments) {
                self.buffer.extend_from_slice(b"pub type ");
                conv::vk2rt(name, self.buffer);
                self.buffer
                    .extend_from_slice(b" = unsafe extern \"system\" fn(");
                for (i, argument) in arguments.enumerate() {
                    let arg_name = argument.last();
                    let arg_type = if argument.is_empty() {
                        None
                    } else {
                        argument.get(..argument.len() - 1)
                    };

                    if let Some((c::Token::Identifier(n), t)) = arg_name.zip(arg_type) {
                        if i > 0 {
                            self.buffer.extend_from_slice(b", ");
                        }

                        conv::vk2rm(self.buffer, n);
                        self.buffer.extend_from_slice(b": ");
                        conv::ct2rt(self.buffer, t.to_owned());
                    }
                }
                self.buffer.push(b')');

                match return_type.first() {
                    Some(c::Token::Identifier("void")) if return_type.len() == 1 => {}
                    _ => {
                        self.buffer.extend_from_slice(b" -> ");
                        conv::ct2rt(self.buffer, return_type.to_owned());
                    }
                }
                self.buffer.extend_from_slice(b";\n");
            }
        }

        self.buffer.push(b'\n');
        self.drain_write_all()
    }

    fn emit_handles(&mut self, handles: &[&str]) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Handles\n");
        for name in handles.iter() {
            self.buffer.extend_from_slice(b"#[repr(C)]\n");
            self.buffer.extend_from_slice(b"#[derive(Clone, Copy)]\n");
            self.buffer.extend_from_slice(b"pub struct ");
            conv::vk2rt(name, self.buffer);
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
        structs: &[registry::Category<registry::strct::Struct>],
        handles: &[&str],
    ) -> std::io::Result<()> {
        self.buffer.extend_from_slice(b"// Structures\n");

        structs
            .iter()
            .filter_map(|c| match c.sub_category {
                registry::ApiCategory::Core => Some(&c.base),
                _ => None,
            })
            .for_each(|s| s.emit(handles, self.buffer));

        self.drain_write_all()
    }
}

fn filter<'b, 'n>(
    node: roxmltree::Node<'b, 'n>,
    name: &'b str,
) -> impl Iterator<Item = roxmltree::Node<'b, 'n>> {
    node.children()
        .filter(move |child| child.is_element() && (name == child.tag_name().name()))
}
