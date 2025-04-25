/// copy and parse the test of implenment of debug
use proc_macro::TokenStream;
use reflect::*;

/// de sugar the macro

// #[allow(non_snake_case)]
// mod RUNTIME {
//     extern crate reflect as _reflect;
//     #[allow(dead_code, non_snake_case)]
//     pub fn MODULE() -> _reflect::Module {
//         _reflect::Module::root()
//     }
//     pub mod std {
//         extern crate reflect as _reflect;
//         #[allow(unused_imports)]
//         use self::_reflect::runtime::prelude::*;
//         #[allow(dead_code, non_snake_case)]
//         pub fn MODULE() -> _reflect::Module {
//             super::MODULE().get_module("std")
//         }
//         struct __Indirect<T>(T);
//         pub mod fmt {
//             extern crate reflect as _reflect;
//             #[allow(unused_imports)]
//             use self::_reflect::runtime::prelude::*;
//             #[allow(dead_code, non_snake_case)]
//             pub fn MODULE() -> _reflect::Module {
//                 super::MODULE().get_module("fmt")
//             }
//             struct __Indirect<T>(T);
//             #[allow(non_camel_case_types)]
//             pub struct Formatter;
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::marker::Copy for Formatter {}
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::clone::Clone for Formatter {
//                 #[inline]
//                 fn clone(&self) -> Formatter {
//                     *self
//                 }
//             }
//             impl _reflect::runtime::RuntimeType for Formatter {
//                 fn SELF(self) -> _reflect::Type {
//                     MODULE().get_type("Formatter")
//                 }
//             }
//             #[allow(non_camel_case_types)]
//             pub struct Result;
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::marker::Copy for Result {}
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::clone::Clone for Result {
//                 #[inline]
//                 fn clone(&self) -> Result {
//                     *self
//                 }
//             }
//             impl _reflect::runtime::RuntimeType for Result {
//                 fn SELF(self) -> _reflect::Type {
//                     MODULE().get_type("Result")
//                 }
//             }
//             #[allow(non_camel_case_types)]
//             pub struct DebugStruct;
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::marker::Copy for DebugStruct {}
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::clone::Clone for DebugStruct {
//                 #[inline]
//                 fn clone(&self) -> DebugStruct {
//                     *self
//                 }
//             }
//             impl _reflect::runtime::RuntimeType for DebugStruct {
//                 fn SELF(self) -> _reflect::Type {
//                     MODULE().get_type("DebugStruct")
//                 }
//             }
//             #[allow(non_camel_case_types)]
//             pub struct Debug;
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::marker::Copy for Debug {}
//             #[automatically_derived]
//             #[allow(non_camel_case_types)]
//             impl ::core::clone::Clone for Debug {
//                 #[inline]
//                 fn clone(&self) -> Debug {
//                     *self
//                 }
//             }
//             impl _reflect::runtime::RuntimeType for Debug {
//                 fn SELF(self) -> _reflect::Type {
//                     MODULE().get_type("Debug")
//                 }
//             }
//             impl _reflect::runtime::RuntimeTrait for Debug {
//                 fn SELF(self) -> _reflect::Path {
//                     MODULE().get_path("Debug")
//                 }
//             }
//             impl __Indirect<Debug> {
//                 #[allow(dead_code)]
//                 fn fmt() {
//                     #[allow(non_camel_case_types)]
//                     pub struct fmt;
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::marker::Copy for fmt {}
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::clone::Clone for fmt {
//                         #[inline]
//                         fn clone(&self) -> fmt {
//                             *self
//                         }
//                     }
//                     impl _reflect::runtime::RuntimeFunction for fmt {
//                         fn SELF(self) -> _reflect::Function {
//                             let mut sig = _reflect::Signature::new();
//                             sig.set_self_by_reference();
//                             sig.add_input(
//                                 _reflect::runtime::RuntimeType::SELF(Formatter).reference_mut(),
//                             );
//                             sig.set_output(_reflect::runtime::RuntimeType::SELF(Result));
//                             _reflect::runtime::RuntimeType::SELF(Debug).get_function("fmt", sig)
//                         }
//                     }
//                     impl fmt {
//                         pub fn INVOKE(
//                             self,
//                             v0: _reflect::Value,
//                             v1: _reflect::Value,
//                         ) -> _reflect::Value {
//                             _reflect::runtime::RuntimeFunction::SELF(self).invoke(&[v0, v1])
//                         }
//                     }
//                     #[allow(unknown_lints, non_local_definitions)]
//                     impl Debug {
//                         #[allow(non_upper_case_globals)]
//                         pub const fmt: fmt = fmt;
//                     }
//                 }
//             }
//             impl __Indirect<Formatter> {
//                 #[allow(dead_code)]
//                 fn debug_struct() {
//                     #[allow(non_camel_case_types)]
//                     pub struct debug_struct;
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::marker::Copy for debug_struct {}
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::clone::Clone for debug_struct {
//                         #[inline]
//                         fn clone(&self) -> debug_struct {
//                             *self
//                         }
//                     }
//                     impl _reflect::runtime::RuntimeFunction for debug_struct {
//                         fn SELF(self) -> _reflect::Function {
//                             let mut sig = _reflect::Signature::new();
//                             sig.set_self_by_reference_mut();
//                             sig.add_input(_reflect::runtime::RuntimeType::SELF(str).reference());
//                             sig.set_output(_reflect::runtime::RuntimeType::SELF(DebugStruct));
//                             _reflect::runtime::RuntimeType::SELF(Formatter)
//                                 .get_function("debug_struct", sig)
//                         }
//                     }
//                     impl debug_struct {
//                         pub fn INVOKE(
//                             self,
//                             v0: _reflect::Value,
//                             v1: _reflect::Value,
//                         ) -> _reflect::Value {
//                             _reflect::runtime::RuntimeFunction::SELF(self).invoke(&[v0, v1])
//                         }
//                     }
//                     #[allow(unknown_lints, non_local_definitions)]
//                     impl Formatter {
//                         #[allow(non_upper_case_globals)]
//                         pub const debug_struct: debug_struct = debug_struct;
//                     }
//                 }
//             }
//             impl __Indirect<DebugStruct> {
//                 #[allow(dead_code)]
//                 fn field() {
//                     #[allow(non_camel_case_types)]
//                     pub struct field;
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::marker::Copy for field {}
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::clone::Clone for field {
//                         #[inline]
//                         fn clone(&self) -> field {
//                             *self
//                         }
//                     }
//                     impl _reflect::runtime::RuntimeFunction for field {
//                         fn SELF(self) -> _reflect::Function {
//                             let mut sig = _reflect::Signature::new();
//                             sig.set_self_by_reference_mut();
//                             sig.add_input(_reflect::runtime::RuntimeType::SELF(str).reference());
//                             sig.add_input(_reflect::runtime::RuntimeType::SELF(Debug).reference());
//                             sig.set_output(
//                                 _reflect::runtime::RuntimeType::SELF(DebugStruct).reference_mut(),
//                             );
//                             _reflect::runtime::RuntimeType::SELF(DebugStruct)
//                                 .get_function("field", sig)
//                         }
//                     }
//                     impl field {
//                         pub fn INVOKE(
//                             self,
//                             v0: _reflect::Value,
//                             v1: _reflect::Value,
//                             v2: _reflect::Value,
//                         ) -> _reflect::Value {
//                             _reflect::runtime::RuntimeFunction::SELF(self).invoke(&[v0, v1, v2])
//                         }
//                     }
//                     #[allow(unknown_lints, non_local_definitions)]
//                     impl DebugStruct {
//                         #[allow(non_upper_case_globals)]
//                         pub const field: field = field;
//                     }
//                 }
//             }
//             impl __Indirect<DebugStruct> {
//                 #[allow(dead_code)]
//                 fn finish() {
//                     #[allow(non_camel_case_types)]
//                     pub struct finish;
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::marker::Copy for finish {}
//                     #[automatically_derived]
//                     #[allow(non_camel_case_types)]
//                     impl ::core::clone::Clone for finish {
//                         #[inline]
//                         fn clone(&self) -> finish {
//                             *self
//                         }
//                     }
//                     impl _reflect::runtime::RuntimeFunction for finish {
//                         fn SELF(self) -> _reflect::Function {
//                             let mut sig = _reflect::Signature::new();
//                             sig.set_self_by_reference_mut();
//                             sig.set_output(_reflect::runtime::RuntimeType::SELF(Result));
//                             _reflect::runtime::RuntimeType::SELF(DebugStruct)
//                                 .get_function("finish", sig)
//                         }
//                     }
//                     impl finish {
//                         pub fn INVOKE(self, v0: _reflect::Value) -> _reflect::Value {
//                             _reflect::runtime::RuntimeFunction::SELF(self).invoke(&[v0])
//                         }
//                     }
//                     #[allow(unknown_lints, non_local_definitions)]
//                     impl DebugStruct {
//                         #[allow(non_upper_case_globals)]
//                         pub const finish: finish = finish;
//                     }
//                 }
//             }
//         }
//     }
// }

reflect::library! {
    extern crate std {
        mod fmt {
            type Formatter;
            type Result;
            type DebugStruct;

            // trait MyDebug {
            //     fn fmt(&self, &mut Formatter) -> Result;
            // }

            trait Debug {
                fn fmt(&self, &mut Formatter) -> Result;
            }

            impl Formatter {
                fn debug_struct(&mut self, &str) -> DebugStruct;
            }

            impl DebugStruct {
                fn field(&mut self, &str, &Debug) -> &mut DebugStruct;
                fn finish(&mut self) -> Result;
            }
        }
    }
}

// pub fn derive(ex: Execution) {
//     ex.make_trait_impl(RUNTIME::std::fmt::MyDebug, ex.target_type(), |block| {
//         block.make_function(RUNTIME::std::fmt::MyDebug::fmt, debug_fmt);
//     });
// }

fn debug_fmt(f: MakeFunction) -> Value {
    let receiver = f.arg(0);
    let formatter = f.arg(1);

    let type_name = receiver.get_type_name();

    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(_receiver) => unimplemented!(),
            Struct::Tuple(_receiver) => unimplemented!(),
            Struct::Struct(receiver) => {
                let builder = RUNTIME::std::fmt::Formatter::debug_struct
                    .INVOKE(formatter, type_name)
                    .reference_mut();

                for field in receiver.fields() {
                    RUNTIME::std::fmt::DebugStruct::field.INVOKE(
                        builder,
                        field.get_name(),
                        field.get_value(),
                    );
                }

                RUNTIME::std::fmt::DebugStruct::finish.INVOKE(builder)
            }
        },
        Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
            Variant::Unit(_variant) => unimplemented!(),
            Variant::Tuple(_variant) => unimplemented!(),
            Variant::Struct(_variant) => unimplemented!(),
        }),
    }
}

///////////////

// Macro that is called when someone writes derive(MyDebug) on a data structure.
// It returns a fragment of Rust source code (TokenStream) containing an
// implementation of Debug for the input data structure. The macro uses
// compile-time reflection internally, but the generated Debug impl is exactly
// as if this macro were handwritten without reflection.
#[proc_macro_derive(MyDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Feed the tokens describing the data structure into the reflection library
    // for parsing and analysis. We provide a callback that describes what trait
    // impl(s) the reflection library will need to generate code for.
    reflect::derive(input, |ex| {
        // Instruct the library to generate an impl of Debug for the derive
        // macro's target type / Self type.
        ex.make_trait_impl(RUNTIME::std::fmt::Debug, ex.target_type(), |block| {
            // Instruct the library to compile debug_fmt (a function shown
            // below) into the source code for the impl's Debug::fmt method.
            block.make_function(RUNTIME::std::fmt::Debug::fmt, debug_fmt);
        });
    })
}
