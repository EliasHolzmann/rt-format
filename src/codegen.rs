//! The `generate_code!` macro generates the `Specifier` struct, `format_value` function, and all
//! the code they need.
//! 
//! The macro expects definitions of "dimensions" of the format specifier (e.g. width, precision,
//! and format to use). Each dimension has to define the name of the field to generate in the
//! `Specifier` struct, the name of the enum type to generate for that field, and the definition of
//! each variant for that enum. Each variant definition declares the variant name, optionally with
//! one or more fields for that variant to contain, and then the format string fragment to generate
//! when that variant is matched. Each dimension except the format needs to also supply the name of
//! the `FormattingOptions` method corresponding to this field as well as the value to use for each
//! variant.
//! 
//! The way `format_value` function works is
//! - without the `nightly` feature, through a tree of nested `match` blocks on `Specifier` fields,
//! with a call to `write!` macro with a different formatting string at each leaf.
//! - with the `nightly` feature, through a `fmt::FormattingOptions` instance that is used to build
//! a `Formatter` matching the `Specifier` and is then used to call the `fmt` method of the
//! formatting trait corresponding to the `Format` variant used in specifier (which is recognized
//! by missing identifiers for the `FormattiongOptions` method, a missing expression for the value,
//! the struct field name `format` and the enum name `Format`). Note that the constructed
//! `format_value` function processes the dimensions in the order fed to the macro. Therefore, the
//! `Format` dimension has to be the last one, as all the other dimensions are necessary to
//! construct the `Formatter` used in the `Format` dimension.
//! 
//! # Examples
//! ```ignore
//! generate_code! {
//!     foo: Foo as fn mod_foo {
//!         Argle => "" as None,
//!         Bargle { glop_glyf: usize } => "glop_glyf$" as Some(glop_glyf),
//!     }
//! 
//!     format: Format {
//!         Olle => "",
//!         Bolle => "@",
//!     }
//! }
//! ```
//! 
//! The resulting `format_value` would look like this without the `nightly` feature:
//! ```ignore
//! pub fn format_value<V>(specifier: &Specifier, value: &V, f: &mut fmt::Formatter) -> fmt::Result {
//!     match (specifier.foo) {
//!         Argle => match (specifier.format) {
//!             Olle => write!(f, "{:}", value),
//!             Bolle => write!(f, "{:@}", value),
//!         },
//!         Bargle { glop_glyf } => match (specifier.format) {
//!             Olle => write!(f, "{:glop_glyf$}", value, glop_glyf),
//!             Bolle => write!(f, "{:glop_glyf$@}", value, glop_glyf),
//!         }
//!     }
//! }
//! ```
//! 
//! And it would look like this with the `nightly` feature:
//! ```ignore
//! pub fn format_value<V>(specifier: &Specifier, value: &V, f: &mut fmt::Formatter) -> fmt::Result {
//!     use std::fmt;
//!     let mut formatting_options = fmt::FormattingOptions::new();
//!     match specifier.foo {
//!         Foo::Argle => formatting_options.mod_foo(None),
//!         Foo::Bargle { glop_glyf } => formatting_options.mod_foo(Some(glop_glyf)),
//!     }
//!     
//!     let mut formatter = f.with_options(formatting_options);
//!     return match specifier.format {
//!         Format::Olle => std::fmt::Olle::fmt(value, &mut formatter),
//!         Format::Bolle => std::fmt::Bolle::fmt(value, &mut formatter),
//!     };
//! }
//! ````

macro_rules! generate_code {
    {
        $(
            $(#[doc = $($doc_tt:tt)*])*
            $(#[cfg($($cfg_tt:tt)*)])*
            $field:ident : $type:ident $(as fn $fmt_opt_method:ident)? {
                $(
                    $variant:ident $({ $($var_field:ident : $var_type:ty),+ })? => $spec_string:tt $(as $fmt_opt_value:expr)?
                ),+ $(,)? 
            }
        )+
    } => {
        generate_code!(@munch_cfg [] $(
            $(#[doc = $($doc_tt)*])*
            $(#[cfg($($cfg_tt)*)])*
            $field : $type $(as fn $fmt_opt_method)? {
                $(
                    $variant $({ $($var_field : $var_type),+ })? => $spec_string $(as $fmt_opt_value)?
                ),+
            }
        )+);
    };
    {@munch_cfg [$($munched:tt)*]
        $(#[doc = $($doc_tt:tt)*])*
        $(#[cfg($($cfg_tt:tt)*)])*
        $field:ident : $type:ident $(as fn $fmt_opt_method:ident)? {
            $(
                $variant:ident $({ $($var_field:ident : $var_type:ty),+ })? => $spec_string:tt $(as $fmt_opt_value:expr)?
            ),+ $(,)? 
        }
        $($tail:tt)*
    } => {
        $(#[cfg($($cfg_tt)*)])*
        generate_code!(@munch_cfg [$($munched)* 
            $(#[doc = $($doc_tt)*])*
            $(#[cfg($($cfg_tt)*)])*
            $field : $type $(as fn $fmt_opt_method)? {
                $(
                    $variant $({ $($var_field : $var_type),+ })? => $spec_string $(as $fmt_opt_value)?
                ),+
            }
        ] $($tail)*);
        #[cfg(not(all($($($cfg_tt)*),*)))]
        generate_code!(@munch_cfg [$($munched)*] $($tail)*);
    };
    {@munch_cfg [$($munched:tt)*]
    } => {
        generate_code!(@inner $($munched)*);
    };
    {@inner
        $(
            $(#[$dim_meta:meta])*
            $field:ident : $type:ident $(as fn $fmt_opt_method:ident)? {
                $(
                    $variant:ident $({ $($var_field:ident : $var_type:ty),+ })? => $spec_string:tt $(as $fmt_opt_value:expr)?
                ),+ $(,)? 
            }
        )+
    } => {
        $(
            $(#[$dim_meta])*
            #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
            #[allow(missing_docs)]
            pub enum $type {
                $(
                    $variant $({ $($var_field: $var_type),+ })?
                ),+
            }
            generate_code!(@enum_try_from $type [] [$(($spec_string $variant $({$($var_field)+})?))+]);
            generate_code!(@enum_display $type [] [$(($spec_string $variant $({$($var_field)+})?))+]);
        )+

        /// The specification for the format of an argument in the formatting string.
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub struct Specifier {
            $(
                $(#[$dim_meta])*
                pub $field: $type
            ),+
        }

        #[cfg(not(feature = "nightly"))]
        generate_code!(@fn_format_value
            $(
                [$field $type $([$spec_string $variant $([$($var_field)+])?])+]
            )+
        );

        #[cfg(feature = "nightly")]
        generate_code!(@fn_format_value_with_formatting_options
            $(
                [$field $type ($($fmt_opt_method)?) $([$variant $([$($var_field)+])? ($($fmt_opt_value)?)])+]
            )+
        );

        impl Default for Specifier {
            fn default() -> Self {
                Self {
                    $(
                        $field: generate_code!(@first_variant $type $($variant)+)
                    ),+
                }
            }
        }

        impl fmt::Display for Specifier {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(
                    f,
                    concat!($("{", stringify!($field), "}"),+),
                    $(
                        $field=self.$field
                    ),+
                )
            }
        }
    };
    (@enum_try_from
        $type:ident [$($munched:tt)*] [($spec_string:tt $variant:ident) $($tail:tt)*]
    ) => {
        generate_code!(@enum_try_from $type [$($munched)* ($spec_string $variant)] [$($tail)*]);
    };
    (@enum_try_from
        $type:ident [$($munched:tt)*] [($spec_string:tt $variant:ident $_:tt) $($tail:tt)*]
    ) => {
    };
    (@enum_try_from
        $type:ident [$(($spec_string:tt $variant:ident))+] []
    ) => {
        impl TryFrom<&str> for $type {
            type Error = ();
            fn try_from(value: &str) -> Result<Self, Self::Error> {
                match value {
                    $($spec_string => Ok($type::$variant),)+
                    _ => Err(())
                }
            }
        }
    };
    (@enum_display
        $type:ident [$($munched:tt)*] [($spec_string:tt $variant:ident) $($tail:tt)*]
    ) => {
        generate_code!(@enum_display $type [$($munched)* ($spec_string $variant)] [$($tail)*]);
    };
    (@enum_display
        $type:ident [$($munched:tt)*] [($spec_string:tt $variant:ident $_:tt) $($tail:tt)*]
    ) => {
    };
    (@enum_display
        $type:ident [$(($spec_string:tt $variant:ident))+] []
    ) => {
        impl fmt::Display for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $($type::$variant => write!(f, $spec_string),)+
                }
            }
        }
    };
    (@first_variant $type:ident $first:ident $($rest:ident)*) => { $type::$first };
    (@fn_format_value $($dim:tt)+) => {
        /// Formats the given value using the given formatter and the given format specification.
        /// 
        /// Since the implementation of `format_value` employs the `write!` macro, the `value` must
        /// implement all of the `std::fmt` formatting traits. Which trait will actually be used is
        /// determined at runtime, based on the contents of the `specifier`.
        pub fn format_value<V>(specifier: &Specifier, value: &V, f: &mut fmt::Formatter) -> fmt::Result
        where
            V: fmt::Display
                + fmt::Debug
                + fmt::Octal
                + fmt::LowerHex
                + fmt::UpperHex
                + fmt::Binary
                + fmt::LowerExp
                + fmt::UpperExp,
        {
            generate_code!(@matcher (specifier, value, f, "", []) $($dim)+)
        }
    };
    (@matcher ($spec:ident, $val:ident, $out:ident, $prefix:expr, $named_args:tt) $head_dim:tt $($tail_dim:tt)+) => {
        generate_code!(@matcher_branch ($spec, $val, $out, $prefix, $named_args) $head_dim [$($tail_dim)+])
    };
    (@matcher ($spec:ident, $val:ident, $out:ident, $prefix:expr, $named_args:tt) $last_dim:tt) => {
        generate_code!(@matcher_leaf ($spec, $val, $out, $prefix, $named_args) $last_dim)
    };
    (@matcher_branch
        ($spec:ident, $val:ident, $out:ident, $prefix:expr, $named_args:tt)
        [$field:ident $type:ident $([$spec_string:tt $variant:ident $([$($var_field:ident)+])?])+]
        $tail:tt
    ) => {
        match $spec.$field {
            $(
                $type::$variant $({ $($var_field),+ })? => generate_code!(
                    @matcher_tail
                    ($spec, $val, $out, concat!($prefix, $spec_string))
                    $named_args
                    [$($($var_field)+)?]
                    $tail
                )
            ),+
        }
    };
    (@matcher_tail ($spec:ident, $val:ident, $out:ident, $prefix:expr) [$($lhs_arg:ident)*] [$($rhs_arg:ident)*] [$($dim:tt)+]) => {
        generate_code!(@matcher ($spec, $val, $out, $prefix, [$($lhs_arg)* $($rhs_arg)*]) $($dim)+)
    };
    (@matcher_leaf
        ($spec:ident, $val:ident, $out:ident, $prefix:expr, $named_args:tt)
        [$field:ident $type:ident $([$spec_string:tt $variant:ident $([$($var_field:ident)+])?])+]
    ) => {
        match $spec.$field {
            $(
                $type::$variant $({ $($var_field),+ })? => generate_code!(
                    @matcher_concat_args
                    ($spec, $val, $out, concat!($prefix, $spec_string))
                    $named_args
                    [$($($var_field)+)?]
                )
            ),+
        }
    };
    (@matcher_concat_args ($spec:ident, $val:ident, $out:ident, $format_str:expr) [$($lhs_arg:ident)*] [$($rhs_arg:ident)*]) => {
        generate_code!(@matcher_write ($spec, $val, $out, $format_str) [$($lhs_arg)* $($rhs_arg)*])
    };
    (@matcher_write ($spec:ident, $val:ident, $out:ident, $format_str:expr) [$($named_arg:ident)*]) => {
        write!(
            $out,
            concat!("{:", $format_str, "}"),
            $val,
            $($named_arg = $named_arg),*
        )
    };
    (@fn_format_value_with_formatting_options $($dimension:tt)+) => {
        /// Formats the given value using the given formatter and the given format specification.
        /// 
        /// The `value` must implement all of the `std::fmt` formatting traits. Which trait will
        /// actually be used is determined at runtime, based on the contents of the `specifier`.
        pub fn format_value<V>(specifier: &Specifier, value: &V, f: &mut fmt::Formatter) -> fmt::Result
        where
            V: fmt::Display
                + fmt::Debug
                + fmt::Octal
                + fmt::LowerHex
                + fmt::UpperHex
                + fmt::Binary
                + fmt::LowerExp
                + fmt::UpperExp,
        {
            use std::fmt;
            let mut formatting_options = fmt::FormattingOptions::new();

            $(generate_code!(@mutate_formatting_options formatting_options specifier value f $dimension);)+

        }
    };
    (@mutate_formatting_options $formatting_options:ident $specifier:ident $value:ident $out:ident
        [$field:ident $type:ident ($fmt_opt_method:ident) $([$variant:ident $([$($var_field:tt)+])? ($fmt_opt_value:expr)])+]
    ) => {
        match $specifier.$field {
            $(
                $type::$variant $({$($var_field)+})?  => $formatting_options.$fmt_opt_method($fmt_opt_value),
            )+
        }
    };
    (@mutate_formatting_options $formatting_options:ident $specifier:ident $value:ident $out:ident
        [format Format () $([$variant:ident ()])+]
    ) => {
        let mut formatter = $out.with_options($formatting_options);
       return match $specifier.format {
            $(
                Format::$variant => std::fmt::$variant::fmt($value, &mut formatter),
            )+
        }
    };
}
