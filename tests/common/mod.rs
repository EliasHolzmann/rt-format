use std::fmt;

use rt_format::{Format, FormattableValue, Specifier};

pub enum Variant {
    Int(i32),
    Float(f64)
}

impl FormattableValue for Variant {
    fn supports_format(&self, spec: &Specifier) -> bool {
        match self {
            Self::Int(_) => true,
            Self::Float(_) => match spec.format {
                Format::Display | Format::Debug | Format::LowerExp | Format::UpperExp => true,
                _ => false
            }
        }
    }

    fn fmt_display(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::Display::fmt(&val, f),
            Self::Float(val) => fmt::Display::fmt(&val, f),
        }
    }

    fn fmt_debug(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::Debug::fmt(&val, f),
            Self::Float(val) => fmt::Debug::fmt(&val, f),
        }
    }

    fn fmt_octal(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::Octal::fmt(&val, f),
            _ => Err(fmt::Error)
        }
    }

    fn fmt_lower_hex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::LowerHex::fmt(&val, f),
            _ => Err(fmt::Error)
        }
    }

    fn fmt_upper_hex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::UpperHex::fmt(&val, f),
            _ => Err(fmt::Error)
        }
    }

    fn fmt_binary(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::Binary::fmt(&val, f),
            _ => Err(fmt::Error)
        }
    }

    fn fmt_lower_exp(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::LowerExp::fmt(&val, f),
            Self::Float(val) => fmt::LowerExp::fmt(&val, f),
        }
    }

    fn fmt_upper_exp(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(val) => fmt::UpperExp::fmt(&val, f),
            Self::Float(val) => fmt::UpperExp::fmt(&val, f),
        }
    }
}
