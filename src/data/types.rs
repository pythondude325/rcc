use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::Symbol;
use crate::arch::SIZE_T;
use crate::intern::InternedStr;

static TYPES: Vec<Type> = Default::default();
lazy_static! {
    static ref TYPE_INDEXES: HashMap<&'static Type, usize> = Default::default();
}
static NUM_TYPES: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone, Debug, Eq, Hash)]
pub struct InternedType(usize);

impl std::ops::Deref for InternedType {
    type Target = Type;
    fn deref(&self) -> &'static Self::Target {
        &TYPES[self.0]
    }
}

impl PartialEq for InternedType {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || *self == *other
    }
}

impl fmt::Display for InternedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl Type {
    pub fn get_or_intern(self) -> InternedType {
        if let Some(index) = TYPE_INDEXES.get(&self) {
            return InternedType(*index);
        }
        let index = NUM_TYPES.fetch_add(1, Ordering::AcqRel);
        TYPE_INDEXES.insert(&self, index);
        TYPES.insert(index, self);
        InternedType(index)
    }
}

impl Into<&Type> for InternedType {
    fn into(self) -> &'static Type {
        &*self
    }
}

impl Into<InternedType> for Type {
    fn into(self) -> InternedType {
        self.get_or_intern()
    }
}

// preallocate commonly used primitives
lazy_static! {
    pub static ref VOID: InternedType = Type::get_or_intern(Type::Void);
    pub static ref BOOL: InternedType = Type::get_or_intern(Type::Bool);
    pub static ref CHAR: InternedType = Type::get_or_intern(Type::Char(true));
    pub static ref INT: InternedType = Type::get_or_intern(Type::Int(true));
    pub static ref LONG: InternedType = Type::get_or_intern(Type::Long(true));
    pub static ref FLOAT: InternedType = Type::get_or_intern(Type::Float);
    pub static ref DOUBLE: InternedType = Type::get_or_intern(Type::Double);
    pub static ref INT_POINTER: InternedType = Type::get_or_intern(Type::Pointer(*INT));
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Bool,
    Char(bool), // signed or unsigned
    Short(bool),
    Int(bool),
    Long(bool),
    Float,
    Double,
    Pointer(InternedType),
    Array(InternedType, ArrayType),
    Function(FunctionType),
    /// We need to know which scope `Struct`s came from in order to tell whether they are equal
    /// This is done by storing a unique identifier for the scope in the struct itself
    Struct(Option<InternedStr>, StructType),
    Union(Option<InternedStr>, StructType),
    /// Enums should always have members, since tentative definitions are not allowed
    Enum(Option<InternedStr>, Vec<(InternedStr, i64)>),
    /// This should probably be merged into Struct at some point
    Bitfield(Vec<BitfieldType>),
    /// This is the type used for variadic arguments.
    VaList,
    /// A semantic error occured while parsing this type.
    Error,
}

/*
/// Structs can be either named or anonymous.
/// Anonymous structs carry all their information with them,
/// there's no need (or way) to use tag_scope.
/// Named structs can have forward declarations and be defined at any point
/// in the program. In order to support self referential structs, named structs
/// do NOT contain a list of their members, only the information that the
/// backend needs to compile them.
///
/// The parser has access to a `tag_scope` that allows it to update the named
/// structs as necessary.
///
/// WARNING: because the parser returns declarations eagerly, it may return a
/// struct that has not yet been defined. This may be fixed at some point in
/// the future. Until then, all consumers are stuck. See
/// https://github.com/jyn514/rcc/issues/44 for an example of how this can manifest.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructType {
    /// name, size, alignment, offsets
    Named(InternedStr, u64, u64, HashMap<InternedStr, u64>),
    Anonymous(Vec<Symbol>),
}
*/
pub type StructType = Vec<Symbol>;

#[derive(Clone, Debug, Hash)]
pub enum ArrayType {
    Fixed(SIZE_T),
    Unbounded,
}

#[derive(Clone, Debug, Eq, Hash)]
// note: old-style declarations are not supported at this time
pub struct FunctionType {
    // why Symbol instead of Type?
    // 1. we need to know qualifiers for the params. if we made that part of Type,
    //    we'd need qualifiers for every step along the way
    //    (consider that int a[][][] parses as 4 nested types).
    // 2. when we do scoping, we need to know the names of formal parameters
    //    (as opposed to concrete arguments).
    //    this is as good a place to store them as any.
    pub return_type: InternedType,
    pub params: Vec<Symbol>,
    pub varargs: bool,
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BitfieldType {
    pub offset: i32,
    pub name: Option<InternedStr>,
    pub ctype: InternedType,
}

impl Type {
    /// https://stackoverflow.com/questions/14821936/what-is-a-scalar-object-in-c#14822074
    #[inline]
    pub fn is_scalar(&self) -> bool {
        use Type::*;
        match self {
            Enum(_, _) => true,
            k if k.is_arithmetic() || k.is_pointer() => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_bool(&self) -> bool {
        match self {
            Type::Bool => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_char(&self) -> bool {
        match self {
            Type::Char(true) => true,
            _ => false,
        }
    }
    #[inline]
    // returns whether `self` is a signed integer type
    pub fn is_signed(&self) -> bool {
        use Type::*;
        match self {
            Bool | Char(true) | Short(true) | Int(true) | Long(true) | Enum(_, _) => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_integral(&self) -> bool {
        use Type::*;
        match self {
            Bool | Char(_) | Short(_) | Int(_) | Long(_) | Enum(_, _) => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_floating(&self) -> bool {
        match self {
            Type::Float | Type::Double => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_arithmetic(&self) -> bool {
        self.is_integral() || self.is_floating()
    }
    #[inline]
    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_void_pointer(&self) -> bool {
        match self {
            Type::Pointer(t) => **t == Type::Void,
            _ => false,
        }
    }
    #[inline]
    pub fn is_char_pointer(&self) -> bool {
        match self {
            Type::Pointer(t) => match **t {
                Type::Char(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
    #[inline]
    /// used for pointer addition and subtraction, see section 6.5.6 of the C11 standard
    pub fn is_pointer_to_complete_object(&self) -> bool {
        match self {
            Type::Pointer(ctype) => ctype.is_complete() && !ctype.is_function(),
            Type::Array(_, _) => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_struct(&self) -> bool {
        match self {
            Type::Struct(_, _) | Type::Union(_, _) => true,
            _ => false,
        }
    }
    #[inline]
    pub fn is_complete(&self) -> bool {
        match self {
            Type::Void | Type::Function(_) | Type::Array(_, ArrayType::Unbounded) => false,
            // TODO: update when we allow incomplete struct and union types (e.g. `struct s;`)
            _ => true,
        }
    }
    #[inline]
    pub fn is_function(&self) -> bool {
        match self {
            Type::Function(_) => true,
            _ => false,
        }
    }
    pub fn member_offset(&self, member: InternedStr) -> Result<u64, ()> {
        match self {
            Type::Struct(_, members) => Ok(self.struct_offset(members, member)),
            Type::Union(_, _) => Ok(0),
            _ => Err(()),
        }
    }
}

impl PartialEq for ArrayType {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
impl Eq for ArrayType {}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        // no prototype: any parameters are allowed
        // TODO: issue a warning if a function has empty parameters, it's a holdover
        // from C89
        self.params.is_empty()
            || other.params.is_empty()
            || self.varargs == other.varargs
            && self.return_type == other.return_type
            // don't require parameter names and storage_class to match
            && self.params
                .iter()
                .zip(other.params.iter())
                .all(|(this_param, other_param)| {
                    this_param.ctype == other_param.ctype
                        && this_param.qualifiers == other_param.qualifiers
                })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_type(self, None, f)
    }
}

pub fn print_type(ctype: &Type, name: Option<InternedStr>, f: &mut fmt::Formatter) -> fmt::Result {
    print_pre(ctype, f)?;
    print_mid(ctype, name, f)?;
    print_post(ctype, f)
}

fn print_pre(ctype: &Type, f: &mut fmt::Formatter) -> fmt::Result {
    use Type::*;
    match ctype {
        Char(signed) | Short(signed) | Int(signed) | Long(signed) => {
            let lower = &format!("{:?}", ctype).to_lowercase();
            let substr = match lower.find('(') {
                Some(n) => &lower[..n],
                None => lower.as_str(),
            };
            write!(f, "{}{}", if *signed { "" } else { "unsigned " }, substr)
        }
        Bool => write!(f, "_Bool"),
        Float | Double | Void => write!(f, "{}", format!("{:?}", ctype).to_lowercase()),
        Pointer(inner) | Array(inner, _) => print_pre(inner, f),
        Function(ftype) => write!(f, "{}", ftype.return_type),
        Enum(Some(ident), _) => write!(f, "enum {}", ident),
        Enum(None, _) => write!(f, "<anonymous enum>"),
        Union(Some(ident), _) => write!(f, "union {}", ident),
        Union(None, _) => write!(f, "<anonymous union>"),
        Struct(Some(ident), _) => write!(f, "struct {}", ident),
        Struct(None, _) => write!(f, "<anonymous struct>"),
        Bitfield(_) => unimplemented!("printing bitfield type"),
        VaList => write!(f, "va_list"),
        Error => write!(f, "<type error>"),
    }
}

fn print_mid(ctype: &Type, name: Option<InternedStr>, f: &mut fmt::Formatter) -> fmt::Result {
    match ctype {
        Type::Pointer(to) => {
            print_mid(to, None, f)?;
            match &**to {
                Type::Array(_, _) | Type::Function(_) => {
                    write!(f, "(*{})", name.unwrap_or_default())?
                }
                _ => write!(f, " *{}", name.unwrap_or_default())?,
            }
        }
        Type::Array(to, _) => print_mid(to, name, f)?,
        _ => {
            if let Some(name) = name {
                write!(f, " {}", name)?;
            }
        }
    }
    Ok(())
}
fn print_post(ctype: &Type, f: &mut fmt::Formatter) -> fmt::Result {
    match ctype {
        Type::Pointer(to) => print_post(to, f),
        Type::Array(to, size) => {
            write!(f, "[")?;
            if let ArrayType::Fixed(size) = size {
                write!(f, "{}", size)?;
            }
            write!(f, "]")?;
            print_post(to, f)
        }
        Type::Function(func_type) => {
            // https://stackoverflow.com/a/30325430
            let mut comma_seperated = "(".to_string();
            for param in &func_type.params {
                comma_seperated.push_str(&param.ctype.to_string());
                if param.id != Default::default() {
                    comma_seperated.push(' ');
                    comma_seperated.push_str(&param.id.to_string());
                }
                comma_seperated.push_str(", ");
            }
            if func_type.varargs {
                comma_seperated.push_str("...");
            } else if !func_type.params.is_empty() {
                comma_seperated.pop();
                comma_seperated.pop();
            }
            comma_seperated.push(')');
            write!(f, "{}", comma_seperated)
        }
        _ => Ok(()),
    }
}

impl FunctionType {
    pub fn should_return(&self) -> bool {
        self.return_type != *VOID
    }
    pub fn has_params(&self) -> bool {
        !(self.params.len() == 1 && self.params[0].ctype == *VOID)
    }
}
