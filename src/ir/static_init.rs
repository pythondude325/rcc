//! Store an object in static memory.
//! TODO: move this into a semantic analysis stage so that it's easier to deal with structs
//! TODO: see https://github.com/jyn514/rcc/issues/53
use std::convert::{TryFrom, TryInto};

use cranelift::codegen::ir::types;
use cranelift_module::DataContext;

use super::{Compiler, Id};
use crate::arch::{PTR_SIZE, TARGET};
use crate::data::prelude::*;
use crate::data::{types::ArrayType, Initializer};
use crate::utils::warn;

const_assert!(PTR_SIZE <= std::usize::MAX as u16);
const ZERO_PTR: [u8; PTR_SIZE as usize] = [0; PTR_SIZE as usize];

impl<'a> Compiler<'a> {
    pub(crate) fn store_static(
        &mut self,
        mut symbol: Symbol,
        init: Option<Initializer>,
        location: Location,
    ) -> CompileResult<()> {
        use crate::get_str;
        let err_closure = |err| Locatable {
            data: err,
            location,
        };
        let linkage = symbol.storage_class.try_into().map_err(err_closure)?;
        let align = symbol
            .ctype
            .alignof()
            .map_err(|err| err.to_string())
            .and_then(|size| {
                size.try_into()
                    .map_err(|_| format!("align of {} is greater than 256 bytes", symbol.id))
            })
            .map_err(err_closure)?;
        if align == 0 {
            // struct that was declared but never used
            return Ok(());
        }
        let id = self
            .module
            .declare_data(
                get_str!(symbol.id),
                linkage,
                !symbol.qualifiers.c_const,
                Some(align),
            )
            .map_err(|err| Locatable {
                data: format!("error storing static value: {}", err),
                location,
            })?;

        self.scope.insert(symbol.id, Id::Global(id));

        if linkage == Linkage::Import {
            debug_assert!(init.is_none());
            return Ok(());
        }

        let mut ctx = DataContext::new();
        if let Some(init) = init {
            if let Type::Array(_, size @ ArrayType::Unbounded) = &mut *symbol.ctype {
                if let Some(len) = match &init {
                    Initializer::InitializerList(list) => Some(list.len()),
                    Initializer::Scalar(expr) => match &expr.expr {
                        ExprType::Literal(Token::Str(s)) => Some(s.len()),
                        _ => None,
                    },
                    _ => None,
                } {
                    *size = ArrayType::Fixed(len.try_into().unwrap());
                };
            }
            let size_t = symbol.ctype.sizeof().map_err(|err| Locatable {
                data: err.into(),
                location,
            })?;
            let size = size_t
                .try_into()
                .expect("initializer is larger than SIZE_T on host platform");
            let mut buf = vec![0; size];
            let offset = 0;
            self.init_symbol(&mut ctx, &mut buf, offset, init, &symbol.ctype, &location)?;
            ctx.define(buf.into_boxed_slice());
        } else {
            ctx.define_zeroinit(
                symbol
                    .ctype
                    .sizeof()
                    .map_err(|err| err_closure(err.to_string()))? as usize,
            );
        };
        self.module.define_data(id, &ctx).map_err(|err| {
            CompileError::Semantic(Locatable {
                data: format!("error defining static variable: {}", err),
                location,
            })
        })
    }
    fn init_expr(
        &self,
        ctx: &mut DataContext,
        buf: &mut [u8],
        offset: u32,
        expr: Expr,
    ) -> CompileResult<()> {
        let expr = expr.const_fold()?;
        // static address-of
        match expr.expr {
            ExprType::StaticRef(inner) => match inner.expr {
                ExprType::Id(symbol) => self.static_ref(symbol, 0, offset, ctx),
                ExprType::Literal(Token::Str(_)) => {
                    unimplemented!("address of literal in static context")
                }
                ExprType::Literal(ref token) if token.is_zero() => buf.copy_from_slice(&ZERO_PTR),
                ExprType::Cast(ref inner) if inner.is_zero() => buf.copy_from_slice(&ZERO_PTR),
                ExprType::Member(struct_expr, member) => {
                    let member_offset = struct_expr
                        .ctype
                        .member_offset(member)
                        .expect("parser shouldn't allow Member for non-struct types");
                    if let ExprType::Id(symbol) = struct_expr.expr {
                        self.static_ref(symbol, member_offset.try_into().unwrap(), offset, ctx);
                    } else {
                        semantic_err!(
                            "expression is not a compile time constant".into(),
                            struct_expr.location
                        );
                    }
                }
                _ => semantic_err!("cannot take the address of an rvalue".into(), expr.location),
            },
            ExprType::Literal(token) => {
                let bytes = token.into_bytes(&expr.ctype, &expr.location)?;
                buf.copy_from_slice(&bytes);
            }
            _ => semantic_err!(
                "expression is not a compile time constant".into(),
                expr.location
            ),
        }
        Ok(())
    }
    fn static_ref(&self, symbol: Symbol, member_offset: i64, offset: u32, ctx: &mut DataContext) {
        match self.scope.get(&symbol.id) {
            Some(Id::Function(func_id)) => {
                let func_ref = self.module.declare_func_in_data(*func_id, ctx);
                debug_assert!(member_offset == 0);
                ctx.write_function_addr(offset, func_ref);
            }
            Some(Id::Global(data_id)) => {
                let global_val = self.module.declare_data_in_data(*data_id, ctx);
                ctx.write_data_addr(offset, global_val, member_offset);
            }
            Some(Id::Local(_)) => unreachable!("cannot have local variable at global scope"),
            None => unreachable!("parser should catch undeclared variables"),
        }
    }
    fn init_symbol(
        &self,
        ctx: &mut DataContext,
        buf: &mut [u8],
        offset: u32,
        initializer: Initializer,
        ctype: &Type,
        location: &Location,
    ) -> CompileResult<()> {
        match initializer {
            Initializer::InitializerList(mut initializers) => match ctype {
                Type::Array(ty, ArrayType::Unbounded) => {
                    self.init_array(ctx, buf, offset, initializers, ty, location)
                }
                Type::Array(ty, ArrayType::Fixed(size)) => {
                    if initializers.len() as u64 > *size {
                        Err(CompileError::Semantic(Locatable {
                            data: format!(
                                "too many elements for array (expected {}, got {})",
                                size,
                                initializers.len()
                            ),
                            // TODO: this location points to the declarator, not the initializer
                            location: *location,
                        }))
                    } else {
                        self.init_array(ctx, buf, offset, initializers, ty, location)
                    }
                }
                ty if ty.is_scalar() => {
                    assert_eq!(initializers.len(), 1);
                    self.init_symbol(ctx, buf, offset, initializers.remove(0), ctype, location)
                }
                Type::Union(_, members) => self.init_symbol(
                    ctx,
                    buf,
                    offset,
                    initializers.remove(0),
                    &members.first().unwrap().ctype,
                    location,
                ),
                Type::Struct(_, members) => {
                    // TODO: initialize members that aren't named to 0 (struct s { int i; float f; } = { 1 };)
                    for (init, member) in initializers.into_iter().zip(members.iter()) {
                        self.init_symbol(ctx, buf, offset, init, &member.ctype, location)?;
                    }
                    Ok(())
                }
                Type::Bitfield(_) => unimplemented!("bitfield initalizers"),

                Type::Function(_) => unreachable!("function initializers"),
                Type::Void => unreachable!("initializer for void type"),
                _ => unreachable!("scalar types should have been handled"),
            },
            Initializer::Scalar(expr) => self.init_expr(ctx, buf, offset, *expr),
            Initializer::FunctionBody(_) => {
                panic!("function definitions should go through compile_function, not store_static")
            }
        }
    }
    fn init_array(
        &self,
        ctx: &mut DataContext,
        buf: &mut [u8],
        mut offset: u32,
        initializers: Vec<Initializer>,
        inner_type: &Type,
        location: &Location,
    ) -> CompileResult<()> {
        if let Type::Array(_, ArrayType::Unbounded) = inner_type {
            semantic_err!(
                "nested array must declare the size of each inner array".into(),
                *location
            );
        }
        let inner_size: usize = inner_type
            .sizeof()
            .map_err(|err| Locatable {
                data: err.into(),
                location: *location,
            })?
            .try_into()
            .expect("cannot initialize array larger than address space of host");
        let mut element_offset: usize = 0;
        for init in initializers {
            self.init_symbol(
                ctx,
                // pass a buffer of size `inner_size` to `init_symbol`
                &mut buf[element_offset..element_offset + inner_size],
                offset,
                init,
                inner_type,
                location,
            )?;
            offset +=
                u32::try_from(inner_size).expect("cannot initialize array larger than 2^32 bytes");
            element_offset += inner_size;
        }
        // zero-init should already have been taken care of by init_symbol
        Ok(())
    }
}

macro_rules! cast {
    ($i: expr, $from: ty, $to: ty, $ctype: expr, $location: expr) => {{
        let cast = $i as $to;
        if cast as $from != $i {
            warn(
                &format!(
                    "conversion to {} loses precision ({} != {})",
                    $ctype, cast as $from, $i
                ),
                $location,
            )
        }
        cast
    }};
}

macro_rules! bytes {
    ($int: expr, $be: expr) => {{
        let boxed: Box<[u8]> = if $be {
            Box::new($int.to_be_bytes())
        } else {
            Box::new($int.to_le_bytes())
        };
        boxed
    }};
}

impl Token {
    fn into_bytes(self, ctype: &Type, location: &Location) -> CompileResult<Box<[u8]>> {
        let ir_type = ctype.as_ir_type();
        let big_endian = TARGET
            .endianness()
            .expect("target should be big or little endian")
            == target_lexicon::Endianness::Big;

        match self {
            Token::Int(i) => Ok(match ir_type {
                types::I8 => bytes!(cast!(i, i64, i8, &ctype, *location), big_endian),
                types::I16 => bytes!(cast!(i, i64, i16, &ctype, *location), big_endian),
                types::I32 => bytes!(cast!(i, i64, i32, &ctype, *location), big_endian),
                types::I64 => bytes!(i, big_endian),
                x => unreachable!(format!(
                    "ir_type {} for integer {} is not of integer type",
                    x, i
                )),
            }),
            Token::UnsignedInt(i) => Ok(match ir_type {
                types::I8 => bytes!(cast!(i, u64, u8, &ctype, *location), big_endian),
                types::I16 => bytes!(cast!(i, u64, u16, &ctype, *location), big_endian),
                types::I32 => bytes!(cast!(i, u64, u32, &ctype, *location), big_endian),
                types::I64 => bytes!(i, big_endian),
                x => unreachable!(format!(
                    "ir_type {} for integer {} is not of integer type",
                    x, i
                )),
            }),
            Token::Float(f) => Ok(match ir_type {
                types::F32 => {
                    let cast = f as f32;
                    if (f64::from(cast) - f).abs() >= std::f64::EPSILON {
                        warn(&format!("conversion from double to float loses precision ({} is different from {} by more than DBL_EPSILON ({}))",
                        f64::from(cast), f, std::f64::EPSILON), *location);
                    }
                    let float_as_int = cast.to_bits();
                    bytes!(float_as_int, big_endian)
                }
                types::F64 => bytes!(f.to_bits(), big_endian),
                x => unreachable!(format!(
                    "ir_type {} for float {} is not of integer type",
                    x, f
                )),
            }),
            Token::Str(string) => Ok(string.resolve_and_clone().into_boxed_str().into()),
            Token::Char(c) => Ok(Box::new([c])),
            x => unimplemented!("storing static of type {:?}", x),
        }
    }
}

use crate::data::StorageClass;
use cranelift_module::Linkage;

impl TryFrom<StorageClass> for Linkage {
    type Error = String;
    // INVARIANT: this should be the linkage for an object, not for a function
    fn try_from(sc: StorageClass) -> Result<Linkage, String> {
        match sc {
            StorageClass::Extern => Ok(Linkage::Import),
            StorageClass::Static => Ok(Linkage::Local),
            StorageClass::Auto => Ok(Linkage::Export),
            StorageClass::Register => {
                Err(format!("illegal storage class {} for global variable", sc))
            }
            StorageClass::Typedef => unreachable!("typedefs should be handled by parser"),
        }
    }
}
