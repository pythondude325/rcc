use cranelift::codegen::ir::{condcodes, types, MemFlags};
use cranelift::prelude::{FunctionBuilder, InstBuilder, Type as IrType, Value as IrValue};
use cranelift_module::DataContext;
use log::debug;

use super::{Compiler, Id};
use crate::data::prelude::*;
use crate::data::{lex::Token, Expr, ExprType};

type IrResult = CompileResult<Value>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Value {
    pub(crate) ir_val: IrValue,
    ir_type: IrType,
    ctype: InternedType,
}

enum FuncCall {
    Named(InternedStr),
    Indirect(Value),
}

impl<'a> Compiler<'a> {
    // clippy doesn't like big match statements, but this is kind of essential complexity,
    // it can't be any smaller without supporting fewer features
    #[allow(clippy::cognitive_complexity)]
    pub(crate) fn compile_expr(&mut self, expr: Expr, builder: &mut FunctionBuilder) -> IrResult {
        let expr = expr.const_fold()?;
        let location = expr.location;
        let ir_type = if expr.lval {
            Type::ptr_type()
        } else {
            expr.ctype.as_ir_type()
        };
        match expr.expr {
            ExprType::Literal(token) => {
                self.compile_literal(ir_type, expr.ctype, token, location, builder)
            }
            ExprType::Id(var) => self.load_addr(var, builder),

            // unary operators
            ExprType::Deref(pointer) => {
                let val = self.compile_expr(*pointer, builder)?;
                let flags = MemFlags::new();
                Ok(Value {
                    ir_type,
                    ctype: expr.ctype,
                    ir_val: builder.ins().load(ir_type, flags, val.ir_val, 0),
                })
            }
            // NOTE: this may be an implicit cast (float f = 1.2) not an explicit cast (1 + (int)1.2)
            // NOTE: it may also be a widening conversion (1 + 1.2)
            ExprType::Cast(orig) => self.cast(*orig, expr.ctype, builder),
            ExprType::Negate(expr) => self.negate(*expr, builder),
            ExprType::BitwiseNot(expr) => self.unary_op(
                *expr,
                builder,
                |ir_val, ir_type, _, builder| match ir_type {
                    ty if ty.is_int() => builder.ins().bnot(ir_val),
                    _ => unreachable!("parser should catch illegal types"),
                },
            ),
            ExprType::LogicalNot(expr) => self.logical_not(*expr, builder),

            // binary operators
            ExprType::Add(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::Plus, builder)
            }
            ExprType::Sub(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::Minus, builder)
            }
            ExprType::Mul(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::Star, builder)
            }
            ExprType::Div(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::Divide, builder)
            }
            ExprType::Mod(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::Mod, builder)
            }
            ExprType::BitwiseAnd(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::Ampersand, builder)
            }
            ExprType::BitwiseOr(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::BitwiseOr, builder)
            }
            // left shift
            ExprType::Shift(left, right, true) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::ShiftRight, builder)
            }
            // right shift
            ExprType::Shift(left, right, false) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::ShiftRight, builder)
            }
            ExprType::Xor(left, right) => {
                self.binary_assign_op(*left, *right, expr.ctype, Token::Xor, builder)
            }
            ExprType::Compare(left, right, token) => self.compare(*left, *right, &token, builder),

            // misfits
            ExprType::Assign(lval, rval, token) => self.assignment(*lval, *rval, token, builder),
            ExprType::FuncCall(func, args) => match func.expr {
                ExprType::Id(var) => self.call(FuncCall::Named(var.id), func.ctype, args, builder),
                _ => {
                    let ctype = func.ctype.clone();
                    let val = self.compile_expr(*func, builder)?;
                    self.call(FuncCall::Indirect(val), ctype, args, builder)
                }
            },
            ExprType::Comma(left, right) => {
                self.compile_expr(*left, builder)?;
                self.compile_expr(*right, builder)
            }
            ExprType::Member(cstruct, id) => {
                let ctype = cstruct.ctype.clone();
                let pointer = self.compile_expr(*cstruct, builder)?;
                let offset = ctype
                    .member_offset(id)
                    .expect("only structs and unions can have members");
                let ir_offset = builder.ins().iconst(Type::ptr_type(), offset as i64);
                Ok(Value {
                    ir_val: builder.ins().iadd(pointer.ir_val, ir_offset),
                    ir_type,
                    ctype,
                })
            }
            ExprType::PostIncrement(lval, increase) => {
                let lval = self.compile_expr(*lval, builder)?;
                let loaded_ctype = match lval.ctype {
                    Type::Pointer(t) => *t,
                    _ => lval.ctype,
                };
                let ir_type = loaded_ctype.as_ir_type();
                let previous_value = Value {
                    ir_val: builder.ins().load(ir_type, MemFlags::new(), lval.ir_val, 0),
                    ir_type,
                    ctype: loaded_ctype,
                };

                let addend = if increase { 1 } else { -1 };
                let (addend_ir, add_func): (_, fn(_, _, _) -> _) = match previous_value.ctype {
                    Type::Double => (builder.ins().f64const(addend as f64), InstBuilder::fadd),
                    Type::Float => (builder.ins().f32const(addend as f32), InstBuilder::fadd),
                    _ => (
                        builder.ins().iconst(previous_value.ir_type, addend),
                        InstBuilder::iadd,
                    ),
                };
                let new_value = add_func(builder.ins(), previous_value.ir_val, addend_ir);
                builder
                    .ins()
                    .store(MemFlags::new(), new_value, lval.ir_val, 0);
                Ok(previous_value)
            }
            ExprType::Noop(inner) => {
                let mut val = self.compile_expr(*inner, builder)?;
                val.ctype = expr.ctype;
                Ok(val)
            }
            ExprType::LogicalOr(left, right) => self.logical_expr(*left, *right, false, builder),
            ExprType::LogicalAnd(left, right) => self.logical_expr(*left, *right, true, builder),
            ExprType::Ternary(condition, left, right) => {
                self.ternary(*condition, *left, *right, builder)
            }
            ExprType::Sizeof(_) => unimplemented!("sizeof variable length arrays"),
            ExprType::StaticRef(_) => {
                unreachable!("static refs can only appear in top level declarations")
            }
        }
    }
    fn ternary(
        &mut self,
        condition: Expr,
        left: Expr,
        right: Expr,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        let target_ebb = builder.create_ebb();
        let target_type = left.ctype.as_ir_type();
        builder.append_ebb_param(target_ebb, target_type);

        let condition = self.compile_expr(condition, builder)?;
        let (ebb_if_true, ebb_if_false) = (builder.create_ebb(), builder.create_ebb());
        builder.ins().brnz(condition.ir_val, ebb_if_true, &[]);
        builder.ins().jump(ebb_if_false, &[]);

        builder.switch_to_block(ebb_if_true);
        let left_val = self.compile_expr(left, builder)?;
        builder.ins().jump(target_ebb, &[left_val.ir_val]);

        builder.switch_to_block(ebb_if_false);
        let right_val = self.compile_expr(right, builder)?;
        builder.ins().jump(target_ebb, &[right_val.ir_val]);
        builder.switch_to_block(target_ebb);

        Ok(Value {
            ir_val: *builder.ebb_params(target_ebb).first().unwrap(),
            ir_type: target_type,
            ctype: left_val.ctype,
        })
    }
    fn logical_expr(
        &mut self,
        left: Expr,
        right: Expr,
        brz: bool,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        let target_ebb = builder.create_ebb();
        builder.append_ebb_param(target_ebb, types::B1);
        let left = self.compile_expr(left, builder)?;

        let branch_func = if brz {
            InstBuilder::brz
        } else {
            InstBuilder::brnz
        };
        branch_func(builder.ins(), left.ir_val, target_ebb, &[left.ir_val]);

        let right = self.compile_expr(right, builder)?;
        builder.ins().jump(target_ebb, &[right.ir_val]);

        builder.switch_to_block(target_ebb);
        Ok(Value {
            ir_val: *builder
                .ebb_params(target_ebb)
                .first()
                .expect("if we passed an EBB arg it should be here"),
            ir_type: types::B1,
            ctype: Type::Bool,
        })
    }
    fn compile_literal(
        &mut self,
        ir_type: IrType,
        ctype: InternedType,
        token: Token,
        location: Location,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        let ir_val = match (token, ir_type) {
            (Token::Int(i), types::B1) => builder.ins().bconst(ir_type, i != 0),
            (Token::Int(i), _) => builder.ins().iconst(ir_type, i),
            (Token::UnsignedInt(u), types::B1) => builder.ins().bconst(ir_type, u != 0),
            (Token::UnsignedInt(u), _) => builder.ins().iconst(ir_type, u as i64),
            (Token::Float(f), types::F32) => builder.ins().f32const(f as f32),
            (Token::Float(f), types::F64) => builder.ins().f64const(f),
            (Token::Char(c), _) => builder.ins().iconst(ir_type, i64::from(c)),
            (Token::Str(string), _) => self.compile_string(string, builder, location)?,
            _ => unimplemented!("aggregate literals"),
        };
        Ok(Value {
            ir_val,
            ir_type,
            ctype,
        })
    }
    fn compile_string(
        &mut self,
        string: InternedStr,
        builder: &mut FunctionBuilder,
        location: Location,
    ) -> CompileResult<IrValue> {
        use cranelift_module::Linkage;
        let name = format!("str.{}", string.to_usize());
        let str_id = match self.module.declare_data(&name, Linkage::Local, false, None) {
            Ok(id) => id,
            Err(err) => semantic_err!(format!("error declaring static string: {}", err), location),
        };
        if self.strings.insert(string, str_id).is_none() {
            let mut ctx = DataContext::new();
            ctx.define(string.resolve_and_clone().into_boxed_str().into());
            self.module
                .define_data(str_id, &ctx)
                .map_err(|err| Locatable {
                    data: format!("error defining static string: {}", err),
                    location,
                })?;
        }
        let addr = self.module.declare_data_in_func(str_id, builder.func);
        Ok(builder.ins().global_value(Type::ptr_type(), addr))
    }
    fn unary_op<F>(&mut self, expr: Expr, builder: &mut FunctionBuilder, func: F) -> IrResult
    where
        F: FnOnce(IrValue, IrType, &Type, &mut FunctionBuilder) -> IrValue,
    {
        let ctype = expr.ctype.clone();
        let val = self.compile_expr(expr, builder)?;
        let ir_val = func(val.ir_val, val.ir_type, &ctype, builder);
        Ok(Value {
            ir_val,
            ctype: *ctype,
            ir_type: val.ir_type,
        })
    }
    #[inline]
    fn binary_assign_op(
        &mut self,
        left: Expr,
        right: Expr,
        ctype: Type,
        token: Token,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        let (left, right) = (
            self.compile_expr(left, builder)?,
            self.compile_expr(right, builder)?,
        );
        Self::binary_assign_ir(left, right, ctype, token, builder)
    }
    fn binary_assign_ir<T: Into<Type>>(
        left: Value,
        right: Value,
        ctype: T,
        token: Token,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        use cranelift::codegen::ir::InstBuilder as b;
        assert_eq!(left.ir_type, right.ir_type);
        let ctype = ctype.into();
        let ir_type = ctype.as_ir_type();
        let signed = ctype.is_signed();
        let func = match (token, ir_type, signed) {
            (Token::Plus, ty, _) if ty.is_int() => b::iadd,
            (Token::Plus, ty, _) if ty.is_float() => b::fadd,
            (Token::Minus, ty, _) if ty.is_int() => b::isub,
            (Token::Minus, ty, _) if ty.is_float() => b::fsub,
            (Token::Star, ty, _) if ty.is_int() => b::imul,
            (Token::Star, ty, _) if ty.is_float() => b::fmul,
            (Token::Divide, ty, true) if ty.is_int() => b::sdiv,
            (Token::Divide, ty, false) if ty.is_int() => b::udiv,
            (Token::Divide, ty, _) if ty.is_float() => b::fdiv,
            (Token::Mod, ty, true) if ty.is_int() => b::srem,
            (Token::Mod, ty, false) if ty.is_int() => b::urem,
            (Token::Ampersand, ty, _) if ty.is_int() => b::band,
            (Token::BitwiseOr, ty, _) if ty.is_int() => b::bor,
            (Token::ShiftLeft, ty, _) if ty.is_int() => b::ishl,
            // arithmetic shift: keeps the sign of `left`
            (Token::ShiftRight, ty, true) if ty.is_int() => b::sshr,
            // logical shift: shifts in zeros
            (Token::ShiftRight, ty, false) if ty.is_int() => b::ushr,
            (Token::Xor, ty, _) if ty.is_int() => b::bxor,
            (token, _, _) => unreachable!(
                "only valid assign binary ops should be passed to binary_assign_op (got {})",
                token
            ),
        };
        let ir_val = func(builder.ins(), left.ir_val, right.ir_val);
        Ok(Value {
            ir_val,
            ir_type,
            ctype,
        })
    }
    fn cast(&mut self, expr: Expr, ctype: InternedType, builder: &mut FunctionBuilder) -> IrResult {
        // calculate this here before it's moved to `compile_expr`
        let orig_signed = expr.ctype.is_signed();
        let original = self.compile_expr(expr, builder)?;
        if ctype == Type::Void {
            // this cast is a no-op, it's just here for the frontend
            return Ok(original);
        }
        let cast_type = ctype.as_ir_type();
        let cast = Self::cast_ir(
            original.ir_type,
            cast_type,
            original.ir_val,
            orig_signed,
            ctype.is_signed(),
            builder,
        );
        Ok(Value {
            ir_val: cast,
            ir_type: cast_type,
            ctype,
        })
    }
    fn cast_ir(
        from: IrType,
        to: IrType,
        val: IrValue,
        from_signed: bool,
        to_signed: bool,
        builder: &mut FunctionBuilder,
    ) -> IrValue {
        // NOTE: we compare the IR types, not the C types, because multiple C types
        // NOTE: may have the same representation (e.g. both `int` and `long` are i64)
        if from == to {
            // no-op
            return val;
        }
        match (from, to) {
            // narrowing and widening float conversions
            (types::F32, types::F64) => builder.ins().fpromote(to, val),
            (types::F64, types::F32) => builder.ins().fdemote(to, val),
            // narrowing and widening integer conversions
            (b, i) if b.is_bool() && i.is_int() => builder.ins().bint(to, val),
            (i, b) if i.is_int() && b.is_bool() => {
                builder.ins().icmp_imm(condcodes::IntCC::NotEqual, val, 0)
            }
            (big_int, small_int)
                if big_int.is_int()
                    && small_int.is_int()
                    && big_int.lane_bits() > small_int.lane_bits() =>
            {
                builder.ins().ireduce(small_int, val)
            }
            (small_int, big_int)
                if big_int.is_int()
                    && small_int.is_int()
                    && big_int.lane_bits() > small_int.lane_bits() =>
            {
                if from_signed {
                    builder.ins().sextend(big_int, val)
                } else {
                    builder.ins().uextend(big_int, val)
                }
            }
            // int/float conversions
            (i, f) if i.is_int() && f.is_float() => {
                if from_signed {
                    builder.ins().fcvt_from_sint(to, val)
                } else {
                    builder.ins().fcvt_from_uint(to, val)
                }
            }
            (f, i) if f.is_float() && i.is_int() => {
                if to_signed {
                    builder.ins().fcvt_to_sint(to, val)
                } else {
                    builder.ins().fcvt_to_uint(to, val)
                }
            }
            // bool/float conversions
            // cranelift doesn't seem to have a builtin way to do this
            // instead, this converts from bool to signed int and then int to float
            (b, f) if b.is_bool() && f.is_float() => {
                let int_val = Self::cast_ir(b, types::I32, val, false, true, builder);
                Self::cast_ir(types::I8, f, int_val, true, true, builder)
            }
            (f, b) if b.is_bool() && f.is_float() => {
                let int_val = Self::cast_ir(f, types::I32, val, true, true, builder);
                Self::cast_ir(types::I8, b, int_val, true, false, builder)
            }
            _ => unreachable!("cast from {} to {}", from, to),
        }
    }
    fn logical_not(&mut self, expr: Expr, builder: &mut FunctionBuilder) -> IrResult {
        let Value {
            ir_type, ir_val, ..
        } = self.compile_expr(expr, builder)?;
        let ir_bool = match ir_type {
            types::F32 => {
                let zero = builder.ins().f32const(0.0);
                builder.ins().fcmp(condcodes::FloatCC::Equal, ir_val, zero)
            }
            types::F64 => {
                let zero = builder.ins().f64const(0.0);
                builder.ins().fcmp(condcodes::FloatCC::Equal, ir_val, zero)
            }
            ty if ty.is_int() => builder.ins().icmp_imm(condcodes::IntCC::Equal, ir_val, 0),
            ty if ty.is_bool() => {
                // TODO: change this if Cranelift ever implements boolean negation on x86
                // see https://github.com/CraneStation/cranelift/issues/922
                let int = Self::cast_ir(ir_type, types::I32, ir_val, false, true, builder);
                builder.ins().icmp_imm(condcodes::IntCC::Equal, int, 0)
            }
            _ => unreachable!("all scalars should be float, int, or bool"),
        };
        Ok(Value {
            ir_val: ir_bool,
            ir_type: types::B1,
            ctype: Type::Bool,
        })
    }
    fn negate(&mut self, expr: Expr, builder: &mut FunctionBuilder) -> IrResult {
        self.unary_op(expr, builder, |ir_val, ir_type, _, builder| match ir_type {
            i if i.is_int() => builder.ins().irsub_imm(ir_val, 0),
            f if f.is_float() => builder.ins().fneg(ir_val),
            _ => unreachable!("parser should catch illegal types"),
        })
    }
    fn load_addr(&self, var: Symbol, builder: &mut FunctionBuilder) -> IrResult {
        let ptr_type = Type::ptr_type();
        let ir_val = match self.scope.get(&var.id).unwrap() {
            Id::Function(func_id) => {
                let func_ref = self.module.declare_func_in_func(*func_id, builder.func);
                builder.ins().func_addr(ptr_type, func_ref)
            }
            Id::Global(static_id) => {
                let global = self.module.declare_data_in_func(*static_id, builder.func);
                builder.ins().global_value(ptr_type, global)
            }
            Id::Local(stack_slot) => builder.ins().stack_addr(ptr_type, *stack_slot, 0),
        };
        let ctype = Type::Pointer(var.ctype);
        Ok(Value {
            ir_type: ctype.as_ir_type(),
            ir_val,
            ctype,
        })
    }
    fn compare(
        &mut self,
        left: Expr,
        right: Expr,
        token: &Token,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        let (left, right) = (
            self.compile_expr(left, builder)?,
            self.compile_expr(right, builder)?,
        );
        assert_eq!(left.ir_type, right.ir_type);

        let ir_val = if left.ir_type.is_int() {
            let code = token
                .to_int_compare(left.ctype.is_signed())
                .expect("Expr::Compare should only have comparison tokens");
            builder.ins().icmp(code, left.ir_val, right.ir_val)
        } else {
            assert!(left.ir_type.is_float());
            let code = token
                .to_float_compare()
                .expect("Expr::Compare should only have comparison tokens");
            builder.ins().fcmp(code, left.ir_val, right.ir_val)
        };
        Ok(Value {
            ir_val,
            ir_type: types::B1,
            ctype: left.ctype,
        })
    }
    fn assignment(
        &mut self,
        lval: Expr,
        rval: Expr,
        token: Token,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        let ctype = lval.ctype.clone();
        let is_id = match lval.expr {
            ExprType::Id(_) => true,
            _ => false,
        };
        let (mut target, mut value) = (
            self.compile_expr(lval, builder)?,
            self.compile_expr(rval, builder)?,
        );
        let ir_target = target.ir_val;
        if token != Token::Equal {
            // need to deref explicitly to get an rval, the frontend didn't do it for us
            if is_id {
                let ctype = match target.ctype {
                    Type::Pointer(t) => *t,
                    _ => unreachable!("parser should only allow lvals to be assigned"),
                };
                let ir_type = ctype.as_ir_type();
                target = Value {
                    ir_val: builder
                        .ins()
                        .load(ir_type, MemFlags::new(), target.ir_val, 0),
                    ir_type,
                    ctype,
                };
            }
            if value.ir_type != target.ir_type {
                unimplemented!(
                    "binary promotion for complex assignment ({} -> {})",
                    value.ir_type,
                    target.ir_type
                );
            }
            value = Self::binary_assign_ir(
                target,
                value,
                ctype,
                token
                    .without_assignment()
                    .expect("only valid assignment tokens should be passed to assignment"),
                builder,
            )?;
        }
        builder
            .ins()
            .store(MemFlags::new(), value.ir_val, ir_target, 0);
        Ok(value)
    }
    fn call(
        &mut self,
        func: FuncCall,
        ctype: Type,
        args: Vec<Expr>,
        builder: &mut FunctionBuilder,
    ) -> IrResult {
        use crate::data::{Qualifiers, StorageClass};
        use cranelift::codegen::ir::{AbiParam, ArgumentPurpose};

        let mut ftype = match ctype {
            Type::Function(ftype) => ftype,
            _ => unreachable!("parser should only allow calling functions"),
        };
        let mut float_variadic = 0;
        if ftype.varargs {
            // needs to be done before we move the args by compiling them
            if self.module.isa().name() != "x86" {
                unimplemented!("variadic args for architectures other than x86");
            }
            // this is an utter hack
            // https://github.com/CraneStation/cranelift/issues/212#issuecomment-549111736
            for arg in &args[ftype.params.len()..] {
                if arg.ctype.is_floating() {
                    float_variadic += 1;
                }
                debug!("adding variadic arg with type {}", arg.ctype);
                ftype.params.push(Symbol {
                    ctype: arg.ctype.clone(),
                    id: Default::default(),
                    init: true,
                    qualifiers: Qualifiers::NONE,
                    storage_class: StorageClass::Auto,
                });
            }
        }
        let mut compiled_args: Vec<IrValue> = args
            .into_iter()
            .map(|arg| self.compile_expr(arg, builder).map(|val| val.ir_val))
            .collect::<CompileResult<_>>()?;
        if ftype.varargs {
            debug!("adding number of float args");
            let float_ir = builder.ins().iconst(types::I8, float_variadic);
            compiled_args.push(float_ir);
        }
        let call = match func {
            FuncCall::Named(func_name) => {
                let func_id = match self.scope.get(&func_name) {
                    Some(Id::Function(func_id)) => *func_id,
                    _ => panic!("parser should catch illegal function calls"),
                };
                let func_ref = self.module.declare_func_in_func(func_id, builder.func);
                let call = builder.ins().call(func_ref, compiled_args.as_slice());
                // stolen from https://github.com/bjorn3/rustc_codegen_cranelift/blob/82fde5b62281fa51a/src/abi/mod.rs#L535
                if ftype.varargs {
                    let call_sig = builder.func.dfg.call_signature(call).unwrap();
                    let al = self
                        .module
                        .isa()
                        .register_info()
                        .parse_regunit("rax")
                        .expect("x86 should have an rax register");
                    let float_arg = AbiParam::special_reg(types::I8, ArgumentPurpose::Normal, al);
                    // NOTE: this is added both here and in signature() because we overwrite the previous params
                    let abi_params = ftype
                        .params
                        .into_iter()
                        .map(|param| AbiParam::new(param.ctype.as_ir_type()))
                        .chain(std::iter::once(float_arg))
                        .collect();
                    builder.func.dfg.signatures[call_sig].params = abi_params;
                }
                call
            }
            FuncCall::Indirect(callee) => {
                let sig = ftype.signature(self.module.isa());
                let sigref = builder.import_signature(sig);
                builder
                    .ins()
                    .call_indirect(sigref, callee.ir_val, compiled_args.as_slice())
            }
        };
        let ir_val = match builder.inst_results(call).first() {
            // Just a placeholder.
            None => builder.ins().iconst(types::I32, 0),
            Some(ret) => *ret,
        };
        Ok(Value {
            ir_val,
            ir_type: ftype.return_type.as_ir_type(),
            ctype: *ftype.return_type,
        })
    }
}
