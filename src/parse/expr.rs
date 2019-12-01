use super::{Lexeme, Parser};
use crate::arch::SIZE_T;
use crate::data::prelude::*;
use crate::data::{lex::Keyword, types::ArrayType, StorageClass::Typedef};

type SyntaxResult = Result<Expr, SyntaxError>;

macro_rules! struct_member_helper {
    ($self: expr, $members: expr, $expr: expr, $id: expr, $location: expr) => {
        if let Some(member) = $members.iter().find(|member| member.id == $id) {
            Ok(Expr {
                ctype: member.ctype.clone(),
                constexpr: $expr.constexpr,
                lval: true,
                location: $location,
                expr: ExprType::Member(Box::new($expr), $id),
            })
        } else {
            $self.semantic_err(
                format!("no member named '{}' in '{}'", $id, $expr.ctype),
                $location,
            );
            Ok($expr)
        }
    };
}

impl<I: Iterator<Item = Lexeme>> Parser<I> {
    /// expr_opt: expr ';' | ';'
    pub fn expr_opt(&mut self, token: Token) -> Result<Option<Expr>, SyntaxError> {
        if self.match_next(&token).is_some() {
            Ok(None)
        } else {
            let expr = self.expr()?;
            self.expect(token)?;
            Ok(Some(expr))
        }
    }
    /// expr(): Parse an expression.
    ///
    /// In programming language terminology, an 'expression' is anything that has a value.
    /// This can be a literal, a function call, or a series of operations like `2 + 3`.
    /// You can think of it as anything you can put on the right-hand side of an assignment.
    /// Contrast expressions to statements, which do NOT have values: you can't assign a while loop.
    ///
    /// For more discussion on the difference between expressions and statements,
    /// see https://stackoverflow.com/a/19224 or an introductory programming textbook
    /// (it will be discussed in the first few chapters,
    /// in a part of the book no one usually reads).
    // Original:
    // expr:
    //      assignment_expr
    //      | expr ',' assignment_expr
    //      ;
    //
    // We rewrite it as follows:
    // expr:
    //     assignment_expr (',' assignment_expr)*
    //     ;
    //
    // Comma operator: evalutate the first expression (usually for its side effects)
    // and return the second
    pub fn expr(&mut self) -> SyntaxResult {
        self.left_associative_binary_op(
            Self::assignment_expr,
            &[&Token::Comma],
            |left, right, token| {
                let right = right.rval();
                Ok(Expr {
                    ctype: right.ctype.clone(),
                    // TODO: this is technically right but will almost certainly be buggy
                    // If we use constexpr to check if we can optimize things out,
                    // then we'll discard all side effects from `left`.
                    // That's not really `expr()`'s problem, but it is something the constant
                    // folding has to worry about
                    constexpr: right.constexpr,
                    lval: false,
                    expr: ExprType::Comma(Box::new(*left), Box::new(right)),
                    location: token.location,
                })
            },
        )
    }

    /// Parses an expression and ensures that it can be evaluated at compile time.
    // constant_expr: conditional_expr;
    pub fn constant_expr(&mut self) -> SyntaxResult {
        let expr = self.conditional_expr()?;
        if !expr.constexpr {
            self.semantic_err("not a constant expression".to_string(), expr.location)
        }
        Ok(expr)
    }

    /// assignment_expr
    /// : conditional_expr
    /// | unary_expr assignment_operator assignment_expr
    /// ;
    ///
    /// Assignment expression: evaluate the right-hand side, assign it to the left,
    /// and return the right.
    ///
    /// Unlike most operators, this is right-associative, that is
    /// `a = b = 3` is parsed as `a = (b = 3)`. Contrast addition:
    /// `a + b + 3` is parsed as `(a + b) + 3`.
    ///
    /// NOTE: because it's hard to tell the different between lvals and rvals in the grammar,
    /// we parse an entire expression, see if an assignment operator follows it,
    /// and only then check if the left is an lval.
    /// NOTE: comma operators aren't allowed on the RHS of an assignment.
    ///
    /// This is public so that we can parse expressions that can't have commas
    /// (usually initializers)
    pub fn assignment_expr(&mut self) -> SyntaxResult {
        let lval = self.conditional_expr()?;
        if self
            .peek_token()
            .map_or(false, Token::is_assignment_operator)
        {
            let assign_op = self.next_token().unwrap();
            let mut rval = self.assignment_expr()?.rval();
            if !lval.lval {
                self.semantic_err(
                    "expression is not assignable".to_string(),
                    assign_op.location,
                );
                Ok(lval)
            } else {
                if rval.ctype != lval.ctype {
                    rval = rval
                        .cast(&lval.ctype)
                        .into_inner(self.default_err_handler());
                }
                if rval.ctype.is_struct() {
                    unimplemented!("struct assignment");
                }
                Ok(Expr {
                    ctype: lval.ctype.clone(),
                    constexpr: rval.constexpr,
                    lval: false, // `(i = j) = 4`; is invalid
                    location: assign_op.location,
                    expr: ExprType::Assign(Box::new(lval), Box::new(rval), assign_op.data),
                })
            }
        } else {
            Ok(lval)
        }
    }

    /// conditional_expr
    /// : logical_or_expr
    /// | logical_or_expr '?' expr ':' conditional_expr
    /// ;
    ///
    /// Ternary operator. If logical_or_expr evaluates to true,
    /// evaluates to `expr`, otherwise evaluates to `conditional_expr`.
    /// This is the analog to `if` statements for expressions.
    ///
    /// Note that comma operators are allowed within ternaries (!!).
    ///
    /// The C standard requires that `expr` and `conditional_expr` have compatible types;
    /// see https://stackoverflow.com/questions/13318336/ or section 6.5.15 of
    /// http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf for formal requirements.
    /// It does not specify what happens if this is not the case.
    /// Clang and GCC give a warning; we are more strict and emit an error.
    fn conditional_expr(&mut self) -> SyntaxResult {
        let condition = self.logical_or_expr()?;
        if let Some(Locatable { location, .. }) = self.match_next(&Token::Question) {
            let condition = condition.truthy().into_inner(self.compile_err_handler());
            let mut then = self.expr()?.rval();
            self.expect(Token::Colon)?;
            let mut otherwise = self.conditional_expr()?.rval();
            if then.ctype.is_arithmetic() && otherwise.ctype.is_arithmetic() {
                let (tmp1, tmp2) =
                    Expr::binary_promote(then, otherwise).into_inner(self.default_err_handler());
                then = tmp1;
                otherwise = tmp2;
            } else if !Type::pointer_promote(&mut then, &mut otherwise) {
                self.semantic_err(
                    format!(
                        "incompatible types in ternary expression: '{}' cannot be converted to '{}'",
                        then.ctype, otherwise.ctype
                    ),
                    location,
                );
            }
            Ok(Expr {
                ctype: then.ctype.clone(),
                // TODO: evaluate condition and only require the corresponding
                // expression to be constexpr
                constexpr: condition.constexpr && then.constexpr && otherwise.constexpr,
                lval: false,
                location,
                expr: ExprType::Ternary(Box::new(condition), Box::new(then), Box::new(otherwise)),
            })
        } else {
            Ok(condition)
        }
    }

    /// Original:
    /// logical_or_expr
    /// : logical_and_expr
    /// | logical_or_expr OR_OP logical_and_expr
    /// ;
    ///
    /// Rewritten:
    /// logical_or_expr:
    ///     logical_and_expr (OR_OP logical_and_expr)*
    ///
    /// A logical or ('||') evaluates the left-hand side. If the left is falsy, it evaluates and
    /// returns the right-hand side as a boolean.
    /// Both the left and right hand sides must be scalar types.
    fn logical_or_expr(&mut self) -> SyntaxResult {
        self.scalar_left_associative_binary_op(
            Self::logical_and_expr,
            |a, b| {
                Ok(ExprType::LogicalOr(
                    Box::new(a.cast(&Type::Bool)?),
                    Box::new(b.cast(&Type::Bool)?),
                ))
            },
            &Token::LogicalOr,
            Type::Bool,
        )
    }

    /// logical_and_expr
    /// : inclusive_or_expr
    /// | logical_and_expr AND_OP inclusive_or_expr
    /// ;
    ///
    /// Rewritten:
    /// logical_and_expr:
    ///     inclusive_or_expr (AND_OP inclusive_or_expr)*
    ///
    /// A logical and ('&&') evaluates the left-hand side. If the left is truthy, it evaluates
    /// and returns the right-hand side as a boolean.
    /// Both the left and right hand sides must be scalar types.
    fn logical_and_expr(&mut self) -> SyntaxResult {
        self.scalar_left_associative_binary_op(
            Self::inclusive_or_expr,
            |a, b| {
                Ok(ExprType::LogicalAnd(
                    Box::new(a.cast(&Type::Bool)?),
                    Box::new(b.cast(&Type::Bool)?),
                ))
            },
            &Token::LogicalAnd,
            Type::Bool,
        )
    }

    /// inclusive_or_expr
    /// : exclusive_or_expr
    /// | inclusive_or_expr '|' exclusive_or_expr
    /// ;
    ///
    /// Rewritten similarly.
    fn inclusive_or_expr(&mut self) -> SyntaxResult {
        self.integral_left_associative_binary_op(
            Self::exclusive_or_expr,
            &[&Token::BitwiseOr],
            Expr::default_expr(ExprType::BitwiseOr),
        )
    }

    /// exclusive_or_expr
    /// : and_expr
    /// | exclusive_or_expr '^' and_expr
    /// ;
    fn exclusive_or_expr(&mut self) -> SyntaxResult {
        self.integral_left_associative_binary_op(
            Self::and_expr,
            &[&Token::Xor],
            Expr::default_expr(ExprType::Xor),
        )
    }

    /// and_expr
    /// : equality_expr
    /// | and_expr '&' equality_expr
    /// ;
    fn and_expr(&mut self) -> SyntaxResult {
        self.integral_left_associative_binary_op(
            Self::equality_expr,
            &[&Token::Ampersand],
            Expr::default_expr(ExprType::BitwiseAnd),
        )
    }

    /// equality_expr
    /// : relational_expr
    /// | equality_expr EQ_OP relational_expr
    /// | equality_expr NE_OP relational_expr
    /// ;
    fn equality_expr(&mut self) -> SyntaxResult {
        self.left_associative_binary_op(
            Self::relational_expr,
            &[&Token::EqualEqual, &Token::NotEqual],
            Expr::relational_expr,
        )
    }

    /// relational_expr
    /// : shift_expr
    /// | relational_expr '<' shift_expr
    /// | relational_expr '>' shift_expr
    /// | relational_expr LE_OP shift_expr
    /// | relational_expr GE_OP shift_expr
    /// ;
    fn relational_expr(&mut self) -> SyntaxResult {
        self.left_associative_binary_op(
            Self::shift_expr,
            &[
                &Token::Less,
                &Token::Greater,
                &Token::LessEqual,
                &Token::GreaterEqual,
            ],
            Expr::relational_expr,
        )
    }

    /// shift_expr
    /// : additive_expr
    /// | shift_expr LEFT_OP additive_expr
    /// | shift_expr RIGHT_OP additive_expr
    /// ;
    fn shift_expr(&mut self) -> SyntaxResult {
        self.integral_left_associative_binary_op(
            Self::additive_expr,
            &[&Token::ShiftLeft, &Token::ShiftRight],
            |left, right, token| {
                Ok(Expr {
                    ctype: left.ctype.clone(),
                    location: token.location,
                    lval: false,
                    constexpr: left.constexpr && right.constexpr,
                    expr: ExprType::Shift(
                        Box::new(left),
                        Box::new(right),
                        token.data == Token::ShiftLeft,
                    ),
                })
            },
        )
    }

    /// additive_expr
    /// : multiplicative_expr
    /// | additive_expr '+' multiplicative_expr
    /// | additive_expr '-' multiplicative_expr
    /// ;
    fn additive_expr(&mut self) -> SyntaxResult {
        self.left_associative_binary_op(
            Self::multiplicative_expr,
            &[&Token::Plus, &Token::Minus],
            |mut left, mut right, token| {
                match (&left.ctype, &right.ctype) {
                    (Type::Pointer(to), i)
                    | (Type::Array(to, _), i) if i.is_integral() && to.is_complete() => {
                        let to = to.clone();
                        let (left, right) = (left.rval(), right.rval());
                        return Expr::pointer_arithmetic(left, right, &*to, token.location);
                    }
                    (i, Type::Pointer(to))
                        // `i - p` for pointer p is not valid
                    | (i, Type::Array(to, _)) if i.is_integral() && token.data == Token::Plus && to.is_complete() => {
                        let to = to.clone();
                        let (left, right) = (left.rval(), right.rval());
                        return Expr::pointer_arithmetic(right, left, &*to, token.location);
                    }
                    _ => {}
                };
                let (ctype, lval) = if left.ctype.is_arithmetic() && right.ctype.is_arithmetic() {
                    let tmp = Expr::binary_promote(*left, *right).map_err(flatten)?;
                    *left = tmp.0;
                    right = Box::new(tmp.1);
                    (left.ctype.clone(), false)
                // `p1 + p2` for pointers p1 and p2 is not valid
                } else if token.data == Token::Minus && left.ctype.is_pointer_to_complete_object() && left.ctype == right.ctype {
                    // not sure what type to use here, C11 standard doesn't mention it
                    (left.ctype.clone(), true)
                } else {
                    return Err((Locatable::new(
                        format!(
                            "invalid operators for '{}' (expected either arithmetic types or pointer operation, got '{} {} {}'",
                            token.data,
                            left.ctype,
                            token.data,
                            right.ctype
                        ),
                        token.location
                    ), *left));
                };
                Ok(Expr {
                    ctype,
                    lval,
                    location: token.location,
                    constexpr: left.constexpr && right.constexpr,
                    expr: (if token.data == Token::Plus {
                        ExprType::Add
                    } else {
                        ExprType::Sub
                    })(Box::new(*left), right),
                })
            },
        )
    }

    /// multiplicative_expr
    /// : cast_expr
    /// | multiplicative_expr '*' cast_expr
    /// | multiplicative_expr '/' cast_expr
    /// | multiplicative_expr '%' cast_expr
    /// ;
    fn multiplicative_expr(&mut self) -> SyntaxResult {
        self.left_associative_binary_op(
            Self::cast_expr,
            &[&Token::Star, &Token::Divide, &Token::Mod],
            |left, right, token| {
                if token.data == Token::Mod
                    && !(left.ctype.is_integral() && right.ctype.is_integral())
                {
                    return Err((Locatable::new(
                        format!(
                            "expected integers for both operators of %, got '{}' and '{}'",
                            left.ctype, right.ctype
                        ),
                        token.location,
                    ), *left));
                } else if !(left.ctype.is_arithmetic() && right.ctype.is_arithmetic()) {
                    return Err((Locatable::new(
                        format!(
                            "expected float or integer types for both operands of {}, got '{}' and '{}'",
                            token.data, left.ctype, right.ctype
                        ),
                        token.location,
                    ), *left));
                }
                let (p_left, right) = Expr::binary_promote(*left, *right).map_err(flatten)?;
                Ok(Expr {
                    ctype: p_left.ctype.clone(),
                    location: token.location,
                    constexpr: p_left.constexpr && right.constexpr,
                    lval: false,
                    expr: match token.data {
                        Token::Star => ExprType::Mul(Box::new(p_left), Box::new(right)),
                        Token::Divide => ExprType::Div(Box::new(p_left), Box::new(right)),
                        Token::Mod => ExprType::Mod(Box::new(p_left), Box::new(right)),
                        _ => {
                            panic!("left_associative_binary_op should only return tokens given to it")
                        }
                    },
                })
            },
        )
    }

    /// cast_expr
    /// : unary_expr
    /// | '(' type_name ')' cast_expr
    /// ;
    fn cast_expr(&mut self) -> SyntaxResult {
        let seen_param = self.peek_token() == Some(&Token::LeftParen);
        let next_token = self.peek_next_token();
        let is_cast = seen_param
            && match next_token {
                Some(Token::Keyword(k)) => k.is_decl_specifier(),
                Some(Token::Id(id)) => {
                    let id = *id;
                    match self.scope.get(&id) {
                        Some(symbol) => symbol.storage_class == Typedef,
                        _ => false,
                    }
                }
                _ => false,
            };
        if is_cast {
            self.next_token();
            let Locatable {
                location,
                data: (ctype, _),
            } = self.type_name()?;
            self.expect(Token::RightParen)?;
            let expr = self.cast_expr()?.rval();
            if ctype == Type::Void {
                // casting anything to void is allowed
                return Ok(Expr {
                    lval: false,
                    constexpr: expr.constexpr,
                    ctype,
                    // this just signals to the backend to ignore this outer expr
                    expr: ExprType::Cast(Box::new(expr)),
                    location,
                });
            }
            if !ctype.is_scalar() {
                self.semantic_err(
                    format!("cannot cast to non-scalar type '{}'", ctype),
                    location,
                );
            } else if expr.ctype.is_floating() && ctype.is_pointer()
                || expr.ctype.is_pointer() && ctype.is_floating()
            {
                self.semantic_err(
                    format!("cannot cast pointer to float or vice versa. hint: if you really want to do this, use '({})(int)' instead",
                    ctype),
                    location,
                );
            } else if expr.ctype.is_struct() {
                // not implemented: galaga (https://github.com/jyn514/rcc/issues/98)
                self.semantic_err("cannot cast a struct to any type", location);
            }
            Ok(Expr {
                lval: false,
                constexpr: expr.constexpr,
                expr: ExprType::Cast(Box::new(expr)),
                ctype,
                location,
            })
        } else {
            self.unary_expr()
        }
    }

    /// unary_expr
    /// : postfix_expr
    /// | INC_OP unary_expr
    /// | DEC_OP unary_expr
    /// | unary_operator cast_expr
    /// | SIZEOF unary_expr
    /// | SIZEOF '(' type_name ')'
    /// ;
    fn unary_expr(&mut self) -> SyntaxResult {
        match self.peek_token() {
            Some(Token::PlusPlus) => {
                let Locatable { location, .. } = self.next_token().unwrap();
                let expr = self.unary_expr()?;
                Ok(Expr::increment_op(true, true, expr, location)
                    .into_inner(self.default_err_handler()))
            }
            Some(Token::MinusMinus) => {
                let Locatable { location, .. } = self.next_token().unwrap();
                let expr = self.unary_expr()?;
                Ok(Expr::increment_op(true, false, expr, location)
                    .into_inner(self.default_err_handler()))
            }
            Some(Token::Keyword(Keyword::Sizeof)) => {
                self.next_token();
                let (location, ctype) = if self.match_next(&Token::LeftParen).is_some() {
                    let ret = match self.peek_token() {
                        Some(Token::Keyword(k)) if k.is_decl_specifier() => {
                            let ty = self.type_name()?;
                            (ty.location, ty.data.0)
                        }
                        _ => {
                            let expr = self.expr()?;
                            (expr.location, expr.ctype)
                        }
                    };
                    self.expect(Token::RightParen)?;
                    ret
                } else {
                    let result = self.unary_expr()?;
                    (result.location, result.ctype)
                };
                Ok(Expr {
                    // the C11 standard states (6.5.3.4)
                    // "If the type of the operand is a variable length array type, the operand is evaluated; otherwise, the operand is not evaluated and the result is an integer constant."
                    // In an example, it states that the following code would perform
                    // an 'execution time `sizeof`':
                    // int f(int n) {
                    //   char b[n+3];
                    //   return sizeof b;
                    // }
                    // We do not currently handle this case.
                    constexpr: true,
                    expr: ExprType::Sizeof(ctype),
                    lval: false,
                    location,
                    ctype: Type::Int(false),
                })
            }
            Some(op) if op.is_unary_operator() => {
                let Locatable { location, data: op } = self.next_token().unwrap();
                let expr = self.cast_expr()?;
                match op {
                    // TODO: semantic checking for expr
                    Token::Ampersand => match expr.expr {
                        // parse &*p as p
                        ExprType::Deref(inner) => Ok(*inner),
                        _ if expr.lval => Ok(Expr {
                            constexpr: false,
                            lval: false,
                            location,
                            ctype: Type::Pointer(Box::new(expr.ctype.clone())),
                            expr: expr.expr,
                        }),
                        _ => {
                            self.semantic_err("cannot take address of a value", location);
                            Ok(expr)
                        }
                    },
                    Token::Star => match &expr.ctype {
                        Type::Array(t, _) => Ok(Expr {
                            constexpr: false,
                            lval: true,
                            ctype: (**t).clone(),
                            location,
                            expr: expr.expr,
                        }),
                        Type::Pointer(t) => Ok(Expr {
                            constexpr: expr.constexpr,
                            lval: !t.is_function(),
                            ctype: (**t).clone(),
                            location,
                            // this is super hacky but the only way I can think of to prevent
                            // https://github.com/jyn514/rcc/issues/90
                            expr: ExprType::Noop(Box::new(expr.rval())),
                        }),
                        _ => {
                            self.semantic_err(
                                format!(
                                    "cannot dereference expression of non-pointer type '{}'",
                                    expr.ctype
                                ),
                                location,
                            );
                            Ok(expr)
                        }
                    },
                    Token::Plus => {
                        if !expr.ctype.is_arithmetic() {
                            self.semantic_err(
                                format!("cannot use unary plus on expression of non-arithmetic type '{}'",
                                expr.ctype),
                                location,
                            );
                            Ok(expr)
                        } else {
                            let expr = expr
                                .integer_promote()
                                .into_inner(self.default_err_handler());
                            Ok(Expr {
                                lval: false,
                                location,
                                ..expr
                            })
                        }
                    }
                    Token::Minus => {
                        if !expr.ctype.is_arithmetic() {
                            self.semantic_err(
                                format!("cannot use unary minus on expression of non-arithmetic type '{}'", expr.ctype),
                                location,
                            );
                            Ok(expr)
                        } else {
                            let expr = expr
                                .integer_promote()
                                .into_inner(self.default_err_handler());
                            Ok(Expr {
                                lval: false,
                                ctype: expr.ctype.clone(),
                                constexpr: expr.constexpr,
                                location,
                                expr: ExprType::Negate(Box::new(expr)),
                            })
                        }
                    }
                    Token::BinaryNot => {
                        if !expr.ctype.is_integral() {
                            self.semantic_err(
                                format!("cannot use unary negation on expression of non-integer type '{}'", expr.ctype),
                                location,
                            );
                            Ok(expr)
                        } else {
                            let expr = expr
                                .integer_promote()
                                .into_inner(|err| self.semantic_err(err.data, err.location));
                            Ok(Expr {
                                lval: false,
                                ctype: expr.ctype.clone(),
                                constexpr: expr.constexpr,
                                location,
                                expr: ExprType::BitwiseNot(Box::new(expr)),
                            })
                        }
                    }
                    Token::LogicalNot => Ok(expr.logical_not(location)),
                    x => unreachable!("didn't expect '{}' to be an unary operand", x),
                }
            }
            _ => self.postfix_expr(),
        }
    }

    /// postfix_expr
    /// : primary_expr
    /// | postfix_expr '[' expr ']'
    /// | postfix_expr '(' argument_expr_list_opt ')'
    /// | postfix_expr '.' identifier
    /// | postfix_expr PTR_OP identifier
    /// | postfix_expr INC_OP
    /// | postfix_expr DEC_OP
    /// ;
    fn postfix_expr(&mut self) -> SyntaxResult {
        let mut expr = self.primary_expr()?;
        while let Some(Locatable {
            location,
            data: token,
        }) = self.next_token()
        {
            expr = match token {
                // a[i] desugars to *(a + i)
                Token::LeftBracket => {
                    let left = expr.rval();
                    let right = self.expr()?.rval();
                    self.expect(Token::RightBracket)?;
                    let (target_type, array, index) = match (&left.ctype, &right.ctype) {
                        (Type::Pointer(target), _) => ((**target).clone(), left, right),
                        (_, Type::Pointer(target)) => ((**target).clone(), right, left),
                        (l, r) => {
                            self.semantic_err(
                                format!("neither {} nor {} are pointers types", l, r),
                                location,
                            );
                            expr = left;
                            continue;
                        }
                    };
                    let mut addr = Expr::pointer_arithmetic(array, index, &target_type, location)
                        .into_inner(|err| self.semantic_err(err.data, err.location));
                    addr.ctype = target_type;
                    addr.lval = true;
                    addr
                }
                // function call
                Token::LeftParen => {
                    let args = self.argument_expr_list_opt()?;
                    self.expect(Token::RightParen)?;
                    // if fp is a function pointer, fp() desugars to (*fp)()
                    match expr.ctype {
                        Type::Pointer(ref pointee) if pointee.is_function() => {
                            expr = Expr {
                                lval: false,
                                location: expr.location,
                                constexpr: expr.constexpr,
                                ctype: (**pointee).clone(),
                                expr: ExprType::Deref(Box::new(expr)),
                            }
                        }
                        _ => (),
                    };
                    let functype = match expr.ctype {
                        Type::Function(ref functype) => functype,
                        Type::Error => continue, // we've already reported this error
                        _ => {
                            self.semantic_err(
                                format!("called object of type '{}' is not a function", expr.ctype),
                                location,
                            );
                            continue;
                        }
                    };
                    let mut expected = functype.params.len();
                    if expected == 1 && functype.params[0].ctype == Type::Void {
                        expected = 0;
                    }
                    if !functype.params.is_empty()
                        && (args.len() < expected || args.len() > expected && !functype.varargs)
                    {
                        self.semantic_err(
                            format!(
                                "too {} arguments to function call: expected {}, have {}",
                                if args.len() > expected { "many" } else { "few" },
                                expected,
                                args.len(),
                            ),
                            location,
                        );
                    }
                    let mut promoted_args = vec![];
                    for (i, arg) in args.into_iter().enumerate() {
                        let maybe_err = match functype.params.get(i) {
                            Some(expected) => arg.rval().cast(&expected.ctype),
                            None => arg.default_promote(),
                        };
                        let promoted = maybe_err.into_inner(self.default_err_handler());
                        promoted_args.push(promoted);
                    }
                    Expr {
                        location,
                        constexpr: false,
                        lval: false, // no move semantics here!
                        ctype: *functype.return_type.clone(),
                        expr: ExprType::FuncCall(Box::new(expr), promoted_args),
                    }
                }
                Token::Dot => {
                    let Locatable { location, data } =
                        self.expect(Token::Id(Default::default()))?;
                    let id = match data {
                        Token::Id(id) => id,
                        _ => unreachable!("bug in Parser::expect"),
                    };
                    self.struct_member(expr, id, location)?
                }
                Token::StructDeref => {
                    let Locatable { location, data } =
                        self.expect(Token::Id(Default::default()))?;
                    let id = match data {
                        Token::Id(id) => id,
                        _ => unreachable!("bug in Parser::expect"),
                    };
                    let struct_type = match &expr.ctype {
                        Type::Pointer(ctype) => match **ctype {
                            Type::Union(_, _) | Type::Struct(_, _) => (**ctype).clone(),
                            _ => {
                                self.semantic_err(
                                    "pointer does not point to a struct or union",
                                    location,
                                );
                                continue;
                            }
                        },
                        _ => {
                            self.semantic_err(
                                "cannot use '->' operator on type that is not a pointer",
                                location,
                            );
                            continue;
                        }
                    };
                    let expr = Expr {
                        constexpr: false,
                        location,
                        ctype: struct_type,
                        lval: true,
                        expr: ExprType::Deref(Box::new(expr)),
                    };
                    self.struct_member(expr, id, location)?
                }
                Token::PlusPlus => Expr::increment_op(false, true, expr, location)
                    .into_inner(self.default_err_handler()),
                Token::MinusMinus => Expr::increment_op(false, false, expr, location)
                    .into_inner(self.default_err_handler()),
                _ => {
                    self.unput(Some(Locatable {
                        location,
                        data: token,
                    }));
                    break;
                }
            }
        }
        Ok(expr)
    }

    /// argument_expr_list_opt
    /// : /* empty */
    /// | assignment_expr (',' assignment_expr)*
    /// ;
    fn argument_expr_list_opt(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        if self.peek_token() == Some(&Token::RightParen) {
            return Ok(vec![]);
        }
        let mut args = vec![self.assignment_expr()?];
        while self.match_next(&Token::Comma).is_some() {
            args.push(self.assignment_expr()?);
        }
        Ok(args)
    }

    /// primary_expr
    /// : identifier
    /// | INT_CONSTANT
    /// | DOUBLE_CONSTANT
    /// | STRING_LITERAL
    /// | '(' expr ')'
    /// ;
    fn primary_expr(&mut self) -> SyntaxResult {
        use Token::*;
        if let Some(Locatable { location, data }) = self.next_token() {
            match data {
                Id(name) => match self.scope.get(&name) {
                    None => {
                        self.semantic_err(
                            format!("use of undeclared identifier '{}'", name),
                            location,
                        );
                        let mut pretend_zero = Expr::zero(location);
                        pretend_zero.ctype = Type::Error;
                        Ok(pretend_zero)
                    }
                    Some(symbol) => {
                        if let Type::Enum(ident, members) = &symbol.ctype {
                            let enumerator = members.iter().find_map(|(member, value)| {
                                if name == *member {
                                    Some(*value)
                                } else {
                                    None
                                }
                            });
                            if let Some(e) = enumerator {
                                return Ok(Expr {
                                    constexpr: true,
                                    ctype: Type::Enum(*ident, members.clone()),
                                    location,
                                    lval: false,
                                    expr: ExprType::Literal(Token::Int(e)),
                                });
                            }
                        }
                        Ok(Expr::id(symbol, location))
                    }
                },
                Char(literal) => Ok(Expr::char_literal(literal, location)),
                Str(literal) => Ok(Expr::string_literal(literal, location)),
                Int(literal) => Ok(Expr::int_literal(literal, location)),
                UnsignedInt(literal) => Ok(Expr::unsigned_int_literal(literal, location)),
                Float(literal) => Ok(Expr::float_literal(literal, location)),
                LeftParen => {
                    let expr = self.expr()?;
                    self.expect(RightParen)?;
                    Ok(expr)
                }
                other => {
                    let err = Err(Locatable {
                        location,
                        data: format!("expected '(' or literal in expression, got '{}'", other),
                    }
                    .into());
                    self.unput(Some(Locatable {
                        location,
                        data: other,
                    }));
                    err
                }
            }
        } else {
            Err(Locatable {
                location: self.next_location(),
                data: "expected '(' or literal in expression, got <end-of-file>".to_string(),
            }
            .into())
        }
    }

    // parse a struct member
    // used for both s.a and s->a
    fn struct_member(&mut self, expr: Expr, id: InternedStr, location: Location) -> SyntaxResult {
        match &expr.ctype {
            Type::Struct(_, members) | Type::Union(_, members) => {
                if members.is_empty() {
                    self.semantic_err(format!("{} has not yet been defined", expr.ctype), location);
                    Ok(expr)
                } else {
                    struct_member_helper!(self, members, expr, id, location)
                }
            }
            _ => {
                self.semantic_err(
                    format!("expected struct or union, got type '{}'", expr.ctype),
                    location,
                );
                Ok(expr)
            }
        }
    }

    /// Parse a grammar rule of the form
    /// rule:
    ///     grammar_item (TOKEN grammar_item)*
    ///
    /// which requires its operands to be scalar rvalues.
    ///
    /// next_grammar_func should parse `grammar_item`.
    /// `expr_func` is usually an Enum constructor.
    /// `token` should be the token for the operation.
    /// `ctype` should be the type of the resulting expression (_not_ the operands).
    fn scalar_left_associative_binary_op<E, G>(
        &mut self,
        next_grammar_func: G,
        expr_func: E,
        token: &Token,
        ctype: Type,
    ) -> SyntaxResult
    where
        E: Fn(Box<Expr>, Box<Expr>) -> Result<ExprType, (SemanticError, Expr)>,
        G: Fn(&mut Self) -> SyntaxResult,
    {
        self.left_associative_binary_op(next_grammar_func, &[token], move |left, right, token| {
            let non_scalar = if !left.ctype.is_scalar() {
                Some(left.ctype.clone())
            } else if !right.ctype.is_scalar() {
                Some(right.ctype.clone())
            } else {
                None
            };
            if let Some(ctype) = non_scalar {
                return Err((Locatable {
                    data: format!(
                        "expected scalar type (number or pointer) for both operands of '{}', got '{}'",
                        token.data, ctype
                    ),
                    location: token.location,
                }, *left));
            }
            Ok(Expr {
                lval: false,
                constexpr: left.constexpr && right.constexpr,
                location: token.location,
                ctype: ctype.clone(),
                expr: expr_func(Box::new(left.rval()), Box::new(right.rval()))?,
            })
        })
    }

    /// Parse a grammar rule of the form
    /// rule:
    ///     grammar_item (TOKEN grammar_item)*
    ///
    /// which requires its operands to be integral.
    /// The type of the resulting expression will be the same as that of its inputs.
    ///
    /// next_grammar_func should parse `grammar_item`.
    /// `expr_func` is usually an Enum constructor.
    fn integral_left_associative_binary_op<E, G>(
        &mut self,
        next_grammar_func: G,
        tokens: &[&Token],
        expr_func: E,
    ) -> SyntaxResult
    where
        E: Fn(Expr, Expr, Locatable<Token>) -> RecoverableResult<Expr, Locatable<String>>,
        G: Fn(&mut Self) -> SyntaxResult,
    {
        self.left_associative_binary_op(next_grammar_func, tokens, |expr, next, token| {
            let non_scalar = if !expr.ctype.is_integral() {
                Some(&expr.ctype)
            } else if !next.ctype.is_integral() {
                Some(&next.ctype)
            } else {
                None
            };
            if let Some(ctype) = non_scalar {
                return Err((
                    Locatable {
                        data: format!(
                            "expected integer on both sides of '{}', got '{}'",
                            token.data, ctype
                        ),
                        location: token.location,
                    },
                    *expr,
                ));
            }
            let (promoted_expr, next) = Expr::binary_promote(*expr, *next).map_err(flatten)?;
            expr_func(promoted_expr, next, token)
        })
    }

    /// Parse a grammar rule of the form
    /// rule:
    ///     grammar_item (TOKEN grammar_item)*
    ///
    /// next_grammar_func should parse `grammar_item`.
    /// `token` should be the token for the operation.
    /// `expr_func` is called with the arguments (grammar_item, grammar_item, TOKEN)
    /// to allow maximum flexibility. If you want convenience instead,
    /// consider `scalar_left_associative_binary_op` or `left_associative_binary_op`
    #[inline]
    fn left_associative_binary_op<E, G>(
        &mut self,
        next_grammar_func: G,
        tokens: &[&Token],
        mut expr_func: E,
    ) -> SyntaxResult
    where
        E: FnMut(Box<Expr>, Box<Expr>, Locatable<Token>) -> RecoverableResult<Expr, SemanticError>,
        G: Fn(&mut Self) -> SyntaxResult,
    {
        let mut expr = next_grammar_func(self)?;
        while let Some(locatable) = self.match_any(tokens) {
            let next = next_grammar_func(self)?;
            match expr_func(Box::new(expr), Box::new(next), locatable) {
                Ok(combined) => expr = combined,
                Err((err, original)) => {
                    expr = original;
                    self.semantic_err(err.data, err.location);
                }
            }
        }
        Ok(expr)
    }
}

impl Token {
    fn is_unary_operator(&self) -> bool {
        match self {
            Token::Ampersand
            | Token::Star
            | Token::Plus
            | Token::Minus
            | Token::BinaryNot
            | Token::LogicalNot => true,
            _ => false,
        }
    }
    fn is_assignment_operator(&self) -> bool {
        use Token::*;
        match self {
            Equal | PlusEqual | MinusEqual | StarEqual | DivideEqual | ModEqual | LeftEqual
            | RightEqual | AndEqual | OrEqual | XorEqual => true,
            _ => false,
        }
    }
}

/* stateless helper functions */
impl Expr {
    fn is_null(&self) -> bool {
        if let ExprType::Literal(token) = &self.expr {
            match token {
                Token::Int(0) | Token::UnsignedInt(0) => true,
                _ => false,
            }
        } else {
            false
        }
    }
    /// See section 6.3.2.1 of the C Standard. In particular:
    /// "A modifiable lvalue is an lvalue that does not have array type,
    /// does not  have an incomplete type, does not have a const-qualified type,
    /// and if it is a structure or union, does not have any member with a const-qualified type"
    fn is_modifiable_lval(&self) -> bool {
        if !self.lval {
            return false;
        }
        match &self.expr {
            // p = 5;
            ExprType::Id(sym) => !sym.qualifiers.c_const,
            // *p = 1;
            ExprType::Deref(_) => match &self.ctype {
                Type::Pointer(_) => true,
                _ => panic!("only pointers can be dereferenced"),
            },
            ExprType::Member(_, _) => true,
            _ => unimplemented!("what's an lval but not a pointer or id?"),
        }
    }
    // ensure an expression has a value. convert
    // - arrays -> pointers
    // - functions -> pointers
    // - variables -> value stored in that variable
    pub fn rval(self) -> Expr {
        match &*self.ctype {
            // a + 1 is the same as &a + 1
            Type::Array(to, _) => Expr {
                lval: false,
                ctype: Type::Pointer(to).into(),
                constexpr: false,
                ..self
            },
            Type::Function(_) => Expr {
                lval: false,
                ctype: Type::Pointer(self.ctype).into(),
                constexpr: false, // TODO: is this right?
                ..self
            },
            _ if self.lval => Expr {
                ctype: self.ctype.clone(),
                lval: false,
                constexpr: false,
                location: self.location,
                expr: ExprType::Deref(Box::new(self)),
            },
            _ => self,
        }
    }
    fn default_promote(self) -> RecoverableResult<Expr, SemanticError> {
        let expr = self.rval();
        let ctype = expr.ctype.clone().default_promote().into();
        expr.cast(ctype)
    }
    // Perform an integer conversion, including all relevant casts.
    //
    // See `Type::integer_promote` for conversion rules.
    fn integer_promote(self) -> RecoverableResult<Expr, SemanticError> {
        let expr = self.rval();
        let ctype = expr.ctype.clone().integer_promote().into();
        expr.cast(ctype)
    }
    // Perform a binary conversion, including all relevant casts.
    //
    // See `Type::binary_promote` for conversion rules.
    fn binary_promote(left: Expr, right: Expr) -> RecoverableResult<(Expr, Expr), SemanticError> {
        let (left, right) = (left.rval(), right.rval());
        let ctype = Type::binary_promote(left.ctype.into(), right.ctype.into()).into();
        match (left.cast(ctype), right.cast(ctype)) {
            (Ok(left_cast), Ok(right_cast)) => Ok((left_cast, right_cast)),
            (Err((err, left)), Ok(right)) | (Ok(left), Err((err, right)))
            // TODO: don't ignore this right error
            | (Err((err, left)), Err((_, right))) => Err((err, (left, right))),
        }
    }
    // Convert an expression to _Bool. Section 6.3.1.3 of the C standard:
    // "When any scalar value is converted to _Bool,
    // the result is 0 if the value compares equal to 0; otherwise, the result is 1."
    //
    // if (expr)
    pub fn truthy(mut self) -> RecoverableResult<Expr, CompileError> {
        self = self.rval();
        if *self.ctype == Type::Bool {
            return Ok(self);
        }
        if !self.ctype.is_scalar() {
            Err((
                CompileError::Semantic(Locatable {
                    location: self.location,
                    data: format!(
                        "expression of type '{}' cannot be converted to bool",
                        self.ctype
                    ),
                }),
                self,
            ))
        } else {
            let zero = Expr::zero(self.location).cast(self.ctype).unwrap();
            Ok(Expr {
                constexpr: self.constexpr,
                lval: false,
                location: self.location,
                ctype: *types::BOOL,
                expr: ExprType::Compare(Box::new(self), Box::new(zero), Token::NotEqual),
            })
        }
    }
    pub fn logical_not(self, location: Location) -> Expr {
        Expr {
            location,
            ctype: *types::BOOL,
            constexpr: self.constexpr,
            lval: false,
            expr: ExprType::LogicalNot(Box::new(self)),
        }
    }
    // Simple assignment rules, section 6.5.16.1 of the C standard
    // the funky return type is so we don't consume the original expression in case of an error
    pub fn cast(mut self, ctype: InternedType) -> RecoverableResult<Expr, SemanticError> {
        if self.ctype == ctype {
            Ok(self)
        } else if self.ctype.is_arithmetic() && ctype.is_arithmetic()
            || self.is_null() && ctype.is_pointer()
            || self.ctype.is_pointer() && ctype.is_bool()
            || self.ctype.is_pointer() && ctype.is_void_pointer()
            || self.ctype.is_pointer() && ctype.is_char_pointer()
        {
            Ok(Expr {
                location: self.location,
                constexpr: self.constexpr,
                expr: ExprType::Cast(Box::new(self)),
                lval: false,
                ctype,
            })
        } else if ctype.is_pointer()
            && (self.expr == ExprType::Literal(Token::Int(0))
                || self.ctype.is_void_pointer()
                || self.ctype.is_char_pointer())
        {
            self.ctype = ctype;
            Ok(self)
        } else if *self.ctype == Type::Error {
            Ok(self)
        // TODO: allow implicit casts of const pointers
        } else {
            Err((
                Locatable {
                    location: self.location,
                    data: format!(
                        "cannot implicitly convert '{}' to '{}'{}",
                        self.ctype,
                        ctype,
                        if ctype.is_pointer() {
                            format!(". help: use an explicit cast: ({})", ctype)
                        } else {
                            String::new()
                        }
                    ),
                },
                self,
            ))
        }
    }
    fn pointer_arithmetic(
        base: Expr,
        index: Expr,
        pointee: &Type,
        location: Location,
    ) -> RecoverableResult<Expr, SemanticError> {
        let offset = Expr {
            lval: false,
            location: index.location,
            constexpr: index.constexpr,
            expr: ExprType::Cast(Box::new(index)),
            ctype: base.ctype.clone(),
        }
        .rval();
        let size = match pointee.sizeof() {
            Ok(s) => s,
            Err(_) => {
                return Err((
                    Locatable {
                        location,
                        data: format!(
                    "cannot perform pointer arithmetic when size of pointed type '{}' is unknown",
                    pointee
                ),
                    },
                    base,
                ))
            }
        };
        let size_literal = Expr::unsigned_int_literal(size, offset.location);
        let size_cast = Expr {
            lval: false,
            location: offset.location,
            ctype: offset.ctype.clone(),
            constexpr: true,
            expr: ExprType::Cast(Box::new(size_literal)),
        };
        let offset = Expr {
            lval: false,
            location: offset.location,
            ctype: offset.ctype.clone(),
            constexpr: offset.constexpr,
            expr: ExprType::Mul(Box::new(size_cast), Box::new(offset)),
        };
        Ok(Expr {
            lval: false,
            location,
            ctype: base.ctype.clone(),
            constexpr: base.constexpr && offset.constexpr,
            expr: ExprType::Add(Box::new(base), Box::new(offset)),
        })
    }
    fn increment_op(
        prefix: bool,
        increment: bool,
        expr: Expr,
        location: Location,
    ) -> RecoverableResult<Expr, SemanticError> {
        if !expr.is_modifiable_lval() {
            return Err((
                Locatable {
                    location: expr.location,
                    data: "expression is not assignable".to_string(),
                },
                expr,
            ));
        } else if !(expr.ctype.is_arithmetic() || expr.ctype.is_pointer()) {
            return Err((
                Locatable {
                    location: expr.location,
                    data: format!(
                        "cannot increment or decrement value of type '{}'",
                        expr.ctype
                    ),
                },
                expr,
            ));
        }
        // ++i is syntactic sugar for i+=1
        if prefix {
            let rval = Expr {
                constexpr: true,
                lval: false,
                ctype: expr.ctype.clone(),
                location,
                expr: ExprType::Cast(Box::new(Expr::int_literal(1, location))),
            };
            Ok(Expr {
                ctype: expr.ctype.clone(),
                constexpr: rval.constexpr,
                lval: false, // `(i = j) = 4`; is invalid
                expr: ExprType::Assign(
                    Box::new(expr),
                    Box::new(rval),
                    if increment {
                        Token::PlusEqual
                    } else {
                        Token::MinusEqual
                    },
                ),
                location,
            })
        } else {
            Ok(Expr {
                constexpr: expr.constexpr,
                lval: false,
                ctype: expr.ctype.clone(),
                // true, false: pre-decrement
                expr: ExprType::PostIncrement(Box::new(expr), increment),
                location,
            })
        }
    }
    // convenience method for constructing an Expr
    fn default_expr<C>(
        constructor: C,
    ) -> impl Fn(Expr, Expr, Locatable<Token>) -> RecoverableResult<Expr, SemanticError>
    where
        C: Fn(Box<Expr>, Box<Expr>) -> ExprType,
    {
        move |left: Expr, right: Expr, token: Locatable<Token>| {
            Ok(Expr {
                location: token.location,
                ctype: left.ctype.clone(),
                constexpr: left.constexpr && right.constexpr,
                lval: false,
                expr: constructor(Box::new(left), Box::new(right)),
            })
        }
    }
    // helper function since == and > have almost identical logic
    fn relational_expr(
        mut left: Box<Expr>,
        mut right: Box<Expr>,
        token: Locatable<Token>,
    ) -> RecoverableResult<Expr, SemanticError> {
        if left.ctype.is_arithmetic() && right.ctype.is_arithmetic() {
            let tmp = Expr::binary_promote(*left, *right).map_err(flatten)?;
            *left = tmp.0;
            right = Box::new(tmp.1);
        } else {
            let (left_expr, right_expr) = (left.rval(), right.rval());
            if !((left_expr.ctype.is_pointer() && left_expr.ctype == right_expr.ctype)
                // equality operations have different rules :(
                || ((token.data == Token::EqualEqual || token.data == Token::NotEqual)
                    // shoot me now
                    && ((left_expr.ctype.is_pointer() && right_expr.ctype.is_void_pointer())
                        || (left_expr.ctype.is_void_pointer() && right_expr.ctype.is_pointer())
                        || (left_expr.is_null() && right_expr.ctype.is_pointer())
                        || (left_expr.ctype.is_pointer() && right_expr.is_null()))))
            {
                return Err((Locatable {
                    data: format!(
                        "invalid types for '{}' (expected arithmetic types or compatible pointers, got {} {} {}",
                        token.data,
                        left_expr.ctype,
                        token.data,
                        right_expr.ctype
                    ),
                    location: token.location,
                }, left_expr));
            }
            *left = left_expr;
            right = Box::new(right_expr);
        }
        assert!(!left.lval && !right.lval);
        Ok(Expr {
            constexpr: left.constexpr && right.constexpr,
            lval: false,
            location: token.location,
            ctype: *types::BOOL,
            expr: ExprType::Compare(left, right, token.data),
        })
    }
    fn id(symbol: &Symbol, location: Location) -> Self {
        Self {
            // TODO: this clone will get expensive fast
            expr: ExprType::Id(symbol.clone()),
            // TODO: check if symbol is constexpr
            // in particular, I would love for this to compile:
            // `int a = 5; int b[a];`
            // NOTE: neither GCC nor Clang accept that
            constexpr: false,
            ctype: symbol.ctype.clone(),
            lval: true,
            location,
        }
    }
    // this and the next few '*_literal' functions make unit tests more convenient
    fn char_literal(value: u8, location: Location) -> Expr {
        Expr::literal(
            Type::Char(true),
            location,
            ExprType::Literal(Token::Char(value)),
        )
    }
    fn int_literal(value: i64, location: Location) -> Expr {
        Expr::literal(
            Type::Long(true),
            location,
            ExprType::Literal(Token::Int(value)),
        )
    }
    fn unsigned_int_literal(value: u64, location: Location) -> Expr {
        Expr::literal(
            Type::Long(false),
            location,
            ExprType::Literal(Token::UnsignedInt(value)),
        )
    }
    fn float_literal(value: f64, location: Location) -> Expr {
        Expr::literal(
            Type::Double,
            location,
            ExprType::Literal(Token::Float(value)),
        )
    }
    fn string_literal(value: InternedStr, location: Location) -> Expr {
        Expr::literal(
            Type::for_string_literal(value.len() as SIZE_T),
            location,
            ExprType::Literal(Token::Str(value)),
        )
    }
    fn literal<T: Into<InternedType>>(ctype: T, location: Location, expr: ExprType) -> Expr {
        Expr {
            constexpr: true,
            lval: false,
            ctype: ctype.into(),
            location,
            expr,
        }
    }
}

fn flatten<E>((err, (left, _)): (E, (Expr, Expr))) -> (E, Expr) {
    (err, left)
}

/// Implicit conversions.
/// These are handled here and no other part of the compiler deals with them directly.
impl Type {
    // Perform the 'default promotions' from 6.5.2.2.6
    pub fn default_promote(self) -> Type {
        if self.is_integral() {
            self.integer_promote()
        } else if self == Type::Float {
            Type::Double
        } else {
            self
        }
    }
    pub fn integer_promote(self) -> Type {
        if self.rank() <= Type::Int(true).rank() {
            if Type::Int(true).can_represent(&self) {
                Type::Int(true)
            } else {
                Type::Int(false)
            }
        } else {
            self
        }
    }
    /// Perform the 'usual arithmetic conversions' from 6.3.1.8 of the C standard.
    ///
    /// Algorithm:
    /// If either object is a `double`, convert the other to a double.
    /// Else if either is a `float`, convert the other to a float.
    /// Else if both are signed or both are unsigned, convert the object with lesser rank to
    /// the type of the object with greater rank.
    /// Else if the unsigned object has rank >= other, convert other -> unsigned version (!!).
    /// Else if signed type can represent all values of the unsigned type,
    /// convert unsigned -> signed.
    /// Else, convert signed -> unsigned.
    ///
    /// The exclamation marks are because the following will evaluate to MAX_UINT,
    /// _not_ -1: `1ul + (short)-2`.
    ///
    /// Trying to promote derived types (pointers, functions, etc.) is an error.
    /// Pointer arithmetic should not promote either argument, see 6.5.6 of the C standard.
    fn binary_promote(mut left: &Type, mut right: &Type) -> Type {
        use Type::*;
        if *left == Double || *right == Double {
            return Double; // toil and trouble
        } else if *left == Float || *right == Float {
            return Float;
        }
        left = &left.integer_promote();
        right = &right.integer_promote();
        let signs = (left.sign(), right.sign());
        // same sign
        if signs.0 == signs.1 {
            return if left.rank() >= right.rank() {
                left.clone()
            } else {
                right.clone()
            };
        };
        let (signed, unsigned) = if signs.0 {
            (left, right)
        } else {
            (right, left)
        };
        if signed.can_represent(&unsigned) {
            signed.clone()
        } else {
            unsigned.clone()
        }
    }
    fn pointer_promote(left: &mut Expr, right: &mut Expr) -> bool {
        if left.ctype == right.ctype {
            true
        } else if left.ctype.is_void_pointer() || left.ctype.is_char_pointer() || left.is_null() {
            left.ctype = right.ctype.clone();
            true
        } else if right.ctype.is_void_pointer() || right.ctype.is_char_pointer() || right.is_null()
        {
            right.ctype = left.ctype.clone();
            true
        } else {
            false
        }
    }
    /// Return whether self is a signed type.
    ///
    /// Should only be called on integral types.
    /// Calling sign() on a floating or derived type will panic.
    fn sign(&self) -> bool {
        use Type::*;
        match self {
            Char(sign) | Short(sign) | Int(sign) | Long(sign) => *sign,
            Bool => false,
            // TODO: allow enums with values of UINT_MAX
            Enum(_, _) => true,
            x => panic!(
                "Type::sign can only be called on integral types (got {})",
                x
            ),
        }
    }
    /// Return the rank of an integral type, according to section 6.3.1.1 of the C standard.
    ///
    /// It is an error to take the rank of a non-integral type.
    ///
    /// Examples:
    /// ```
    /// use rcc::data::types::Type::*;
    /// assert!(Long(true).rank() > Int(true).rank());
    /// assert!(Int(false).rank() > Short(false).rank());
    /// assert!(Short(true).rank() > Char(true).rank());
    /// assert!(Char(true).rank() > Bool.rank());
    /// assert!(Long(false).rank() > Bool.rank());
    /// assert!(Long(true).rank() == Long(false).rank());
    /// ```
    pub fn rank(&self) -> usize {
        use Type::*;
        match self {
            Bool => 0,
            Char(_) => 1,
            Short(_) => 2,
            Int(_) => 3,
            Long(_) => 4,
            // don't make this 5 in case we add `long long` at some point
            _ => std::usize::MAX,
        }
    }
    pub fn for_string_literal(len: SIZE_T) -> Type {
        Type::Array(*types::CHAR, ArrayType::Fixed(len))
    }
}

#[cfg(test)]
mod tests {
    use crate::data::{prelude::*, types, Scope, StorageClass};
    use crate::intern::InternedStr;
    use crate::parse::tests::*;
    fn parse_expr(input: &str) -> CompileResult<Expr> {
        // because we're a child module of parse, we can skip straight to `expr()`
        let mut p = parser(input);
        let exp = p.expr();
        if let Some(Err(err)) = p.pending.pop_front() {
            Err(err)
        } else {
            exp.map_err(SyntaxError::into)
        }
    }
    fn get_location(r: &CompileResult<Expr>) -> Location {
        match r {
            Ok(expr) => expr.location,
            Err(err) => err.location(),
        }
    }
    fn test_literal<C: std::string::ToString, T>(token: C, parse_literal: T) -> bool
    where
        T: Fn(C, Location) -> Expr,
    {
        let parsed = parse_expr(&token.to_string());
        parsed == Ok(parse_literal(token, get_location(&parsed)))
    }
    fn parse_expr_with_scope<'a>(input: &'a str, variables: &[&Symbol]) -> CompileResult<Expr> {
        let mut parser = parser(input);
        let mut scope = Scope::new();
        for var in variables {
            scope.insert(var.id.clone(), (*var).clone());
        }
        parser.scope = scope;
        let exp = parser.expr();
        if let Some(Err(err)) = parser.pending.pop_front() {
            Err(err)
        } else {
            exp.map_err(SyntaxError::into)
        }
    }
    fn assert_type(input: &str, ctype: InternedType) {
        assert!(match parse_expr(input) {
            Ok(expr) => expr.ctype == ctype,
            _ => false,
        });
    }
    #[test]
    fn test_primaries() {
        assert!(test_literal(141, Expr::int_literal));
        let parsed = parse_expr("\"hi there\"");

        assert_eq!(
            parsed,
            Ok(Expr::string_literal(
                InternedStr::get_or_intern("hi there\0"),
                get_location(&parsed)
            ))
        );
        assert!(test_literal(1.5, Expr::float_literal));
        let parsed = parse_expr("(1)");
        assert_eq!(parsed, Ok(Expr::int_literal(1, get_location(&parsed))));
        let x = Symbol {
            ctype: *types::INT,
            id: InternedStr::get_or_intern("x"),
            qualifiers: Default::default(),
            storage_class: Default::default(),
            init: false,
        };
        let parsed = parse_expr_with_scope("x", &[&x]);
        assert_eq!(
            parsed,
            Ok(Expr {
                location: get_location(&parsed),
                ctype: *types::INT,
                constexpr: false,
                lval: true,
                expr: ExprType::Id(x)
            })
        );
    }
    #[test]
    fn test_mul() {
        assert_type("1*1.0", *types::DOUBLE);
        assert_type("1*2.0 / 1.3", *types::DOUBLE);
        assert_type("3%2", *types::LONG);
    }
    #[test]
    fn test_funcall() {
        let f = Symbol {
            id: InternedStr::get_or_intern("f"),
            init: false,
            qualifiers: Default::default(),
            storage_class: Default::default(),
            ctype: Type::Function(types::FunctionType {
                params: vec![Symbol {
                    ctype: Type::Void,
                    id: Default::default(),
                    init: false,
                    qualifiers: Default::default(),
                    storage_class: StorageClass::Auto,
                }],
                return_type: Box::new(Type::Int(true)),
                varargs: false,
            }),
        };
        assert!(parse_expr_with_scope("f(1,2,3)", &[&f]).is_err());
        let parsed = parse_expr_with_scope("f()", &[&f]);
        assert!(
            match parsed {
                Ok(Expr {
                    expr: ExprType::FuncCall(_, _),
                    ..
                }) => true,
                _ => false,
            },
            parsed
        );
    }
    #[test]
    fn test_type_errors() {
        assert!(parse_expr("1 % 2.0").is_err());
    }

    #[test]
    fn test_explicit_casts() {
        assert_type("(int)4.2", Type::Int(true));
        assert_type("(unsigned int)4.2", Type::Int(false));
        assert_type("(float)4.2", Type::Float);
        assert_type("(double)4.2", Type::Double);
        assert!(parse_expr("(int*)4.2").is_err());
        assert_type("(int*)(int)4.2", Type::Pointer(Box::new(Type::Int(true))));
    }
}
