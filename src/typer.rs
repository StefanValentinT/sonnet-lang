use crate::ast::*;
use std::collections::{HashMap, HashSet};

pub type Env = HashMap<String, Type>;

pub struct Typer {
    next_var: usize,
    subst: HashMap<String, Type>,
    type_env: HashMap<String, Type>,
}

pub fn typecheck(program: Program) -> TypedProgram {
    let mut typer = Typer::new();
    typer
        .typecheck_program(program)
        .expect("Type Identification Failed")
}

impl Typer {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            subst: HashMap::new(),
            type_env: HashMap::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let name = format!("?{}", self.next_var);
        self.next_var += 1;
        Type::TypeVar(name)
    }

    pub fn apply(&self, ty: &Type) -> Type {
        self.apply_recursive(ty, &mut HashSet::new())
    }

    fn apply_recursive(&self, ty: &Type, seen: &mut HashSet<String>) -> Type {
        match ty {
            Type::TypeVar(name) => {
                if seen.contains(name) {
                    // Stop unrolling if we hit a cycle
                    return ty.clone();
                }
                if let Some(resolved) = self.subst.get(name) {
                    seen.insert(name.clone());
                    let res = self.apply_recursive(resolved, seen);
                    seen.remove(name);
                    res
                } else {
                    ty.clone()
                }
            }
            Type::TypeFun(d, c) => {
                Type::fun(self.apply_recursive(d, seen), self.apply_recursive(c, seen))
            }
            Type::Union(a, b) => {
                Type::union(self.apply_recursive(a, seen), self.apply_recursive(b, seen))
            }
            Type::Inter(a, b) => {
                Type::inter(self.apply_recursive(a, seen), self.apply_recursive(b, seen))
            }
            Type::Record(fields) => Type::Record(
                fields
                    .iter()
                    .map(|(n, t)| (n.clone(), self.apply_recursive(t, seen)))
                    .collect(),
            ),
            Type::Iota(id, inner) => Type::Iota(*id, Box::new(self.apply_recursive(inner, seen))),
            Type::Neg(t) => Type::Neg(Box::new(self.apply_recursive(t, seen))),
            other => other.clone(),
        }
    }

    pub fn subtype(&mut self, t1: &Type, t2: &Type) -> Result<(), String> {
        match (t1, t2) {
            (Type::TypeVar(v), t) | (t, Type::TypeVar(v)) => {
                return self.unify_var(v.clone(), t.clone());
            }
            _ => {}
        }

        let mut t1 = self.apply(t1);
        let mut t2 = self.apply(t2);

        if let Type::TypeIdent(name) = &t1 {
            if let Some(real_ty) = self.type_env.get(name) {
                t1 = real_ty.clone();
            }
        }
        if let Type::TypeIdent(name) = &t2 {
            if let Some(real_ty) = self.type_env.get(name) {
                t2 = real_ty.clone();
            }
        }

        if t1 == t2 || t2 == Type::Top || t1 == Type::Bot {
            return Ok(());
        }

        match (t1, t2) {
            (Type::TypeLit(Literal::I32(_)), Type::TypeIdent(ref n)) if n == "I32" => Ok(()),
            (Type::TypeLit(Literal::I32(_)), Type::TypeIdent(n)) if n == N_I32 => Ok(()),

            (Type::Iota(id1, p1), Type::Iota(id2, p2)) => {
                if id1 == id2 {
                    self.subtype(p1.as_ref(), p2.as_ref())
                } else {
                    Err(format!("Iota mismatch: ID {} != ID {}", id1, id2))
                }
            }

            (Type::TypeFun(d1, c1), Type::TypeFun(d2, c2)) => {
                self.subtype(&d2, &d1)?; // Contravariant
                self.subtype(&c1, &c2) // Covariant
            }

            (Type::Record(f1), Type::Record(f2)) => {
                for (name, ty2) in f2 {
                    let ty1 = f1
                        .iter()
                        .find(|(n, _)| n == &name)
                        .map(|(_, t)| t)
                        .ok_or_else(|| format!("Missing field {}", name))?;
                    self.subtype(ty1, &ty2)?;
                }
                Ok(())
            }

            (Type::Union(a, b), c) => {
                self.subtype(&a, &c)?;
                self.subtype(&b, &c)
            }

            (a, Type::Union(b, c)) => self.subtype(&a, &b).or_else(|_| self.subtype(&a, &c)),

            (a, b) => Err(format!("Cannot subtype {:?} <: {:?}", a, b)),
        }
    }

    fn unify_var(&mut self, var_name: String, target: Type) -> Result<(), String> {
        let current_resolved = self.apply(&Type::TypeVar(var_name.clone()));

        // Normalize target (e.g., constants to I32)
        let target = match target {
            Type::TypeLit(Literal::I32(_)) => Type::TypeIdent("I32".to_string()),
            other => other,
        };

        if let Type::TypeVar(v_target) = &target {
            if &var_name == v_target {
                return Ok(());
            }
        }

        match (current_resolved, target) {
            (Type::TypeVar(_), t) => {
                self.subst.insert(var_name, t);
                Ok(())
            }

            (Type::Record(mut fields1), Type::Record(fields2)) => {
                for (n2, t2) in fields2 {
                    if let Some(pos) = fields1.iter().position(|(n1, _)| n1 == &n2) {
                        self.subtype(&fields1[pos].1.clone(), &t2)?;
                    } else {
                        fields1.push((n2, t2));
                    }
                }
                self.subst.insert(var_name, Type::Record(fields1));
                Ok(())
            }

            (resolved, t) => {
                if resolved == t {
                    Ok(())
                } else {
                    // Broaden the type to a Union
                    let new_ty = Type::union(resolved, t);
                    self.subst.insert(var_name, new_ty);
                    Ok(())
                }
            }
        }
    }
    fn infer_pattern(
        &mut self,
        env: &mut Env,
        pat: &Pattern,
        expected: Option<Type>,
    ) -> Result<(TypedPattern, Type), String> {
        let ty = expected.unwrap_or_else(|| self.fresh_var());
        match pat {
            Pattern::Wildcard => Ok((TypedPattern::Wildcard(ty.clone()), ty)),
            Pattern::PatternApp(constructor_ty, inner_pat) => {
                let iota_ty = match constructor_ty {
                    Type::TypeIdent(name) => self
                        .type_env
                        .get(name)
                        .cloned()
                        .ok_or_else(|| format!("Unknown Iota constructor: {}", name))?,
                    _ => {
                        return Err(format!(
                            "Constructor must be an identifier: {:?}",
                            constructor_ty
                        ));
                    }
                };

                let payload_req = if let Type::Iota(_, ref payload) = iota_ty {
                    payload.as_ref().clone()
                } else {
                    return Err(format!("{:?} is not an Iota type", constructor_ty));
                };

                self.subtype(&iota_ty, &ty)?;

                let (t_inner_pat, _) = self.infer_pattern(env, inner_pat, Some(payload_req))?;

                Ok((
                    TypedPattern::PatternApp(
                        constructor_ty.clone(),
                        Box::new(t_inner_pat),
                        iota_ty.clone(),
                    ),
                    iota_ty,
                ))
            }

            Pattern::TypePattern(t) => {
                let iota_ty = match t {
                    Type::TypeIdent(name) if name == "I32" => Type::i32(),
                    Type::TypeIdent(name) => self
                        .type_env
                        .get(name)
                        .cloned()
                        .ok_or_else(|| format!("Unknown type identifier: {}", name))?,
                    _ => t.clone(),
                };

                self.subtype(&iota_ty, &ty)?;
                Ok((TypedPattern::TypePattern(iota_ty.clone()), iota_ty))
            }
            Pattern::PatternIdent(name) => {
                let maybe_iota = self.type_env.get(name).cloned();

                if let Some(iota_ty) = maybe_iota {
                    self.subtype(&iota_ty, &ty)?;
                    Ok((TypedPattern::TypePattern(iota_ty.clone()), iota_ty))
                } else {
                    env.insert(name.clone(), ty.clone());
                    Ok((TypedPattern::PatternIdent(name.clone(), ty.clone()), ty))
                }
            }
            Pattern::RecordPattern(pats) => {
                let mut fields = Vec::new();
                let mut typed_pats = Vec::new();
                for (i, p) in pats.iter().enumerate() {
                    let (t_p, t_t) = self.infer_pattern(env, p, None)?;
                    let field_name = if let Pattern::PatternIdent(n) = p {
                        n.clone()
                    } else {
                        format!("_{}", i)
                    };
                    fields.push((field_name, t_t));
                    typed_pats.push(t_p);
                }
                let res = Type::Record(fields);
                self.subtype(&res, &ty)?;
                Ok((TypedPattern::RecordPattern(typed_pats, res.clone()), res))
            }
            Pattern::PatternTyped(inner, annot) => {
                self.subtype(&ty, annot)?;
                self.infer_pattern(env, inner, Some(annot.clone()))
            }
        }
    }

    pub fn infer(&mut self, env: &Env, term: &Term) -> Result<TypedTerm, String> {
        let result = match term {
            Term::Lit(l) => Ok(TypedTerm::Lit(l.clone(), Type::TypeLit(l.clone()))),

            Term::Ident(n) => env
                .get(n)
                .cloned()
                .map(|t| TypedTerm::Ident(n.clone(), t))
                .ok_or_else(|| format!("Unknown identifier: {}", n)),

            Term::Typed(inner, annot) => {
                let t_inner = self.infer(env, inner)?;
                self.subtype(t_inner.ty(), annot)?;
                Ok(t_inner)
            }

            Term::Fun(pat, body) => {
                let mut new_env = env.clone();
                let (t_pat, p_ty) = self.infer_pattern(&mut new_env, pat, None)?;
                let t_body = self.infer(&new_env, body)?;
                let f_ty = Type::fun(p_ty, t_body.ty().clone());
                Ok(TypedTerm::Fun(t_pat, Box::new(t_body), f_ty))
            }

            Term::App(f, arg) => {
                let t_f = self.infer(env, f)?;
                let t_arg = self.infer(env, arg)?;
                let res_ty = self.fresh_var();
                let func_req = Type::fun(t_arg.ty().clone(), res_ty.clone());
                self.subtype(t_f.ty(), &func_req)?;
                Ok(TypedTerm::App(Box::new(t_f), Box::new(t_arg), res_ty))
            }

            Term::Bin(op, l, r) => {
                let t_l = self.infer(env, l)?;
                let t_r = self.infer(env, r)?;

                if op.is_arithmetic() {
                    self.subtype(t_l.ty(), &Type::i32())?;
                    self.subtype(t_r.ty(), &Type::i32())?;
                    Ok(TypedTerm::Bin(
                        op.clone(),
                        Box::new(t_l),
                        Box::new(t_r),
                        Type::i32(),
                    ))
                } else {
                    self.subtype(t_l.ty(), t_r.ty())?;

                    let bool_ty = self
                        .type_env
                        .get("Bool")
                        .cloned()
                        .expect("Bool type must be defined!");
                    Ok(TypedTerm::Bin(
                        op.clone(),
                        Box::new(t_l),
                        Box::new(t_r),
                        bool_ty,
                    ))
                }
            }
            Term::FieldAccess(rec, name) => {
                let t_rec = self.infer(env, rec)?;
                let f_ty = self.fresh_var();
                let rec_req = Type::Record(vec![(name.clone(), f_ty.clone())]);
                self.subtype(t_rec.ty(), &rec_req)?;
                Ok(TypedTerm::FieldAccess(Box::new(t_rec), name.clone(), f_ty))
            }

            Term::RecordVal(fields) => {
                let mut t_fields = Vec::new();
                let mut f_types = Vec::new();
                for (n, v) in fields {
                    let t_v = self.infer(env, v)?;
                    f_types.push((n.clone(), t_v.ty().clone()));
                    t_fields.push((n.clone(), t_v));
                }
                Ok(TypedTerm::RecordVal(t_fields, Type::Record(f_types)))
            }

            Term::CaseOf(expr, branches) => {
                let t_expr = self.infer(env, expr)?;
                let res_ty = self.fresh_var();
                let mut t_branches = Vec::new();

                for (pat, body) in branches {
                    let mut b_env = env.clone();
                    let (t_pat, _) =
                        self.infer_pattern(&mut b_env, pat, Some(t_expr.ty().clone()))?;
                    let t_body = self.infer(&b_env, body)?;
                    self.subtype(t_body.ty(), &res_ty)?;
                    t_branches.push((t_pat, t_body, res_ty.clone()));
                }
                Ok(TypedTerm::CaseOf(Box::new(t_expr), t_branches, res_ty))
            }

            Term::Compound(terms) => {
                let mut t_terms = Vec::new();
                let mut last_ty = Type::unit();
                let mut current_env = env.clone();

                for t in terms {
                    match t {
                        Term::VarDef(pat, val) => {
                            let t_val = self.infer(&current_env, val)?;
                            let (t_pat, _) = self.infer_pattern(
                                &mut current_env,
                                pat,
                                Some(t_val.ty().clone()),
                            )?;

                            last_ty = Type::unit();
                            t_terms.push(TypedTerm::Def(t_pat, Box::new(t_val), Type::unit()));
                        }
                        _ => {
                            let typed = self.infer(&current_env, t)?;
                            last_ty = typed.ty().clone();
                            t_terms.push(typed);
                        }
                    }
                }
                Ok(TypedTerm::Compound(t_terms, last_ty))
            }

            //def x = a = 9; is possible but kind of useless
            Term::VarDef(pat, val) => {
                let t_val = self.infer(env, val)?;
                let mut local_env = env.clone();
                let (t_pat, _) =
                    self.infer_pattern(&mut local_env, pat, Some(t_val.ty().clone()))?;
                Ok(TypedTerm::Def(t_pat, Box::new(t_val), Type::unit()))
            }
        };
        if let Ok(ref typed_t) = result {
            println!(
                "Inferred Term: {:?} => Type: {:?}",
                term,
                self.apply(typed_t.ty())
            );
        }
        result
    }

    pub fn finalize(&self, term: TypedTerm) -> TypedTerm {
        let ty = self.apply(term.ty());
        match term {
            TypedTerm::Ident(n, _) => TypedTerm::Ident(n, ty),
            TypedTerm::Lit(l, _) => TypedTerm::Lit(l, ty),
            TypedTerm::Def(p, b, _) => {
                TypedTerm::Def(self.finalize_pat(p), Box::new(self.finalize(*b)), ty)
            }
            TypedTerm::Fun(p, b, _) => {
                TypedTerm::Fun(self.finalize_pat(p), Box::new(self.finalize(*b)), ty)
            }
            TypedTerm::App(f, a, _) => {
                TypedTerm::App(Box::new(self.finalize(*f)), Box::new(self.finalize(*a)), ty)
            }
            TypedTerm::Bin(op, l, r, _) => TypedTerm::Bin(
                op,
                Box::new(self.finalize(*l)),
                Box::new(self.finalize(*r)),
                ty,
            ),
            TypedTerm::RecordVal(fs, _) => TypedTerm::RecordVal(
                fs.into_iter().map(|(n, v)| (n, self.finalize(v))).collect(),
                ty,
            ),
            TypedTerm::FieldAccess(r, n, _) => {
                TypedTerm::FieldAccess(Box::new(self.finalize(*r)), n, ty)
            }
            TypedTerm::Compound(ts, _) => {
                TypedTerm::Compound(ts.into_iter().map(|t| self.finalize(t)).collect(), ty)
            }
            TypedTerm::CaseOf(e, bs, _) => TypedTerm::CaseOf(
                Box::new(self.finalize(*e)),
                bs.into_iter()
                    .map(|(p, b, t)| (self.finalize_pat(p), self.finalize(b), self.apply(&t)))
                    .collect(),
                ty,
            ),
        }
    }

    fn finalize_pat(&self, pat: TypedPattern) -> TypedPattern {
        match pat {
            TypedPattern::PatternIdent(n, t) => TypedPattern::PatternIdent(n, self.apply(&t)),
            TypedPattern::Wildcard(t) => TypedPattern::Wildcard(self.apply(&t)),
            TypedPattern::RecordPattern(ps, t) => TypedPattern::RecordPattern(
                ps.into_iter().map(|p| self.finalize_pat(p)).collect(),
                self.apply(&t),
            ),
            other => other,
        }
    }

    pub fn typecheck_program(&mut self, program: Program) -> Result<TypedProgram, String> {
        for (name, ty) in &program.types {
            self.type_env.insert(name.clone(), ty.clone());
        }

        let mut global_env = Env::new();
        for (name, ty) in &program.types {
            let actual_ty = self.apply(ty);
            if let Type::Iota(_, payload) = actual_ty {
                let constructor_sig = Type::fun(payload.as_ref().clone(), ty.clone());
                global_env.insert(name.clone(), constructor_sig);
            }
        }

        let mut protos = Vec::new();

        for (pat, _) in &program.terms {
            let (t_pat, p_ty) = self.infer_pattern(&mut global_env, pat, None)?;
            protos.push((t_pat, p_ty));
        }

        let mut final_terms = Vec::new();
        for (i, (_, term)) in program.terms.iter().enumerate() {
            let (t_pat, p_ty) = &protos[i];

            let t_body = self.infer(&global_env, term)?;

            self.subtype(t_body.ty(), p_ty)?;

            final_terms.push((self.finalize_pat(t_pat.clone()), self.finalize(t_body)));

            // println!(
            //     "[Debug] Typechecker finished term: {:#?}",
            //     final_terms.last()
            // );
        }

        Ok(TypedProgram {
            terms: final_terms,
            types: program.types,
        })
    }
}
