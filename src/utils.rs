use indexmap::IndexMap;
use swc_core::common::Mark;
use swc_core::{
    common::{Span, Spanned, DUMMY_SP},
    ecma::{
        ast::*,
        atoms::{js_word, JsWord},
        utils::{
            is_valid_prop_ident, member_expr, private_ident, quote_ident, quote_str, ExprFactory,
            FunctionFactory,
        },
    },
};

/// {
///     "key": ident,
/// }
pub(crate) struct ObjPropKeyIdent(JsWord, Span, Ident);

impl From<((JsWord, Span), Ident)> for ObjPropKeyIdent {
    fn from(((key, span), ident): ((JsWord, Span), Ident)) -> Self {
        Self(key, span, ident)
    }
}

impl From<(JsWord, Span, Ident)> for ObjPropKeyIdent {
    fn from((key, span, ident): (JsWord, Span, Ident)) -> Self {
        Self(key, span, ident)
    }
}

impl Spanned for ObjPropKeyIdent {
    fn span(&self) -> Span {
        self.1
    }
}

impl ObjPropKeyIdent {
    pub fn key(&self) -> &JsWord {
        &self.0
    }

    pub fn into_expr(self) -> Expr {
        self.2.into()
    }
}

/// ```javascript
/// {
///     key: () => expr,
/// }
/// ```
pub(crate) fn prop_arrow(prop: ObjPropKeyIdent) -> Prop {
    let key = prop_name(prop.key(), prop.span()).into();

    KeyValueProp {
        key,
        value: Box::new(prop.into_expr().into_lazy_arrow(Default::default()).into()),
    }
    .into()
}

pub(crate) fn prop_name(key: &str, span: Span) -> IdentOrStr {
    if is_valid_prop_ident(key) {
        IdentOrStr::Ident(quote_ident!(span, key))
    } else {
        IdentOrStr::Str(quote_str!(span, key))
    }
}

pub(crate) enum IdentOrStr {
    Ident(Ident),
    Str(Str),
}

impl From<IdentOrStr> for PropName {
    fn from(val: IdentOrStr) -> Self {
        match val {
            IdentOrStr::Ident(i) => Self::Ident(i),
            IdentOrStr::Str(s) => Self::Str(s),
        }
    }
}

impl From<IdentOrStr> for MemberProp {
    fn from(val: IdentOrStr) -> Self {
        match val {
            IdentOrStr::Ident(i) => Self::Ident(i),
            IdentOrStr::Str(s) => Self::Computed(ComputedPropName {
                span: DUMMY_SP,
                expr: s.into(),
            }),
        }
    }
}

/// Creates
///
///```js
///
///  Object.defineProperty(target, prop_name, {
///      ...props
///  });
/// ```
pub(super) fn object_define_property(
    target: ExprOrSpread,
    prop_name: ExprOrSpread,
    descriptor: ExprOrSpread,
) -> Expr {
    member_expr!(DUMMY_SP, Object.defineProperty)
        .as_call(DUMMY_SP, vec![target, prop_name, descriptor])
}

pub(crate) fn object_define_enumerable_configurable(
    target: ExprOrSpread,
    prop_name: ExprOrSpread,
    prop: PropOrSpread,
) -> Expr {
    object_define_property(
        target,
        prop_name,
        ObjectLit {
            span: DUMMY_SP,
            props: vec![
                PropOrSpread::Prop(Box::new(
                    KeyValueProp {
                        key: quote_ident!("enumerable").into(),
                        value: Box::new(true.into()),
                    }
                    .into(),
                )),
                prop,
                PropOrSpread::Prop(Box::new(
                    KeyValueProp {
                        key: quote_ident!("configurable").into(),
                        value: Box::new(true.into()),
                    }
                    .into(),
                )),
            ],
        }
        .as_arg(),
    )
}

pub(crate) fn emit_export_star_stmts(
    unresolved_mark: Mark,
    exports: Ident,
    export_star_items: IndexMap<JsWord, Span>,
) -> Vec<Stmt> {
    if export_star_items.is_empty() {
        return Vec::new();
    }

    let esm_export_star_ident = private_ident!("_esmExportStar");

    let mut stmts = Vec::new();
    stmts.push(Stmt::Decl(Decl::Fn(
        esm_export_star().into_fn_decl(esm_export_star_ident.clone()),
    )));

    for (path, span) in export_star_items {
        let require_call = Expr::Call(CallExpr {
            span: DUMMY_SP,
            callee: quote_ident!(DUMMY_SP.apply_mark(unresolved_mark), "require").as_callee(),
            args: vec![Lit::Str(Str {
                span,
                raw: None,
                value: path,
            })
            .as_arg()],

            type_args: None,
        });
        let esm_export_star_call = esm_export_star_ident.clone().as_call(
            DUMMY_SP,
            vec![require_call.into(), exports.clone().as_arg()],
        );
        stmts.push(esm_export_star_call.into_stmt());
    }

    stmts
}

/// ```javascript
/// function _esmExportStar(from, to) {
///     Object.keys(from).forEach(function(k) {
///         if (k !== "default" && !Object.prototype.hasOwnProperty.call(to, k)) Object.defineProperty(to, k, {
///             enumerable: true,
///             get: function get() {
///                 return from[k];
///             },
///             configurable: true
///         });
///     });
/// }
/// ```
pub(crate) fn esm_export_star() -> Function {
    let from = private_ident!("from");
    let to = private_ident!("to");
    let k = private_ident!("k");

    let prop: Prop = KeyValueProp {
        key: prop_name("get", DUMMY_SP).into(),
        value: Box::new(Expr::Fn(FnExpr {
            ident: None,
            function: Box::new(
                from.clone()
                    .computed_member(k.clone())
                    .into_lazy_fn(Vec::new()),
            ),
        })),
    }
    .into();
    let k_neq_default: Expr = BinExpr {
        span: DUMMY_SP,
        op: BinaryOp::NotEqEq,
        left: Box::new(k.clone().into()),
        right: Box::new(quote_str!("default").into()),
    }
    .into();
    let has_own_property: Expr = member_expr!(DUMMY_SP, Object.prototype.hasOwnProperty.call)
        .as_call(DUMMY_SP, vec![to.clone().as_arg(), k.clone().as_arg()]);
    let neg_has_own_property: Expr = UnaryExpr {
        span: DUMMY_SP,
        op: UnaryOp::Bang,
        arg: Box::new(has_own_property),
    }
    .into();
    let test: Expr = BinExpr {
        span: DUMMY_SP,
        op: BinaryOp::LogicalAnd,
        left: Box::new(k_neq_default),
        right: Box::new(neg_has_own_property),
    }
    .into();
    let if_stmt: Stmt = IfStmt {
        span: DUMMY_SP,
        test: Box::new(test),
        cons: Box::new(
            object_define_enumerable_configurable(
                to.clone().as_arg(),
                k.clone().as_arg(),
                prop.into(),
            )
            .into_stmt(),
        ),
        alt: None,
    }
    .into();

    let callback_fn = Function {
        params: vec![k.into()],
        decorators: Vec::new(),
        span: DUMMY_SP,
        body: Some(BlockStmt {
            span: DUMMY_SP,
            stmts: vec![if_stmt],
        }),
        is_generator: false,
        is_async: false,
        type_params: None,
        return_type: None,
    };

    let object_keys_expr =
        member_expr!(DUMMY_SP, Object.keys).as_call(DUMMY_SP, vec![from.clone().as_arg()]);
    let for_each = member_expr!(@EXT, DUMMY_SP, Box::new(object_keys_expr), forEach)
        .as_call(DUMMY_SP, vec![callback_fn.as_arg()])
        .into_stmt();

    Function {
        params: vec![from.into(), to.into()],
        decorators: Vec::new(),
        span: DUMMY_SP,
        body: Some(BlockStmt {
            span: DUMMY_SP,
            stmts: vec![for_each],
        }),
        is_generator: false,
        is_async: false,
        type_params: None,
        return_type: None,
    }
}

pub(crate) fn emit_export_stmts(exports: Ident, mut prop_list: Vec<ObjPropKeyIdent>) -> Vec<Stmt> {
    match prop_list.len() {
        0 | 1 => prop_list
            .pop()
            .map(|obj_prop| {
                object_define_enumerable_configurable(
                    exports.as_arg(),
                    quote_str!(obj_prop.span(), obj_prop.key()).as_arg(),
                    prop_arrow((js_word!("get"), DUMMY_SP, obj_prop.2.clone()).into()).into(),
                )
                .into_stmt()
            })
            .into_iter()
            .collect(),
        _ => {
            let props = prop_list
                .into_iter()
                .map(prop_arrow)
                .map(Into::into)
                .collect();
            let obj_lit = ObjectLit {
                span: DUMMY_SP,
                props,
            };

            let esm_export_ident = private_ident!("_export");

            vec![
                Stmt::Decl(Decl::Fn(
                    esm_export().into_fn_decl(esm_export_ident.clone()),
                )),
                esm_export_ident
                    .as_call(DUMMY_SP, vec![exports.as_arg(), obj_lit.as_arg()])
                    .into_stmt(),
            ]
        }
    }
}

/// ```javascript
/// function _esmExport(target, all) {
///    for (var name in all)Object.defineProperty(target, name, { get: all[name], enumerable: true, configurable: true });
/// }
/// ```
pub(crate) fn esm_export() -> Function {
    let target = private_ident!("target");
    let all = private_ident!("all");
    let name = private_ident!("name");

    let getter = KeyValueProp {
        key: quote_ident!("get").into(),
        value: Box::new(all.clone().computed_member(Expr::from(name.clone()))),
    };

    let body = object_define_enumerable_configurable(
        target.clone().as_arg(),
        name.clone().as_arg(),
        PropOrSpread::Prop(Box::new(Prop::KeyValue(getter))),
    )
    .into_stmt();

    let for_in_stmt: Stmt = ForInStmt {
        span: DUMMY_SP,
        left: VarDecl {
            span: DUMMY_SP,
            kind: VarDeclKind::Var,
            declare: false,
            decls: vec![VarDeclarator {
                span: DUMMY_SP,
                name: name.into(),
                init: None,
                definite: false,
            }],
        }
        .into(),
        right: Box::new(all.clone().into()),
        body: Box::new(body),
    }
    .into();

    Function {
        params: vec![target.into(), all.into()],
        decorators: Default::default(),
        span: DUMMY_SP,
        body: Some(BlockStmt {
            span: DUMMY_SP,
            stmts: vec![for_in_stmt],
        }),
        is_generator: false,
        is_async: false,
        type_params: None,
        return_type: None,
    }
}
