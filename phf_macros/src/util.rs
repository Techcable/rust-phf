use std::hash::{Hash, Hasher};
use std::rc::Rc;

use syntax::ast::{Expr, LitKind, LitIntType, UintTy};
use syntax_pos::Span;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::ptr::P;
use syntax::symbol::LocalInternedString;

use phf_generator::HashState;

#[derive(PartialEq, Eq, Clone)]
pub enum Key {
    Str(LocalInternedString),
    Binary(Rc<Vec<u8>>),
}

impl Hash for Key {
    fn hash<S: Hasher>(&self, state: &mut S) {
        match *self {
            Key::Str(ref s) => s.hash(state),
            Key::Binary(ref b) => b.hash(state),
        }
    }
}

impl AsRef<[u8]> for Key {
    fn as_ref(&self) -> &[u8] {
        match *self {
            Key::Str(ref s) => s.as_ref(),
            Key::Binary(ref s) => s.as_ref(),
        }
    }
}

pub struct Entry {
    pub key_contents: Key,
    pub key: P<Expr>,
    pub value: P<Expr>,
}

impl AsRef<[u8]> for Entry {
    fn as_ref(&self) -> &[u8] {
        self.key_contents.as_ref()
    }
}

pub fn create_map(
    cx: &mut ExtCtxt,
    sp: Span,
    entries: Vec<Entry>,
    state: HashState,
) -> Box<MacResult + 'static> {
    let disps = state
        .disps
        .iter()
        .map(|&(d1, d2)| cx.expr_tuple(sp, vec![cx.expr_u32(sp, d1), cx.expr_u32(sp, d2)]))
        .collect();
    let disps = cx.expr_vec(sp, disps);

    let entries = state
        .map
        .iter()
        .map(|&idx| {
            let &Entry {
                ref key, ref value, ..
            } = &entries[idx];
            cx.expr_tuple(sp, vec![key.clone(), value.clone()])
        })
        .collect();
    let entries = cx.expr_vec(sp, entries);

    let key = state.key;
    MacEager::expr(cx.expr_struct(
        sp,
        cx.path_global(sp, vec![cx.ident_of("phf"), cx.ident_of("Map")]),
        vec![
            cx.field_imm(sp, cx.ident_of("key"), cx.expr_lit(sp, LitKind::Int(key as u128, LitIntType::Unsigned(UintTy::U64)))),
            cx.field_imm(sp, cx.ident_of("disps"), cx.expr_addr_of(sp, disps)),
            cx.field_imm(sp, cx.ident_of("entries"), cx.expr_addr_of(sp, entries)),
        ]
    ))
}

pub fn create_set(
    cx: &mut ExtCtxt,
    sp: Span,
    entries: Vec<Entry>,
    state: HashState,
) -> Box<MacResult + 'static> {
    let map = create_map(cx, sp, entries, state).make_expr().unwrap();
    MacEager::expr(cx.expr_struct(
        sp,
        cx.path_global(sp, vec![cx.ident_of("phf"), cx.ident_of("Set")]),
        vec![
            cx.field_imm(sp, cx.ident_of("map"), map)
        ]
    ))
}
