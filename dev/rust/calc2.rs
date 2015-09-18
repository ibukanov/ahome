extern crate arena;

use arena::TypedArena;


#[deriving(Show)]
enum Term<'a> {
    False,
    True,
    Zero,
    Succ(&'a Term<'a>),
    Pred(&'a Term<'a>),
    IsZero(&'a Term<'a>),
    If(&'a Term<'a>, &'a Term<'a>, &'a Term<'a>)
}

fn is_numeric_val(term: &Term) -> bool {
    let mut t = term;
    loop {
        match *t {
            Zero => return true,
            Succ(x) => t = x,
            _ => return false
        }
    }
}

fn is_val(term: &Term) -> bool {
    match *term {
        False | True => true,
        _ if is_numeric_val(term) => true,
        _ => false
    }

}

fn eval1<'a>(t: &'a Term<'a>, arena: &'a TypedArena<Term<'a>>) -> Option<&'a Term<'a>> {
    match *t {
        If(t1, t2, t3) => match *t1 {
            False => Some(t3),
            True => Some(t2),
            _ => eval1(t1, arena).map(|t1_| arena.alloc(If(t1_, t2, t2))),
        },
        Succ(t1) => eval1(t1, arena).map(|t1_| arena.alloc(Succ(t1_))),
        Pred(t1) => match *t1 {
            Zero => Some(t1),
            Succ(t2) if is_numeric_val(t2) => Some(t2),
            _ => eval1(t1, arena).map(|t1_| arena.alloc(Pred(t1_)))
        },
        IsZero(t1) => match *t1 {
            Zero => Some(arena.alloc(True)),
            Succ(t2) if is_numeric_val(t2) => Some(arena.alloc(False)),
            _ => eval1(t1, arena).map(|t1_| arena.alloc(IsZero(t1_)))
        },
        _ => None
    }
}

fn eval_small_step<'a>(mut t: &'a Term<'a>, arena: &'a TypedArena<Term<'a>>) -> &'a Term<'a> {
    loop {
        match eval1(t, arena) {
            None => break,
            Some(t2) => t = t2
        }
    }
    return t;
}

fn eval_big_step1<'a>(mut t: &'a Term<'a>, arena: &'a TypedArena<Term<'a>>) 
                      -> (&'a Term<'a>, bool) {
    loop {
        match *t {
            False | True | Zero => return (t, true),
            If(t1, t2, t3) => {
                let (condition, evaluated) = eval_big_step1(t1, arena);
                if evaluated {
                    match *condition {
                        False => { t = t3; continue; },
                        True => { t = t2; continue; },
                        _ => ()
                    }
                }
                return (arena.alloc(If(condition, t2, t3)), false);
            },
            Succ(t1) => {
                let (x, mut evaluated) = eval_big_step1(t1, arena);
                if evaluated {
                    match *x {
                        Zero | Succ(_) => (),
                        _ => evaluated = false
                    }
                }
                return (arena.alloc(Succ(x)), evaluated);
            },
            Pred(t1) => {
                let (x, evaluated) = eval_big_step1(t1, arena);
                if evaluated {
                    match *x {
                        Zero => return (x, true),
                        Succ(y) => return (y, true),
                        _ => ()
                    }
                }
                return (arena.alloc(Pred(x)), false);
            }
            IsZero(t1) => {
                let (x, evaluated) = eval_big_step1(t1, arena);
                if evaluated {
                    match *x {
                        Zero => return (arena.alloc(True), true),
                        Succ(_) => return (arena.alloc(False), true),
                        _ => ()
                    }
                }
                return (arena.alloc(IsZero(x)), false);
            }
        }
    }
}

fn eval_big_step<'a>(t: &'a Term<'a>, arena: &'a TypedArena<Term<'a>>) 
                     -> &'a Term<'a> {
    let (result, _) = eval_big_step1(t, arena);
    return result;
}

fn main() {
    let zero = &Zero;
    let one = &Succ(zero);
    let two = &Succ(one);
    let t = &True;
    let x1 = &If(t, two, one);
    let x2 = &Pred(x1);
    let x3 = &IsZero(x2);

    let arena = TypedArena::new();
    let result = eval_small_step(x3, &arena);
    println!("{}", result);
    println!("{}", eval_big_step(x3, &arena));
    println!("{} -> {}", x2, eval_small_step(x2, &arena));
    println!("{} -> {}", x2, eval_big_step(x2, &arena));
}
