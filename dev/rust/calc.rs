//extern crate collections;

//use collections::enum_set::EnumSet;

#[deriving(Show, Clone)]
enum Term {
    False,
    True,
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>)
}

#[deriving(Eq)]
enum TermType {
    BoolTerm = 1,
    NumericTerm = 2
}

fn getType(term: &Term) -> TermType {
    match *term {
        False | True => BoolTerm,
        Zero => NumericTerm,
        Succ(..) | Pred(..) => NumericTerm,
        IsZero(..) => BoolTerm,
        If(_, box ref thenPart, _) => getType(thenPart)
    }
}

fn checkTypes(term: &Term) -> bool {
    checkTypesImpl(term, BoolTerm as uint | NumericTerm as uint)
}

fn checkTypesImpl(term: &Term, flags: uint) -> bool {
    match *term {
        False | True => (flags & BoolTerm as uint) != 0,
        Zero => (flags & NumericTerm as uint) != 0,
        Succ(box ref x) | Pred(box ref x) => 
            (flags & NumericTerm as uint) != 0 &&
            checkTypesImpl(x, NumericTerm as uint),
        IsZero(box ref x) =>
            (flags & BoolTerm as uint) != 0 &&
            checkTypesImpl(x, NumericTerm as uint),
        If(box ref condition, box ref thenPart, box ref elsePart) =>
            checkTypesImpl(condition, BoolTerm as uint) &&
            checkTypesImpl(thenPart, flags) &&
            checkTypesImpl(elsePart, flags) &&
            getType(thenPart) == getType(elsePart)
    }
}

#[deriving(Show)]
enum Value {
    BoolVal(bool),
    IntVal(int)
}


fn eval(term: &Term) -> Value {
    match *term {
        False => BoolVal(false),
        True => BoolVal(true),
        Zero => IntVal(0),
        Succ(box ref x) => match eval(x) {
            IntVal(num) => IntVal(num + 1),
            other => fail!("Cannot apply Succ to {}", other)
        },
        Pred(box ref x) => match eval(x) {
            IntVal(num) => IntVal(num - 1),
            other => fail!("Cannot apply Pred to {}", other)
        },
        IsZero(box ref x) => match eval(x) {
            IntVal(num) => BoolVal(num == 0),
            other => fail!("Cannot apply IsZero to {}", other)
        },
        If(box ref condition, box ref thenPart, box ref elsePart) => {
            match eval(condition) {
                BoolVal(flag) => 
                    if flag { eval(thenPart) } else { eval(elsePart) },
                other => fail!("Cannot treat {} as boolean condition", other),
            }
        }
    }
}


fn main() {

    let one = box Succ(box Zero);
    let two = box Succ(one.clone());

    let badTerm = &If(box True, two.clone(), box False);
    println!("Typed {} - {}", badTerm, checkTypes(badTerm));
    let goodTerm = box If(box True, two.clone(), box Zero);
    println!("Typed {} - {}", goodTerm, checkTypes(goodTerm));
    let goodTerm2 = box Succ (goodTerm);
    println!("Typed {} - {}", goodTerm2, checkTypes(goodTerm2));
    let badTerm2 = If(goodTerm2, box True, box False);
    println!("Typed {} - {}", badTerm2, checkTypes(&badTerm2));



    let t : Term = IsZero(box Pred(box If(box True, two, one)));
    let result = eval(&t);
    println!("{}", result);
}
