fn test() -> (int, bool) { (3, true) }


fn main() {
    let (i, mut flag) = test();
    if flag {
        match i {
            1 | 2 => (),
            _ => flag = false
        }
    }
    println!("{}", flag);
}
