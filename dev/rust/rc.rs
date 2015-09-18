extern crate sync;

use std::rc::Rc;
use sync::Arc;

fn foo(x: Rc<int>) -> () {
    println!("x={}", *x);
}

fn bar(x: Rc<int>) -> () {
    println!("x={}", *x);
}


fn main() {
    let numbers = Vec::from_fn(100, |i| i as f32);
    let shared_numbers = Rc::new(numbers);

    for _ in range(0, 10) {
        let child_numbers = shared_numbers.clone();

        spawn(proc() {
            let local_numbers = child_numbers.as_slice();

            // Work with the local numbers
        });
    }
}

