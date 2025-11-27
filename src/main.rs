mod structures;
mod parser;

use structures::*;
use parser::*;
use std::collections::HashMap;

fn main() {
    /*let x : Variable = Variable{name: "x".to_string()};
    let y : Variable = Variable{name: "y".to_string()};
    let mut v : Model = HashMap::new();
    v.insert(&x, true);
    v.insert(&y, false);
    let p : Formula = Formula::Equiv(&Formula::Var(&y), &Formula::Var(&x));
    match p.interpret(&mut v) {
        Some(t) => println!("{}", t.to_string()),
        None => println!("ERROR\n")
    }*/
    let in_string: &str = "(alice & bob) => charles <=> daniel";
    match lex(in_string) {
        Some(ts) => {
            for item in ts.iter() {
                print!("{} ", item);
            }
        },
        None => println!("error"),
    }
}
