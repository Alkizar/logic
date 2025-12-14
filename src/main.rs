mod structures;
mod parser;
mod truth_table;

use structures::*;
use parser::*;
use truth_table::*;
use std::collections::HashMap;

fn main() {
    /*let x : Variable = Variable{name: "xxx".to_string()};
    let y : Variable = Variable{name: "yy".to_string()};
    let mut v : Model = HashMap::new();
    v.insert(&x, true);
    v.insert(&y, false);
    let v_x = Formula::Var(&x);
    let p: Formula = Formula::Implies(&Formula::Xor(&Formula::Var(&y), &Formula::Var(&x)), &Formula::Var(&y));
    display_truth_table(&p);*/

    let string: String = String::from("x + (~~y => z)");
    if let Some(p) = read_input(string) {
        print!("{}\n", p);
    } else {
        print!("error\n");
    }

    /*match p.interpret(&mut v) {
        Some(t) => println!("{}", t.to_string()),
        None => println!("ERROR\n")
    }*/
    /*for item in extract_variables(&p).iter() {
        println!("{}", item);
    }*/
    /*let in_string: &str = "(alice & bob) => charles <=> daniel";
    match lex(in_string) {
        Some(ts) => {
            for item in ts.iter() {
                print!("{} ", item);
            };
        },
        None => println!("error"),
    }*/
}
