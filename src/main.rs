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

    let string: String = String::from("x & ~y");
    if let Some(p) = read_input(string) {
        print!("{}\n", p);
        let mut v : Model2 = HashMap::new();
        v.insert("x".to_string(), false);
        v.insert("y".to_string(), false);
        if let Some(b) = p.interpret(&v) {
            println!("{}", b);
        }
        display_truth_table(&p);
        if let Some(m) = find_model(&p) {
            print_model(&m);
        } else {
            println!("not satisfiable");
        }
    } else {
        print!("error\n");
    }

    let x = Variable{name: "x".to_string()};
    let xx = Variable{name: "x".to_string()};
    println!("{}", x == xx);

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
