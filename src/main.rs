#![allow(unused)]

mod structures;
mod parser;
mod truth_table;
mod proofs;
mod resolution;

use structures::*;
use parser::*;
use truth_table::*;
use proofs::*;
use resolution::*;
use std::collections::HashMap;
use std::io;

// TODO
// * Fitch proof system
// * clausal form + resolution
// * command line interface
// * clean up parser to be more idiomatic + better error handling
// * SMT solver? find models more intelligently? (do this last)
// * documentation
// * change "Option" to "Result" for error handling

fn main() {
    /*let x : Variable = Variable{name: "xxx".to_string()};
    let y : Variable = Variable{name: "yy".to_string()};
    let mut v : Model = HashMap::new();
    v.insert(&x, true);
    v.insert(&y, false);
    let v_x = Formula::Var(&x);
    let p: Formula = Formula::Implies(&Formula::Xor(&Formula::Var(&y), &Formula::Var(&x)), &Formula::Var(&y));
    display_truth_table(&p);*/

    /*let string: String = String::from("x & ~y | ~z + x");
    if let Some(p) = read_input(string) {
        print!("{}\n", p);
        let mut v : Model = HashMap::new();
        v.insert("x".to_string(), false);
        v.insert("y".to_string(), false);
        if let Some(b) = p.interpret(&v) {
            println!("{}", b);
        }
        display_truth_table(&p);
        print!("{}\n", p.substitute(&"x".to_string(), &"y".to_string()));
        if let Some(m) = find_model(&p) {
            print_model(&m);

        } else {
            println!("not satisfiable");
        }
    } else {
        print!("error\n");
    }

    println!("================");

    let premise_1: String = String::from("x | y");
    let premise_2: String = String::from("x & y => z | w");
    let conc_1: String = String::from("x & y & z");
    let conc_2: String = String::from("x | y");
    if let (Some(p1), Some(p2), Some(c1), Some(c2)) = (read_input(premise_1), read_input(premise_2), read_input(conc_1), read_input(conc_2)) {
        let rule: InferenceRule = InferenceRule{ name: "TestRule".to_string(), premises: Vec::from([p1, p2]), conclusions: Vec::from([c1, c2]) };
        println!("{}", rule);
        println!("{}", &rule.premises[0].substitute_formula(&"x".to_string(), &rule.conclusions[0]));
        println!("{}", &rule.premises[0].substitute_formula_safe(&"x".to_string(), &rule.conclusions[0]));
        println!("{}", rule.premises[0] == rule.conclusions[1]);
    }*/

    /*let src_str = String::from("~x & y => z");
    let dst_str1 = String::from("p => q");
    let dst_str2 = String::from("q => r");
    if let (Some(src), Some(dst1), Some(dst2)) = (read_input(src_str), read_input(dst_str1), read_input(dst_str2)) {
        println!("{}", src);
        println!("{}", dst1);
        println!("{}", dst2);
        println!("=====");
        if let Some(m) = unify(&src, &dst1) {
            println!("p -> {}", m.get("p").unwrap());
            println!("q -> {}", m.get("q").unwrap());
            println!("=====");

            if let Some(mm) = unify(&src, &dst2) {
                println!("q -> {}", mm.get("q").unwrap());
                println!("r -> {}", mm.get("r").unwrap());
                println!("=====");

                if let Some(mmm) = merge_assignments(m, mm) {
                    println!("p -> {}", mmm.get("p").unwrap());
                    println!("r -> {}", mmm.get("r").unwrap());
                    println!("q -> {}", mmm.get("q").unwrap());
                } else {
                    println!("AH");
                }
            }
        } else {
            println!("!!!!");
        }
    }*/
    /*let premise1 = String::from("phi");
    let premise2 = String::from("psi");
    let conc1 = String::from("phi & psi");
    let conc2 = String::from("phi & phi");
    let conc3 = String::from("psi & psi");
    let conc4 = String::from("psi & phi");
    let given1 = String::from("~x");
    let given2 = String::from("~y");
    if let (Some(p1), Some(p2), Some(c1), Some(c2), Some(c3), Some(c4), Some(g1), Some(g2)) = (read_input(premise1), read_input(premise2), read_input(conc1), read_input(conc2), read_input(conc3), read_input(conc4), read_input(given1), read_input(given2)) {
        let rule_ai: InferenceRule = InferenceRule{ name: "AndIntro".to_string(), premises: Vec::from([p1, p2]), conclusions: Vec::from([c1, c2, c3, c4]) };
        let mut m: HashMap<&Formula, &Formula> = HashMap::new();
        m.insert(&rule_ai.premises[0], &g1);
        m.insert(&rule_ai.premises[1], &g2);
        if let Some(conclusions) = conclusions_from_premises(&m, &rule_ai) {
            for conclusion in conclusions.iter() {
                println!("{}", conclusion);
            }
        }
    }*/

    /*let premise1 = String::from("phi => theta");
    let premise2 = String::from("psi => theta");
    let premise3 = String::from("phi | psi");
    let conc1 = String::from("theta");
    let given1 = String::from("(x & y) => ~z");
    let given2 = String::from("(~x + ~y) => ~z");
    let given3 = String::from("(x & y) | (~x + ~y)");
    if let (Some(p1), Some(p2), Some(p3), Some(c1), Some(g1), Some(g2), Some(g3)) = (read_input(premise1), read_input(premise2), read_input(premise3), read_input(conc1), read_input(given1), read_input(given2), read_input(given3)) {
        let rule_ai: InferenceRule = InferenceRule{ name: "OrElim".to_string(), premises: Vec::from([p1, p2, p3]), conclusions: Vec::from([c1]) };
        let formulae: Vec<&Formula> = Vec::from([&g1, &g2, &g3]);
        for conclusion in apply_rule(formulae, &rule_ai).iter() {
            println!("{}", conclusion);
        }
    }*/

    /*let premise1 = String::from("phi");
    let conc1 = String::from("phi | psi");
    let free = String::from("psi");
    let given1 = String::from("(x & y)");
    let given2 = String::from("(~x + ~y)");
    if let (Some(p1), Some(c1), Some(g1), Some(g2), Some(f)) = (read_input(premise1), read_input(conc1), read_input(given1), read_input(given2), read_input(free)) {
        let rule_oi: InferenceRule = InferenceRule{ name: "OrIntro".to_string(), premises: Vec::from([p1]), conclusions: Vec::from([c1]) };
        let formulae: Vec<&Formula> = Vec::from([&g1, &g2]);
        /*let mut supplied: HashMap<&Formula, &Formula> = HashMap::new();
        supplied.insert(&f, &g2);
        for conclusion in apply_rule_with_supplied(formulae, &rule_oi, supplied).iter() {
            println!("{}", conclusion);
        }*/
        let mut supplied: Assignment<'_> = HashMap::new();
        supplied.insert("psi".to_string(), &g2);
        for conclusion in apply_rule_with_supplied(formulae, &rule_oi, supplied).iter() {
            println!("{}", conclusion);
        }
    }*/

    /*let mut proof = Proof::new();
    let rules = initialize_rules();
    let premise_1 = String::from("x => y");
    let premise_2 = String::from("y => z");
    let assumption_1 = String::from("x");
    if let (Some(p1), Some(p2), Some(a1)) = (read_input(premise_1), read_input(premise_2), read_input(assumption_1)) {
        proof.add_premise(p1);
        proof.add_premise(p2);
        proof.add_assumption(a1);
        if let Some(rule) = rules.get(&"ImpliesElim".to_string()) {
            proof.infer(rule, Vec::from([0, 2]));
            proof.infer(rule, Vec::from([1, 3]));
            proof.implication_intro();
        }
    }
    print!("{}", proof);*/

    /*let mut proof = Proof::new();
    let rules = initialize_rules();

    loop {
        println!("Select option:\n  P - premise\n  A - assumption\n  D - delete\n  R - rule\n  Q - quit");
        let mut input_string = String::new();
        io::stdin()
            .read_line(&mut input_string) // read_line appends to the string
            .expect("Failed to read line"); // basic error handling
        match input_string.trim() {
            "P" => {
                println!("Enter premise:");
                input_string = String::new();
                io::stdin()
                    .read_line(&mut input_string) // read_line appends to the string
                    .expect("Failed to read line");
                match read_input(input_string) {
                    Some(p) => {
                        proof.add_premise(p);
                        println!("{}", proof);
                    },
                    None => println!("Invalid formula."),
                }
            },
            "A" => {
                println!("Enter assumption:");
                input_string = String::new();
                io::stdin()
                    .read_line(&mut input_string) // read_line appends to the string
                    .expect("Failed to read line");
                match read_input(input_string) {
                    Some(p) => {
                        proof.add_assumption(p);
                        println!("{}", proof);
                    },
                    None => println!("Invalid formula."),
                }
            },
            "D" => {
                proof.delete_bottom();
                println!("{}", proof);
            },
            "R" => {
                println!("Select rule:");
                input_string = String::new();
                io::stdin()
                    .read_line(&mut input_string) // read_line appends to the string
                    .expect("Failed to read line");

                if input_string.trim() == "ImpliesIntro".to_string() {
                    proof.implication_intro();
                    println!("{}", proof);
                } else {
                    match rules.get(input_string.trim()) {
                        Some(rule) => {
                            println!("Enter the desired indices:");
                            input_string = String::new();
                            io::stdin()
                                .read_line(&mut input_string) // read_line appends to the string
                                .expect("Failed to read line");

                            let mut premise_indices: Vec<usize> = Vec::new();
                            for index in input_string.trim().split(',') {
                                premise_indices.push(index.parse().expect("Not a valid usize."));
                            }

                            proof.infer(rule, premise_indices);
                            println!("{}", proof);
                        },
                        None => println!("Invalid rule."),
                    }
                }
            },
            "Q" => {
                println!("Quitting...");
                return;
            },
            _ => println!("Invalid input."),
        }
    }*/


    println!("Input formula:");
    let mut input_string = String::new();
    io::stdin()
        .read_line(&mut input_string) // read_line appends to the string
        .expect("Failed to read line"); // basic error handling
    match read_input(input_string) {
        Some(p) => {
            println!("{}", negation_clausal(&xor_impl_clausal(&p)));
        },
        None => println!("Invalid formula."),
    }

    /*let mut input_string = String::new();
    io::stdin()
        .read_line(&mut input_string) // read_line appends to the string
        .expect("Failed to read line"); // basic error handling
    let trimmed_input = input_string.trim();

    println!("You entered: {}", trimmed_input);*/
}
