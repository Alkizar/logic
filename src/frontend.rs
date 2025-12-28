use std::io;
use std::io::{stdin, stdout, Write};
use std::collections::HashMap;

use crate::structures::*;
use crate::parser::*;
use crate::truth_table::*;
use crate::proofs::*;
use crate::resolution::*;

struct EngineData {
    proof: Proof,
    inference_rules: HashMap<String, InferenceRule>,
    last_formula: Option<Formula>,
    selected_formula: Option<Formula>,
}

fn clear() {
    print!("\x1B[2J\x1B[1;1H");
    stdout().flush().unwrap();
}

fn sanitize_option(input: String) -> String {
    input.trim().to_lowercase()
}

fn await_option() -> String {
    println!("Select an option: ");
    let mut input_string = String::new();
    stdin()
        .read_line(&mut input_string) // read_line appends to the string
        .expect("Failed to read line"); // basic error handling
    sanitize_option(input_string)
}

fn get_input_formula() -> Formula {
    println!("Enter a formula: ");
    loop {
        let mut input_string = String::new();
        stdin()
        .read_line(&mut input_string) // read_line appends to the string
        .expect("Failed to read line"); // basic error handling
        if let Some(p) = read_input(input_string) {
            return p;
        }
    }
}

fn get_most_recent(data: &EngineData) -> Option<Formula> {
    data.last_formula.clone()
}

fn get_selected(data: &EngineData) -> Option<Formula> {
    data.selected_formula.clone()
}

fn await_formula(data: &EngineData) -> Formula {
    println!("  (a) enter a formula
  (b) use the most recent formula
  (c) use the selected formula");
    loop {
        let mut option = await_option();
        match option.as_str() {
            "a" => return get_input_formula(),
            "b" => match get_most_recent(data) {
                Some(p) => return p,
                None    => println!("No recent formula to use."),
            },
            "c" => match get_selected(data) {
                Some(p) => return p,
                None    => println!("No formula selected."),
            }
            _   => println!("Invalid selection.\n"),
        }
    }
}

fn await_number() -> usize {
    loop {
        println!("Enter a positive number:");
        let mut input_string = String::new();
        stdin()
            .read_line(&mut input_string) // read_line appends to the string
            .expect("Failed to read line"); // basic error handling
        match input_string.trim().parse::<usize>() {
            Ok(n)  => return n,
            Err(_) => println!("Invalid input.")
        }
    }
}

pub fn initialize_engine() {
    let data = EngineData {
        proof:            Proof::new(),
        inference_rules:  initialize_rules(),
        last_formula:     None,
        selected_formula: None,
    };
    main_menu(data);
}

/*
          MAIN MENU          
-----------------------------
selected: ...
+---------------------------+
| (a) start new Fitch proof |
| (b) view current proof    |
| (c) check entailment      |
| (d) generate truth table  |
| (e) quit                  |
+---------------------------+
*/
fn main_menu(mut data: EngineData) {
    loop {
        clear();
        println!("          MAIN MENU          ");
        if let Some(ref p) = data.selected_formula {
            println!("-----------------------------\nselected: {}", p);
        }
        println!("+---------------------------+
| (a) start new Fitch proof |
| (b) view current proof    |
| (c) check entailment      |
| (d) generate truth table  |
| (e) quit                  |
+---------------------------+");
        let mut option = await_option();
        match option.as_str() {
            "a" => data = fitch_menu(reset_proof(data)),
            "b" => data = fitch_menu(data),
            //"c" => data = resolution_menu(data),
            //"d" => data = truth_table_menu(data),
            "e" => { quit(); break; },
            _   => println!("Invalid selection.\n"),
        }
    }
}

fn quit() {
    println!("Quitting...");
}

fn reset_proof(mut data: EngineData) -> EngineData {
    data.proof = Proof::new();
    data
}

/*
        FITCH PROOFS         
-----------------------------
Current Proof:
  1. x
  ...
  n. x
+---------------------------+
| (a) add premise           |
| (b) add assumption        |
| (c) apply inference rule  |
| (d) view inference rule   |
| (e) select formula        |
| (f) back                  |
+---------------------------+
*/
fn fitch_menu(mut data: EngineData) -> EngineData {
    loop {
        clear();
        println!("        FITCH PROOFS         ");
        if data.proof.len() > 0 {
            println!("-----------------------------");
            println!("Current Proof:");
            println!("{}", data.proof);
        }
        if let Some(ref p) = data.selected_formula {
            println!("-----------------------------\nselected: {}", p);
        }
        println!("+---------------------------+
| (a) add premise           |
| (b) add assumption        |
| (c) apply inference rule  |
| (d) view inference rule   |
| (e) select formula        |
| (f) back                  |
+---------------------------+");
        let mut option = await_option();
        match option.as_str() {
            "a" => data = fitch_add_premise(data),
            "b" => data = fitch_add_assumption(data),
            "c" => data = fitch_apply_rule(data),
            "d" => rule_menu(&data),
            "e" => data = fitch_select_formula(data),
            "f" => break,
            _   => println!("Invalid selection."),
        }
    }
    data
}

fn fitch_add_premise(mut data: EngineData) -> EngineData {
    let mut formula = await_formula(&data);
    data.last_formula = Some(formula.clone());
    data.proof.add_premise(formula);
    data
}

fn fitch_add_assumption(mut data: EngineData) -> EngineData {
    let mut formula = await_formula(&data);
    data.last_formula = Some(formula.clone());
    data.proof.add_assumption(formula);
    data
}

fn fitch_apply_rule(mut data: EngineData) -> EngineData {
    println!("NOT IMPLEMENTED YET!");
    data
}

/*
       INFERENCE RULES       
-----------------------------
  RuleName
  ...
  RuleName
*/
// TODO -- show full names and minimal abbreviations for each rule; eg [IE] ImpliesElim (Implication Elimination)
fn rule_menu(data: &EngineData) {
    clear();
    println!("       INFERENCE RULES       
-----------------------------");
    for rule in data.inference_rules.values() {
        println!("  {}", rule.name);
    }
    loop {
        let mut option = await_option();
        // parse option, compare to names
        // if it doesn't match, loop
        // TODO
        break;
    }
}

fn fitch_select_formula(mut data: EngineData) -> EngineData {
    let index = await_number();
    if let Some(p) = data.proof.get_formula_at(index.saturating_sub(1)) {
        data.selected_formula = Some(p);
    }
    data
}