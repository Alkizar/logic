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
    premises: Vec<Formula>,
    conclusion: Option<Formula>,
    rule_abbrevs: HashMap<String, String>,
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

// should adjust the indices based on input to start at 1 and not 0
fn await_indices() -> Vec<usize> {
    let mut indices = Vec::new();
    println!("Enter one or more positive numbers, separated by commas:");
    let mut input_string = String::new();
    stdin()
        .read_line(&mut input_string) // read_line appends to the string
        .expect("Failed to read line");
    for index in input_string.trim().split(',') {
        if let Ok(n) = index.trim().parse::<usize>() {
            if n > 0 {
                indices.push(n.saturating_sub(1));
            }
        }
    }
    indices

}

pub fn initialize_engine() {
    let rules = initialize_rules();
    let mut abbrevs = HashMap::new();
    for name in rules.keys() {
        abbrevs.insert(get_rule_abbrev(name).to_lowercase(), name.clone());
    }
    let data = EngineData {
        proof:            Proof::new(),
        inference_rules:  rules,
        last_formula:     None,
        selected_formula: None,
        premises:         Vec::new(),
        conclusion:       None,
        rule_abbrevs:     abbrevs,
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
            "c" => data = entails_menu(data),
            "d" => data = truth_table_menu(data),
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
current proof:
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
            println!("current proof:");
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
| (e) delete step           |
| (f) select formula        |
| (g) back                  |
+---------------------------+");
        let mut option = await_option();
        match option.as_str() {
            "a" => data = fitch_add_premise(data),
            "b" => data = fitch_add_assumption(data),
            "c" => data = fitch_apply_rule(data),
            "d" => rule_menu(&data),
            "e" => data = fitch_delete(data),
            "f" => data = fitch_select_formula(data),
            "g" => break,
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
    println!("[II] [XI] [EI] [AI] [OI] [NI]\n[IE] [XE] [EE] [AE] [OE] [NE]");
    loop {
        let mut option = await_option();
        if let Some(name) = data.rule_abbrevs.get(&option) {
            if let Some(rule) = data.inference_rules.get(name) {
                let indices = await_indices(); // should return a valid Vec<usize>
                data.proof.infer(rule, indices);
                break;
            } 
        } else if option.as_str() == "ii" {
            // ImpliesIntro
            data.proof.implication_intro();
            break;
        } else {
            println!("Invalid selection.");
        }
    }
    data
}

fn get_rule_abbrev(name: &String) -> String {
    let mut abbr = String::new();
    for c in name.chars() {
        if c.is_uppercase() {
            abbr.push(c);
        }
    }
    abbr
}

/*
       INFERENCE RULES       
-----------------------------
  RuleName
  ...
  RuleName
*/
fn rule_menu(data: &EngineData) {
    clear();
    println!("       INFERENCE RULES       
-----------------------------");
    for rule in data.inference_rules.values() {
        let abbr = get_rule_abbrev(&rule.name);
        println!("  [{}]  {}", abbr, rule.name);
    }
    println!("-----------------------------\n  (a) back");
    loop {
        let mut option = await_option();
        if option.as_str() == "a" {
            break;
        }
        else if let Some(name) = data.rule_abbrevs.get(&option) {
            if let Some(rule) = data.inference_rules.get(name) {
                println!("\n{}", rule);
            }
        } else {
            println!("Invalid selection.");
        }
    }
}

fn fitch_select_formula(mut data: EngineData) -> EngineData {
    let index = await_number();
    if let Some(p) = data.proof.get_formula_at(index.saturating_sub(1)) {
        data.selected_formula = Some(p);
    }
    data
}

fn fitch_delete(mut data: EngineData) -> EngineData {
    data.proof.delete_bottom();
    data
}

/*
      ENTAILMENT CHECKER     
-----------------------------
premises:
  x
  ...
  x
conclusion:
  y
entails? T/F
-----------------------------
selected: 
+---------------------------+
| (a) add premise           |
| (b) remove premise        |
| (c) set conclusion        |
| (d) select premise        |
| (e) select conclusion     |
| (f) check entailment      |
| (g) back                  |
+---------------------------+
*/
fn entails_menu(mut data: EngineData) -> EngineData {
    loop {
        clear();
        println!("      ENTAILMENT CHECKER     
-----------------------------");
        if data.premises.len() == 0 {
            println!("premises: {{}}");
        } else {
            println!("premises:");
            for (index, premise) in data.premises.iter().enumerate() {
                println!("{}. {}", index + 1, premise);
            }
        }
        if let Some(ref p) = data.conclusion {
            println!("conclusion:\n  {}", p);
            println!("entailed? {}", all_entail(&data.premises, &p))
        }
        if let Some(ref p) = data.selected_formula {
            println!("-----------------------------\nselected: {}", p);
        }
        println!("+---------------------------+
| (a) add premise           |
| (b) remove premise        |
| (c) set conclusion        |
| (d) select premise        |
| (e) select conclusion     |
| (f) clear all             |
| (g) back                  |
+---------------------------+");
        let mut option = await_option();
        match option.as_str() {
            "a" => data = entails_add_premise(data),
            "b" => data = entails_remove_premise(data),
            "c" => data = entails_set_conclusion(data),
            "d" => data = entails_select_premise(data),
            "e" => data = entails_set_conclusion(data),
            "f" => data = entails_clear(data),
            "g" => break,
            _   => println!("Invalid selection."),
        }
    }
    data
}

fn entails_add_premise(mut data: EngineData) -> EngineData {
    let mut formula = await_formula(&data);
    data.last_formula = Some(formula.clone());
    data.premises.push(formula);
    data
}

fn entails_remove_premise(mut data: EngineData) -> EngineData {
    let index = await_number();
    if index > 0 && index <= data.premises.len() {
        data.premises.remove(index.saturating_sub(1));
    }
    data
}

fn entails_set_conclusion(mut data: EngineData) -> EngineData {
    let mut formula = await_formula(&data);
    data.last_formula = Some(formula.clone());
    data.conclusion = Some(formula);
    data
}

fn entails_select_premise(mut data: EngineData) -> EngineData {
    let index = await_number();
    if let Some(p) = data.premises.get(index.saturating_sub(1)) {
        data.selected_formula = Some(p.clone());
    }
    data
}

fn entails_select_conclusion(mut data: EngineData) -> EngineData {
    if let Some(ref p) = data.conclusion {
        data.selected_formula = Some(p.clone());
    }
    data
}

fn entails_clear(mut data: EngineData) -> EngineData {
    data.premises = Vec::new();
    data.conclusion = None;
    data
}

/*
         TRUTH TABLES        
+---------------------------+
| (a) generate truth table  |
| (b) back                  |
+---------------------------+
*/
fn truth_table_menu(mut data: EngineData) -> EngineData {
    clear();
    println!("         TRUTH TABLES        ");
    if let Some(ref p) = data.selected_formula {
        println!("-----------------------------\nselected: {}", p);
    }
    loop {
        println!("+---------------------------+
| (a) generate truth table  |
| (b) clear all             |
| (c) back                  |
+---------------------------+");
        let mut option = await_option();
        match option.as_str() {
            "a" => data = truth_table_display(data),
            "b" => truth_table_clear(&data),
            "c" => break,
            _   => println!("Invalid selection."),
        }
    }
    data
}

fn truth_table_display(mut data: EngineData) -> EngineData { // WHERE DO WE DISPLAY THIS?
    let mut formula = await_formula(&data);
    data.last_formula = Some(formula.clone());
    display_truth_table(&formula);
    data
}

fn truth_table_clear(data: &EngineData) {
    clear();
    println!("         TRUTH TABLES        ");
    if let Some(ref p) = data.selected_formula {
        println!("-----------------------------\nselected: {}", p);
    }
}