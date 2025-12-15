use structures::*;
use std::collections::HashSet;
use std::collections::HashMap;

pub fn extract_variables(p: &Formula) -> HashSet<String> {
	let mut vars: HashSet<String> = HashSet::new();
	extract_variables_rec(p, &mut vars);
	vars
}

fn extract_variables_rec<'a, 'b>(p: &Formula, vars: &mut HashSet<String>) {
	match p {
		Formula::Var(x) 		=> { vars.insert(x.name.clone()); },
		Formula::Not(q) 		=> { extract_variables_rec(q, vars); },
		Formula::And(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Or(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Implies(q, r) 	=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Equiv(q, r) 	=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Xor(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
	}
}

pub fn find_truth_table(p: &Formula) -> Vec<Model> {
	let mut vars: Vec<String> = extract_variables(p).into_iter().collect();
	vars.sort();
	let n = vars.len();
	let mut models: Vec<Model> = Vec::new();
	
	for i in 0..(1 << n) {
		let mut model: Model = HashMap::new();
		for (index, var) in vars.iter().enumerate() {
			if (i & (1 << index)) != 0 {
				model.insert(var.clone(), true);
			} else {
				model.insert(var.clone(), false);
			}
		}
		models.push(model);
	}
	models
}

pub fn display_truth_table(p: &Formula) { // TODO -- add support for multiple formulae
	let mut vars: Vec<String> = extract_variables(p).into_iter().collect();
	vars.sort();
	for var in vars.iter() {
		print!("{} | ", var);
	}
	print!("{}\n", p);

	for var in vars.iter() {
		print!("-{}+-", "-".repeat(var.len()));
	}

	print!("{}\n", "-".repeat(p.to_string().len()));

	let models: Vec<Model> = find_truth_table(p);
	for model in models.iter() {
		for var in vars.iter() {
			match model.get(var) {
				Some(t) => { if *t { print!("1{}| ", " ".repeat(var.len())) } else { print!("0{}| ", " ".repeat(var.len())) } },
				None 	=> print!("<DISPLAY ERROR>\n"),
			}
		}
		match p.interpret(model) {
			Some(t) => { if t { print!("1\n") } else { print!("0\n") } },
			None 	=> print!("<DISPLAY ERROR>\n"),
		}
	}
}

pub fn find_model(p: &Formula) -> Option<Model> {
	let mut models: Vec<Model> = find_truth_table(p);
	while let Some(model) = models.pop() {
		if let Some(true) = p.interpret(&model) {
			return Some(model);
		}
	}
	None
}


pub fn is_satisfiable(p: &Formula) -> bool {
	match find_model(p) {
		Some(_) => true,
		None	=> false,
	}
}

pub fn print_model(v: &Model) {
	for (var, b) in v.iter() {
		println!("{} -> {}", var, b);
	}
}