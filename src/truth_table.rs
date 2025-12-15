use structures::*;
use std::collections::HashSet;
use std::collections::HashMap;
//use std::vec;

/*
fn extract_variables<'a>(p: &'a Formula) -> HashSet<&'a Variable> {
	let mut vars: HashSet<&'a Variable> = HashSet::new();
	extract_variables_rec(p, &mut vars);
	vars
}

fn extract_variables_rec<'a, 'b>(p: &'a Formula, vars: &'b mut HashSet<&'a Variable>) {
	match p {
		Formula::Var(x) 		=> { vars.insert(&x); },
		Formula::Not(q) 		=> { extract_variables_rec(q, vars); },
		Formula::And(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Or(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Implies(q, r) 	=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Equiv(q, r) 	=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Xor(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
	}
}

pub fn find_truth_table<'a>(p: &'a Formula<'a>) -> Vec<Model<'a>> {
	let mut vars: Vec<&'_ Variable> = extract_variables(p).into_iter().collect();
	vars.sort();
	let n = vars.len();
	let mut models: Vec<Model<'a>> = Vec::new();
	
	for i in 0..(1 << n) {
		let mut model: Model<'a> = HashMap::new();
		for (index, var) in vars.iter().enumerate() {
			if (i & (1 << index)) != 0 {
				model.insert(var, true);
			} else {
				model.insert(var, false);
			}
		}
		models.push(model);
	}
	models
}

pub fn display_truth_table<'a>(p: &'a Formula<'a>) { // TODO -- add support for multiple formulae
	let mut vars: Vec<&'_ Variable> = extract_variables(p).into_iter().collect();
	vars.sort();
	for var in vars.iter() {
		print!("{} | ", var);
	}
	print!("{}\n", p);

	for var in vars.iter() {
		print!("-{}+-", "-".repeat(var.name.to_string().len()));
	}

	print!("{}\n", "-".repeat(p.to_string().len()));

	let models: Vec<Model<'_>> = find_truth_table(p);
	for model in models.iter() {
		for var in vars.iter() {
			match model.get(var) {
				Some(t) => { if *t { print!("1{}| ", " ".repeat(var.name.to_string().len())) } else { print!("0{}| ", " ".repeat(var.name.to_string().len())) } },
				None 	=> print!("<DISPLAY ERROR>\n"),
			}
		}
		match p.interpret(model) {
			Some(t) => { if t { print!("1\n") } else { print!("0\n") } },
			None 	=> print!("<DISPLAY ERROR>\n"),
		}
	}
}*/

pub fn extract_variables(p: &Formula2) -> HashSet<String> {
	let mut vars: HashSet<String> = HashSet::new();
	extract_variables_rec(p, &mut vars);
	vars
}

fn extract_variables_rec<'a, 'b>(p: &Formula2, vars: &mut HashSet<String>) {
	match p {
		Formula2::Var(x) 		=> { vars.insert(x.name.clone()); },
		Formula2::Not(q) 		=> { extract_variables_rec(q, vars); },
		Formula2::And(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula2::Or(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula2::Implies(q, r) 	=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula2::Equiv(q, r) 	=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula2::Xor(q, r) 		=> { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
	}
}

pub fn find_truth_table(p: &Formula2) -> Vec<Model2> {
	let mut vars: Vec<String> = extract_variables(p).into_iter().collect();
	vars.sort();
	let n = vars.len();
	let mut models: Vec<Model2> = Vec::new();
	
	for i in 0..(1 << n) {
		let mut model: Model2 = HashMap::new();
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

pub fn display_truth_table(p: &Formula2) { // TODO -- add support for multiple formulae
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

	let models: Vec<Model2> = find_truth_table(p);
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

pub fn find_model(p: &Formula2) -> Option<Model2> {
	let mut models: Vec<Model2> = find_truth_table(p);
	while let Some(model) = models.pop() {
		if let Some(true) = p.interpret(&model) {
			return Some(model);
		}
	}
	None
}

pub fn print_model(v: &Model2) {
	for (var, b) in v.iter() {
		println!("{} -> {}", var, b);
	}
}