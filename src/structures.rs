use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Clone)] // TODO -- can we just replace this with String? refactoring required
pub struct Variable {
	pub name: String
}

impl fmt::Display for Variable {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

fn not(a: Option<bool>) -> Option<bool> {
	match a {
		Some(t) => Some(!t),
		_ => None,
	}
}

fn and(a: Option<bool>, b: Option<bool>) -> Option<bool> {
	match (a, b) {
		(Some(ta), Some(tb)) => Some(ta && tb),
		_ => None,
	}
}

fn or(a: Option<bool>, b: Option<bool>) -> Option<bool> {
	match (a, b) {
		(Some(ta), Some(tb)) => Some(ta || tb),
		_ => None,
	}
}

fn implies(a: Option<bool>, b: Option<bool>) -> Option<bool> {
	or(not(a), b)
}

fn equiv(a: Option<bool>, b: Option<bool>) -> Option<bool> {
	and(implies(a, b), implies(b, a))
}

fn xor(a: Option<bool>, b: Option<bool>) -> Option<bool> {
	and(or(a, b), not(and(a, b)))
}

fn extract_bool(t: Option<&bool>) -> Option<bool> {
	match t {
		Some(x) => Some(*x),
		None => None,
	}
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Formula {
	Var 	(Variable),
	Not		(Box<Formula>),
	And		(Box<Formula>, Box<Formula>),
	Or 		(Box<Formula>, Box<Formula>),
	Implies	(Box<Formula>, Box<Formula>),
	Equiv	(Box<Formula>, Box<Formula>),
	Xor		(Box<Formula>, Box<Formula>),
}

impl fmt::Display for Formula {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Formula::Var(x) 	   => write!(f, "{}", x),
			Formula::Not(y) 	   => write!(f, "~{}", y),
			Formula::And(y, z) 	   => write!(f, "({} & {})", y, z),
			Formula::Or(y, z) 	   => write!(f, "({} | {})", y, z),
			Formula::Implies(y, z) => write!(f, "({} => {})", y, z),
			Formula::Equiv(y, z)   => write!(f, "({} <=> {})", y, z),
			Formula::Xor(y, z) 	   => write!(f, "({} + {})", y, z),
		}
	}
}

pub type Model = HashMap<String, bool>;
pub type Assignment<'a> = HashMap<String, &'a Formula>;

impl Formula {
	pub fn interpret(&self, v: &Model) -> Option<bool> {
		match self { 
			Formula::Var(x) 	   => extract_bool(v.get(&x.name)),
			Formula::Not(y) 	   => not(y.interpret(v)),
			Formula::And(y, z) 	   => and(y.interpret(v), z.interpret(v)),
			Formula::Or(y, z) 	   => or(y.interpret(v), z.interpret(v)),
			Formula::Implies(y, z) => implies(y.interpret(v), z.interpret(v)),
			Formula::Equiv(y, z)   => equiv(y.interpret(v), z.interpret(v)),
			Formula::Xor(y, z) 	   => xor(y.interpret(v), z.interpret(v)),
		}
	}

	pub fn substitute(&self, src: &String, dst: &String) -> Formula {
		match self {
			Formula::Var(x) 	   => {
				if x.name == *src {
					Formula::Var(Variable{ name: dst.clone() })
				} else {
					Formula::Var(Variable{ name: x.name.clone() })
				}
			},
			Formula::Not(y) 	   => Formula::Not(Box::new(y.substitute(src, dst))),
			Formula::And(y, z) 	   => Formula::And(Box::new(y.substitute(src, dst)), Box::new(z.substitute(src, dst))),
			Formula::Or(y, z) 	   => Formula::Or(Box::new(y.substitute(src, dst)), Box::new(z.substitute(src, dst))),
			Formula::Implies(y, z) => Formula::Implies(Box::new(y.substitute(src, dst)), Box::new(z.substitute(src, dst))),
			Formula::Equiv(y, z)   => Formula::Equiv(Box::new(y.substitute(src, dst)), Box::new(z.substitute(src, dst))),
			Formula::Xor(y, z)     => Formula::Xor(Box::new(y.substitute(src, dst)), Box::new(z.substitute(src, dst))),
		}
	}

	pub fn substitute_formula(&self, src: &String, dst: &Formula) -> Formula {
		match self {
			Formula::Var(x) 	   => {
				if x.name == *src {
					dst.clone()
				} else {
					Formula::Var(Variable{ name: x.name.clone() })
				}
			},
			Formula::Not(y) 	   => Formula::Not(Box::new(y.substitute_formula(src, dst))),
			Formula::And(y, z) 	   => Formula::And(Box::new(y.substitute_formula(src, dst)), Box::new(z.substitute_formula(src, dst))),
			Formula::Or(y, z) 	   => Formula::Or(Box::new(y.substitute_formula(src, dst)), Box::new(z.substitute_formula(src, dst))),
			Formula::Implies(y, z) => Formula::Implies(Box::new(y.substitute_formula(src, dst)), Box::new(z.substitute_formula(src, dst))),
			Formula::Equiv(y, z)   => Formula::Equiv(Box::new(y.substitute_formula(src, dst)), Box::new(z.substitute_formula(src, dst))),
			Formula::Xor(y, z)     => Formula::Xor(Box::new(y.substitute_formula(src, dst)), Box::new(z.substitute_formula(src, dst))),
		}
	}

	pub fn extract_variables(&self) -> HashSet<String> {
		let mut vars: HashSet<String> = HashSet::new();
		extract_variables_rec(&self, &mut vars);
		vars
	}

	// Performs alpha conversion on formula substituted first, to prevent unsafe variable capture
	pub fn substitute_formula_safe(&self, src: &String, dst: &Formula) -> Formula {
		let current_vars = self.extract_variables();
		let new_vars = dst.extract_variables();
		let var_intersection = current_vars.intersection(&new_vars);
		let new_var = find_fresh_var(&(&current_vars | &new_vars), "a".to_string());

		match var_intersection.into_iter().next() {
			Some(var) => {
				let new_dst = dst.substitute(var, &new_var);
				self.substitute_formula_safe(src, &new_dst)
			},
			None => self.substitute_formula(src, dst)
		}
	}

	pub fn substitute_formula_from_assignment(&self, assignment: &Assignment) -> Formula {
		let mut p = self.clone();
		for (key, val) in assignment.iter() {
			p = p.substitute_formula(key, val);
		}
		p
	}
}

fn extract_variables_rec(p: &Formula, vars: &mut HashSet<String>) {
	match p {
		Formula::Var(x) 	   => { vars.insert(x.name.clone()); },
		Formula::Not(q) 	   => { extract_variables_rec(q, vars); },
		Formula::And(q, r) 	   => { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Or(q, r) 	   => { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Implies(q, r) => { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Equiv(q, r)   => { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
		Formula::Xor(q, r) 	   => { extract_variables_rec(q, vars); extract_variables_rec(r, vars); },
	}
}

fn find_fresh_var(vars: &HashSet<String>, prefix: String) -> String {
	let mut index: usize = 0;
	let mut var = prefix.clone() + &index.to_string();
	while vars.contains(&var) {
		index += 1;
		var = prefix.clone() + &index.to_string();
	}
	var
}