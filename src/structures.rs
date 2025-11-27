use std::collections::HashMap;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Variable {
	pub name: String
}

impl fmt::Display for Variable {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

pub type Model<'a> = HashMap<&'a Variable, bool>;

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

pub enum Formula<'a> {
	Var 	(&'a Variable),
	Not		(&'a Formula<'a>),
	And		(&'a Formula<'a>, &'a Formula<'a>),
	Or 		(&'a Formula<'a>, &'a Formula<'a>),
	Implies	(&'a Formula<'a>, &'a Formula<'a>),
	Equiv	(&'a Formula<'a>, &'a Formula<'a>),
	Xor		(&'a Formula<'a>, &'a Formula<'a>),
}

impl Formula<'_> {
	pub fn interpret(&self, v: &Model) -> Option<bool> {
		match self { 
			Formula::Var(x) 		=> extract_bool(v.get(x)),
			Formula::Not(y) 		=> not(y.interpret(v)),
			Formula::And(y, z) 		=> and(y.interpret(v), z.interpret(v)),
			Formula::Or(y, z) 		=> or(y.interpret(v), z.interpret(v)),
			Formula::Implies(y, z) 	=> implies(y.interpret(v), z.interpret(v)),
			Formula::Equiv(y, z) 	=> equiv(y.interpret(v), z.interpret(v)),
			Formula::Xor(y, z) 		=> xor(y.interpret(v), z.interpret(v)),
		}
	}
}