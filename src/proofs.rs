//mod proofs;

use crate::structures::*;
use crate::parser::*;
use std::fmt;
use std::collections::HashMap;

pub struct InferenceRule {
	pub name:		 String,
	pub premises: 	 Vec<Formula>,
	pub conclusions: Vec<Formula>,
	pub free:		 Option<Vec<String>>, // These are the names of the free variables that must be supplied with instantiations
}

// merges a pair of assignments if consistent; fails if inconsistent
pub fn merge_assignments<'a>(assignment1: Assignment<'a>, assignment2: Assignment<'a>) -> Option<Assignment<'a>> {
	let mut new_assignment: Assignment<'a> = HashMap::new();
	for (key, val) in assignment1.iter() {
		if let Some(p) = assignment2.get(key) {
			if val != p {
				return None;
			} 
		}
		new_assignment.insert(key.to_string(), val);
	}
	for (key, val) in assignment2.iter() {
		new_assignment.insert(key.to_string(), val);
	}
	Some(new_assignment)
}

// attempts to consistently assign each variable in dst to a subformula of src
pub fn unify<'a>(src: &'a Formula, dst: &Formula) -> Option<Assignment<'a>> {
	let assignment = HashMap::new();
	unify_rec(src, dst, Some(assignment))
}

// TODO -- why are we continuing to recurse when we encounter an error?? break on None, and change input to receive assignment
fn unify_rec<'a>(src: &'a Formula, dst: &Formula, mut assignment: Option<Assignment<'a>>) -> Option<Assignment<'a>> {
	match (assignment, dst) {
		(None, _) => None,
		(Some(mut m), Formula::Var(x)) => {
			match m.insert(x.name.clone(), src) {
				Some(p) => {
					if p == src {
						Some(m)
					} else {
						None
					}
				},
				None => Some(m),
			}
		},
		(Some(m), Formula::Not(q)) => {
			match src {
				Formula::Not(p) => unify_rec(p, q, Some(m)),
				_ => None,
			}
		},
		(Some(m), Formula::And(q1, q2)) => {
			match src {
				Formula::And(p1, p2) => {
					assignment = unify_rec(p1, q1, Some(m));
					unify_rec(p2, q2, assignment)
				},
				_ => None,
			}
		},
		(Some(m), Formula::Or(q1, q2)) => {
			match src {
				Formula::Or(p1, p2) => {
					assignment = unify_rec(p1, q1, Some(m));
					unify_rec(p2, q2, assignment)
				},
				_ => None,
			}
		},
		(Some(m), Formula::Implies(q1, q2)) => {
			match src {
				Formula::Implies(p1, p2) => {
					assignment = unify_rec(p1, q1, Some(m));

					unify_rec(p2, q2, assignment)
				},
				_ => None,
			}
		},
		(Some(m), Formula::Equiv(q1, q2)) => {
			match src {
				Formula::Equiv(p1, p2) => {
					assignment = unify_rec(p1, q1, Some(m));
					unify_rec(p2, q2, assignment)
				},
				_ => None,
			}
		},
		(Some(m), Formula::Xor(q1, q2)) => {
			match src {
				Formula::Xor(p1, p2) => {
					assignment = unify_rec(p1, q1, Some(m));
					unify_rec(p2, q2, assignment)
				},
				_ => None,
			}
		},
		_ => None,
	}
}

// TODO -- better names
// given the chosen mapping of premise to instantiation, get the conclusions from the rule; if the rule does not match, returns None
pub fn conclusions_from_premises(instantiation: &HashMap<&Formula, &Formula>, rule: &InferenceRule) -> Option<Vec<Formula>> {
	let mut assignment: Assignment<'_> = HashMap::new();
	for premise in rule.premises.iter() {
		match instantiation.get(premise) {
			Some(p) => {
				match unify(p, premise) {
					Some(m) => assignment = merge_assignments(assignment, m)?, // TODO -- check that this is sound
					None => return None,
				}
			},
			None =>	return None,
		}
	}

	// at this point should have an assignment to all premises in rule
	// next, apply this assignment to each conclusion
	let mut conclusions: Vec<Formula> = Vec::new();
	for conclusion in rule.conclusions.iter() {
		conclusions.push(conclusion.substitute_formula_from_assignment(&assignment));
	}
	Some(conclusions)
}

pub fn make_instantiations<'a>(formulae: &Vec<&'a Formula>, premises: &'a Vec<Formula>) -> Vec<HashMap<&'a Formula, &'a Formula>> {
	let mut instantiations: Vec<HashMap<&'a Formula, &'a Formula>> = Vec::from([HashMap::new()]);
	let mut new_instantiations: Vec<HashMap<&'a Formula, &'a Formula>>;
	for premise in premises.iter() {
		new_instantiations = Vec::new();
		while let Some(instantiation) = instantiations.pop() {
			for formula in formulae.iter() {
				let mut new_inst = instantiation.clone();
				new_inst.insert(premise, formula);
				new_instantiations.push(new_inst);
			}
		}
		instantiations = new_instantiations
	}
	instantiations
}


pub fn apply_rule(formulae: Vec<&Formula>, rule: &InferenceRule) -> Vec<Formula> { // TO DO: admit an optional initialization on the instantiations (eg for OR intro)
	let instantiations = make_instantiations(&formulae, &rule.premises);
	instantiations
		.iter()
		.fold(
			Vec::new(), 
			|mut acc_conclusions: Vec<Formula>, instantiation| match conclusions_from_premises(instantiation, rule) {
				Some(conclusions) => { acc_conclusions.extend(conclusions); acc_conclusions },
				None			  => acc_conclusions,
			} 
		)
}

// TODO -- this is messy. combine with other similar map above
pub fn conclusions_from_premises_with_supplied(instantiation: &HashMap<&Formula, &Formula>, rule: &InferenceRule, supplied: &Assignment<'_>) -> Option<Vec<Formula>> {
	let mut assignment: Assignment<'_> = supplied.clone();
	for premise in rule.premises.iter() {
		match instantiation.get(premise) {
			Some(p) => {
				match unify(p, premise) {
					Some(m) => assignment = merge_assignments(assignment, m)?, // TODO -- check that this is sound
					None => return None,
				}
			},
			None =>	return None,
		}
	}

	// at this point should have an assignment to all premises in rule
	// next, apply this assignment to each conclusion
	let mut conclusions: Vec<Formula> = Vec::new();
	for conclusion in rule.conclusions.iter() {
		conclusions.push(conclusion.substitute_formula_from_assignment(&assignment));
	}
	Some(conclusions)
}

// TODO -- better name!
// supply the rule with some instantiation for free premises
// NOTE: currently this does not check that supplied is compatible with the computed instantiations (TODO -- add a check?)
pub fn apply_rule_with_supplied(formulae: Vec<&Formula>, rule: &InferenceRule, supplied: Assignment<'_>) -> Vec<Formula> {
	let mut instantiations = make_instantiations(&formulae, &rule.premises);
	instantiations
		.iter()
		.fold(
			Vec::new(), 
			|mut acc_conclusions: Vec<Formula>, instantiation| match conclusions_from_premises_with_supplied(instantiation, rule, &supplied) {
				Some(conclusions) => { 
					acc_conclusions.extend(conclusions); 
					acc_conclusions 
				},
				None			  => acc_conclusions,
			} 
		)
}

fn conclusions_from_premises2(instantiation: &HashMap<&Formula, &Formula>, rule: &InferenceRule, supplied: &Option<Assignment<'_>>) -> Option<Vec<Formula>> {
	let mut assignment: Assignment<'_>;
	match supplied {
		Some(supplied_assignment) => assignment = supplied_assignment.clone(),
		None 					  => assignment = HashMap::new(),
	}
	for premise in rule.premises.iter() {
		match instantiation.get(premise) {
			Some(p) => {
				match unify(p, premise) {
					Some(m) => assignment = merge_assignments(assignment, m)?, // TODO -- check that this is sound
					None => return None,
				}
			},
			None =>	return None,
		}
	}

	// at this point should have an assignment to all premises in rule
	// next, apply this assignment to each conclusion
	let mut conclusions: Vec<Formula> = Vec::new();
	for conclusion in rule.conclusions.iter() {
		conclusions.push(conclusion.substitute_formula_from_assignment(&assignment));
	}
	Some(conclusions)
}

// TODO: where do we check that all the free variables in the rule are bound by supplied?
pub fn apply_rule2(formulae: Vec<&Formula>, rule: &InferenceRule, supplied: Option<Assignment<'_>>) -> Vec<Formula> {
	let mut instantiations = make_instantiations(&formulae, &rule.premises);
	instantiations
		.iter()
		.fold(
			Vec::new(), 
			|mut acc_conclusions: Vec<Formula>, instantiation| match conclusions_from_premises2(instantiation, rule, &supplied) {
				Some(conclusions) => { 
					acc_conclusions.extend(conclusions); 
					acc_conclusions 
				},
				None			  => acc_conclusions,
			} 
		)
}

/*impl InferenceRule {
	fn apply_rule(&self, ps: Vec<Formula>) -> Option<Vec<Formula>> {
		// try to match against formulas
		for premise in self.premises {
			if !ps.contains(premise) {
				return None;
			}
		}
		return Some(self.conclusions); // TODO -- need to unify here
	}
}*/

// AndIntro
fn initialize_and_intro(name: String) -> Option<InferenceRule> {
	let premise_1 = String::from("phi");
	let premise_2 = String::from("psi");
	let conclusion = String::from("phi & psi");
	if let (Some(p1), Some(p2), Some(c)) = (read_input(premise_1), read_input(premise_2), read_input(conclusion)) {
		let rule_ai: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p1, p2]), conclusions: Vec::from([c]), free: None };
		Some(rule_ai)
	} else {
		// ERROR
		None
	}
}

// AndElim
fn initialize_and_elim(name: String) -> Option<InferenceRule> {
	let premise = String::from("phi & psi");
	let conclusion_1 = String::from("phi");
	let conclusion_2 = String::from("psi");
	if let (Some(p), Some(c1), Some(c2)) = (read_input(premise), read_input(conclusion_1), read_input(conclusion_2)) {
		let rule_ae: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p]), conclusions: Vec::from([c1, c2]), free: None };
		Some(rule_ae)
	} else {
		// ERROR
		None
	}
}

// OrIntro
fn initialize_or_intro(name: String) -> Option<InferenceRule> {
	let premise = String::from("phi");
	let conclusion = String::from("phi | psi");
	if let (Some(p), Some(c)) = (read_input(premise), read_input(conclusion)) {
		let rule_oi: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p]), conclusions: Vec::from([c]), free: Some(Vec::from([String::from("psi")])) };
		Some(rule_oi)
	} else {
		// ERROR
		None
	}
}

// OrElim
fn initialize_or_elim(name: String) -> Option<InferenceRule> {
	let premise_1 = String::from("phi | psi");
	let premise_2 = String::from("phi => theta");
	let premise_3 = String::from("psi => theta");
	let conclusion = String::from("theta");
	if let (Some(p1), Some(p2), Some(p3), Some(c)) = (read_input(premise_1), read_input(premise_2), read_input(premise_3), read_input(conclusion)) {
		let rule_oe: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p1, p2, p3]), conclusions: Vec::from([c]), free: None };
		Some(rule_oe)
	} else {
		// ERROR
		None
	}
}

// ImpliesElim
fn initialize_implies_elim(name: String) -> Option<InferenceRule> {
	let premise_1 = String::from("phi => psi");
	let premise_2 = String::from("phi");
	let conclusion = String::from("psi");
	if let (Some(p1), Some(p2), Some(c)) = (read_input(premise_1), read_input(premise_2), read_input(conclusion)) {
		let rule_ie: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p1, p2]), conclusions: Vec::from([c]), free: None };
		Some(rule_ie)
	} else {
		// ERROR
		None
	}
}

// XorIntro
fn initialize_xor_intro(name: String) -> Option<InferenceRule> {
	let premise_1 = String::from("phi");
	let premise_2 = String::from("~psi");
	let conclusion = String::from("phi + psi");
	if let (Some(p1), Some(p2), Some(c)) = (read_input(premise_1), read_input(premise_2), read_input(conclusion)) {
		let rule_xi: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p1, p2]), conclusions: Vec::from([c]), free: None };
		Some(rule_xi)
	} else {
		// ERROR
		None
	}
}

// XorElim
fn initialize_xor_elim(name: String) -> Option<InferenceRule> {
	let premise_1 = String::from("phi + psi");
	let premise_2 = String::from("~phi");
	let conclusion = String::from("psi");
	if let (Some(p1), Some(p2), Some(c)) = (read_input(premise_1), read_input(premise_2), read_input(conclusion)) {
		let rule_xe: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p1, p2]), conclusions: Vec::from([c]), free: None };
		Some(rule_xe)
	} else {
		// ERROR
		None
	}
}

// EquivIntro
fn initialize_equiv_intro(name: String) -> Option<InferenceRule> {
	let premise_1 = String::from("phi => psi");
	let premise_2 = String::from("psi => phi");
	let conclusion = String::from("phi <=> psi");
	if let (Some(p1), Some(p2), Some(c)) = (read_input(premise_1), read_input(premise_2), read_input(conclusion)) {
		let rule_bi: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p1, p2]), conclusions: Vec::from([c]), free: None };
		Some(rule_bi)
	} else {
		// ERROR
		None
	}
}

// EquivElim
fn initialize_equiv_elim(name: String) -> Option<InferenceRule> {
	let premise = String::from("phi <=> psi");
	let conclusion_1 = String::from("phi => psi");
	let conclusion_2 = String::from("psi => phi");
	if let (Some(p), Some(c1), Some(c2)) = (read_input(premise), read_input(conclusion_1), read_input(conclusion_2)) {
		let rule_be: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p]), conclusions: Vec::from([c1, c2]), free: None };
		Some(rule_be)
	} else {
		// ERROR
		None
	}
}

// NotIntro
fn initialize_not_intro(name: String) -> Option<InferenceRule> {
	let premise_1 = String::from("phi => psi");
	let premise_2 = String::from("phi => ~psi");
	let conclusion = String::from("~phi");
	if let (Some(p1), Some(p2), Some(c)) = (read_input(premise_1), read_input(premise_2), read_input(conclusion)) {
		let rule_ni: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p1, p2]), conclusions: Vec::from([c]), free: None };
		Some(rule_ni)
	} else {
		// ERROR
		None
	}
}

// NotElim
fn initialize_not_elim(name: String) -> Option<InferenceRule> {
	let premise = String::from("~~phi");
	let conclusion = String::from("phi");
	if let (Some(p), Some(c)) = (read_input(premise), read_input(conclusion)) {
		let rule_ne: InferenceRule = InferenceRule{ name: name, premises: Vec::from([p]), conclusions: Vec::from([c]), free: None };
		Some(rule_ne)
	} else {
		// ERROR
		None
	}
}

pub fn initialize_rules() -> HashMap<String, InferenceRule> {
	let mut rules: HashMap<String, InferenceRule> = HashMap::new();
	// AndIntro

	match initialize_and_intro("AndIntro".to_string()) {
		Some(rule) => { rules.insert("AndIntro".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate AndIntro rule"),
	};

	// AndElim
	match initialize_and_elim("AndElim".to_string()) {
		Some(rule) => { rules.insert("AndElim".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate AndElim rule"),
	};

	// OrIntro [TODO]
	match initialize_or_intro("OrIntro".to_string()) {
		Some(rule) => { rules.insert("OrIntro".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate OrIntro rule"),
	};

	// OrElim
	match initialize_or_elim("OrElim".to_string()) {
		Some(rule) => { rules.insert("OrElim".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate OrElim rule"),
	};

	// ImpliesElim
	match initialize_implies_elim("ImpliesElim".to_string()) {
		Some(rule) => { rules.insert("ImpliesElim".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate ImpliesElim rule"),
	};

	// XorIntro
	match initialize_xor_intro("XorIntro".to_string()) {
		Some(rule) => { rules.insert("XorIntro".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate XorIntro rule"),
	};

	// XorElim
	match initialize_xor_elim("XorElim".to_string()) {
		Some(rule) => { rules.insert("XorElim".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate XorElim rule"),
	};

	// EquivIntro
	match initialize_equiv_intro("EquivIntro".to_string()) {
		Some(rule) => { rules.insert("EquivIntro".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate EquivIntro rule"),
	};

	// EquivElim
	match initialize_equiv_elim("EquivElim".to_string()) {
		Some(rule) => { rules.insert("EquivElim".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate EquivElim rule"),
	};

	// NotIntro
	match initialize_not_intro("NotIntro".to_string()) {
		Some(rule) => { rules.insert("NotIntro".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate NotIntro rule"),
	};

	// NotElim
	match initialize_not_elim("NotElim".to_string()) {
		Some(rule) => { rules.insert("NotElim".to_string(), rule); },
		None => eprintln!("ERROR: failed to generate NotElim rule"),
	};

	rules
}

impl fmt::Display for InferenceRule {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut display_str = String::new();
		let mut max_len = 0;
		for premise in self.premises.iter() {
			let string = premise.to_string();
			display_str.push_str(&format!("{}\n", string));
			if string.len() > max_len {
				max_len = string.len();
			}
		}
		display_str.push_str(&format!("{}    [{}]\n", "-".repeat(max_len), self.name));
		for conclusion in self.conclusions.iter() {
			let string = conclusion.to_string();
			display_str.push_str(&format!("{}\n", string));
		}

		write!(f, "{}", display_str)
	}
}

enum Derivation {
	Rule(String), // contains the name of the rule used; TODO -- add the indices of the proof used as well
	Assumption,
	Premise,
	ImplicationIntro,
}

impl fmt::Display for Derivation {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Derivation::Rule(name) 		 => write!(f, "[{}]", name),
			Derivation::Assumption 		 => write!(f, "[A]"),
			Derivation::Premise 		 => write!(f, "[P]"),
			Derivation::ImplicationIntro => write!(f, "[ImpliesIntro]"),
		}
	}
}

struct ProofStep {
	formula: Formula,
	rule: Derivation,
	depth: usize, // how many assumptions deep we are; start at depth = 0
	index: usize,
}

impl fmt::Display for ProofStep {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut display_str = String::new();
		display_str.push_str(&format!("{}. ", self.index));
		for _ in 0..self.depth {
			display_str.push_str("| ");
		}
		display_str.push_str(&format!("{}    {}\n", self.formula, self.rule));
		write!(f, "{}", display_str)
	}
}

pub struct Proof {
	steps: Vec<ProofStep>,
	bottom_depth: usize,
	bottom_index: usize,
	assumption_stack: Vec<Formula>,
}

impl fmt::Display for Proof {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut display_str = String::new();
		for proof_step in self.steps.iter() {
			display_str.push_str(&format!("{}", proof_step));
		}
		write!(f, "{}", display_str)
	}
}

impl Proof {
	pub fn new() -> Self {
		Proof{ steps: Vec::new(), bottom_depth: 0, bottom_index: 0, assumption_stack: Vec::new() }
	}

	pub fn add_premise(&mut self, premise: Formula) {
		self.bottom_index += 1;
		self.steps.push(ProofStep{ formula: premise, rule: Derivation::Premise, depth: self.bottom_depth, index: self.bottom_index });
	}

	pub fn add_assumption(&mut self, assumption: Formula) {
		self.bottom_depth += 1;
		self.bottom_index += 1;
		self.assumption_stack.push(assumption.clone());
		self.steps.push(ProofStep{ formula: assumption, rule: Derivation::Assumption, depth: self.bottom_depth, index: self.bottom_index });
	}

	pub fn implication_intro(&mut self) { // handle case of only one assumption? eg assume x, derive x=>x
		if self.bottom_depth > 0 {
			self.bottom_depth -= 1;
			if let (Some(assumption), Some(proof_step)) = (self.assumption_stack.pop(), self.steps.last()) { // HERE: don't pop from steps, just get bottom
				let implication: Formula = Formula::Implies(Box::new(assumption), Box::new(proof_step.formula.clone()));
				self.bottom_index += 1;
				self.steps.push(ProofStep{ formula: implication, rule: Derivation::ImplicationIntro, depth: self.bottom_depth, index: self.bottom_index });
			}
			// Error
		}
		// Error
	}

	pub fn infer(&mut self, rule: &InferenceRule, premise_indices: Vec<usize>) {
		let mut premises: Vec<&Formula> = Vec::new();
		for index in premise_indices.iter() {
			if let Some(p) = self.steps.get(*index) { // TODO -- adjust indices to start at 1
				premises.push(&p.formula);
			}
		}
		let mut conclusions: Vec<Formula> = apply_rule(premises, rule);
		while let Some(conclusion) = conclusions.pop() {
			self.bottom_index += 1;
			self.steps.push(ProofStep{ formula: conclusion, rule: Derivation::Rule(rule.name.clone()), depth: self.bottom_depth, index: self.bottom_index });
		}
	}

	//pub fn infer_with_free() // TODO

	pub fn delete_bottom(&mut self) {
		if self.bottom_index > 0 {
			if let Some(proof_step) = self.steps.pop() {
				self.bottom_index -= 1;
				if let Derivation::Assumption = proof_step.rule {
					self.assumption_stack.pop();
					self.bottom_depth -= 1;
				}
			}
			// ERROR
		}
		// ERROR
	}

	pub fn len(&self) -> usize {
		self.steps.len()
	}

	pub fn get_formula_at(&self, index: usize) -> Option<Formula> {
		if let Some(step) = self.steps.get(index) {
			return Some(step.formula.clone());
		}
		None
	}
}