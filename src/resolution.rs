use structures::*;
use std::fmt;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum ClausalFormula {
	Var 	(String),
	Not		(Box<ClausalFormula>),
	And		(Box<ClausalFormula>, Box<ClausalFormula>),
	Or 		(Box<ClausalFormula>, Box<ClausalFormula>),
}

impl fmt::Display for ClausalFormula {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ClausalFormula::Var(x) 	   => write!(f, "{}", x),
			ClausalFormula::Not(y) 	   => write!(f, "~{}", y),
			ClausalFormula::And(y, z) 	   => write!(f, "({} & {})", y, z),
			ClausalFormula::Or(y, z) 	   => write!(f, "({} | {})", y, z),
		}
	}
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Literal {
	pub name: String,
	pub sign: bool, // true for positive (eg. x), false for negated (eg. ~x)
}

// Clausal Form
type Clause = HashSet<Literal>;

pub fn into_clausal(p: &Formula) -> Vec<Clause> {
	let clausal_formula = xor_impl_clausal(p);
	let literal_formula = negation_clausal(&clausal_formula);
	distribution_clausal(&literal_formula)
}

fn xor_impl_clausal(p: &Formula) -> ClausalFormula {
	match p {
		Formula::Var(x) 	   => ClausalFormula::Var(x.name.clone()),
		Formula::Not(y) 	   => ClausalFormula::Not(Box::new(xor_impl_clausal(y))),
		Formula::And(y, z) 	   => ClausalFormula::And(Box::new(xor_impl_clausal(y)), Box::new(xor_impl_clausal(z))),
		Formula::Or(y, z) 	   => ClausalFormula::Or(Box::new(xor_impl_clausal(y)), Box::new(xor_impl_clausal(z))),
		Formula::Xor(y, z) 	   => {
			// phi + psi ----> (phi | psi) & ~(phi & psi)
			ClausalFormula::And(
				Box::new(ClausalFormula::Or(
					Box::new(xor_impl_clausal(y)), 
					Box::new(xor_impl_clausal(z))
				)),
				Box::new(ClausalFormula::Not(
					Box::new(ClausalFormula::And(
						Box::new(xor_impl_clausal(y)), 
						Box::new(xor_impl_clausal(z))
					))
				))
			)
		},
		Formula::Implies(y, z) => {
			// phi => psi ----> ~phi | psi
			ClausalFormula::Or(Box::new(ClausalFormula::Not(Box::new(xor_impl_clausal(y)))), Box::new(xor_impl_clausal(z)))
		},
		Formula::Equiv(y, z)   => {
			// phi <=> psi ----> (~phi | psi) & (~psi | phi)
			let y_implies_z = Formula::Implies(y.clone(), z.clone());
			let z_implies_y = Formula::Implies(z.clone(), y.clone());
			ClausalFormula::And(Box::new(xor_impl_clausal(&y_implies_z)), Box::new(xor_impl_clausal(&z_implies_y)))
		},
	}
}

fn negation_clausal(p: &ClausalFormula) -> ClausalFormula {
	negation_clausal_rec(p, false)
}

fn negation_clausal_rec(p: &ClausalFormula, negation_parity: bool) -> ClausalFormula {
	// negation_count is the parity of negations encountered up to this point
	// true => odd number of negations
	// false => even number of negations

	match p {
		ClausalFormula::Var(x) => {
			if negation_parity {
				ClausalFormula::Not(Box::new(p.clone()))
			} else {
				p.clone()
			}
		},
		ClausalFormula::Not(y) => {
			negation_clausal_rec(y, !negation_parity)
		},
		ClausalFormula::And(y, z) => {
			if negation_parity {
				ClausalFormula::Or(Box::new(negation_clausal_rec(y, negation_parity)), Box::new(negation_clausal_rec(z, negation_parity)))
			} else {
				ClausalFormula::And(Box::new(negation_clausal_rec(y, negation_parity)), Box::new(negation_clausal_rec(z, negation_parity)))
			}
		},
		ClausalFormula::Or(y, z) => {
			if negation_parity {
				ClausalFormula::And(Box::new(negation_clausal_rec(y, negation_parity)), Box::new(negation_clausal_rec(z, negation_parity)))
			} else {
				ClausalFormula::Or(Box::new(negation_clausal_rec(y, negation_parity)), Box::new(negation_clausal_rec(z, negation_parity)))
			}
		},
	}
}

fn distribution_clausal(p: &ClausalFormula) -> Vec<Clause> {
	// phi | (psi & theta) ----> {phi, psi}, {phi, theta}
	// (psi & theta) | phi ----> {psi, phi}, {theta, phi}
	// at this point Var and ~Var should be fully inside, so these are both base cases

	match p {
		ClausalFormula::Var(x) => Vec::from([HashSet::from([Literal{ name: x.clone(), sign: true }])]),
		ClausalFormula::Not(y) => {
			let clauses = distribution_clausal(y);
			let mut negated_clauses = Vec::new();
			for clause in clauses.iter() {
				let mut negated_clause = HashSet::new();
				for literal in clause.iter() {
					negated_clause.insert(Literal{ name: literal.name.clone(), sign: !literal.sign });
				}
				negated_clauses.push(negated_clause);
			}
			negated_clauses
		},
		// And(y, z) ---> clauses(y) ++ clauses(z)
		ClausalFormula::And(y, z) => {
			let mut clauses = distribution_clausal(y);
			clauses.extend(distribution_clausal(z));
			clauses
		},
		ClausalFormula::Or(y, z) => {
			let clauses_y = distribution_clausal(y);
			let clauses_z = distribution_clausal(z);
			let mut clauses = Vec::new();
			for clause_y in clauses_y.iter() {
				for clause_z in clauses_z.iter() {
					let mut clause_pair = clause_y.clone();
					clause_pair.extend(clause_z.clone());
					clauses.push(clause_pair);
				}
			}
			clauses
		},
	}
}

// returns all available resolutions from the provided clauses
// {p, q}, {~p, ~q} => {p, ~p}, {q, ~q}
pub fn resolve(r: &Clause, s: &Clause) -> Vec<Clause> {
	let mut resolved_clauses = Vec::new();
	for literal_r in r.iter() {
		for literal_s in s.iter() {
			if literal_r.name == literal_s.name && literal_r.sign ^ literal_s.sign {
				let mut other_r: Clause = r.iter().filter(|&literal| literal != literal_r).cloned().collect();
				let other_s: Clause = s.iter().filter(|&literal| literal != literal_s).cloned().collect(); 
				other_r.extend(other_s);
				resolved_clauses.push(other_r);
			}
		}
	}
	resolved_clauses
}

// Returns true if r subsumes (eg. is a subset of) s
fn subsumes(r: &Clause, s: &Clause) -> bool {
	r.is_subset(s)
}


// ICE: check if clause already in proof before adding it
// TE: check if clause is tautology before adding it

// TODO -- also implement subsumotion elim, pure literal elim
// TODO -- look into unit restriction, input restriction, linear restriction, set of support restriction

pub fn resolve_all(clauses: &Vec<Clause>) -> Vec<Clause> {
	let mut resolved_clauses = clauses.clone();
	let mut forward_finger = 0;
	let mut catchup_finger = 0;
	while let (Some(r), Some(s)) = (resolved_clauses.get(forward_finger), resolved_clauses.get(catchup_finger)) { // TODO -- is this the right stopping condition?
		let new_clauses = resolve(r, s);
		resolved_clauses.extend(new_clauses);
		if forward_finger == catchup_finger {
			forward_finger += 1;
			catchup_finger = 0;
		} else {
			catchup_finger += 1;
		}
	}
	resolved_clauses
}

pub fn resolve_towards(clauses: &Vec<Clause>, target: &Clause) -> Vec<Clause> {
	let mut resolved_clauses = clauses.clone();
	if resolved_clauses.contains(target) {
		return resolved_clauses;
	}
	let mut forward_finger = 0;
	let mut catchup_finger = 0;
	while let (Some(r), Some(s)) = (resolved_clauses.get(forward_finger), resolved_clauses.get(catchup_finger)) {
		let mut new_clauses = resolve(r, s);
		while let Some(new_clause) = new_clauses.pop() {
			let mut end = false;
			if new_clause == *target {
				end = true;
			}
			resolved_clauses.push(new_clause);
			if end {
				return resolved_clauses;
			}
		}
		resolved_clauses.extend(new_clauses);
		if forward_finger == catchup_finger {
			forward_finger += 1;
			catchup_finger = 0;
		} else {
			catchup_finger += 1;
		}
	}
	resolved_clauses
}

pub fn resolve_with<F>(clauses: &Vec<Clause>, target: &Clause, eliminator: F) -> Vec<Clause> 
	where F: Fn(&Clause, &Vec<Clause>) -> bool // should return true when element should be kept
{
	let mut resolved_clauses = clauses.clone();
	if resolved_clauses.contains(target) {
		return resolved_clauses;
	}
	let mut forward_finger = 0;
	let mut catchup_finger = 0;
	while let (Some(r), Some(s)) = (resolved_clauses.get(forward_finger), resolved_clauses.get(catchup_finger)) {
		let mut new_clauses = resolve(r, s);
		while let Some(new_clause) = new_clauses.pop() {
			let mut end = false;
			if new_clause == *target {
				end = true;
			}
			if eliminator(&new_clause, &resolved_clauses) {
				resolved_clauses.push(new_clause);
			}
			if end {
				return resolved_clauses;
			}
		}
		resolved_clauses.extend(new_clauses);
		if forward_finger == catchup_finger {
			forward_finger += 1;
			catchup_finger = 0;
		} else {
			catchup_finger += 1;
		}
	}
	resolved_clauses
}

pub fn identical_clause_eliminator(new_clause: &Clause, clauses: &Vec<Clause>) -> bool {
	/*println!("====");
	for literal in new_clause.iter() {
		println!(">> {}, {}", literal.name, literal.sign);
	}
	println!("verdict: {}", clauses.contains(new_clause));*/
	!clauses.contains(new_clause)
}

pub fn tautology_eliminator(new_clause: &Clause, clauses: &Vec<Clause>) -> bool {
	for literal in new_clause.iter() {
		for other in new_clause.iter() {
			if other.name == literal.name && other.sign == !literal.sign {
				return false;
			}
		}
	}
	true
}