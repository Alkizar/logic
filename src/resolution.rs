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
	name: String,
	sign: bool, // true for positive (eg. x), false for negated (eg. ~x)
}

// Clausal Form
type Clause = Vec<Literal>;

//fn into_clausal(p: &Formula) -> Vec<Clause> {
//
//}

pub fn xor_impl_clausal(p: &Formula) -> ClausalFormula {
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

pub fn negation_clausal(p: &ClausalFormula) -> ClausalFormula {
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

// TODO -- need to flatten ands and ors? should not have or(or(and(_, _), _), _)
// instead should have or(and(_, _), _, _)

pub fn distribution_clausal(p: &ClausalFormula) -> Vec<Clause> {
	// phi | (psi & theta) ----> {phi, psi}, {phi, theta}
	// (psi & theta) | phi ----> {psi, phi}, {theta, phi}
	// at this point Var and ~Var should be fully inside, so these are both base cases

	match p {
		ClausalFormula::Var(x) => Vec::from([Vec::from([Literal{ name: x.clone(), sign: true }])]),
		ClausalFormula::Not(y) => {
			let mut clauses = distribution_clausal(y);
			for clause in clauses.iter_mut() {
				for literal in clause.iter_mut() {
					literal.sign = !literal.sign;
				}
			}
			clauses
		},
		// And(y, z) ---> clauses(y) ++ clauses(z)
		// Or(y, z) ---> (clause_y, clause_z) : clause_y in clauses(y), clause_z in clauses(z)
		/*ClausalFormula::And(y, z) => {
			let clauses_y = distribution_clausal(y);
			let clauses_z = distribution_clausal(z);
			let clauses = Vec::new();
			for 
		}*/
		_ => Vec::new(),
	}
}