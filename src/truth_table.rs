use structures::*;
use std::collections::HashSet;

fn extract_variables<'a>(p: &'a Formula) -> HashSet<&'a Variable> {
	let mut vars: HashSet<&Variable> = HashSet::new();
	extract_variables_rec(p, &vars);
	vars
}

fn extract_variables_rec<'a>(p: &Formula, mut vars: &'a HashSet<&Variable>) -> &'a HashSet<&Variable> {
	match p {
		Formula::Var(x) => vars.insert(&x),
		Formula::Not(q) => vars.extend(extract_variables(q, vars)),
		Formula::And(q, r) => { vars.extend(extract_variables(q, vars)); vars.extend(extract_variables(r, vars)) },
		Formula::Or(q, r) => { vars.extend(extract_variables(q, vars)); vars.extend(extract_variables(r, vars)) },
		Formula::Implies(q, r) => { vars.extend(extract_variables(q, vars)); vars.extend(extract_variables(r, vars)) },
		Formula::Equiv(q, r) => { vars.extend(extract_variables(q, vars)); vars.extend(extract_variables(r, vars)) },
		Formula::Xor(q, r) => { vars.extend(extract_variables(q, vars)); vars.extend(extract_variables(r, vars)) },
	}
}

/*fn find_truth_tables(p: &Formula) -> HashSet<Model<'a>> {
	// TODO
}*/