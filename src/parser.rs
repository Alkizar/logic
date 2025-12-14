//use std::vec;
use std::fmt;
use std::collections::HashMap;
use structures::*;

// TODO -- revoke pub
#[derive(PartialEq, Eq)]
pub enum Token {
	Var(String), // TODO -- need to resolve variables into symbol table so that all variables with the same name map to a single object
				 // (lexing should also create a map from variable names to variable objects; before emitting a variable, check if it's already present in the map)
	Not,
	And,
	Or,
	Implies,
	Equiv,
	Xor,
	LeftParen,
	RightParen,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Token::Var(name) 	=> write!(f, "Var({})", name),
			Token::Not 			=> write!(f, "Not"),
			Token::And 			=> write!(f, "And"),
			Token::Or 			=> write!(f, "Or"),
			Token::Implies 		=> write!(f, "Implies"),
			Token::Equiv 		=> write!(f, "Equiv"),
			Token::Xor 			=> write!(f, "Xor"),
			Token::LeftParen 	=> write!(f, "LeftParen"),
			Token::RightParen 	=> write!(f, "RightParen"),
		}
	}
}

pub fn lex(string: &str) -> Option<Vec<Token>> {
	let toks: Vec<Token> = Vec::new();
	next_waiting(string, toks)
}

fn lex_error(c: char, string: &str, state: &str) {
	println!("lexing error: unexpected character {} in {} while in state {}", c, string, state);
}

// NOTE: vec.push can panic; TODO: make this safer
fn next_waiting(string: &str, mut toks: Vec<Token>) -> Option<Vec<Token>> {
	let next: Option<char> = string.chars().next();
	match next {
		Some(c) => {
			match c {
				c if c.is_whitespace() 	=> next_waiting(&string[1..], toks), 									// whitespace: consume and call next_waiting
				'a'..='z'				=> next_variable(&string, toks, String::new()), 						// start of variable: call next_variable
				'='						=> next_implies(&string[1..], toks), 									// start of implies: consume and call next_implies
				'<'						=> next_equiv(&string[1..], toks, '<'),   								// start of equiv: consume and call next_equiv
				'~'						=> { toks.push(Token::Not); next_waiting(&string[1..], toks) }, 		// not: consume, emit Not token, call next_waiting 
				'&'						=> { toks.push(Token::And); next_waiting(&string[1..], toks) }, 		// and: consume, emit And token, call next_waiting
				'|'						=> { toks.push(Token::Or); next_waiting(&string[1..], toks) }, 			// or: consume, emit Or token, call next_waiting
				'+'						=> { toks.push(Token::Xor); next_waiting(&string[1..], toks) }, 		// xor: consume, emit Xor token, call next_waiting
				'('						=> { toks.push(Token::LeftParen); next_waiting(&string[1..], toks) }, 	// left paren: consume, emit LeftParen token, call next_waiting
				')'						=> { toks.push(Token::RightParen); next_waiting(&string[1..], toks) }, 	// right paren: consume, emit RightParen token, call next_waiting
				_ 						=> { lex_error(c, string, "waiting"); None }, 							// lexing error
			}
		},
		None 	=> Some(toks)	// no characters left: lexing complete
	}
}

fn next_variable(string: &str, mut toks: Vec<Token>, mut name: String) -> Option<Vec<Token>> {
	let next: Option<char> = string.chars().next();
	match next {
		Some(c) => {
			match c {
				'a'..='z' 	=> { name.push(c); next_variable(&string[1..], toks, name) },	// part of variable: consume, append to variable, call next_variable
				_ 			=> { toks.push(Token::Var(name)); next_waiting(string, toks) },	// end of variable: emit complete variable, call next_waiting
			}
		},
		None 	=> { toks.push(Token::Var(name)); next_waiting(string, toks) },	// no characters left: emit complete variable, call next_waiting
	}
	// get next character, c, of string
	// if no characters remain, emit completed variable to toks and return success
	// if lowercase letter, consume, append to current variable, and call next_variable
	// else emit completed variable to toks, and call next_waiting
}

fn next_implies(string: &str, mut toks: Vec<Token>) -> Option<Vec<Token>> {
	let next: Option<char> = string.chars().next();
	match next {
		Some(c) => {
			match c {
				'>' => { toks.push(Token::Implies); next_waiting(&string[1..], toks) },	// end of implies: consume, emit Implies token, call next_waiting
				_ 	=> { lex_error(c, string, "implies"); None },						// lexing error
			}
		},
		None 	=> { lex_error('=', string, "implies"); None },	// lexing error
	}
}

fn next_equiv(string: &str, mut toks: Vec<Token>, prev: char) -> Option<Vec<Token>> {
	let next: Option<char> = string.chars().next();
	match next {
		Some(c) => {
			match (prev, c) {
				('<', '=') => { next_equiv(&string[1..], toks, '=') },							// incomplete equiv: consume, call next_equiv
				('=', '>') => { toks.push(Token::Equiv); next_waiting(&string[1..], toks) },	// end of equiv: consume, emit Equiv token, call next_waiting
				_ 	=> { lex_error(c, string, "equiv"); None },									// lexing error
			}
		},
		None 	=> { lex_error(prev, string, "equiv"); None },	// lexing error
	}
}

// TODO: LL(1) parser (can we get away with top-down? if not, do we need LR(1)?)

// TODO -- convert to a formula by traversing; build sym table along the way
struct AST {
	node: Token,
	children: Vec<AST>,
}

impl AST {
	fn new(n: Token, cs: Option<Vec<AST>>) -> Self {
		match cs {
			Some(xs) 	=> Self { node:n, children:xs },
			None 		=> Self { node:n, children:Vec::new() },
		}
	}

	fn insert_child(&mut self, child: AST) {
		self.children.push(child);
	}

	fn to_formula<'a>(&self) -> Option<Formula2> { 
		match &self.node {
			Token::Var(x) => {
				return Some(Formula2::Var(Variable{ name: x.clone() }));
			},
			Token::Not => {  
				match self.children.first() {
					Some(child) => {
						match child.to_formula() {
							Some(p) => return Some(Formula2::Not(Box::new(p))),
							None => return None,
						}
					},
					None => return None,
				}
			},
			Token::And => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => return Some(Formula2::And(Box::new(p), Box::new(q))),
						_ => return None,
					}
				} else {
					return None;
				}
			},
			Token::Or => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => return Some(Formula2::Or(Box::new(p), Box::new(q))),
						_ => return None,
					}
				} else {
					return None;
				}
			},
			Token::Implies => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => return Some(Formula2::Implies(Box::new(p), Box::new(q))),
						_ => return None,
					}
				} else {
					return None;
				}
			},
			Token::Equiv => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => return Some(Formula2::Equiv(Box::new(p), Box::new(q))),
						_ => return None,
					}
				} else {
					return None;
				}
			},
			Token::Xor => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => return Some(Formula2::Xor(Box::new(p), Box::new(q))),
						_ => return None,
					}
				} else {
					return None;
				}
			},
			_ => return None, // ERROR
		}
	}
}

// E -> LeftParen E RightParen | Not E | E And E | E Or E | E Implies E | E Equiv E | E Xor E | Var(x)
//
// Left Factored:
// E -> T X
// X -> op E | eps
// T -> (E) | ~E | Var

// With semantic markers
// E -> T X
// X -> op E *3 | eps
// T -> (E) | ~E *2 | Var
// when * is consumed by the parser, merge that many elements at the top of the semantic stack
// *3 pops three elements, *2 pops too, etc

//parse table Q:
// Q(E, Var) = T X
// Q(E, ~) = T X
// Q(E, () = T X
// Q(T, Var) = Var
// Q(T, ~) = ~E
// Q(T, () = (E)
// Q(X, )) = eps
// Q(X, op) = op E
// Q(X, $) = eps

/*enum ParseToken {
	Terminal(Token),
	E,
	T,
	X,
	End,
	Marker(usize),
}*/

enum ParseToken {
	Terminal(Token),
	E,
	eE,
	eE_,
	iE,
	iE_,
	xE,
	xE_,
	oE,
	oE_,
	aE,
	aE_,
	nE,
	X,
	End,
	Marker(usize),
}

fn process_lex_stream(input: Vec<Token>) -> Vec<ParseToken> {
	let mut output: Vec<ParseToken> = Vec::from([ParseToken::End]);
	for tok in input.into_iter().rev() {
		output.push(ParseToken::Terminal(tok));
	}
	return output;
}


pub fn parse(input: Vec<Token>) -> Option<AST> {
	let parse_token_input: Vec<ParseToken> = process_lex_stream(input);
	let stack: Vec<ParseToken> = Vec::from([ParseToken::End, ParseToken::E]);
	let sem_stack: Vec<AST> = Vec::new();
	match parse_LL1(stack, parse_token_input, sem_stack) {
		Some(mut xs) => return xs.pop(),
		None => { println!("PARSE ERROR: LL(1) failed."); return None; },
	}
}

/*fn parse_table(nonterminal: ParseToken, terminal: &ParseToken) -> Option<Vec<ParseToken>> {
	match nonterminal {
		ParseToken::E => match terminal {
			ParseToken::Terminal(t) 	=> match t {
				Token::Var(x) 		=> Some(Vec::from([ParseToken::X, ParseToken::T])),
				Token::Not 			=> Some(Vec::from([ParseToken::X, ParseToken::T])),
				Token::LeftParen 	=> Some(Vec::from([ParseToken::X, ParseToken::T])),
				_ 					=> None,
			},
			_ 							=> None,
		},
		ParseToken::T => match terminal {
			ParseToken::Terminal(t) 	=> match t {
				Token::Var(x) 		=> Some(Vec::from([ParseToken::Terminal(Token::Var(x.clone()))])),
				Token::Not 			=> Some(Vec::from([ParseToken::Marker(2), ParseToken::E, ParseToken::Terminal(Token::Not)])),
				Token::LeftParen	=> Some(Vec::from([ParseToken::Terminal(Token::RightParen), ParseToken::E, ParseToken::Terminal(Token::LeftParen)])),
				_ 					=> None,
			},
			_ 							=> None,
		},
		ParseToken::X => match terminal {
			ParseToken::Terminal(t) 	=> match t {
				Token::RightParen 	=> Some(Vec::new()),
				Token::And			=> Some(Vec::from([ParseToken::Marker(3), ParseToken::E, ParseToken::Terminal(Token::And)])),
				Token::Or			=> Some(Vec::from([ParseToken::Marker(3), ParseToken::E, ParseToken::Terminal(Token::Or)])),
				Token::Implies		=> Some(Vec::from([ParseToken::Marker(3), ParseToken::E, ParseToken::Terminal(Token::Implies)])),
				Token::Equiv		=> Some(Vec::from([ParseToken::Marker(3), ParseToken::E, ParseToken::Terminal(Token::Equiv)])),
				Token::Xor			=> Some(Vec::from([ParseToken::Marker(3), ParseToken::E, ParseToken::Terminal(Token::Xor)])),
				_ 					=> None,
			},
			ParseToken::End 			=> Some(Vec::new()),
			_ 							=> None,
		},
		_ 			=> None,
	}
}*/

/*
         |    (    |    )    |   <2>   |   =>    |   VAR   |    +    |    ~    |    &    |   <3>   |   <=>   |    |    |    $    |
---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   aE'   |         |eps      |         |eps      |         |eps      |         |&aE<3>   |eps      |eps      |eps      |eps      |
   eE    |iEeE'    |         |         |         |iEeE'    |         |iEeE'    |         |         |         |         |         |
   eE'   |         |eps      |         |         |         |         |         |         |eps      |<=>eE<3> |         |eps      |
   nE    |X        |         |         |         |X        |         |~nE<2>   |         |         |         |         |         |
    X    |(E)      |         |         |         |VAR      |         |         |         |         |         |         |         |
    E    |eE       |         |         |         |eE       |         |eE       |         |         |         |         |         |
   xE    |oExE'    |         |         |         |oExE'    |         |oExE'    |         |         |         |         |         |
   oE    |aEoE'    |         |         |         |aEoE'    |         |aEoE'    |         |         |         |         |         |
   iE    |xEiE'    |         |         |         |xEiE'    |         |xEiE'    |         |         |         |         |         |
   iE'   |         |eps      |         |=>iE<3>  |         |         |         |         |eps      |eps      |         |eps      |
   xE'   |         |eps      |         |eps      |         |+xE<3>   |         |         |eps      |eps      |         |eps      |
   oE'   |         |eps      |         |eps      |         |eps      |         |         |eps      |eps      ||oE<3>   |eps      |
   aE    |nEaE'    |         |         |         |nEaE'    |         |nEaE'    |         |         |         |         |         |
*/

fn parse_table(nonterminal: ParseToken, terminal: &ParseToken) -> Option<Vec<ParseToken>> {
	match (nonterminal, terminal) {
		// E
		(ParseToken::E, ParseToken::Terminal(Token::LeftParen)) => Some(Vec::from([ParseToken::eE])),
		(ParseToken::E, ParseToken::Terminal(Token::Not)) 		=> Some(Vec::from([ParseToken::eE])),
		(ParseToken::E, ParseToken::Terminal(Token::Var(_))) 	=> Some(Vec::from([ParseToken::eE])),
		// eE
		(ParseToken::eE, ParseToken::Terminal(Token::LeftParen)) => Some(Vec::from([ParseToken::eE_, ParseToken::iE])),
		(ParseToken::eE, ParseToken::Terminal(Token::Not)) 		 => Some(Vec::from([ParseToken::eE_, ParseToken::iE])),
		(ParseToken::eE, ParseToken::Terminal(Token::Var(_))) 	 => Some(Vec::from([ParseToken::eE_, ParseToken::iE])),
		// eE'
		(ParseToken::eE_, ParseToken::Terminal(Token::RightParen)) => Some(Vec::new()),
		(ParseToken::eE_, ParseToken::End) 						   => Some(Vec::new()),
		(ParseToken::eE_, ParseToken::Terminal(Token::Equiv))	   => Some(Vec::from([ParseToken::Marker(3), ParseToken::eE, ParseToken::Terminal(Token::Equiv)])),
		// iE
		(ParseToken::iE, ParseToken::Terminal(Token::LeftParen)) => Some(Vec::from([ParseToken::iE_, ParseToken::xE])),
		(ParseToken::iE, ParseToken::Terminal(Token::Not)) 		 => Some(Vec::from([ParseToken::iE_, ParseToken::xE])),
		(ParseToken::iE, ParseToken::Terminal(Token::Var(_))) 	 => Some(Vec::from([ParseToken::iE_, ParseToken::xE])),
		// iE'
		(ParseToken::iE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::iE_, ParseToken::Terminal(Token::Equiv)) 		=> Some(Vec::new()),
		(ParseToken::iE_, ParseToken::End) 							=> Some(Vec::new()),
		(ParseToken::iE_, ParseToken::Terminal(Token::Implies)) 	=> Some(Vec::from([ParseToken::Marker(3), ParseToken::iE, ParseToken::Terminal(Token::Implies)])),
		// xE
		(ParseToken::xE, ParseToken::Terminal(Token::LeftParen)) => Some(Vec::from([ParseToken::xE_, ParseToken::oE])),
		(ParseToken::xE, ParseToken::Terminal(Token::Not)) 		 => Some(Vec::from([ParseToken::xE_, ParseToken::oE])),
		(ParseToken::xE, ParseToken::Terminal(Token::Var(_))) 	 => Some(Vec::from([ParseToken::xE_, ParseToken::oE])),
		// xE'
		(ParseToken::xE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::xE_, ParseToken::Terminal(Token::Equiv)) 		=> Some(Vec::new()),
		(ParseToken::xE_, ParseToken::Terminal(Token::Implies)) 	=> Some(Vec::new()),
		(ParseToken::xE_, ParseToken::End) 							=> Some(Vec::new()),
		(ParseToken::xE_, ParseToken::Terminal(Token::Xor)) 		=> Some(Vec::from([ParseToken::Marker(3), ParseToken::xE, ParseToken::Terminal(Token::Xor)])),
		// oE
		(ParseToken::oE, ParseToken::Terminal(Token::LeftParen)) => Some(Vec::from([ParseToken::oE_, ParseToken::aE])),
		(ParseToken::oE, ParseToken::Terminal(Token::Not)) 		 => Some(Vec::from([ParseToken::oE_, ParseToken::aE])),
		(ParseToken::oE, ParseToken::Terminal(Token::Var(_))) 	 => Some(Vec::from([ParseToken::oE_, ParseToken::aE])),
		// oE'
		(ParseToken::oE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::oE_, ParseToken::Terminal(Token::Equiv)) 		=> Some(Vec::new()),
		(ParseToken::oE_, ParseToken::Terminal(Token::Implies)) 	=> Some(Vec::new()),
		(ParseToken::oE_, ParseToken::Terminal(Token::Xor)) 		=> Some(Vec::new()),
		(ParseToken::oE_, ParseToken::End) 							=> Some(Vec::new()),
		(ParseToken::oE_, ParseToken::Terminal(Token::Or)) 		=> Some(Vec::from([ParseToken::Marker(3), ParseToken::oE, ParseToken::Terminal(Token::Or)])),
		// aE
		(ParseToken::aE, ParseToken::Terminal(Token::LeftParen)) => Some(Vec::from([ParseToken::aE_, ParseToken::nE])),
		(ParseToken::aE, ParseToken::Terminal(Token::Not)) 		 => Some(Vec::from([ParseToken::aE_, ParseToken::nE])),
		(ParseToken::aE, ParseToken::Terminal(Token::Var(_))) 	 => Some(Vec::from([ParseToken::aE_, ParseToken::nE])),
		// aE'
		(ParseToken::aE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::aE_, ParseToken::Terminal(Token::Equiv)) 		=> Some(Vec::new()),
		(ParseToken::aE_, ParseToken::Terminal(Token::Implies)) 	=> Some(Vec::new()),
		(ParseToken::aE_, ParseToken::Terminal(Token::Xor)) 		=> Some(Vec::new()),
		(ParseToken::aE_, ParseToken::Terminal(Token::Or)) 			=> Some(Vec::new()),
		(ParseToken::aE_, ParseToken::End) 							=> Some(Vec::new()),
		(ParseToken::aE_, ParseToken::Terminal(Token::And)) 		=> Some(Vec::from([ParseToken::Marker(3), ParseToken::aE, ParseToken::Terminal(Token::And)])),
		// nE
		(ParseToken::nE, ParseToken::Terminal(Token::LeftParen)) 	=> Some(Vec::from([ParseToken::X])),
		(ParseToken::nE, ParseToken::Terminal(Token::Var(_))) 		=> Some(Vec::from([ParseToken::X])),
		(ParseToken::nE, ParseToken::Terminal(Token::Not)) 			=> Some(Vec::from([ParseToken::Marker(2), ParseToken::nE, ParseToken::Terminal(Token::Not)])),
		// X
		(ParseToken::X, ParseToken::Terminal(Token::LeftParen)) 	=> Some(Vec::from([ParseToken::Terminal(Token::RightParen), ParseToken::E, ParseToken::Terminal(Token::LeftParen)])),
		(ParseToken::X, ParseToken::Terminal(Token::Var(x))) 		=> Some(Vec::from([ParseToken::Terminal(Token::Var(x.clone()))])),
		// Else
		_ => None,
	}
}


// semantic stack: push incomplete nodes to the stack
// invariant: sem_stack will always contain <= 3 ASTs
// when ) or $ consumed, if stack has <=1 AST then leave it alone, if 3 then pop them and make top and bottom children of middle, if 2 then make top the child of bottom
// The first case will correspond to a single expression in the tree; the second is a binary op, flanked by its args; the third is a unary op

// TODO your stack discipline above doesn't work; consider (x + y) + (y + z)
// Update: fixed, TODO - fix documentation and overall code appearance; also TODO: fix Formula2 -> Formula, and change the structure of models to use variable name
// instead of the variable object itself
// TODO -- better error detection in lex/parse -- report where something breaks

// JUST KIDDING DISCIPLINE IS STILL BUSTED: eg consider ~(~x) --- hint: this does not use the first case lmao
// need to look at the second to last symbol to determine if it is unary or binary when calling evolve
/*fn evolve_sem_stack(mut sem_stack: Vec<AST>) -> Vec<AST> {
	/*if sem_stack.len() == 2 {
		if let Some(mut unary) = sem_stack.pop() {
			if let Some(arg) = sem_stack.pop() {
				unary.insert_child(arg);
				sem_stack.push(unary);
			}
		}
	} else if sem_stack.len() >= 3 {
		if let Some(arg2) = sem_stack.pop() {
			if let Some(mut binary) = sem_stack.pop() {
				if let Some(arg1) = sem_stack.pop() {
					binary.insert_child(arg1);
					binary.insert_child(arg2);
					sem_stack.push(binary);
				}
			}
		}
	}
	println!("!! SIZE OF SEM STACK {} !!", sem_stack.len());
	return sem_stack;*/
	if sem_stack.len() >= 2 {
		if let (Some(arg1), Some(mut op)) = (sem_stack.pop(), sem_stack.pop()) {
			match op.node {
				Token::Not => { op.insert_child(arg1); sem_stack.push(op); },
				Token::Var(_) | Token::LeftParen | Token::RightParen => { println!("PARSE ERROR: INVALID ENTRY IN SEMANTIC STACK") }, // TODO error
				_ => { 
						if let Some(arg0) = sem_stack.pop() {
							op.insert_child(arg0);
							op.insert_child(arg1);
							sem_stack.push(op);
						} 
				},
			}
		}
	}
	return sem_stack;
}*/

fn evolve_sem_stack(mut sem_stack: Vec<AST>, n: usize) -> Vec<AST>{
	if sem_stack.len() >= n && n > 0 {
		let mut xs: Vec<AST> = sem_stack.split_off(sem_stack.len() - n);
		let mut op: AST = {
			if n == 3 {
				// infix (binary) op
				xs.remove(1)
			} else {
				// prefix op
				xs.remove(0)
			}
		};
		xs.reverse();
		for _ in 0..n-1 {
			if let Some(x) = xs.pop() {
				op.insert_child(x); // TODO this reverses order??
			}
		}
		sem_stack.push(op);
	}  else {
		println!("PARSE WARNING: insufficient semantic stack for marker {}", n);
	}
	return sem_stack;
}

fn push_sem_stack(terminal: Token, mut sem_stack: Vec<AST>) -> Vec<AST> {
	println!("!! SIZE OF SEM STACK {} !!", sem_stack.len());
	match terminal {
		Token::LeftParen  => return sem_stack,
		Token::RightParen  => return sem_stack,
		//Token::RightParen => return evolve_sem_stack(sem_stack), // TODO
		_ 				  => { sem_stack.push(AST::new(terminal, None)); return sem_stack; },
	}
}

fn parse_LL1(mut stack: Vec<ParseToken>, mut input: Vec<ParseToken>, mut sem_stack: Vec<AST>) -> Option<Vec<AST>> {
	/*print!("STACK: ");
	for v in stack.iter().rev() {
		match v {
			ParseToken::Terminal(t) => print!("{} ", t),
			ParseToken::E => print!("E "),
			ParseToken::X => print!("X "),
			ParseToken::T => print!("T "),
			ParseToken::End => print!("$ "),
			ParseToken::Marker(n) => print!("<{}> ", n),
		}
	}
	print!("\n\nINPUT: ");
	for v in input.iter().rev() {
		match v {
			ParseToken::Terminal(t) => print!("{} ", t),
			ParseToken::E => print!("E "),
			ParseToken::X => print!("X "),
			ParseToken::T => print!("T "),
			ParseToken::End => print!("$ "),
			ParseToken::Marker(n) => print!("<{}> ", n),
		}
	}

	print!("\n\nSEM STACK:\n");
	for v in sem_stack.iter() {
		print!("  AST: {} [", v.node);
		for child in v.children.iter() {
			print!("{}, ", child.node);
		}
		print!("]\n");
	}
	print!("\n==========\n");*/

	match (stack.pop(), input.last()) {
		(Some(v), Some(x)) 		=> {
			match v {
				ParseToken::Terminal(t) => {
					// Terminal
					match x {
						ParseToken::Terminal(tx) => {
							if t == *tx {
								// Consume input token and perform required action
								input.pop();
								return parse_LL1(stack, input, push_sem_stack(t, sem_stack));
							} else {
								return None;
							}
						},
						_ => return None,
					}
				},
				ParseToken::End 		=> {
					// End of stack
					match x {
						ParseToken::End => {
							/*while (sem_stack.len() > 1) {
								sem_stack = evolve_sem_stack(sem_stack);
							}*/ // TODO
							return Some(sem_stack);
						},
						_ 	=> return None,
					}
				},
				ParseToken::Marker(n)	=> {
					// Semantic marker; evolve semantic stack
					sem_stack = evolve_sem_stack(sem_stack, n);
					return parse_LL1(stack, input, sem_stack);
				},
				_ 			=> {
					// Production
					match parse_table(v, x) {
						Some(production) 	=> {
							for tok in production.into_iter() {
								stack.push(tok);
							}
							return parse_LL1(stack, input, sem_stack);
						},
						None 				=> return None,
					}
				},
			}
		},
		(None, None) 			=> return None,
		_ 						=> return None,
	}
}

pub fn read_input(input: String) -> Option<Formula2> {
	if let Some(lexed_token_stream) = lex(&input) {
		if let Some(parsed_AST) = parse(lexed_token_stream) {
			return parsed_AST.to_formula();
		}
	}
	return None;
}