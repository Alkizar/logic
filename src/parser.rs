use std::fmt;
use std::collections::HashMap;
use structures::*;

#[derive(PartialEq, Eq)]
enum Token {
	Var(String),
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

	fn to_formula<'a>(&self) -> Option<Formula> { 
		match &self.node {
			Token::Var(x) => {
				return Some(Formula::Var(Variable{ name: x.clone() }));
			},
			Token::Not => {  
				match self.children.first() {
					Some(child) => {
						match child.to_formula() {
							Some(p) => return Some(Formula::Not(Box::new(p))),
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
						(Some(p), Some(q)) => return Some(Formula::And(Box::new(p), Box::new(q))),
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
						(Some(p), Some(q)) => return Some(Formula::Or(Box::new(p), Box::new(q))),
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
						(Some(p), Some(q)) => return Some(Formula::Implies(Box::new(p), Box::new(q))),
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
						(Some(p), Some(q)) => return Some(Formula::Equiv(Box::new(p), Box::new(q))),
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
						(Some(p), Some(q)) => return Some(Formula::Xor(Box::new(p), Box::new(q))),
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

// Productions
// E 	-> eE
// eE 	-> iE eE'
// eE' 	-> EQUIV eE <3> | eps
// iE 	-> xE iE'
// iE' 	-> IMPLIES iE <3> | eps
// xE 	-> oE xE'
// xE' 	-> XOR xE <3> | eps
// oE 	-> aE oE'
// oE' 	-> OR oE <3> | eps
// aE 	-> nE aE'
// aE' 	-> AND aE <3> | eps
// nE 	-> NOT nE <2> | X
// X 	-> (E) | VAR

// Terminals: EQUIV, IMPLIES, XOR, OR, AND, NOT, VAR, (, )
// Semantic markers: <n>
// 		When a marker <n> is encountered on the stack, that many ASTs are popped from the
// 		semantic stack. For n == 3, the middle element becomes the parent of the remaining 
//		elements; for all other n, the bottom element becomes the parent.)

// NOTE: all binary operations associate to the right
// Precence is as follows:
// 1. NOT
// 2. AND
// 3. OR
// 4. XOR
// 5. IMPLIES
// 6. EQUIV

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

/* PARSE TABLE
==============
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
==============
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
				op.insert_child(x);
			}
		}
		sem_stack.push(op);
	}  else {
		println!("PARSE WARNING: insufficient semantic stack for marker {}", n);
	}
	return sem_stack;
}

fn push_sem_stack(terminal: Token, mut sem_stack: Vec<AST>) -> Vec<AST> {
	match terminal {
		Token::LeftParen | Token::RightParen  => return sem_stack,
		_ 				  					  => { sem_stack.push(AST::new(terminal, None)); return sem_stack; },
	}
}

fn parse_LL1(mut stack: Vec<ParseToken>, mut input: Vec<ParseToken>, mut sem_stack: Vec<AST>) -> Option<Vec<AST>> {
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

pub fn read_input(input: String) -> Option<Formula> {
	if let Some(lexed_token_stream) = lex(&input) {
		if let Some(parsed_AST) = parse(lexed_token_stream) {
			return parsed_AST.to_formula();
		}
	}
	return None;
}