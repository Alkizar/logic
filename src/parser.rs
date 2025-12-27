//mod parser;

use std::fmt;
use crate::structures::*;

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

// ============= LEXER =============

fn lex(string: &str) -> Option<Vec<Token>> {
	let toks: Vec<Token> = Vec::new();
	next_waiting(string, toks)
}

fn lex_error(c: char, string: &str, state: &str) {
	eprintln!("LEXING ERROR: unexpected character {} in {} while in state {}", c, string, state);
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

// ============= PARSER =============

fn ast_error(node: &Token) {
	eprintln!("SEMANTIC ERROR: failed to construct AST node {}", node);
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

	// TODO -- clean up the nested matches
	fn to_formula<'a>(&self) -> Option<Formula> { 
		match &self.node {
			Token::Var(x) => Some(Formula::Var(Variable{ name: x.clone() })),
			Token::Not => {
				match self.children.first() {
					Some(child) => {
						match child.to_formula() {
							Some(p) => Some(Formula::Not(Box::new(p))),
							None => None,
						}
					},
					None => None,
				}
			},
			Token::And => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => Some(Formula::And(Box::new(p), Box::new(q))),
						_ => None,
					}
				} else {
					None
				}
			},
			Token::Or => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => Some(Formula::Or(Box::new(p), Box::new(q))),
						_ => None,
					}
				} else {
					None
				}
			},
			Token::Implies => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => Some(Formula::Implies(Box::new(p), Box::new(q))),
						_ => None,
					}
				} else {
					None
				}
			},
			Token::Equiv => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => Some(Formula::Equiv(Box::new(p), Box::new(q))),
						_ => None,
					}
				} else {
					None
				}
			},
			Token::Xor => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Some(p), Some(q)) => Some(Formula::Xor(Box::new(p), Box::new(q))),
						_ => None,
					}
				} else {
					None
				}
			},
			_ => None, // ERROR
		}
	}
}

// Productions
// E 	-> EE
// EE 	-> IE EE'
// EE' 	-> EQUIV EE <3> | eps
// IE 	-> XE IE'
// IE' 	-> IMPLIES IE <3> | eps
// XE 	-> OE XE'
// XE' 	-> XOR XE <3> | eps
// OE 	-> AE OE'
// OE' 	-> OR OE <3> | eps
// AE 	-> NE AE'
// AE' 	-> AND AE <3> | eps
// NE 	-> NOT NE <2> | X
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
	EE,
	EE_,
	IE,
	IE_,
	XE,
	XE_,
	OE,
	OE_,
	AE,
	AE_,
	NE,
	X,
	End,
	Marker(usize),
}

fn parse_error() {
	eprintln!("SYNTAX ERROR: ");
}

fn process_lex_stream(input: Vec<Token>) -> Vec<ParseToken> {
	let mut output: Vec<ParseToken> = Vec::from([ParseToken::End]);
	for tok in input.into_iter().rev() {
		output.push(ParseToken::Terminal(tok));
	}
	return output;
}


fn parse(input: Vec<Token>) -> Option<AST> {
	let parse_token_input: Vec<ParseToken> = process_lex_stream(input);
	let stack: Vec<ParseToken> = Vec::from([ParseToken::End, ParseToken::E]);
	let sem_stack: Vec<AST> = Vec::new();
	match parse_ll1(stack, parse_token_input, sem_stack) {
		Some(mut xs) => xs.pop(),
		None 		 => { 
			println!("PARSE ERROR: LL(1) failed."); // TODO -- E2E failure in parser
			None 
		},
	}
}

/* PARSE TABLE
==============
         |    (    |    )    |   <2>   |   =>    |   VAR   |    +    |    ~    |    &    |   <3>   |   <=>   |    |    |    $    |
---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   AE'   |         |eps      |         |eps      |         |eps      |         |&AE<3>   |eps      |eps      |eps      |eps      |
   EE    |IE EE'   |         |         |         |IE EE'   |         |IE EE'   |         |         |         |         |         |
   EE'   |         |eps      |         |         |         |         |         |         |eps      |<=>EE<3> |         |eps      |
   NE    |X        |         |         |         |X        |         |~NE<2>   |         |         |         |         |         |
    X    |(E)      |         |         |         |VAR      |         |         |         |         |         |         |         |
    E    |EE       |         |         |         |EE       |         |EE       |         |         |         |         |         |
   XE    |OE XE'   |         |         |         |OE XE'   |         |OE XE'   |         |         |         |         |         |
   OE    |AE OE'   |         |         |         |AE OE'   |         |AE OE'   |         |         |         |         |         |
   IE    |XE IE'   |         |         |         |XE IE'   |         |XE IE'   |         |         |         |         |         |
   IE'   |         |eps      |         |=>IE<3>  |         |         |         |         |eps      |eps      |         |eps      |
   XE'   |         |eps      |         |eps      |         |+XE<3>   |         |         |eps      |eps      |         |eps      |
   OE'   |         |eps      |         |eps      |         |eps      |         |         |eps      |eps      ||OE<3>   |eps      |
   AE    |NE AE'   |         |         |         |NE AE'   |         |NE AE'   |         |         |         |         |         |
==============
*/

fn parse_table(nonterminal: ParseToken, terminal: &ParseToken) -> Option<Vec<ParseToken>> {
	match (nonterminal, terminal) {
		// E
		(ParseToken::E, ParseToken::Terminal(Token::LeftParen))    => Some(Vec::from([ParseToken::EE])),
		(ParseToken::E, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::EE])),
		(ParseToken::E, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::EE])),
		// EE
		(ParseToken::EE, ParseToken::Terminal(Token::LeftParen))   => Some(Vec::from([ParseToken::EE_, ParseToken::IE])),
		(ParseToken::EE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::EE_, ParseToken::IE])),
		(ParseToken::EE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::EE_, ParseToken::IE])),
		// EE'
		(ParseToken::EE_, ParseToken::Terminal(Token::RightParen)) => Some(Vec::new()),
		(ParseToken::EE_, ParseToken::End) 						   => Some(Vec::new()),
		(ParseToken::EE_, ParseToken::Terminal(Token::Equiv))	   => Some(Vec::from([ParseToken::Marker(3), ParseToken::EE, ParseToken::Terminal(Token::Equiv)])),
		// IE
		(ParseToken::IE, ParseToken::Terminal(Token::LeftParen))   => Some(Vec::from([ParseToken::IE_, ParseToken::XE])),
		(ParseToken::IE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::IE_, ParseToken::XE])),
		(ParseToken::IE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::IE_, ParseToken::XE])),
		// IE'
		(ParseToken::IE_, ParseToken::Terminal(Token::RightParen)) => Some(Vec::new()),
		(ParseToken::IE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::IE_, ParseToken::End) 						   => Some(Vec::new()),
		(ParseToken::IE_, ParseToken::Terminal(Token::Implies))    => Some(Vec::from([ParseToken::Marker(3), ParseToken::IE, ParseToken::Terminal(Token::Implies)])),
		// XE
		(ParseToken::XE, ParseToken::Terminal(Token::LeftParen))   => Some(Vec::from([ParseToken::XE_, ParseToken::OE])),
		(ParseToken::XE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::XE_, ParseToken::OE])),
		(ParseToken::XE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::XE_, ParseToken::OE])),
		// XE'
		(ParseToken::XE_, ParseToken::Terminal(Token::RightParen)) => Some(Vec::new()),
		(ParseToken::XE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::XE_, ParseToken::Terminal(Token::Implies))    => Some(Vec::new()),
		(ParseToken::XE_, ParseToken::End) 						   => Some(Vec::new()),
		(ParseToken::XE_, ParseToken::Terminal(Token::Xor)) 	   => Some(Vec::from([ParseToken::Marker(3), ParseToken::XE, ParseToken::Terminal(Token::Xor)])),
		// OE
		(ParseToken::OE, ParseToken::Terminal(Token::LeftParen))   => Some(Vec::from([ParseToken::OE_, ParseToken::AE])),
		(ParseToken::OE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::OE_, ParseToken::AE])),
		(ParseToken::OE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::OE_, ParseToken::AE])),
		// OE'
		(ParseToken::OE_, ParseToken::Terminal(Token::RightParen)) => Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Implies))    => Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Xor)) 	   => Some(Vec::new()),
		(ParseToken::OE_, ParseToken::End) 						   => Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Or)) 		   => Some(Vec::from([ParseToken::Marker(3), ParseToken::OE, ParseToken::Terminal(Token::Or)])),
		// AE
		(ParseToken::AE, ParseToken::Terminal(Token::LeftParen))   => Some(Vec::from([ParseToken::AE_, ParseToken::NE])),
		(ParseToken::AE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::AE_, ParseToken::NE])),
		(ParseToken::AE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::AE_, ParseToken::NE])),
		// AE'
		(ParseToken::AE_, ParseToken::Terminal(Token::RightParen)) => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Implies))    => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Xor)) 	   => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Or)) 		   => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::End) 						   => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::And)) 	   => Some(Vec::from([ParseToken::Marker(3), ParseToken::AE, ParseToken::Terminal(Token::And)])),
		// NE
		(ParseToken::NE, ParseToken::Terminal(Token::LeftParen))   => Some(Vec::from([ParseToken::X])),
		(ParseToken::NE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::X])),
		(ParseToken::NE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::Marker(2), ParseToken::NE, ParseToken::Terminal(Token::Not)])),
		// X
		(ParseToken::X, ParseToken::Terminal(Token::LeftParen))    => Some(Vec::from([ParseToken::Terminal(Token::RightParen), ParseToken::E, ParseToken::Terminal(Token::LeftParen)])),
		(ParseToken::X, ParseToken::Terminal(Token::Var(x))) 	   => Some(Vec::from([ParseToken::Terminal(Token::Var(x.clone()))])),
		// Else
		_ => None, // PARSE ERROR: invalid entry in parse table
	}
}

fn evolve_sem_stack(mut sem_stack: Vec<AST>, n: usize) -> Vec<AST>{
	if sem_stack.len() >= n && n > 0 { 				// TODO -- do this more idiomatically
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
		Token::LeftParen | Token::RightParen => return sem_stack,
		_ 				  					 => { sem_stack.push(AST::new(terminal, None)); return sem_stack; },
	}
}

// TODO clean up the nested matches here too
fn parse_ll1(mut stack: Vec<ParseToken>, mut input: Vec<ParseToken>, mut sem_stack: Vec<AST>) -> Option<Vec<AST>> {
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
								parse_ll1(stack, input, push_sem_stack(t, sem_stack))
							} else {
								None // PARSE ERROR: terminals at front of stack and input do not match
							}
						},
						_ => None, // PARSE ERROR: top of input stack is nonterminal
					}
				},
				ParseToken::End 		=> {
					// End of stack
					match x {
						ParseToken::End => {
							Some(sem_stack)
						},
						_ 	=> return None, // PARSE ERROR: unexpected end of stack
					}
				},
				ParseToken::Marker(n)	=> {
					// Semantic marker; evolve semantic stack
					sem_stack = evolve_sem_stack(sem_stack, n);
					parse_ll1(stack, input, sem_stack)
				},
				_ 			=> {
					// Production
					match parse_table(v, x) {
						Some(production) 	=> {
							for tok in production.into_iter() {
								stack.push(tok);
							}
							parse_ll1(stack, input, sem_stack)
						},
						None 				=> None,	// PARSE ERROR: invalid entry in parse table
					}
				},
			}
		},
		_ 						=> None, // PARSE ERROR (TODO: split errors between end up input and end of stack?)
	}
}

pub fn read_input(input: String) -> Option<Formula> {
	if let Some(lexed_token_stream) = lex(&input) {
		if let Some(parsed_ast) = parse(lexed_token_stream) {
			return parsed_ast.to_formula();
		}
	}
	None
}