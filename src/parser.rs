use std::fmt;
use crate::structures::*;

const DEBUG: bool = false;

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
			Token::Implies 	=> write!(f, "Implies"),
			Token::Equiv 		=> write!(f, "Equiv"),
			Token::Xor 			=> write!(f, "Xor"),
			Token::LeftParen 	=> write!(f, "LeftParen"),
			Token::RightParen => write!(f, "RightParen"),
		}
	}
}

// ============= LEXER =============

fn lex(string: &str) -> Result<Vec<Token>, String> {
	let toks: Vec<Token> = Vec::new();
	next_waiting(string, toks)
}

fn lex_error(c: char, string: &str, state: &str) -> String {
	format!("[LEXING ERROR] unexpected character {} in {} while in state {}.", c, string, state)
}

fn next_waiting(string: &str, mut toks: Vec<Token>) -> Result<Vec<Token>, String> {
	let next: Option<char> = string.chars().next();
	match next {
		Some(c) => {
			match c {
				c if c.is_whitespace() 	=> next_waiting(&string[1..], toks), 												// whitespace: consume and call next_waiting
				'a'..='z'					=> next_variable(&string, toks, String::new()), 								// start of variable: call next_variable
				'='							=> next_implies(&string[1..], toks), 												// start of implies: consume and call next_implies
				'<'							=> next_equiv(&string[1..], toks, '<'),   										// start of equiv: consume and call next_equiv
				'~'							=> { toks.push(Token::Not); next_waiting(&string[1..], toks) }, 			// not: consume, emit Not token, call next_waiting 
				'&'							=> { toks.push(Token::And); next_waiting(&string[1..], toks) }, 			// and: consume, emit And token, call next_waiting
				'|'							=> { toks.push(Token::Or); next_waiting(&string[1..], toks) }, 			// or: consume, emit Or token, call next_waiting
				'+'							=> { toks.push(Token::Xor); next_waiting(&string[1..], toks) }, 			// xor: consume, emit Xor token, call next_waiting
				'('							=> { toks.push(Token::LeftParen); next_waiting(&string[1..], toks) }, 	// left paren: consume, emit LeftParen token, call next_waiting
				')'							=> { toks.push(Token::RightParen); next_waiting(&string[1..], toks) }, 	// right paren: consume, emit RightParen token, call next_waiting
				_ 								=> { Err(lex_error(c, string, "next_waiting")) },
			}
		},
		None 	=> Ok(toks)	// no characters left: lexing complete
	}
}

fn next_variable(string: &str, mut toks: Vec<Token>, mut name: String) -> Result<Vec<Token>, String> {
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
}

fn next_implies(string: &str, mut toks: Vec<Token>) -> Result<Vec<Token>, String> {
	let next: Option<char> = string.chars().next();
	match next {
		Some(c) => {
			match c {
				'>' => { toks.push(Token::Implies); next_waiting(&string[1..], toks) },	// end of implies: consume, emit Implies token, call next_waiting
				_ 	 => { Err(lex_error(c, string, "next_implies")) },
			}
		},
		None 	=> { Err(lex_error('=', string, "next_implies")) },
	}
}

fn next_equiv(string: &str, mut toks: Vec<Token>, prev: char) -> Result<Vec<Token>, String> {
	let next: Option<char> = string.chars().next();
	match next {
		Some(c) => {
			match (prev, c) {
				('<', '=') => { next_equiv(&string[1..], toks, '=') },								// incomplete equiv: consume, call next_equiv
				('=', '>') => { toks.push(Token::Equiv); next_waiting(&string[1..], toks) },	// end of equiv: consume, emit Equiv token, call next_waiting
				_ 			  => { Err(lex_error(c, string, "next_equiv")) },
			}
		},
		None 	=> { Err(lex_error(prev, string, "next_equiv")) },
	}
}

// ============= PARSER =============

fn ast_error(message: &str) -> String {
	format!("[SEMANTIC ERROR] failed to construct formula from AST: {}.", message)
}

struct AST {
	node: 	 Token,
	children: Vec<AST>,
}

impl AST {
	fn new(n: Token, cs: Option<Vec<AST>>) -> Self {
		match cs {
			Some(xs) 	=> Self { node:n, children:xs },
			None 			=> Self { node:n, children:Vec::new() },
		}
	}

	fn insert_child(&mut self, child: AST) {
		self.children.push(child);
	}

	// TODO -- clean up the nested matches
	fn to_formula(&self) -> Result<Formula, String> { 
		match &self.node {
			Token::Var(x) => Ok(Formula::Var(Variable{ name: x.clone() })),
			Token::Not => {
				match self.children.first() {
					Some(child) => {
						child.to_formula().map(|p| Formula::Not(Box::new(p)))
					},
					None => Err(ast_error("empty Not node")),
				}
			},
			Token::And => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Ok(p), Ok(q)) => Ok(Formula::And(Box::new(p), Box::new(q))),
						(Err(e1), _) => Err(e1),
						(_, Err(e2)) => Err(e2),
					}
				} else {
					Err(ast_error("fewer than two children in And node"))
				}
			},
			Token::Or => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Ok(p), Ok(q)) => Ok(Formula::Or(Box::new(p), Box::new(q))),
						(Err(e1), _) => Err(e1),
						(_, Err(e2)) => Err(e2),
					}
				} else {
					Err(ast_error("fewer than two children in Or node"))
				}
			},
			Token::Implies => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Ok(p), Ok(q)) => Ok(Formula::Implies(Box::new(p), Box::new(q))),
						(Err(e1), _) => Err(e1),
						(_, Err(e2)) => Err(e2),
					}
				} else {
					Err(ast_error("fewer than two children in Implies node"))
				}
			},
			Token::Equiv => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Ok(p), Ok(q)) => Ok(Formula::Equiv(Box::new(p), Box::new(q))),
						(Err(e1), _) => Err(e1),
						(_, Err(e2)) => Err(e2),
					}
				} else {
					Err(ast_error("fewer than two children in Equiv node"))
				}
			},
			Token::Xor => {
				let mut first_two_children = self.children.iter().take(2);
				if let (Some(arg1), Some(arg2)) = (first_two_children.next(), first_two_children.next()) { 
					match (arg1.to_formula(), arg2.to_formula()) {
						(Ok(p), Ok(q)) => Ok(Formula::Xor(Box::new(p), Box::new(q))),
						(Err(e1), _) => Err(e1),
						(_, Err(e2)) => Err(e2),
					}
				} else {
					Err(ast_error("fewer than two children in Xor node"))
				}
			},
			_ => Err(ast_error(&format!("unexpected token {} as node in AST", self.node))), // ERROR
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

impl fmt::Display for ParseToken {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ParseToken::Terminal(t) => write!(f, "{}", t),
			ParseToken::E 				=> write!(f, "E"),
			ParseToken::EE 			=> write!(f, "EE"),
			ParseToken::EE_ 			=> write!(f, "EE'"),
			ParseToken::IE 			=> write!(f, "IE"),
			ParseToken::IE_ 			=> write!(f, "IE'"),
			ParseToken::XE 			=> write!(f, "XE"),
			ParseToken::XE_ 			=> write!(f, "XE'"),
			ParseToken::OE 			=> write!(f, "OE"),
			ParseToken::OE_ 			=> write!(f, "OE'"),
			ParseToken::AE 			=> write!(f, "AE"),
			ParseToken::AE_ 			=> write!(f, "AE'"),
			ParseToken::NE 			=> write!(f, "NE"),
			ParseToken::X 				=> write!(f, "X"),
			ParseToken::End 			=> write!(f, "$"),
			ParseToken::Marker(n) 	=> write!(f, "<n>'"),
		}
	}
}

fn parse_error(message: &str) -> String {
	format!("[SYNTAX ERROR] failed to parse formula: {}.", message)
}

fn process_lex_stream(input: Vec<Token>) -> Vec<ParseToken> {
	let mut output: Vec<ParseToken> = Vec::from([ParseToken::End]);
	for tok in input.into_iter().rev() {
		output.push(ParseToken::Terminal(tok));
	}
	output
}


fn parse(input: Vec<Token>) -> Option<AST> {
	let parse_token_input: Vec<ParseToken> = process_lex_stream(input);
	let stack: Vec<ParseToken> = Vec::from([ParseToken::End, ParseToken::E]);
	let sem_stack: Vec<AST> = Vec::new();
	match parse_ll1(stack, parse_token_input, sem_stack) {
		Ok(mut xs) => { 
			if xs.len() != 1 {
				if DEBUG {
					eprintln!("[PARSER WARNING] generated semantic stack contains {} elements, expected 1.", xs.len());
				}
			}
			xs.pop() 
		},
		Err(e) => {
			if DEBUG {
				eprintln!("[PARSER ERROR] encountered a problem in LL(1) parser (see below).");
				eprintln!("{}", e);
			}
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
		(ParseToken::E, ParseToken::Terminal(Token::LeftParen))  	=> Some(Vec::from([ParseToken::EE])),
		(ParseToken::E, ParseToken::Terminal(Token::Not)) 		   	=> Some(Vec::from([ParseToken::EE])),
		(ParseToken::E, ParseToken::Terminal(Token::Var(_))) 	   	=> Some(Vec::from([ParseToken::EE])),
		// EE
		(ParseToken::EE, ParseToken::Terminal(Token::LeftParen))   	=> Some(Vec::from([ParseToken::EE_, ParseToken::IE])),
		(ParseToken::EE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::EE_, ParseToken::IE])),
		(ParseToken::EE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::EE_, ParseToken::IE])),
		// EE'
		(ParseToken::EE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::EE_, ParseToken::End) 						   		=> Some(Vec::new()),
		(ParseToken::EE_, ParseToken::Terminal(Token::Equiv))	   	=> Some(Vec::from([ParseToken::Marker(3), ParseToken::EE, ParseToken::Terminal(Token::Equiv)])),
		// IE
		(ParseToken::IE, ParseToken::Terminal(Token::LeftParen))   	=> Some(Vec::from([ParseToken::IE_, ParseToken::XE])),
		(ParseToken::IE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::IE_, ParseToken::XE])),
		(ParseToken::IE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::IE_, ParseToken::XE])),
		// IE'
		(ParseToken::IE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::IE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::IE_, ParseToken::End) 						   		=> Some(Vec::new()),
		(ParseToken::IE_, ParseToken::Terminal(Token::Implies))    	=> Some(Vec::from([ParseToken::Marker(3), ParseToken::IE, ParseToken::Terminal(Token::Implies)])),
		// XE
		(ParseToken::XE, ParseToken::Terminal(Token::LeftParen))   	=> Some(Vec::from([ParseToken::XE_, ParseToken::OE])),
		(ParseToken::XE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::XE_, ParseToken::OE])),
		(ParseToken::XE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::XE_, ParseToken::OE])),
		// XE'
		(ParseToken::XE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::XE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::XE_, ParseToken::Terminal(Token::Implies))    	=> Some(Vec::new()),
		(ParseToken::XE_, ParseToken::End) 						   		=> Some(Vec::new()),
		(ParseToken::XE_, ParseToken::Terminal(Token::Xor)) 	   	=> Some(Vec::from([ParseToken::Marker(3), ParseToken::XE, ParseToken::Terminal(Token::Xor)])),
		// OE
		(ParseToken::OE, ParseToken::Terminal(Token::LeftParen))   	=> Some(Vec::from([ParseToken::OE_, ParseToken::AE])),
		(ParseToken::OE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::OE_, ParseToken::AE])),
		(ParseToken::OE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::OE_, ParseToken::AE])),
		// OE'
		(ParseToken::OE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Implies))    	=> Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Xor)) 	   	=> Some(Vec::new()),
		(ParseToken::OE_, ParseToken::End) 						   		=> Some(Vec::new()),
		(ParseToken::OE_, ParseToken::Terminal(Token::Or)) 		   => Some(Vec::from([ParseToken::Marker(3), ParseToken::OE, ParseToken::Terminal(Token::Or)])),
		// AE
		(ParseToken::AE, ParseToken::Terminal(Token::LeftParen))   	=> Some(Vec::from([ParseToken::AE_, ParseToken::NE])),
		(ParseToken::AE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::AE_, ParseToken::NE])),
		(ParseToken::AE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::AE_, ParseToken::NE])),
		// AE'
		(ParseToken::AE_, ParseToken::Terminal(Token::RightParen)) 	=> Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Equiv)) 	   => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Implies))    	=> Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Xor)) 	   	=> Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::Or)) 		   => Some(Vec::new()),
		(ParseToken::AE_, ParseToken::End) 						   		=> Some(Vec::new()),
		(ParseToken::AE_, ParseToken::Terminal(Token::And)) 	   	=> Some(Vec::from([ParseToken::Marker(3), ParseToken::AE, ParseToken::Terminal(Token::And)])),
		// NE
		(ParseToken::NE, ParseToken::Terminal(Token::LeftParen))   	=> Some(Vec::from([ParseToken::X])),
		(ParseToken::NE, ParseToken::Terminal(Token::Var(_))) 	   => Some(Vec::from([ParseToken::X])),
		(ParseToken::NE, ParseToken::Terminal(Token::Not)) 		   => Some(Vec::from([ParseToken::Marker(2), ParseToken::NE, ParseToken::Terminal(Token::Not)])),
		// X
		(ParseToken::X, ParseToken::Terminal(Token::LeftParen))    	=> Some(Vec::from([ParseToken::Terminal(Token::RightParen), ParseToken::E, ParseToken::Terminal(Token::LeftParen)])),
		(ParseToken::X, ParseToken::Terminal(Token::Var(x))) 	   	=> Some(Vec::from([ParseToken::Terminal(Token::Var(x.clone()))])),
		// Else
		_ => { 
			if DEBUG {
				eprintln!("[PARSER WARNING] attempted to read invalid entry from parse table.");
			}
			None 
		},
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
	}  else if DEBUG {
		eprintln!("[PARSER WARNING] insufficient semantic stack for marker <{}>.", n);
	}
	sem_stack
}

fn push_sem_stack(terminal: Token, mut sem_stack: Vec<AST>) -> Vec<AST> {
	match terminal {
		Token::LeftParen | Token::RightParen => sem_stack,
		_ 				  					 			 => { 
			sem_stack.push(AST::new(terminal, None)); 
			sem_stack 
		},
	}
}

// TODO clean up the nested matches here too
fn parse_ll1(mut stack: Vec<ParseToken>, mut input: Vec<ParseToken>, mut sem_stack: Vec<AST>) -> Result<Vec<AST>, String> {
	match (stack.pop(), input.last()) {
		(Some(v), Some(x)) => {
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
								Err(parse_error("terminals at front of stack and input do not match"))
							}
						},
						_ => Err(parse_error("top of input stack is nonterminal")),
					}
				},
				ParseToken::End => {
					// End of stack
					match x {
						ParseToken::End => {
							Ok(sem_stack)
						},
						_ => Err(parse_error("unexpected end of stack")),
					}
				},
				ParseToken::Marker(n) => {
					// Semantic marker; evolve semantic stack
					sem_stack = evolve_sem_stack(sem_stack, n);
					parse_ll1(stack, input, sem_stack)
				},
				_ 			=> {
					// Production
					match parse_table(v, x) {
						Some(production) => {
							for tok in production.into_iter() {
								stack.push(tok);
							}
							parse_ll1(stack, input, sem_stack)
						},
						None => Err(parse_error("invalid entry in parse table")),
					}
				},
			}
		},
		(None, _) => Err(parse_error("unexpected end of stack")),
		(_, None) => Err(parse_error("unexpected end of input")),
	}
}

pub fn read_input(input: String) -> Option<Formula> {
	match lex(&input) {
		Ok(lexed_token_stream) => {
			if let Some(parsed_ast) = parse(lexed_token_stream) {
				match parsed_ast.to_formula() {
					Ok(p)  => return Some(p),
					Err(e_ast) => {
						if DEBUG {
							eprintln!("{}", e_ast);
						}
						return None;
					}
				}
			}
			None
		}
		Err(e_lex) => {
			if DEBUG {
				eprintln!("{}", e_lex);
			}
			None
		},
	}
}