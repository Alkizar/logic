use std::vec;
use std::fmt;

pub enum Token {
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

// TODO: LL(1) parser (can we get away with top-down? if not, do we need LR(1)?)