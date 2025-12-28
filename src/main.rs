#![allow(unused)]

mod structures;
mod parser;
mod truth_table;
mod proofs;
mod resolution;
mod frontend;

use crate::structures::*;
use crate::parser::*;
use crate::truth_table::*;
use crate::proofs::*;
use crate::resolution::*;
use crate::frontend::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io;

// TODO
// * Fitch proof system
// * clean up parser to be more idiomatic + better error handling
// * documentation
// * change "Option" to "Result" for error handling
// * need to support deleting proof steps further back in the proof (should only be able to delete steps that don't have dependencies?)
//   eg. and intro creates 4 steps, may only need 1
//   can also maybe implement a "simplify" feature which trims all unused steps

fn main() {
    initialize_engine()
}
