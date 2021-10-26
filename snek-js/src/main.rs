use std::env;
use std::path::Path;

fn main() {
    let arg = env::args_os().nth(1).expect("Expected a path argument");
    let path = Path::new(&arg);
    let _asts = snek::parse(path);
}
