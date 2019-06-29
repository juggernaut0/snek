mod scanner;
mod parser;
mod ast;

fn main() {
    let src = "\
import from 'foo'
import hello.dog from 'bar'

type Option = Some x | None

namespace foo {
  public let x = 5
  let b = true && false
  let 5 = 6
  public type Pair a b
}

let foox = foo.x

let a = \"global\"
let _ = (println a)
let main = { ->
  let f = { -> (println a) }
  let _ = (f) # this should error with \"a used before definition\" instead of printing \"global\"
  let a = \"local\"
  let _ = (f)
  f
}
((main))
";
    match parser::parse(src) {
        Ok(_) => {} // TODO execution
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()))
        }
    }
}
