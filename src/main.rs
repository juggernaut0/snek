mod scanner;
mod parse;

fn main() {
    let src = "\
let a = \"global\"
let _ = (println a)
let main = { ->
  let f = { -> (println a) }
  let _ = (f) # this should error with \"a used before definition\" instead of printing \"global\"
  let a = \"local\"
  let _ = (f)
  f
}
let _ = ((main))
let x = 5
let b = true && false
";
    parse::parse(src);
}
