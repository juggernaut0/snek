mod scanner;
mod parse;

fn main() {
    let src = "\
# comment 1
123.45   87 'hello'
1. \"str\"
# comment2 5.6 \"";
    parse::parse(src);
}
