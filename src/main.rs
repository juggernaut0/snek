mod scanner;
mod parse;

fn main() {
    let src = "\
# comment 1
123.45   87
# comment2";
    parse::parse(src);
}
