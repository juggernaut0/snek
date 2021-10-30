cargo build -p snek-js
target/debug/snek-js $1 | docker run -i --rm node:16 -
