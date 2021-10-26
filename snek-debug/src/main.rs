use std::env;
use std::path::Path;
use std::rc::Rc;

mod debug;

fn main() {
    let path = env::args_os().nth(1);
    if let Some(p) = path {
        run_from_file(Path::new(&p));
    }
}

fn run_from_file(path: &Path) {
    let asts = snek::parse(path);
    #[cfg(debug_assertions)] {
        let mut queue = vec![asts.root()];
        while let Some(name) = queue.pop() {
            let (ast, deps) = asts.get_ast(name);
            println!("{} - deps: {:?}", name, deps);
            debug::AstPrinter::new().print_ast(ast);
            for dep in deps {
                queue.push(dep)
            }
        }
    }
    let root_name = Rc::clone(asts.root());
    let (root, _deps) = asts.get_ast(&root_name);
    // TODO deps
    let (_decls, irt) = snek::resolve(root_name, &[], root);
    #[cfg(debug_assertions)] {
        use snek::resolver::irt::IrtNode;
        irt.accept(&mut debug::IrPrinter::new())
    }
}
