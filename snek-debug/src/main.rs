use std::collections::HashMap;
use std::env;
use std::path::Path;
use std::rc::Rc;

use snek::resolver::irt::IrtNode;
use debug::{AstPrinter, IrPrinter};

mod debug;

fn main() {
    let path = env::args_os().nth(1);
    if let Some(p) = path {
        run_from_file(Path::new(&p));
    }
}

fn run_from_file(path: &Path) {
    let modules = snek::parse(path).sort().unwrap();

    for module in &modules {
        println!("{} - deps: {:?}", module.name, module.dependencies);
        AstPrinter::new().print_ast(&module.ast);
    }

    let mut decls = HashMap::new();
    for module in modules {
        let deps: Vec<_> = module.dependencies.iter().map(|name| decls.remove(name).expect("missing module")).collect();

        let irt = snek::resolve(Rc::clone(&module.name), &deps, &module.ast);
        println!("{}", module.name);
        irt.accept(&mut IrPrinter::new());

        decls.insert(module.name, irt.decls);
        for dep in deps {
            decls.insert(Rc::clone(dep.name()), dep);
        }
    }
}
