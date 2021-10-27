use std::collections::HashMap;
use std::env;
use std::path::Path;
use std::rc::Rc;

mod codegen;

fn main() {
    let arg = env::args_os().nth(1).expect("Expected a path argument");
    let path = Path::new(&arg);
    let modules = snek::parse(path).sort().unwrap();
    let mut decls = HashMap::new();
    for module in modules {
        let deps: Vec<_> = module.dependencies.iter().map(|name| decls.remove(name).expect("missing module")).collect();

        let irt = snek::resolve(Rc::clone(&module.name), &deps, &module.ast);
        codegen::generate(&irt);

        decls.insert(module.name, irt.decls);
        for dep in deps {
            decls.insert(Rc::clone(dep.name()), dep);
        }
    }
}
