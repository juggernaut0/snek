use std::collections::HashMap;
use std::env;
use std::path::Path;
use std::rc::Rc;

mod codegen;

fn main() {
    let arg = env::args_os().nth(1).expect("Expected a path argument");
    let path = Path::new(&arg);
    let asts = snek::parse(path).sort().unwrap();
    let mut decls = HashMap::new();
    for (mod_name, ast, dep_names) in asts {
        let deps: Vec<_> = dep_names.iter().map(|name| decls.remove(name).expect("missing module")).collect();

        let (mod_decls, _irt) = snek::resolve(Rc::clone(&mod_name), &deps, &ast);

        decls.insert(mod_name, mod_decls);
        for dep in deps {
            decls.insert(Rc::clone(dep.name()), dep);
        }
    }
}
