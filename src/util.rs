use std::fmt::{Display, Formatter};

pub fn join<T: Display>(f: &mut Formatter<'_>, ts: &[T], sep: &str) -> std::fmt::Result {
    join_map(f, ts, sep, |f, t| write!(f, "{}", t))
}

pub fn join_map<'t, T: 't, F>(f: &mut Formatter<'_>, ts: &'t [T], sep: &str, map: F) -> std::fmt::Result
    where F: Fn(&mut Formatter<'_>, &'t T) -> std::fmt::Result
{
    if ts.is_empty() {
        return Ok(());
    }
    let mut it = ts.iter();
    map(f, it.next().unwrap())?;
    for t in it {
        write!(f, "{}", sep)?;
        map(f, t)?;
    }
    Ok(())
}
