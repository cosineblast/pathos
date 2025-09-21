mod check;
mod syntax;

#[cfg(test)]
mod tests;

fn main() {
    let input = std::fs::read_to_string("example.txt").unwrap();

    match syntax::parse_module(&input) {
        Ok(result) => {
            eprintln!("parsed tree: {:?}", result);

            eprintln!("checking...");

            match check::check_module(&result) {
                Ok(()) => eprintln!("all right!"),
                Err(err) => eprintln!("check failed: {:?}", err),
            }
        }

        Err(err) => {
            eprintln!("parse failed! {}", err)
        }
    }
}
