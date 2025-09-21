
#[allow(dead_code)]
mod analysis;
#[allow(dead_code)]
mod generation;
#[allow(dead_code)]
mod syntax;

#[cfg(test)]
mod tests;

fn main() {
    eprintln!();
    eprintln!("Opening example.txt...");

    let input = std::fs::read_to_string("example.txt").unwrap();

    eprintln!("Parsing example.txt...");

    match syntax::parse_module(&input) {
        Ok(result) => {
            eprintln!("Parse Tree: ");
            eprintln!(
                "parsed tree: {}",
                serde_json::to_string_pretty(&result).unwrap()
            );

            eprintln!("Checking the module...");

            match analysis::full_check(&result) {
                Ok(()) => eprintln!("All right!"),
                Err(err) => eprintln!("Source code did not pass analysis check: {:?}", err),
            }
        }

        Err(err) => {
            eprintln!("parse failed! {}", err)
        }
    }
}
