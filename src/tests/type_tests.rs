use crate::{
    check::{self, CheckError, check_module},
    syntax,
};

use assert_matches::assert_matches;

#[test]
fn check_module_checks_return_correctly() {
    let module = syntax::parse_module(
        r#"
            
        "#,
    );
}
