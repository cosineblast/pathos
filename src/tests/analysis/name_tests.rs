use crate::{
    analysis::{self, CheckError, full_check},
    syntax,
};

use assert_matches::assert_matches;

#[test]
fn full_check_succeeds_when_given_empty_procedure() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    
                }
            "#,
    )
    .unwrap();

    assert_matches!(analysis::full_check(&module), Ok(()));
}

#[test]
fn full_check_succeeds_when_given_existent_variables() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    int a = 10;
                    int b = 20;

                    if (a) {
                        return b;
                    } else {
                        return a;
                    }
                }
            "#,
    )
    .unwrap();

    assert_matches!(analysis::full_check(&module), Ok(()));
}

#[test]
fn full_check_fails_when_name_does_not_exist() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    int a = 10;
                    int b = 20;

                    if (a) {
                        return c;
                    } else {
                        return a;
                    }
                }
            "#,
    )
    .unwrap();

    assert_matches!(
        full_check(&module),
        Err(analysis::CheckError::NameNotFound(name))
        if name == "c"
    );
}

#[test]
fn full_check_respects_block_scopes() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    int a = 10;
                    int b = 20;

                    if (a) {
                        int c = 20;
                    }

                    return c;
                }
            "#,
    )
    .unwrap();

    assert_matches!(
        full_check(&module),
        Err(analysis::CheckError::NameNotFound(name))
        if name == "c"
    );
}

#[test]
fn full_check_respects_shawdoing() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    int a = 10;

                    if (123) {
                        int a = 20;
                    }

                    return a;
                }
            "#,
    )
    .unwrap();

    assert_matches!(full_check(&module), Ok(()));
}

#[test]
fn full_check_is_aware_of_other_procedures() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    return foo();
                }

                int foo() {
                    return 10;
                }
            "#,
    )
    .unwrap();

    assert_matches!(full_check(&module), Ok(()));
}

#[test]
fn full_check_is_aware_of_parameters() {
    let module = syntax::parse_module(
        r#"
                int main(int a, int b) {
                    if (a) {
                        return b;
                    } else {
                        return 0;
                    }
                }
            "#,
    )
    .unwrap();

    assert_matches!(full_check(&module), Ok(()));
}

#[test]
fn full_check_fails_when_local_is_used_for_procedure() {
    let module = syntax::parse_module(
        r#"
                int main(int a, int b) {
                    if (a) {
                        return b;
                    } else {
                        return 0;
                    }
                }
            "#,
    )
    .unwrap();

    assert_matches!(full_check(&module), Ok(()));
}

#[test]
fn full_check_fails_when_procedure_is_used_instead_of_local() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    return foo;
                }

                int foo() {
                    return 0;
                }
            "#,
    )
    .unwrap();

    assert_matches!(
        full_check(&module),
        Err(CheckError::ProcedureAsExpression(name))
        if name == "foo"
    );
}

#[test]
fn full_check_fails_when_local_is_used_instead_of_procedure() {
    let module = syntax::parse_module(
        r#"
                int main() {
                    int x = 10;
                    return x();
                }
            "#,
    )
    .unwrap();

    assert_matches!(
        full_check(&module),
        Err(CheckError::LocalVariableAsProcedure(name))
        if name == "x"
    );
}
