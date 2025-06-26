use crate::{
    analysis::{CheckError, ValueType},
    syntax,
};

use assert_matches::assert_matches;

#[test]
fn type_check_checks_return() {
    let module = syntax::parse_module(
        r#"
            array get(array a, int i) {
            	return a[i];
            }
        "#,
    )
    .unwrap();

    assert_matches!(
        crate::analysis::full_check(&module),
        Err(
            CheckError::ReturnTypeMismatch {
            procedure_name: procedure,
            expected: ValueType::Array,
            got: ValueType::Int,
            ..
        })

        if procedure == "get"
    );
}
