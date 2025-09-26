use crate::generation::codegen_procedure;

#[test]
fn generates_sum_ir() {
    let source = r#"
            int foo() {
                return 1 + 2;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_declaration_ir() {
    let source = r#"
            int foo() {
                int abc = 1;
                return abc;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_assignment_ir() {
    let source = r#"
            int foo() {
                int a = 1;
                int b = a + 1;
                return b;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_if_ir() {
    let source = r#"
            int foo() {
                int a = 1;
                if (a) {
                    return 10;
                }
                return 20;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_if_else_ir() {
    let source = r#"
            int foo() {
                int a = 1;
                if (a) {
                    return 10;
                } else {
                    return 20;
                }
                return 30;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_reassignment_ir() {
    let source = r#"
            int foo() {
                int a = 1;
                a = a + 1;
                int b = a;
                b = b + 1;
                return b;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_if_else_reassignment_ir() {
    let source = r#"
            int foo() {
                int then_only = 1;
                int else_only = 1;
                int then_else = 1;
                if (1) {
                    then_only = 10;
                    then_else = 10;
                } else {
                    else_only = 20;
                    then_else = 20;
                }
                return 0;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_sequential_if_else_ir() {
    let source = r#"
            int foo() {
                int a = 1;
                int b = 1;

                if (1) {
                    a = 2;
                } else {
                    a = 3;
                }
                
                if (1) {
                } else {
                    a = 0;
                    b = 0;
                }

                return 10 * a +  b;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}

#[test]
fn generates_while() {
    let source = r#"
            int foo() {
                int a = 1;
                int b = 10;

                while (b) {
                    a = a * 2;
                    b = b - 1;
                }

                return a;
            }
        "#;

    let procedure = crate::syntax::parse_procedure(source).unwrap();

    let ir = codegen_procedure(&procedure);

    insta::assert_yaml_snapshot!(ir);
}
