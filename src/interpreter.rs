use std::collections::HashMap;

use crate::generation::IRInstruction;

use super::generation::{IRExpression, IRProcedure, IRValueId};

use IRExpression as IRExpr;
use RuntimeValue as RV;

#[derive(Clone, Debug)]
enum RuntimeValue {
    Int(i64),
    Array(Vec<i64>),
}

struct RuntimeState {
    vars: HashMap<IRValueId, RuntimeValue>,
}

impl RuntimeState {
    fn get_int(&mut self, id: IRValueId) -> i64 {
        match self.vars.get(&id).unwrap() {
            RV::Int(i) => *i,
            RV::Array(_vec) => panic!("expected int, got array"),
        }
    }

    fn eval_ir_expression(&mut self, expr: IRExpr) -> RuntimeValue {
        let get_int = |id: &IRValueId| -> i64 {
            match self.vars.get(id).unwrap() {
                RV::Int(i) => *i,
                RV::Array(_vec) => panic!("expected int, got array"),
            }
        };

        match expr {
            IRExpr::Literal(value) => RV::Int(value),
            IRExpr::AnotherValue(other) => self.vars.get(&other).unwrap().clone(),
            IRExpr::Add(left, right) => RV::Int(get_int(&left) + get_int(&right)),
            IRExpr::Sub(left, right) => RV::Int(get_int(&left) - get_int(&right)),
            IRExpr::Mul(left, right) => RV::Int(get_int(&left) * get_int(&right)),
            IRExpr::Div(left, right) => RV::Int(get_int(&left) / get_int(&right)),
            IRExpr::Deref { .. } => todo!(),
            IRExpr::Call { .. } => todo!(),
        }
    }
}

fn run_ir(procedure: IRProcedure, arguments: &[RuntimeValue]) -> RuntimeValue {
    // TODO: load arguments into variables... somehow.
    _ = arguments;

    let mut segment = procedure.segments.get(&procedure.root_segment_id).unwrap();

    let mut instruction_index = 0;

    let vars = HashMap::<IRValueId, RuntimeValue>::new();

    let mut state = RuntimeState { vars };

    loop {
        let instruction = segment.0[instruction_index].clone();

        match instruction {
            IRInstruction::Declare(name, expression) => {
                let value = state.eval_ir_expression(expression);
                if let Some(_) = state.vars.insert(name.clone(), value) {
                    panic!("tried to re-declare an existing value at runtime");
                }

                instruction_index += 1;
            }
            IRInstruction::ConditionalJump(condition, then_segment, else_segment) => {
                let condition = state.get_int(condition);
                let then_segment = procedure.segments.get(&then_segment).unwrap();
                let else_segment = procedure.segments.get(&else_segment).unwrap();

                if condition != 0 {
                    segment = then_segment;
                } else {
                    segment = else_segment;
                }

                instruction_index = 0;
            }
            IRInstruction::InconditionalJump(segment_id) => {
                let the_segment = procedure.segments.get(&segment_id).unwrap();

                segment = the_segment;
                instruction_index = 0;
            }
            IRInstruction::Return(value_id) => {
                return state.vars.get(&value_id).unwrap().clone();
            }
        }
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;

    use crate::generation::codegen_procedure;

    use super::{RuntimeValue, run_ir};

    fn eval(src: &str) -> RuntimeValue {
        let syntax_procedure = crate::syntax::parse_procedure(src).unwrap();

        let ir = codegen_procedure(&syntax_procedure);

        let result = run_ir(ir, &[]);

        result
    }

    #[test]
    fn computes_literal_sum() {
        let result = eval(
            r#"
            int foo() {
                return 1 + 2;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(3));
    }

    #[test]
    fn computes_literal_multiplication() {
        let result = eval(
            r#"
            int foo() {
                return 3 * 5;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(15));
    }

    #[test]
    fn computes_single_variable() {
        let result = eval(
            r#"
            int foo() {
                int abc = 42;
                return abc;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(42));
    }

    #[test]
    fn computes_multiple_variables() {
        let result = eval(
            r#"
            int foo() {
                int a = 1;
                int b = a + 1;
                return b;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(2));
    }

    #[test]
    fn computes_active_branch_of_if() {
        let result = eval(
            r#"
            int foo() {
                int a = 1;
                if (a) {
                    return 10;
                }
                return 20;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(10));
    }

    #[test]
    fn computes_inactive_branch_of_if() {
        let result = eval(
            r#"
            int foo() {
                int a = 0;
                if (a) {
                    return 10;
                }
                return 20;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(20));
    }

    #[test]
    fn computes_active_branch_of_if_else() {
        let result = eval(
            r#"
            int foo() {
                int a = 1;
                if (a) {
                    return 10;
                } else {
                    return 20;
                }
                return 30;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(10));
    }

    #[test]
    fn computes_inactive_branch_of_if_else() {
        let result = eval(
            r#"
            int foo() {
                int a = 0;
                if (a) {
                    return 10;
                } else {
                    return 20;
                }
                return 30;
            }
        "#,
        );

        assert_matches!(result, RuntimeValue::Int(20));
    }
}
