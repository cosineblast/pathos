
use std::collections::{BTreeMap, HashMap};

use serde::{Serialize, Serializer};

use crate::{analysis, syntax};

type RuntimeValueType = analysis::ValueType;

struct IRModule(Vec<IRProcedure>);

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum IRInstruction {
    Declare(IRValueId, IRExpression),
    ConditionalJump(IRValueId, IRSegmentId, IRSegmentId),
    InconditionalJump(IRSegmentId),
    Return(IRValueId)
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum IRExpression {
    Literal(i64),
    AnotherValue(IRValueId),
    Add(IRValueId, IRValueId),
    Sub(IRValueId, IRValueId),
    Mul(IRValueId, IRValueId),
    Div(IRValueId, IRValueId),
    Deref { array: IRValueId, index: IRValueId },
    Call { name: String, args: Vec<IRValueId> }
}

pub type IRValueId = (String, u64);


#[derive(Debug,PartialEq, Eq,Clone, Copy, Hash, Serialize, PartialOrd, Ord)]
pub struct IRSegmentId(u64);

impl IRSegmentId {
    fn inc(self) -> Self {
        IRSegmentId(self.0 + 1)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct IRSegment(Vec<IRInstruction>);

// root segment is necessarily zero
#[derive(Serialize)]
pub struct IRProcedure {
    #[serde(serialize_with = "ordered_map")]
    segments: HashMap<IRSegmentId, IRSegment>,
    root_segment_id: IRSegmentId,
}


fn ordered_map<S, K: Ord + Serialize, V: Serialize>(
    value: &HashMap<K, V>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let ordered: BTreeMap<_, _> = value.iter().collect();
    ordered.serialize(serializer)
}

pub fn codegen_procedure(procedure: &syntax::Procedure) -> IRProcedure {
    let mut state = IRGenerationState {
        counters: HashMap::new(),
        current_segment_id: IRSegmentId(0),
        next_segment_id: IRSegmentId(1),
        values_types: HashMap::new(),
        target_segment: vec![],
        bindings: analysis::BindingStack::new(),
        segments: HashMap::new()
    };

    let (start, _) = state.codegen_block(&procedure.body);

    let segments = state.segments;
    
    return IRProcedure { segments, root_segment_id: start }
}

struct IRGenerationState {
    counters: HashMap<String, u64>,
    current_segment_id: IRSegmentId,
    next_segment_id: IRSegmentId,
 
    values_types: HashMap<IRValueId, RuntimeValueType>,
    target_segment: Vec<IRInstruction>,
    bindings: analysis::BindingStack<IRValueId>,
    segments: HashMap<IRSegmentId, IRSegment>
}

impl IRGenerationState {

// TODO:
// Load arguments into variables
    fn codegen_block(&mut self, block: &syntax::Block) -> (IRSegmentId, IRSegmentId) {

        let current = std::mem::replace(&mut self.current_segment_id, self.next_segment_id);
        let target = std::mem::replace(&mut self.target_segment, vec![]);

        let next = self.next_segment_id;
        self.next_segment_id = self.next_segment_id.inc();
        
        self.bindings.new_frame();
        
        for statement in block.0.iter() {
            self.codegen_statement(statement)
        }

        self.bindings.end_frame();

        let end = std::mem::replace(&mut self.current_segment_id, current);
        let end_segment = std::mem::replace(&mut self.target_segment, target);

        self.segments.insert(end, IRSegment(end_segment));

        return (next, end)
    }

    fn codegen_statement(&mut self, statement: &syntax::Statement) {

        match statement {
            syntax::Statement::Return(expression) => {
                let value = self.codegen_expression(expression);
                
                self.target_segment.push(
                    IRInstruction::Return(value)
                )
            },
            syntax::Statement::If(condition, then_block, else_block) => {

                let condition_value = self.codegen_expression(condition);

                if let Some(else_block) = else_block {
                    let (then_start, then_end) = self.codegen_block(&then_block);
                    let (else_start, else_end) = self.codegen_block(&else_block);

                    self.target_segment.push(
                        IRInstruction::ConditionalJump(condition_value, then_start, else_start)
                    );

                    let current = std::mem::replace(&mut self.current_segment_id, self.next_segment_id);
                    let target = std::mem::replace(&mut self.target_segment, vec![]);

                    self.segments.insert(current, IRSegment(target));

                    // self.current_segment_id is now the final segment 
                    
                    self.segments.get_mut(&then_end).unwrap().0.push(
                        IRInstruction::InconditionalJump(self.current_segment_id)
                    );

                    self.segments.get_mut(&else_end).unwrap().0.push(
                        IRInstruction::InconditionalJump(self.current_segment_id)
                    );
                } else {
                    let (then_start, then_end) = self.codegen_block(&then_block);

                    self.target_segment.push(
                        IRInstruction::ConditionalJump(condition_value, then_start, self.next_segment_id)
                    );

                    let current = std::mem::replace(&mut self.current_segment_id, self.next_segment_id);
                    let target = std::mem::replace(&mut self.target_segment, vec![]);

                    self.segments.insert(current, IRSegment(target));

                    self.segments.get_mut(&then_end).unwrap().0.push(
                        IRInstruction::InconditionalJump(self.current_segment_id)
                    );
                }
            },
            syntax::Statement::While(_expression, _block) => todo!(),
            syntax::Statement::Assignment(target, expression) => {
                let name = match target {
                    syntax::Expression::Name(name) => name,
                    syntax::Expression::Lookup(_, _) => todo!(),
                    _ => todo!()
                };

                let expr_value = self.codegen_expression(expression);

                let new_value = self.new_value(name);

                let bound_value = self.bindings.binding_of_mut(name).unwrap();
                *bound_value = new_value.clone();

                self.target_segment.push(
                    IRInstruction::Declare(new_value, IRExpression::AnotherValue(expr_value))
                );
                
            },
            syntax::Statement::Declaration(_syntax_type, name, expression) => {
                let expr_value = self.codegen_expression(expression);

                let new_value = self.new_value(name);

                self.bindings.push_binding(name.as_str(), new_value.clone());

                self.target_segment.push(
                    IRInstruction::Declare(new_value, IRExpression::AnotherValue(expr_value))
                );
            },
        }
    }

    fn codegen_expression(&mut self, expression: &syntax::Expression) -> IRValueId {
        
        match expression {
            syntax::Expression::Literal(value) => {
                let id = self.new_value("$lit");

                self.target_segment.push(
                    IRInstruction::Declare(id.clone(), IRExpression::Literal(*value))
                );

                id
            },
            syntax::Expression::Name(name) => {
                self.bindings.binding_of(&name).unwrap().clone()
            },
            syntax::Expression::Call(procedure, args) => {
                let id = self.new_value("$call");

                let args = args.iter().map(|expr| self.codegen_expression(expr)).collect();

                self.target_segment.push(
                    IRInstruction::Declare(id.clone(), IRExpression::Call {
                         name: procedure.clone(),
                         args 
                    })
                );

                id
            },
            syntax::Expression::Lookup(array_name, index_expression) => {
                let id = self.new_value("$lookup");
                let array_id = self.bindings.binding_of(array_name).unwrap().clone();
                let index_id = self.codegen_expression(&index_expression);

                self.target_segment.push(
                    IRInstruction::Declare(id.clone(), IRExpression::Deref{
                        array: array_id,
                        index: index_id,
                    },
                ));

                id
            }
            syntax::Expression::BinaryOperation(operation, left, right) => {
                let left_value = self.codegen_expression(&left);
                let right_value = self.codegen_expression(&right);

                let (ir_expression, name) = Self::get_ir_expression_for_binary_operation(
                    *operation,
                    left_value,
                    right_value,
                );
                let id = self.new_value(name);

                self.target_segment
                    .push(IRInstruction::Declare(id.clone(), ir_expression));

                id
            }
        }
    }

    fn get_ir_expression_for_binary_operation(
        operator: syntax::ArithmeticOperation,
        left: IRValueId,
        right: IRValueId,
    ) -> (IRExpression, &'static str) {
        use syntax::ArithmeticOperation as Op;

        match operator {
            Op::Add => (IRExpression::Add(left, right), "$add"),
            Op::Subtract => (IRExpression::Sub(left, right), "$sub"),
            Op::Multiply => (IRExpression::Mul(left, right), "$mul"),
            Op::Divide => (IRExpression::Div(left, right), "$div"),
        }
    }

    fn new_value(&mut self, base: &str) -> IRValueId {
        let counter = self.counters.entry(base.to_string())
            .and_modify(|it| *it += 1)
            .or_insert_with(|| 0);

        return (base.into(), *counter)
    }
}

#[cfg(test)]
mod test {
    use super::codegen_procedure;


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
}
