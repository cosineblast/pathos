
use std::collections::HashMap;

use serde::Serialize;

use crate::{analysis, syntax};

type RuntimeValueType = analysis::ValueType;

struct IRModule(Vec<IRProcedure>);

pub struct IRProcedure {
    name: String,
    parameters: Vec<(String, RuntimeValueType)>,
    segments: HashMap<u64, IRSegment>,
    body_segment_id: u64,
}



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
    Deref { array: IRValueId, index: IRValueId },
    Call { name: String, args: Vec<IRValueId> }
}

pub type IRValueId = (String, u64);


#[derive(Debug,PartialEq, Eq,Clone, Copy, Hash, Serialize)]
pub struct IRSegmentId(u64);

#[derive(Debug, Clone, Serialize)]
pub struct IRSegment(Vec<IRInstruction>);

// root segment is necessarily zero
#[derive(Serialize)]
pub struct ProcedureIR {
    segments: HashMap<u64, IRSegment>,
}

pub fn codegen_procedure(procedure: &syntax::Procedure) -> ProcedureIR {
    let mut state = IRGenerationState {
        counters: HashMap::new(),
        next_segment_id: 1,
        values_types: HashMap::new(),
        target_segment: vec![],
        bindings: analysis::BindingStack::new(),
        segments: HashMap::new()
    };

    state.codegen_block(&procedure.body);

    let target_segment = std::mem::replace(&mut state.target_segment, vec![]);
    let mut segments = std::mem::replace(&mut state.segments, HashMap::new());

    segments.insert(0, IRSegment(target_segment));

    return ProcedureIR { segments }
}

struct IRGenerationState {
    counters: HashMap<String, u64>,
    next_segment_id: u64,
 
    values_types: HashMap<IRValueId, RuntimeValueType>,
    target_segment: Vec<IRInstruction>,
    bindings: analysis::BindingStack<IRValueId>,
    segments: HashMap<u64, IRSegment>
}

impl IRGenerationState {

    fn codegen_block(&mut self, block: &syntax::Block) {
        for statement in block.0.iter() {
            self.codegen_statement(statement)
        }
    }

    fn codegen_statement(&mut self, statement: &syntax::Statement) {

        match statement {
            syntax::Statement::Return(expression) => {
                let value = self.codegen_expression(expression);
                
                self.target_segment.push(
                    IRInstruction::Return(value)
                )
            },
            syntax::Statement::If(_expression, _block, _block1) => todo!(),
            syntax::Statement::While(_expression, _block) => todo!(),
            syntax::Statement::Assignment(_expression, _expression1) => todo!(),
            syntax::Statement::Declaration(_syntax_type, _, _expression) => todo!(),
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
                        index: index_id
                    })
                );

                id
            }
             
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
    fn generates_assignment_numbers() {
        let source = r#"
            int foo() {
                int a = 1;
                int b = a + 1;
                a = a * 10;
                int c = a + 1
                return c;
            }
        "#;

        let procedure = crate::syntax::parse_procedure(source).unwrap();

        let ir = codegen_procedure(&procedure);

        insta::assert_yaml_snapshot!(ir);
    }
}
