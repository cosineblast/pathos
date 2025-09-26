use std::collections::{BTreeMap, HashMap, HashSet};

use serde::{Serialize, Serializer};

use crate::{analysis, syntax};

type RuntimeValueType = analysis::ValueType;

struct IRModule(Vec<IRProcedure>);

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum IRInstruction {
    Declare(IRValueId, IRExpression),
    ConditionalJump(IRValueId, IRSegmentId, IRSegmentId),
    InconditionalJump(IRSegmentId),
    Mutate(IRValueId, IRValueId),
    Return(IRValueId),
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
    Call { name: String, args: Vec<IRValueId> },
    Phi(IRValueId, IRValueId),
}

pub type IRValueId = (String, u64);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize, PartialOrd, Ord)]
pub struct IRSegmentId(u64);

impl IRSegmentId {
    pub fn inc(self) -> Self {
        IRSegmentId(self.0 + 1)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct IRSegment(pub Vec<IRInstruction>);

// root segment is necessarily zero
#[derive(Serialize)]
pub struct IRProcedure {
    #[serde(serialize_with = "ordered_map")]
    pub segments: HashMap<IRSegmentId, IRSegment>,
    pub root_segment_id: IRSegmentId,
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
        segments: HashMap::new(),
        modifications: BTreeMap::new(),
        new_variables: HashSet::new(),
    };

    let (start, _, _) = state.codegen_block(&procedure.body);

    let segments = state.segments;

    return IRProcedure {
        segments,
        root_segment_id: start,
    };
}

struct IRGenerationState {
    counters: HashMap<String, u64>,
    current_segment_id: IRSegmentId,
    next_segment_id: IRSegmentId,

    values_types: HashMap<IRValueId, RuntimeValueType>,
    target_segment: Vec<IRInstruction>,
    bindings: analysis::BindingStack<IRValueId>,
    segments: HashMap<IRSegmentId, IRSegment>,

    new_variables: HashSet<String>,
    modifications: BTreeMap<String, IRValueId>,
}

impl IRGenerationState {
    // TODO:
    // Load arguments into variables
    fn codegen_block(
        &mut self,
        block: &syntax::Block,
    ) -> (IRSegmentId, IRSegmentId, BTreeMap<String, IRValueId>) {
        let current = std::mem::replace(&mut self.current_segment_id, self.next_segment_id);
        let target = std::mem::replace(&mut self.target_segment, vec![]);
        let vars = std::mem::replace(&mut self.new_variables, HashSet::new());
        let modifications = std::mem::replace(&mut self.modifications, BTreeMap::new());

        let next = self.next_segment_id;
        self.next_segment_id = self.next_segment_id.inc();

        self.bindings.new_frame();

        for statement in block.0.iter() {
            self.codegen_statement(statement)
        }

        self.bindings.end_frame();

        let end = std::mem::replace(&mut self.current_segment_id, current);
        let end_segment = std::mem::replace(&mut self.target_segment, target);
        let new_modifications = std::mem::replace(&mut self.modifications, modifications);
        self.new_variables = vars;

        self.segments.insert(end, IRSegment(end_segment));

        return (next, end, new_modifications);
    }

    fn codegen_if_else(
        &mut self,
        condition_value: IRValueId,
        then_block: &syntax::Block,
        else_block: &syntax::Block,
    ) {
        let (then_start, then_end, then_modified) = self.codegen_block(&then_block);
        let (else_start, else_end, else_modified) = self.codegen_block(&else_block);

        self.target_segment.push(IRInstruction::ConditionalJump(
            condition_value,
            then_start,
            else_start,
        ));

        self.switch_to_new_segment();

        self.link_segments_to_current(&[then_end, else_end]);

        self.codegen_two_branch_then_phi(&then_modified, &else_modified);
        self.codegen_two_branch_else_phi(&then_modified, &else_modified);
    }

    fn link_segments_to_current(&mut self, ids: &[IRSegmentId]) {
        for id in ids {
            self.segments
                .get_mut(id)
                .unwrap()
                .0
                .push(IRInstruction::InconditionalJump(self.current_segment_id));
        }
    }

    fn codegen_two_branch_then_phi(
        &mut self,
        then_modified: &BTreeMap<String, IRValueId>,
        else_modified: &BTreeMap<String, IRValueId>,
    ) {
        for (name, then_value) in then_modified.iter() {
            let else_value = if else_modified.contains_key(name) {
                else_modified.get(name).unwrap()
            } else {
                self.bindings.binding_of(name).unwrap()
            }
            .clone();

            let new_value = self.new_value(name);

            self.bindings.push_binding(name, new_value.clone());

            self.target_segment.push(IRInstruction::Declare(
                new_value.clone(),
                IRExpression::Phi(then_value.clone(), else_value.clone()),
            ));

            if !self.new_variables.contains(name) {
                self.modifications.insert(name.clone(), new_value);
            }
        }
    }

    fn codegen_two_branch_else_phi(
        &mut self,
        then_modified: &BTreeMap<String, IRValueId>,
        else_modified: &BTreeMap<String, IRValueId>,
    ) {
        for (name, else_value) in else_modified.iter() {
            if !then_modified.contains_key(name) {
                let old_value = self.bindings.binding_of(name).unwrap().clone();

                let new_value = self.new_value(name);

                self.bindings.push_binding(name, new_value.clone());

                self.target_segment.push(IRInstruction::Declare(
                    new_value.clone(),
                    IRExpression::Phi(old_value.clone(), else_value.clone()),
                ));

                if !self.new_variables.contains(name) {
                    self.modifications.insert(name.clone(), new_value);
                }
            }
        }
    }

    fn codegen_if(&mut self, condition_value: IRValueId, then_block: &syntax::Block) {
        let (then_start, then_end, then_modified) = self.codegen_block(&then_block);

        self.target_segment.push(IRInstruction::ConditionalJump(
            condition_value,
            then_start,
            self.next_segment_id,
        ));

        self.switch_to_new_segment();

        self.link_segments_to_current(&[then_end]);

        self.codegen_two_branch_then_phi(&then_modified, &BTreeMap::new());
    }

    fn switch_to_new_segment(&mut self) {
        let current = std::mem::replace(&mut self.current_segment_id, self.next_segment_id);
        let target = std::mem::replace(&mut self.target_segment, vec![]);
        self.next_segment_id = self.next_segment_id.inc();
        self.segments.insert(current, IRSegment(target));
    }

    fn codegen_conditional(
        &mut self,
        condition: &syntax::Expression,
        then_block: &syntax::Block,
        else_block: &Option<Box<syntax::Block>>,
    ) {
        let condition_value = self.codegen_expression(condition, None);

        if let Some(else_block) = else_block {
            self.codegen_if_else(condition_value, then_block, else_block);
        } else {
            self.codegen_if(condition_value, then_block);
        }
    }

    fn codegen_while(&mut self, condition: &syntax::Expression, block: &syntax::Block) {
        let (body_start, body_end, body_modifed) = self.codegen_block(block);

        let body_end_segment = self.segments.get_mut(&body_end).unwrap();

        for (name, new_value) in body_modifed {
            let previous = self.bindings.binding_of(&name).unwrap();

            body_end_segment
                .0
                .push(IRInstruction::Mutate(previous.clone(), new_value.clone()));
        }

        let condition_segment_id = self.next_segment_id;

        self.target_segment
            .push(IRInstruction::InconditionalJump(condition_segment_id));

        body_end_segment
            .0
            .push(IRInstruction::InconditionalJump(condition_segment_id));

        // now, the condition segment
        self.switch_to_new_segment();

        let final_segment_id = self.next_segment_id;

        let condition_value = self.codegen_expression(condition, None);

        self.target_segment.push(IRInstruction::ConditionalJump(
            condition_value,
            body_start,
            final_segment_id,
        ));

        self.switch_to_new_segment();
    }

    fn codegen_statement(&mut self, statement: &syntax::Statement) {
        match statement {
            syntax::Statement::Return(expression) => {
                let value = self.codegen_expression(expression, None);

                self.target_segment.push(IRInstruction::Return(value))
            }
            syntax::Statement::If(condition, then_block, else_block) => {
                self.codegen_conditional(condition, then_block, else_block)
            }
            syntax::Statement::While(condition, block) => self.codegen_while(condition, block),
            syntax::Statement::Assignment(target, expression) => {
                let name = match target {
                    syntax::Expression::Name(name) => name,
                    syntax::Expression::Lookup(_, _) => todo!(),
                    _ => todo!(),
                };

                let new_value = self.new_value(name);

                let expr_value = self.codegen_expression(expression, Some(new_value.clone()));

                assert_eq!(expr_value, new_value);

                self.bindings.push_binding(name, new_value);

                if !self.new_variables.contains(name) {
                    self.modifications.insert(name.clone(), expr_value);
                }
            }
            syntax::Statement::Declaration(_syntax_type, name, expression) => {
                let new_value = self.new_value(name);

                let expr_value = self.codegen_expression(expression, Some(new_value.clone()));

                assert_eq!(expr_value, new_value);

                self.bindings.push_binding(name.as_str(), new_value.clone());

                self.new_variables.insert(name.clone());
            }
        }
    }

    // if target is some, the compiled expression will necessarily be compiled into the target id,
    // otherwise, an arbitray name may be generated
    fn codegen_expression(
        &mut self,
        expression: &syntax::Expression,
        target: Option<IRValueId>,
    ) -> IRValueId {
        match expression {
            syntax::Expression::Literal(value) => {
                let id = target.unwrap_or_else(|| self.new_value("$lit"));

                self.target_segment.push(IRInstruction::Declare(
                    id.clone(),
                    IRExpression::Literal(*value),
                ));

                id
            }
            syntax::Expression::Name(name) => {
                let current = self.bindings.binding_of(&name).unwrap().clone();

                if let Some(target) = target {
                    self.target_segment.push(IRInstruction::Declare(
                        target.clone(),
                        IRExpression::AnotherValue(current),
                    ));

                    target
                } else {
                    current
                }
            }
            syntax::Expression::Call(procedure, args) => {
                let id = target.unwrap_or_else(|| self.new_value("$call"));

                let args = args
                    .iter()
                    .map(|expr| self.codegen_expression(expr, None))
                    .collect();

                self.target_segment.push(IRInstruction::Declare(
                    id.clone(),
                    IRExpression::Call {
                        name: procedure.clone(),
                        args,
                    },
                ));

                id
            }
            syntax::Expression::Lookup(array_name, index_expression) => {
                let id = target.unwrap_or_else(|| self.new_value("$lookup"));
                let array_id = self.bindings.binding_of(array_name).unwrap().clone();
                let index_id = self.codegen_expression(&index_expression, None);

                self.target_segment.push(IRInstruction::Declare(
                    id.clone(),
                    IRExpression::Deref {
                        array: array_id,
                        index: index_id,
                    },
                ));

                id
            }
            syntax::Expression::BinaryOperation(operation, left, right) => {
                let left_value = self.codegen_expression(&left, None);
                let right_value = self.codegen_expression(&right, None);

                let (ir_expression, name) = Self::get_ir_expression_for_binary_operation(
                    *operation,
                    left_value,
                    right_value,
                );
                let id = target.unwrap_or_else(|| self.new_value(name));

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
        let counter = self
            .counters
            .entry(base.to_string())
            .and_modify(|it| *it += 1)
            .or_insert_with(|| 0);

        return (base.into(), *counter);
    }
}
