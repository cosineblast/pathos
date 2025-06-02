use std::collections::{HashMap, HashSet};

use crate::syntax;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ValueType {
    Int,
    Array,
}

struct ProcedureType {
    parameters: Vec<ValueType>,
    return_type: ValueType,
}

impl From<&syntax::Procedure> for ProcedureType {
    fn from(procedure: &syntax::Procedure) -> Self {
        ProcedureType {
            parameters: procedure
                .parameters
                .iter()
                .map(|parameter| parameter.the_type.into())
                .collect(),
            return_type: procedure.return_type.into(),
        }
    }
}

impl From<syntax::SyntaxType> for ValueType {
    fn from(value: syntax::SyntaxType) -> Self {
        match value {
            syntax::SyntaxType::Int => ValueType::Int,
            syntax::SyntaxType::Array => ValueType::Array,
        }
    }
}

// 1. check if all names exist
// 2. check if all types match
// 3. check if mutability of parameters is respected
// 4. check if all path return
pub fn full_check(module: &syntax::Module) -> Result<(), CheckError> {
    name_check(module)?;
    type_check(module)?;

    Ok(())
}

pub fn name_check(module: &syntax::Module) -> Result<(), CheckError> {
    let mut state = NameAnalysisState {
        stack: NameStack::new(),
        procedure_names: module
            .0
            .iter()
            .map(|procedure| procedure.name.to_string())
            .collect(),
    };

    for procedure in module.0.iter() {
        state.check_procedure_names(procedure)?;
    }

    Ok(())
}

pub fn type_check(module: &syntax::Module) -> Result<(), CheckError> {
    let mut procedure_types: HashMap<String, ProcedureType> = module
        .0
        .iter()
        .map(|procedure| (procedure.name.to_string(), procedure.into()))
        .collect();

    for procedure in module.0.iter() {
        let mut state = TypeAnalysisState {
            procedure_types,
            types: TypeStack::new(),
            expected_return_type: procedure.return_type.into(),
            current_procedure_name: procedure.name.clone(),
        };

        state.check_procedure(procedure)?;

        procedure_types = state.procedure_types;
    }

    Ok(())
}

struct NameStack {
    all_names: HashMap<String, usize>,
    past_frames: Vec<Vec<String>>,
    active_frame: Vec<String>,
}

impl NameStack {
    fn new() -> Self {
        NameStack {
            all_names: HashMap::new(),
            past_frames: vec![],
            active_frame: vec![],
        }
    }

    fn new_frame(&mut self) {
        let active_frame = std::mem::replace(&mut self.active_frame, vec![]);
        self.past_frames.push(active_frame);
    }

    fn push_name(&mut self, name: &str) {
        self.all_names
            .entry(name.to_string())
            .and_modify(|it| *it += 1)
            .or_insert(1);

        self.active_frame.push(name.to_string());
    }

    fn contains(&mut self, name: &str) -> bool {
        self.all_names.get(name).is_some()
    }

    fn end_frame(&mut self) {
        for name in self.active_frame.iter() {
            match self.all_names.get_mut(name).expect("expected name") {
                0 => panic!("zero-count in name-stack"),
                1 => {
                    self.all_names.remove(name);
                }
                value => {
                    *value -= 1;
                }
            };
        }

        self.active_frame = self.past_frames.pop().unwrap_or_else(|| vec![]);
    }
}

struct NameAnalysisState {
    stack: NameStack,
    procedure_names: HashSet<String>,
}

impl NameAnalysisState {
    fn check_procedure_names(
        self: &mut NameAnalysisState,
        procedure: &syntax::Procedure,
    ) -> Result<(), CheckError> {
        self.stack.new_frame();

        for parameter in procedure.parameters.iter() {
            self.stack.push_name(&parameter.name);
        }

        self.check_block(&procedure.body)?;

        self.stack.end_frame();

        Ok(())
    }

    fn check_block(self: &mut NameAnalysisState, block: &syntax::Block) -> Result<(), CheckError> {
        self.stack.new_frame();

        for statement in block.0.iter() {
            self.check_statement(statement)?;
        }

        self.stack.end_frame();

        Ok(())
    }

    fn check_statement(
        self: &mut NameAnalysisState,
        statement: &syntax::Statement,
    ) -> Result<(), CheckError> {
        match statement {
            syntax::Statement::Return(expression) => self.check_expression(expression)?,
            syntax::Statement::If(expression, then_block, else_block) => {
                self.check_expression(expression)?;
                self.check_block(&then_block)?;

                if let Some(else_block) = else_block {
                    self.check_block(&else_block)?;
                }
            }
            syntax::Statement::While(expression, block) => {
                self.check_expression(expression)?;
                self.check_block(block)?;
            }
            syntax::Statement::Assignment(target, value) => {
                self.check_expression(target)?;
                self.check_expression(value)?;
            }
            syntax::Statement::Declaration(_type, name, expression) => {
                self.stack.push_name(&name);
                self.check_expression(expression)?;
            }
        };

        Ok(())
    }

    fn check_expression(
        self: &mut NameAnalysisState,
        expression: &syntax::Expression,
    ) -> Result<(), CheckError> {
        match expression {
            syntax::Expression::Literal(_) => (),
            syntax::Expression::Name(name) => {
                if !self.stack.contains(name) {
                    if self.procedure_names.contains(name) {
                        return Err(CheckError::ProcedureAsExpression(name.clone()));
                    } else {
                        return Err(CheckError::NameNotFound(name.clone()));
                    }
                }
            }
            syntax::Expression::Call(procedure_name, arguments) => {
                if !self.procedure_names.contains(procedure_name) {
                    if self.stack.contains(&procedure_name) {
                        return Err(CheckError::LocalVariableAsProcedure(procedure_name.clone()));
                    } else {
                        return Err(CheckError::ProcedureNotFound(procedure_name.clone()));
                    }
                }

                for argument in arguments.iter() {
                    self.check_expression(argument)?;
                }
            }
            syntax::Expression::Lookup(name, expression) => {
                if !self.stack.contains(name) {
                    if self.procedure_names.contains(name) {
                        return Err(CheckError::ProcedureAsExpression(name.clone()));
                    } else {
                        return Err(CheckError::NameNotFound(name.clone()));
                    }
                }

                self.check_expression(expression)?;
            }
        };

        Ok(())
    }
}

#[derive(Debug)]
pub enum CheckError {
    NameNotFound(String),
    ProcedureNotFound(String),
    ProcedureAsExpression(String),
    LocalVariableAsProcedure(String),

    ReturnTypeMismatch {
        procedure_name: String,
        expression: syntax::Expression,
        expected: ValueType,
        got: ValueType,
    },

    ArrayTypeInIfConditional {
        procedure_name: String,
        expression: syntax::Expression,
    },

    ArrayTypeInWhileConditional {
        procedure_name: String,
        expression: syntax::Expression,
    },
    AssignmentTypeMismatch {
        procedure_name: String,
        target_name: String,
        expression: syntax::Expression,
        expected: ValueType,
        got: ValueType,
    },
    CallTypeMismatch {
        procedure_name: String,
        expression: syntax::Expression,
        argument_index: usize,
        expected: ValueType,
        got: ValueType,
    },
    TriedToIndexInt {
        procedure_name: String,
        value_name: String,
    },
    TriedToUseArrayAsIndex {
        procedure_name: String,
        expression: Box<syntax::Expression>,
    },
}

struct TypeStack {
    all_types: HashMap<String, Vec<ValueType>>,
    past_frames: Vec<Vec<String>>,
    active_frame: Vec<String>,
}

impl TypeStack {
    fn new() -> Self {
        TypeStack {
            all_types: HashMap::new(),
            past_frames: vec![],
            active_frame: vec![],
        }
    }

    fn new_frame(&mut self) {
        let active_frame = std::mem::replace(&mut self.active_frame, vec![]);
        self.past_frames.push(active_frame);
    }

    fn push_type_binding(&mut self, name: &str, ty: ValueType) {
        self.all_types
            .entry(name.to_string())
            .and_modify(|it| it.push(ty))
            .or_insert_with(|| vec![ty]);

        self.active_frame.push(name.to_string());
    }

    fn type_of(&self, name: &str) -> Option<ValueType> {
        self.all_types
            .get(name)
            .map(|stack| *stack.last().expect("no type associated with name in map"))
    }

    fn end_frame(&mut self) {
        for name in self.active_frame.iter() {
            let stack = self.all_types.get_mut(name).expect("expected name");

            match stack.len() {
                0 => panic!("empty type stack in map"),
                1 => {
                    self.all_types.remove(name);
                }
                _ => {
                    stack.pop();
                }
            };
        }

        self.active_frame = self.past_frames.pop().unwrap_or_else(|| vec![]);
    }
}

struct TypeAnalysisState {
    procedure_types: HashMap<String, ProcedureType>,
    types: TypeStack,
    expected_return_type: ValueType,
    current_procedure_name: String,
}

impl TypeAnalysisState {
    fn check_procedure(&mut self, procedure: &syntax::Procedure) -> Result<(), CheckError> {
        // TODO: do this when initializing state instead
        let procedure_type: ProcedureType = procedure.into();

        self.expected_return_type = procedure_type.return_type;

        self.current_procedure_name = procedure.name.clone();

        for parameter in procedure.parameters.iter() {
            self.types
                .push_type_binding(&parameter.name, parameter.the_type.into());
        }

        self.check_block(&procedure.body)?;

        Ok(())
    }

    fn check_block(&mut self, block: &syntax::Block) -> Result<(), CheckError> {
        for statement in block.0.iter() {
            self.check_statement(statement)?;
        }

        Ok(())
    }

    fn check_statement(&mut self, statement: &syntax::Statement) -> Result<(), CheckError> {
        match statement {
            syntax::Statement::Return(expression) => {
                let inferred_type = self.infer(expression)?;

                if inferred_type != self.expected_return_type {
                    return Err(CheckError::ReturnTypeMismatch {
                        procedure_name: self.current_procedure_name.clone(),
                        expression: expression.clone(),
                        expected: self.expected_return_type,
                        got: inferred_type,
                    });
                }
            }
            syntax::Statement::If(expression, then_block, else_block) => {
                let inferred_type = self.infer(expression)?;

                if inferred_type != ValueType::Int {
                    return Err(CheckError::ArrayTypeInIfConditional {
                        procedure_name: self.current_procedure_name.clone(),
                        expression: expression.clone(),
                    });
                }

                self.check_block(then_block)?;

                if let Some(else_block) = else_block {
                    self.check_block(else_block)?;
                }
            }
            syntax::Statement::While(expression, block) => {
                let inferred_type = self.infer(expression)?;

                if inferred_type != ValueType::Int {
                    return Err(CheckError::ArrayTypeInWhileConditional {
                        procedure_name: self.current_procedure_name.clone(),
                        expression: expression.clone(),
                    });
                }

                self.check_block(block)?;
            }

            syntax::Statement::Assignment(target, expression) => match target {
                syntax::Expression::Name(target_name) => {
                    let target_type = self
                        .types
                        .type_of(target_name)
                        .expect("name checking failed");
                    let value_type = self.infer(expression)?;

                    if target_type != value_type {
                        return Err(CheckError::AssignmentTypeMismatch {
                            procedure_name: self.current_procedure_name.clone(),
                            target_name: target_name.clone(),
                            expression: expression.clone(),
                            expected: target_type,
                            got: value_type,
                        });
                    }
                }
                _ => todo!(),
            },
            syntax::Statement::Declaration(ty, name, expression) => {
                let ty: ValueType = (*ty).into();
                let inferred_type = self.infer(expression)?;

                if ty != inferred_type {
                    return Err(CheckError::AssignmentTypeMismatch {
                        procedure_name: self.current_procedure_name.clone(),
                        target_name: name.clone(),
                        expression: expression.clone(),
                        expected: ty,
                        got: inferred_type,
                    });
                }

                self.types.push_type_binding(name, ty);
            }
        }

        Ok(())
    }

    fn infer(&self, expression: &syntax::Expression) -> Result<ValueType, CheckError> {
        match expression {
            syntax::Expression::Literal(_) => Ok(ValueType::Int),
            syntax::Expression::Name(name) => {
                Ok(self.types.type_of(name).expect("name check failed"))
            }
            syntax::Expression::Call(procedure_name, expressions) => {
                let procedure_type = self
                    .procedure_types
                    .get(procedure_name)
                    .expect("name check failed");

                for (i, (parameter_type, argument_expression)) in procedure_type
                    .parameters
                    .iter()
                    .zip(expressions.iter())
                    .enumerate()
                {
                    let inferred = self.infer(argument_expression)?;

                    if *parameter_type != inferred {
                        return Err(CheckError::CallTypeMismatch {
                            procedure_name: self.current_procedure_name.clone(),
                            expression: expression.clone(),
                            argument_index: i,
                            expected: parameter_type.clone(),
                            got: inferred,
                        });
                    }
                }

                Ok(procedure_type.return_type)
            }
            syntax::Expression::Lookup(name, expression) => {
                let value_type = self.types.type_of(name).expect("name check failed");

                if value_type != ValueType::Array {
                    return Err(CheckError::TriedToIndexInt {
                        procedure_name: self.current_procedure_name.clone(),
                        value_name: name.clone(),
                    });
                }

                let expression_type = self.infer(expression)?;

                if expression_type != ValueType::Int {
                    return Err(CheckError::TriedToUseArrayAsIndex {
                        procedure_name: self.current_procedure_name.clone(),
                        expression: expression.clone(),
                    });
                }

                Ok(ValueType::Int)
            }
        }
    }
}
