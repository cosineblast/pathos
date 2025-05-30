use std::collections::{HashMap, HashSet};

use crate::syntax;

enum ValueType {
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
// 3. check if all path returns
pub fn check_module(module: &syntax::Module) -> Result<(), CheckError> {
    check_module_names(module)?;

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

fn check_module_names(module: &syntax::Module) -> Result<(), CheckError> {
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
            syntax::Expression::Literal(_) => (), // okay!
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
        };

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum CheckError {
    NameNotFound(String),
    ProcedureNotFound(String),
    ProcedureAsExpression(String),
    LocalVariableAsProcedure(String),
}
