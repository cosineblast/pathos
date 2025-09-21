use pest::{Parser, error::Error, iterators::Pair};
use pest_derive::Parser;

use serde::Serialize;

#[derive(Parser)]
#[grammar = "syntax.pest"]
pub struct TheParser;

#[derive(Debug, Clone, Serialize)]
pub struct Module(pub Vec<Procedure>);

#[derive(Debug, Clone, Serialize)]
pub struct Procedure {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: SyntaxType,
    pub body: Block,
}

#[derive(Debug, Clone, Serialize)]
pub struct Parameter {
    pub name: String,
    pub the_type: SyntaxType,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum SyntaxType {
    Int,
    Array,
}

#[derive(Debug, Clone, Serialize)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone, Serialize)]
pub enum Statement {
    Return(Expression),
    If(Expression, Box<Block>, Option<Box<Block>>),
    While(Expression, Box<Block>),
    Assignment(Expression, Expression),
    Declaration(SyntaxType, String, Expression),
}

#[derive(Debug, Clone, Serialize)]
pub enum Expression {
    Literal(i64),
    Name(String),
    Call(String, Vec<Expression>),
    Lookup(String, Box<Expression>),
}

fn get_type(input: Pair<Rule>) -> SyntaxType {
    match input.as_str() {
        "int" => SyntaxType::Int,
        "array" => SyntaxType::Array,
        _ => {
            eprintln!("{}", input.as_str());
            unreachable!()
        }
    }
}

fn get_parameter(param: Pair<Rule>) -> Parameter {
    let mut param = param.into_inner();

    let the_type = get_type(param.next().unwrap());
    let name = param.next().unwrap().as_str().to_string();

    Parameter {
        name: name,
        the_type,
    }
}

fn get_parameters(params: Pair<Rule>) -> Vec<Parameter> {
    params.into_inner().map(get_parameter).collect()
}

fn get_expression(expr: Pair<Rule>) -> Expression {
    assert_eq!(expr.as_rule(), Rule::expression);

    let expr = expr.into_inner().next().unwrap();
    let rule = expr.as_rule();

    match rule {
        Rule::call => {
            let mut expr = expr.into_inner();

            Expression::Call(
                expr.next().unwrap().as_str().to_string(),
                expr.map(get_expression).collect(),
            )
        }

        Rule::name => {
            let r = expr.as_str().to_string();
            Expression::Name(r)
        }

        Rule::int_literal => Expression::Literal(expr.as_str().parse().unwrap()),

        Rule::lookup => {
            let mut expr = expr.into_inner();

            Expression::Lookup(
                expr.next().unwrap().as_str().to_string(),
                Box::new(get_expression(expr.next().unwrap())),
            )
        }

        unexpected => {
            eprintln!("unexpected {:?}", unexpected);
            unreachable!();
        }
    }
}

fn get_statement(statement: Pair<Rule>) -> Statement {
    assert_eq!(statement.as_rule(), Rule::statement);

    let statement = statement.into_inner().next().unwrap();
    let rule = statement.as_rule();
    let mut statement = statement.into_inner();

    match rule {
        Rule::declaration => {
            let the_type = get_type(statement.next().unwrap());

            let name = statement.next().unwrap().as_str().trim().to_string();

            let expression = get_expression(statement.next().unwrap());

            Statement::Declaration(the_type, name, expression)
        }

        Rule::assignment => {
            let left = get_expression(statement.next().unwrap());

            let right = get_expression(statement.next().unwrap());

            Statement::Assignment(left, right)
        }

        Rule::r#return => Statement::Return(get_expression(statement.next().unwrap())),

        Rule::r#if => {
            let condition = get_expression(statement.next().unwrap());

            let then = get_block(statement.next().unwrap());

            let r#else = statement.next().map(get_block);

            Statement::If(condition, Box::new(then), r#else.map(Box::new))
        }

        Rule::r#while => {
            let condition = get_expression(statement.next().unwrap());

            let block = get_block(statement.next().unwrap());

            Statement::While(condition, Box::new(block))
        }

        unexpected => {
            eprintln!("unexpected {:?}", unexpected);
            unreachable!();
        }
    }
}

fn get_block(body: Pair<Rule>) -> Block {
    Block(body.into_inner().map(get_statement).collect())
}

pub fn parse_module(input: &str) -> Result<Module, Error<Rule>> {
    let file = TheParser::parse(Rule::file, input)?.next().unwrap();

    let mut result = vec![];

    for procedure in file.into_inner() {
        match procedure.as_rule() {
            Rule::procedure => {
                result.push(get_procedure(procedure));
            }

            Rule::EOI => {}

            _ => unreachable!(),
        }
    }

    Ok(Module(result))
}

pub fn parse_procedure(input: &str) -> Result<Procedure, Error<Rule>> {
    let result = TheParser::parse(Rule::procedure, input)?.next().unwrap();

    Ok(get_procedure(result))
}

fn get_procedure(procedure: Pair<Rule>) -> Procedure {
    let mut procedure = procedure.into_inner();

    let return_type = get_type(procedure.next().unwrap());

    let name = procedure.next().unwrap().as_str().to_string();

    let parameters = get_parameters(procedure.next().unwrap());

    Procedure {
        name: name,
        parameters: parameters,
        return_type: return_type,
        body: get_block(procedure.next().unwrap()),
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;

    use crate::syntax::{Module, Procedure};

    use super::parse_module;

    #[test]
    fn test_complex_module() {
        let result = parse_module(
            r#"
                int main() {
                    array f = foo();

                    int a = 10;
                    int b = 20;

                    if (a) {
                        return b;
                    } else {
                        return a;
                    }
                }

                array foo() {
                    
                }
            "#,
        );

        // TODO: insta goes here
    }
}
