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
    BinaryOperation(ArithmeticOperation, Box<Expression>, Box<Expression>)
}

#[derive(Debug, Clone, Serialize, Copy)]
pub enum ArithmeticOperation {
    Add, Subtract, Multiply, Divide
}

impl TryFrom<char> for ArithmeticOperation {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        use ArithmeticOperation as A;
        
        match value {
            '+' => Ok(A::Add),
            '-' => Ok(A::Subtract),
            '*' => Ok(A::Multiply),
            '/' => Ok(A::Divide),
            _ => Err("unknown character")
        }
    }
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
        name,
        the_type,
    }
}

fn get_parameters(params: Pair<Rule>) -> Vec<Parameter> {
    params.into_inner().map(get_parameter).collect()
}


#[derive(Debug)]
enum ExpressionItem {
    Expression(Expression),
    Operator(ArithmeticOperation),
}


fn parse_expression_items<I>(items: &mut std::iter::Peekable<I>) -> Expression
    where I: Iterator<Item=ExpressionItem> {
    return parse_expression_items_until(items, 0)
}

fn parse_expression_items_until<I>(items: &mut std::iter::Peekable<I>, min_precedence: u64) -> Expression 
    where I: Iterator<Item=ExpressionItem>
    {

    let mut left = match items.next().unwrap() {
        ExpressionItem::Expression(expr) => expr,
        ExpressionItem::Operator(_) => unreachable!(),
    };

    loop {
       let operator = *match items.peek() {
           Some(ExpressionItem::Operator(op)) => op,
           None => break,
           Some(ExpressionItem::Expression(_)) => unreachable!(),
       };

       let precedence = operator_precedence(operator);

       if precedence <= min_precedence {
           break;
       }

       items.next();

       let right = parse_expression_items_until(items, precedence);

       left = Expression::BinaryOperation(operator, Box::new(left), Box::new(right))
    }

    return left;
}

fn operator_precedence(operator: ArithmeticOperation) -> u64 {
    use ArithmeticOperation as A;
    
    match operator {
        A::Add => 6,
        A::Multiply => 8,
        A::Subtract => 6,
        A::Divide => 8
    }
}




fn get_expression(general_expression: Pair<Rule>) -> Expression {
    assert_eq!(general_expression.as_rule(), Rule::expression);

    let items =  general_expression.into_inner().map(|expr| {
        match expr.as_rule() {
            Rule::binary_operator => {
                let c =  expr.as_str().chars().next().unwrap();
                ExpressionItem::Operator(c.try_into().unwrap())
            },

            Rule::core_expression => {
                ExpressionItem::Expression(get_core_expression(expr))
            }

            _ => unreachable!()
        }
    });

    return parse_expression_items(&mut items.peekable());
}


fn get_core_expression(expr: Pair<Rule>) -> Expression {
    assert_eq!(expr.as_rule(), Rule::core_expression);

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
            let r = expr.as_str().trim().to_string();
            Expression::Name(r)
        }

        Rule::int_literal => Expression::Literal(expr.as_str().trim().parse().unwrap()),

        Rule::lookup => {
            let mut expr = expr.into_inner();

            Expression::Lookup(
                expr.next().unwrap().as_str().to_string(),
                Box::new(get_expression(expr.next().unwrap())),
            )
        }

        Rule::paren_expression => {
            let mut expr = expr.into_inner();

            get_expression(expr.next().unwrap())
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

/// Convenience function to parse a module and get the first procedure.
/// This function does allow input to contain more than one procedure.
pub fn parse_procedure(input: &str) -> Result<Procedure, Error<Rule>> {
    let mut result = parse_module(input)?;

    if result.0.get(0).is_some() {
        return Ok(result.0.swap_remove(0));
    } else {
        return Err(pest::error::Error::new_from_pos(
            pest::error::ErrorVariant::CustomError {
                message: "No procedure found in module".into(),
            },
            pest::Position::from_start(input),
        ));
    }
}

fn get_procedure(procedure: Pair<Rule>) -> Procedure {
    let mut procedure = procedure.into_inner();

    let return_type = get_type(procedure.next().unwrap());

    let name = procedure.next().unwrap().as_str().to_string();

    let parameters = get_parameters(procedure.next().unwrap());

    Procedure {
        name,
        parameters,
        return_type,
        body: get_block(procedure.next().unwrap()),
    }
}

#[cfg(test)]
mod test {
     
    use super::{parse_module, parse_procedure};

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

        insta::assert_yaml_snapshot!(result.unwrap());
    }

    #[test]
    fn test_paren_expression() {
        let result = parse_module(
            r#"
                int main() {
                    return ((foo()));
                }

                int foo() {
                    return 0;
                }
            "#,
        );

        insta::assert_yaml_snapshot!(result.unwrap());
    }

    #[test]
    fn test_underline_is_ok() {
        let result = parse_module(
            r#"
                int beep_boop() {
                    int beep_ = 1;
                    int _boop = 2;
                    return 0;
                }
            "#,
        );

        let _ = result.unwrap();
    }

    #[test]
    fn test_simple_precedence() {
        let result = parse_module(
            r#"
                int addmul() {
                    return 1 + 2 * 3 * 4 + 5;
                }
            "#,
        );

        insta::assert_yaml_snapshot!(result.unwrap());
    }

    #[test]
    fn test_subtraction_precedence() {
        let result = parse_module(
            r#"
                int subsub() {
                    return 1 - 2 - 3;
                }
            "#,
        );

        insta::assert_yaml_snapshot!(result.unwrap());
    }

    #[test]
    fn test_parse_procedure_works() {
        let result = parse_procedure(
            r#"
                int main() {
                    return 0;
                }
            "#,
        );

        let _ = result.unwrap();
    }

    #[test]
    fn test_parse_procedure_fails_with_empty_module() {

        use assert_matches::assert_matches;
        
        let result = parse_procedure(
            r#"
            "#,
        );

        assert_matches!(result, Err(_));
    }
}
