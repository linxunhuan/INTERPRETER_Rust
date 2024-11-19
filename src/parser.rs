use crate::{lexer::Lexer, token::Token};
use crate::ast::{Identifier, LetStatement, Program};
use crate::token::TokenKind;
use crate::ast::StatementNode;

struct Parser{
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors:Vec<String>,
}

impl Parser{
    fn new(lexer: Lexer) -> Self{
        let mut parser = Parser{
            lexer: lexer,
            current_token: Default::default(),
            peek_token: Default::default(),
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self){
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program>{
        let mut program = Program{statements: vec![]};

        while !self.current_token_is(TokenKind::Eof) {
            if let Some(statement) = self.parse_statement(){
                program.statements.push(statement);
            }
            self.next_token();
        }
        Some(program)
    }
    
    fn parse_statement(&mut self) -> Option<StatementNode>{
        match self.current_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            _ => None,
        }
    }
    
    fn parse_let_statement(&mut self) -> Option<StatementNode>{
        let mut stmt = LetStatement{
            token: self.current_token.clone(),
            name: Default::default(),
            value: Default::default(),
        };
        
        return if !self.expect_peek(TokenKind::Ident){
            None
        }else { 
            stmt.name = Identifier{
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            };
            
            if !self.expect_peek(TokenKind::Assign){
                None
            }else { 
                self.next_token();
                while !self.current_token_is(TokenKind::Semicolon){
                    self.next_token();
                }
                Some(StatementNode::Let(stmt))
            }
        }
    }
    
    fn expect_peek(&mut self, kind: TokenKind) -> bool{
        if self.peek_token.kind == kind {
            self.next_token();
            return true;
        }
        false
    }
    
    fn current_token_is(&self, kind: TokenKind) -> bool{
        self.current_token.kind == kind
    }
    
    fn errors(&self) -> &Vec<String>{
        &self.errors
    }
    
    fn peek_error(&mut self, expected: TokenKind){
        let msg = format!("expected next token to be {} but got {}", expected, self.peek_token.kind);
    }
    
    
}

#[cfg(test)]
mod tests{
    use crate::ast::Node;
    use super::*;

    #[test]
    fn test_parse_program(){
        let input = r#"
        let x = 5;
        let y = 10;
        let foo = 100;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        match program {
            Some(program) =>{
                assert_eq!(
                    program.statements.len(),
                    3,
                    "statements does not contain 3 statements. got {}",
                    program.statements.len()
                );
                
                let expected = vec!["x","y","foo"];
                
                for (idx,exp) in expected.into_iter().enumerate(){
                    let statement = &program.statements[idx];
                    test_let_statement(statement, exp);
                }
            }
            None => panic!("parse_program() should return a program"),
        }
    }
    
    fn test_let_statement(statement: &StatementNode, expected: &str){
        assert_eq!(
            statement.token_literal(),
            "let",
            "statement.token_literal() not 'let'. got={}",
            statement.token_literal()
        );
        match statement {
            StatementNode::Let(let_stmt) =>{
                assert_eq!(
                    let_stmt.name.value,expected,
                    "name is not '{}'. got={}",
                    expected,
                    let_stmt.name.value
                );
                assert_eq!(
                    let_stmt.name.token_literal(),
                    expected,
                    "token literal not '{}'. got={}",
                    expected,
                    let_stmt.name.token_literal()
                );
            }
        }
    }
}