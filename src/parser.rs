use std::collections::HashMap;
use std::any;
use crate::{
    ast::*,
    lexer::Lexer,
    token::{Token,TokenKind},
};

type PrefixParseFn = fn(parser:&mut Parser) -> Option<ExpressionNode>;
type InfixParseFn = fn(parser:&mut Parser, exp: ExpressionNode) -> Option<ExpressionNode>;

#[derive(Debug, Clone, Copy)]
enum PrecedenceLevel{
    Lowest          = 0,
    Equals          = 1,    // ==
    LessGreater     = 2,    // > < 
    Sum             = 3,    // +
    Product         = 4,
    Prefix          = 5,
    Call            = 6,
}

fn precedence_map(kind:&TokenKind) -> PrecedenceLevel{
    return match kind{
        TokenKind::Eq => PrecedenceLevel::Equals,
        TokenKind::NotEq => PrecedenceLevel::Equals,
        TokenKind::Lt => PrecedenceLevel::LessGreater,
        TokenKind::Gt => PrecedenceLevel::LessGreater,
        TokenKind::Plus => PrecedenceLevel::Sum,
        TokenKind::Minus => PrecedenceLevel::Sum,
        TokenKind::Slash => PrecedenceLevel::Product,
        TokenKind::Asterisk => PrecedenceLevel::Product,
        _ => PrecedenceLevel::Lowest,
    };
}


struct Parser{
    lexer: Lexer,           // 词法分析器，解析出token
    current_token: Token,   // 当前处理的token
    peek_token: Token,      // 下一个token
    errors:Vec<String>,     // 存储错误信息
    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn>,
}

impl Parser{
    fn new(lexer: Lexer) -> Self{
        let mut parser = Parser{
            lexer: lexer,
            current_token: Default::default(),
            peek_token: Default::default(),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        
        parser.register_prefix(TokenKind::Ident, Self::parse_identifier);
        parser.register_prefix(TokenKind::Int, Self::parse_integer_literal);
        parser.register_prefix(TokenKind::Bang, Self::parse_prefix_expression);
        parser.register_prefix(TokenKind::Minus, Self::parse_prefix_expression);
        parser.register_prefix(TokenKind::True, Self::parse_boolean);
        parser.register_prefix(TokenKind::False, Self::parse_boolean);
        parser.register_prefix(TokenKind::Lparen, Self::parse_grouped_expression);
        
        parser.register_infix(TokenKind::Plus, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Minus, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Asterisk, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Slash, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Eq, Self::parse_infix_expression);
        parser.register_infix(TokenKind::NotEq, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Lt, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Gt, Self::parse_infix_expression);
        
        
        // 预读取两个Token
        // 第一次调用，将peek_token移动到current_token
        parser.next_token();
        
        // 第二次调用，peek_token将被设置为词法解析器下一个token
        parser.next_token();
        parser
    }
    
    fn parse_identifier(&mut self) -> Option<ExpressionNode>{
        Some(ExpressionNode::IdentifierNode(Identifier{
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        }))
    }
    
    fn parse_integer_literal(&mut self)-> Option<ExpressionNode>{
        let mut literal = IntegerLiteral{
            token: self.current_token.clone(),
            value: Default::default(),
        };
        
        return match self.current_token.literal.parse::<i64>(){
            Ok(value) => {
                literal.value = value;
                Some(ExpressionNode::Integer(literal))
            },
            Err(_) => {
                let msg = format!("could not parse {} as integer", self.current_token.literal);
                self.errors.push(msg);
                None
            },
        }
    }
    
    fn parse_prefix_expression(&mut self) -> Option<ExpressionNode>{
        let mut expression = PrefixExpression{
            token: self.current_token.clone(),
            operator: self.current_token.literal.clone(),
            right: Default::default(),
        };
        self.next_token();
        match self.parse_expression(PrecedenceLevel::Prefix) {
            Some(exp) => expression.right = Box::new(exp),
            None => return None,
        }
        Some(ExpressionNode::Prefix(expression))
    }
    
    fn parse_infix_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode>{
        self.next_token();
        let mut expression = InfixExpression{
            token: self.current_token.clone(),
            operator: self.current_token.literal.clone(),
            left: Box::new(left),
            right: Default::default(),
        };
        
        let precedence = self.current_precedence();
        self.next_token();
        match self.parse_expression(precedence) {
            Some(exp) => expression.right = Box::new(exp),
            None => return None,
        }
        Some(ExpressionNode::Infix(expression))
    }
    
    fn parse_boolean(&mut self) -> Option<ExpressionNode>{
        Some(ExpressionNode::BooleanNode(Boolean{
            token: self.current_token.clone(),
            value: if self.current_token.literal == "true" {true} else {false},
        }))
    }
    
    fn parse_grouped_expression(&mut self) -> Option<ExpressionNode>{
        self.next_token();
        
        let exp = self.parse_expression(PrecedenceLevel::Lowest);
        if !self.expect_peek(TokenKind::Rparen){
            return None;
        }
        
        exp
    }

    fn next_token(&mut self){
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    // 解析整个程序，并返回一个AST
    pub fn parse_program(&mut self) -> Option<Program>{
        let mut program = Program{statements: vec![]};

        while self.current_token.kind != TokenKind::Eof {
            if let Some(statement) = self.parse_statement(){
                program.statements.push(statement);
            }
            self.next_token();
        }
        Some(program)
    }
    
    // 解析一个语句并返回对应的AST节点
    fn parse_statement(&mut self) -> Option<StatementNode>{
        match self.current_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    
    fn parse_expression_statement(&mut self) -> Option<StatementNode>{
        let stmt = ExpressionStatement{
            token: self.current_token.clone(),
            expression: self.parse_expression(PrecedenceLevel::Lowest),
        };
        
        if self.peek_token.kind == TokenKind::Semicolon{
            self.next_token();
        }
        Some(StatementNode::Expression(stmt))
    }
    
    fn parse_expression(&mut self, precedence: PrecedenceLevel) -> Option<ExpressionNode>{
        let prefix_fn = self.prefix_parse_fns.get(&self.current_token.kind);
        if let Some(prefix_fn) = prefix_fn {
            let mut left_exp = prefix_fn(self);
            
            while !(self.peek_token.kind == TokenKind::Semicolon)
                && (precedence as u8) < (self.peek_precedence() as u8) 
            {
                let infix_fn = self.infix_parse_fns.get(&self.peek_token.kind);
                if let Some(infix) = infix_fn {
                    left_exp = infix(self, left_exp.expect("left exp should be returned"));
                }
            }
            return left_exp;
        }
        self.no_prefix_error(self.current_token.kind.clone());
        None
    }

    fn no_prefix_error(&mut self, kind: TokenKind){
        let msg = format!("no prefix parse function for {} found", kind);
        self.errors.push(msg);
    }
    
    // 解析let语句
    fn parse_let_statement(&mut self) -> Option<StatementNode>{
        let mut stmt = LetStatement{
            token: self.current_token.clone(),  // 当前的‘let’Token
            name: Default::default(),
            value: Default::default(),
        };
        
        return if !self.expect_peek(TokenKind::Ident){
            None
        }else {
            // 如果下一个token是标识符，解析变量名
            stmt.name = Identifier{
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            };
            
            // 检查下一个token是否为‘=’
            if !self.expect_peek(TokenKind::Assign){
                None
            }else {
                // 跳过”=“，读取赋值部分
                self.next_token();
                
                // 暂时跳过赋值表达式，直到遇到分号
                while !(self.current_token.kind == TokenKind::Semicolon){
                    self.next_token();
                }
                Some(StatementNode::Let(stmt))
            }
        };
    }
    
    fn parse_return_statement(&mut self) -> Option<StatementNode>{
        let stmt = ReturnStatement{
            token: self.current_token.clone(),
            return_value: Default::default(),
        };
        
        // 跳过return，读取返回值
        self.next_token();
        
        // 暂时跳过返回值表达式，直到遇到分号
        while !(self.current_token.kind == TokenKind::Semicolon){
            self.next_token();
        }
        Some(StatementNode::Return(stmt))
    }
    
    fn expect_peek(&mut self, kind: TokenKind) -> bool{
        if self.peek_token.kind == kind {
            self.next_token();
            return true;
        }
        // self.peek_error(kind);
        false
    }
    
    fn errors(&self) -> &Vec<String>{
        &self.errors
    }
    
    // 记录一个期待的下一个token，但实际却为另一个token 的错误信息
    fn peek_error(&mut self, expected: TokenKind){
        // 格式化错误信息，并将其添加到错误列表中
        let msg = format!(
                "期待的下一个 token 是 {} 但得到的是 {}", 
                expected,self.peek_token.kind);
        self.errors.push(msg);
    }
    
    fn register_prefix(&mut self, token_kind: TokenKind, prefix_fn: PrefixParseFn){
        self.prefix_parse_fns.insert(token_kind, prefix_fn);
    }
    
    fn register_infix(&mut self, token_kind: TokenKind, infix_fn: InfixParseFn){
        self.infix_parse_fns.insert(token_kind, infix_fn);
    }
    
    fn peek_precedence(&self) -> PrecedenceLevel{
        precedence_map(&self.peek_token.kind)
    }
    
    fn current_precedence(&self) -> PrecedenceLevel{
        precedence_map(&self.current_token.kind)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use super::*;

    #[test]
    fn test_parse_program() {
        let input = r#"
        let x  5;
        let y = 10;
        let foo = 100;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match program {
            Some(program) => {
                assert_eq!(
                    program.statements.len(),
                    3,
                    "statements does not contain 3 statements. got {}",
                    program.statements.len()
                );

                let expected = vec!["x", "y", "foo"];

                for (idx, exp) in expected.into_iter().enumerate() {
                    let statement = &program.statements[idx];
                    test_let_statement(statement, exp);
                }
            }
            None => panic!("parse_program() should return a program"),
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            ("-a * b", "(-(a) * b))"),
            ("!-a", "!(-a)"),
            ("a + b + c", "(a + b) + c"),
            ("a * b * c", "(a * b) * c"),
            ("a * b / c", "(a * b) / c"),
            ("a + b / c", "a + (b / c)"),
            ("a + b * c + d", "((a + b) * c) + d"),
            ("a * (b + c) * d", "a * (b + c) * d"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("(5 + 5) * 2 * 2", "((5 + 5) * 2) * 2"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(10 + 10) - 8", "((10 + 10) - 8)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("3 < 5 == 3 > 5", "((3 < 5) == (3 > 5))"),
            ("1 < 2 < 3", "((1 < 2) < 3)"),
            ("3 == 3", "(3 == 3)"),
            ("3 != 4", "(3 != 4)"),
            ("3 > 4", "(3 > 4)"),
            ("3 <= 4", "(3 <= 4)"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("2 * (5 + 5)", "(2 * (5 + 5))"),
            
            
            
        ];

        for test in tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            let actual = program.print_string();
            assert_eq!(
                actual, test.1,
                "expected={}, got={}",
                test.1, actual
            );
        }
    }

    fn test_let_statement(statement: &StatementNode, expected: &str) {
        assert_eq!(
            statement.token_literal(),
            "let",
            "statement.token_literal() not 'let'. got={}",
            statement.token_literal()
        );
        match statement {
            StatementNode::Let(let_stmt) => {
                assert_eq!(
                    let_stmt.name.value, expected,
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
            _ => panic!("statement is not LetStatement. got={:?}", statement),
        }
    }

    #[test]
    fn test_return_statement() {
        let input = r#"
            return 5; 
            return 10; 
            return 993322;
        "#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match program {
            Some(program) => {
                assert_eq!(
                    program.statements.len(),
                    3,
                    "statements does not contain 3 statements. got {}",
                    program.statements.len()
                );
                for statement in program.statements.iter() {
                    match statement {
                        StatementNode::Return(return_stmt) => {
                            assert_eq!(
                                return_stmt.token_literal(),
                                "return",
                                "return_stmt.token_literal() not 'return'. got={}",
                                return_stmt.token_literal()
                            );
                        }
                        _ => panic!("statement is not ReturnStatement. got={:?}", statement),
                    }
                }
            }
            None => panic!("parse_program() should return a program"),
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain enough statements. got={}",
            program.statements.len()
        );

        match &program.statements[0] {
            StatementNode::Expression(expr) => {
                assert!(expr.expression.is_some());

                match expr.expression.as_ref().unwrap() {
                    ExpressionNode::IdentifierNode(identifier) => {
                        assert_eq!(
                            identifier.value, "foobar",
                            "identifier.value not 'foobar'. got={}",
                            identifier.value
                        );
                        assert_eq!(
                            identifier.token_literal(),
                            "foobar",
                            "identifier.token_literal() not 'foobar'. got={}",
                            identifier.token_literal()
                        );
                    },
                    _ => panic!("expression is not Identifier. got={:?}", expr.expression),
                }
            },
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain enough statements. got={}",
            program.statements.len()
        );
        match &program.statements[0] {
            StatementNode::Expression(expr) => {
                assert!(expr.expression.is_some());
                match expr.expression.as_ref().unwrap() {
                    ExpressionNode::Integer(integer) => {
                        assert_eq!(
                            integer.value, 5,
                            "integer.value not 5. got={}",
                            integer.value
                        );
                        assert_eq!(
                            integer.token_literal(), "5",
                            "integer.token_literal() not '5'. got={}",
                            integer.token_literal()
                        );
                    }
                    _ => panic!("expression is not Integer. got={:?}", expr.expression),
                }
            }
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        }
    }

    #[test]
    fn test_prefix_expression() {
        let prefix_tests = vec![("!5", "!", 5), ("-15", "-", 15)];

        for test in prefix_tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            assert_eq!(
                program.statements.len(), 1,
                "program.statements does not contain enough statements. got={}",
                program.statements.len()
            );

            match &program.statements[0] {
                StatementNode::Expression(expr) => {
                    assert!(expr.expression.is_some());
                    let exp = expr.expression.as_ref().unwrap();

                    match exp {
                        ExpressionNode::Prefix(prefix) => {
                            assert_eq!(
                                prefix.operator, test.1,
                                "prefix.operator not '{}'. got={}",
                                test.1, prefix.operator
                            );
                            test_integer_literal(&prefix.right, test.2);
                        }
                        _ => panic!("expression is not Prefix. got={:?}", exp),
                    }
                }
                _ => panic!(
                    "program.statements[0] is not ExpressionStatement. got={:?}",
                    program.statements[0]
                ),
            }
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let infix_tests:Vec<(&str,Box<dyn any::Any>,&str,Box<dyn any::Any>)> = vec![
            ("5 + 5;", Box::new(5), "+", Box::new(5)),
            ("5 - 5;", Box::new(5), "-", Box::new(5)),
            ("5 * 5;", Box::new(5), "*", Box::new(5)),
            ("5 / 5;", Box::new(5), "/", Box::new(5)),
            ("5 > 5;", Box::new(5), ">", Box::new(5)),
            ("5 < 5;", Box::new(5), "<", Box::new(5)),
            ("5 == 5;", Box::new(5), "==", Box::new(5)),
            ("5 != 5;", Box::new(5), "!=", Box::new(5)),
            ("true == true", Box::new(true),"==", Box::new(true)),
            ("true != false", Box::new(true),"!=", Box::new(false)),
            ("false == false", Box::new(false),"==", Box::new(false)),
            ("false != true", Box::new(false),"!=", Box::new(true)),
        ];
        
        for test in infix_tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            assert_eq!(
                program.statements.len(), 1,
                "program.statements does not contain enough statements. got={}",
                program.statements.len()
            );

            match &program.statements[0] {
                StatementNode::Expression(expr) => {
                    assert!(expr.expression.is_some());
                    let exp = expr.expression.as_ref().unwrap();

                    test_infix_expression(
                        exp,
                        Box::new(test.1),
                        test.2.to_string(),
                        Box::new(test.3),
                    )
                }
                _ => panic!(
                    "program.statements[0] is not ExpressionStatement. got={:?}",
                    program.statements[0]
                ),
            }
        }
    }
    
    #[test]
    fn test_boolean_expression() {
        let input = r#"
            true;
            false;
        "#;
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);
        
        assert_eq!(
            program.statements.len(), 2,
            "program.statements does not contain enough statements. got={}",
            program.statements.len()
        );
        
        let expected_values = vec![(TokenKind::True,"true"),(TokenKind::False,"false")];
        
        for(idx,test) in expected_values.into_iter().enumerate(){
            match &program.statements[idx] {
                StatementNode::Expression(expr) => {
                    assert!(expr.expression.is_some());
                    let exp = expr.expression.as_ref().unwrap();
                    
                    match exp {
                        ExpressionNode::BooleanNode(boolean) => {
                            assert_eq!(
                                boolean.token.kind, test.0,
                                "boolean.value not {}. got={}",
                                test.0, boolean.token.kind
                            );
                            assert_eq!(
                                boolean.token_literal(), test.1,
                                "boolean.token_literal() not {}. got={}",
                                test.1, boolean.token_literal()
                            );
                        }
                        _ => panic!("expression is not Boolean. got={:?}", exp),
                    }
                }
                _ => panic!(
                    "program.statements[{} is not ExpressionStatement. got={:?}",
                    idx, program.statements[idx]
                ),
            }
        }
        
    }

    fn check_parser_errors(parser: Parser) {
        let errors = parser.errors();
        if errors.is_empty() {
            return;
        }
        for err in errors {
            eprintln!("parser error:{}", err);
        }
        panic!("parser error present");
    }

    fn test_integer_literal(exp: &ExpressionNode, value: i64) {
        match exp {
            ExpressionNode::Integer(integer) => {
                assert_eq!(
                    integer.value, value,
                    "integer.value not {}. got={}",
                    value, integer.value
                );
                assert_eq!(
                    integer.token_literal(), value.to_string(),
                    "integer.token_literal() not {}. got={}",
                    value, integer.token_literal()
                );
            }
            _ => panic!("exp not Integer. got={:?}", exp),
        }
    }

    fn test_identifier(exp: &ExpressionNode, value: String) {
        match exp {
            ExpressionNode::IdentifierNode(identifier) => {
                assert_eq!(
                    identifier.value, value,
                    "identifier.value not {}. got={}",
                    value, identifier.value
                );
                assert_eq!(
                    identifier.token_literal(), value,
                    "identifier.token_literal() not {}. got={}",
                    value, identifier.token_literal()
                );
            }
            _ => panic!("exp not Identifier. got={:?}", exp),
        }
    }

    fn test_literal_expression(exp: &ExpressionNode, expected: Box<dyn any::Any>) {
        match expected.downcast_ref::<String>() {
            Some(exp_string) => {
                test_identifier(exp, exp_string.to_string())
            },
            None => match expected.downcast_ref::<i64>() {
                Some(int_exp) => test_integer_literal(exp, int_exp.to_owned()),
                None => match expected.downcast_ref::<bool>() {
                    Some(bool) => test_boolean_literal(exp, bool.to_owned()),
                    None => panic!("unhandled literal type"),
                },
            },
        }
    }
    
    fn test_boolean_literal(exp: &ExpressionNode, value: bool) {
        match exp {
            ExpressionNode::BooleanNode(boolean) => {
                assert_eq!(
                    boolean.value, value,
                    "boolean.value not {}. got={}",
                    value, boolean.value
                );
                assert_eq!(
                    boolean.token_literal(), format!("{}",value),
                    "boolean.token_literal() not {}. got={}",
                    value, boolean.token_literal()
                );
            }
            _ => panic!("exp not Boolean. got={:?}", exp),
        }
    }
    
    
    fn test_infix_expression(
        exp: &ExpressionNode,
        left: Box<dyn any::Any>,
        operator: String,
        right: Box<dyn any::Any>,
    ){
        match exp {
            ExpressionNode::Infix(infix_exp) => {
                test_literal_expression(&infix_exp.left, left);
                assert_eq!(
                    infix_exp.operator, operator,
                    "infix.operator not '{}'. got={}",
                    operator, infix_exp.operator
                );
                test_literal_expression(&infix_exp.right, right);
            }
            other => panic!("exp is not Infix. got={:?}", other),
        }
    }
}
