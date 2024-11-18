use crate::token::{lookup_ident,Token, TokenKind};
pub struct Lexer {
    input:Vec<char>,    // 存储整个输入的字符数据
    position:usize,     // 当前字符在 input 中的索引
    read_position:usize,// 当前读取位置，在 input 中（用于下一个要读取的字符）
    ch:char,            // 当前正在查看的字符
}

impl Lexer{
    pub fn new(input:&str)->Lexer{
        let mut lexer = Lexer{
            input:input.chars().collect(),
            position:0,
            read_position:0,
            ch:Default::default(),
        };
        lexer.read_char();
        lexer
    }

    /// 读取下一个字符并更新 Lexer 状态
    pub fn read_char(&mut self){
        // 检查是否已经到达输入的结尾
        if self.read_position >= self.input.len(){
            self.ch = '\0';
        }else{
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    
    pub fn next_token(&mut self)->Token{
        self.skip_whitespace();
        let token = match self.ch {
            '=' =>{
                if self.peek_char() == '=' {
                    self.read_char();
                    Token{
                        kind: TokenKind::Eq,
                        literal:"==".to_string(),
                    }
                }else {
                    Lexer::new_token(TokenKind::Assign, self.ch)
                }
            } 
            ';' => Lexer::new_token(TokenKind::Semicolon, self.ch),
            ',' => Lexer::new_token(TokenKind::Comma, self.ch),
            '+' => Lexer::new_token(TokenKind::Plus, self.ch),
            '(' => Lexer::new_token(TokenKind::Lparen, self.ch),
            ')' => Lexer::new_token(TokenKind::Rparen, self.ch),
            '{' => Lexer::new_token(TokenKind::Lbrace, self.ch),
            '}' => Lexer::new_token(TokenKind::Rbrace, self.ch),
            '\0' => Token {
                kind: TokenKind::Eof,
                literal: "".to_string(),
            },
            '-' => Lexer::new_token(TokenKind::Minus, self.ch),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token{
                        kind: TokenKind::NotEq,
                        literal:"!=".to_string(),
                    }
                }else {
                    Lexer::new_token(TokenKind::Bang, self.ch)
                }
            }
            '/' => Lexer::new_token(TokenKind::Slash, self.ch),
            '*' => Lexer::new_token(TokenKind::Asterisk, self.ch),
            _ => {
                return if Lexer::is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let kind = lookup_ident(literal.as_str());
                    Token { kind, literal }
                } else if Lexer::is_digit(self.ch){
                    let kind = TokenKind::Int;
                    let literal = self.read_number();
                    Token { kind, literal }
                }else {
                    Lexer::new_token(TokenKind::Illegal, self.ch)
                };
            }
        };
        self.read_char();
        token
    }
    
    fn skip_whitespace(&mut self){
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r'{
            self.read_char();
        }
    }
    
    fn peek_char(&mut self) -> char{
        return if self.read_position >= self.input.len(){
            '\0'
        }else { 
            self.input[self.read_position]
        };
    }
    
    pub fn new_token(kind:TokenKind,ch:char)->Token{
        Token{
            kind,
            literal:ch.to_string(),
        }
    }
    
    fn is_letter(ch:char)->bool{
        ch.is_alphabetic() || ch == '_'
    }
    
    fn read_identifier(&mut self)->String{
        let mut identifier = String::new();
        
        while Lexer::is_letter(self.ch){
            identifier.push(self.ch);
            self.read_char();
        }
        identifier
    }
    
    fn is_digit(ch:char)->bool{
        ch.is_numeric()
    }
    
    fn read_number(&mut self)->String{ 
        let mut number = String::new();
        while Lexer::is_digit(self.ch) {
            number.push(self.ch);
            self.read_char();
        }
        number
    }
}


#[cfg(test)]
mod tests {
    use crate::token::{Token, TokenKind};
    use super::*;

    #[test]
    fn text_next_token() {
        let input = "=+(){},;";
        
        let expected:Vec<Token> = vec![
            Token{
                kind:TokenKind::Assign,
                literal:"=".to_string(),
            },
            Token{
                kind:TokenKind::Plus,
                literal:"+".to_string(),
            },
            Token{
                kind:TokenKind::Lparen,
                literal:"(".to_string(),
            },
            Token{
                kind:TokenKind::Rparen,
                literal:")".to_string(),
            },
            Token{
                kind:TokenKind::Lbrace,
                literal:"{".to_string(),
            },
            Token{
                kind:TokenKind::Rbrace,
                literal:"}".to_string(),
            },
            Token{
                kind:TokenKind::Comma,
                literal:",".to_string(),
            },
            Token{
                kind:TokenKind::Semicolon,
                literal:";".to_string(),
            },
        ];
        
        let mut lexer = Lexer::new(input);
        
        for (idx,exp_token) in expected.into_iter().enumerate(){
            let recv_token = lexer.next_token();
            assert_eq!(
                exp_token.kind,recv_token.kind,
                "tests[{idx}] - token kind wrong. expected={:?},recv={:?}",
                exp_token.kind,recv_token.kind
            );
        }
    }
    
    #[test]
    fn test_lexer_read_char(){
        let input = r#""
            let five = 5;
            let ten = 10;

            let add = fn(a, b) {
                a + b;
            };

            let result = add(five, ten);
        "#;
        
        let expected:Vec<Token> = vec![
            Token{
                kind:TokenKind::Let,
                literal:"let".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"five".to_string(),
            },
            Token{
                kind:TokenKind::Assign,
                literal:"=".to_string(),
            },
            Token{
                kind:TokenKind::Int,
                literal:"5".to_string(),
            },
            Token{
                kind:TokenKind::Semicolon,
                literal:";".to_string(),
            },
            Token{
                kind:TokenKind::Let,
                literal:"let".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"ten".to_string(),
            },
            Token{
                kind:TokenKind::Assign,
                literal:"=".to_string(),
            },
            Token{
                kind:TokenKind::Int,
                literal:"10".to_string(),
            },
            Token{
                kind:TokenKind::Semicolon,
                literal:";".to_string(),
            },
            Token{
                kind:TokenKind::Let,
                literal:"let".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"add".to_string(),
            },
            Token{
                kind:TokenKind::Assign,
                literal:"=".to_string(),
            },
            Token{
                kind:TokenKind::Fn,
                literal:"fn".to_string(),
            },
            Token{
                kind:TokenKind::Lparen,
                literal:"(".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"a".to_string(),
            },
            Token{
                kind:TokenKind::Comma,
                literal:",".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"b".to_string(),
            },
            Token{
                kind:TokenKind::Rparen,
                literal:")".to_string(),
            },
            Token{
                kind:TokenKind::Lbrace,
                literal:"{".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"a".to_string(),
            },
            Token{
                kind:TokenKind::Plus,
                literal:"+".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"b".to_string(),
            },
            Token{
                kind:TokenKind::Semicolon,
                literal:";".to_string(),
            },
            Token{
                kind:TokenKind::Rbrace,
                literal:"}".to_string(),
            },
            Token{
                kind:TokenKind::Semicolon,
                literal:";".to_string(),
            },
            Token{
                kind:TokenKind::Let,
                literal:"let".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"result".to_string(),
            },
            Token{
                kind:TokenKind::Assign,
                literal:"=".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"add".to_string(),
            },
            Token{
                kind:TokenKind::Lparen,
                literal:"(".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"five".to_string(),
            },
            Token{
                kind:TokenKind::Comma,
                literal:",".to_string(),
            },
            Token{
                kind:TokenKind::Ident,
                literal:"ten".to_string(),
            },
            Token{
                kind:TokenKind::Rparen,
                literal:")".to_string(),
            },
            Token{
                kind:TokenKind::Semicolon,
                literal:";".to_string(),
            },
            Token{
                kind:TokenKind::Eof,
                literal:"".to_string(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for (idx,exp_token) in expected.into_iter().enumerate(){
            let recv_token = lexer.next_token();
            assert_eq!(
                exp_token.kind,recv_token.kind,
                "tests[{idx}] - token kind wrong. expected={},recv={}",
                exp_token.kind,recv_token.kind
            );
            assert_eq!(
                exp_token.literal,recv_token.literal,
                "tests[{idx}] - token literal wrong. expected={},recv={}",
                exp_token.literal,recv_token.literal
            );
        }
    }
}