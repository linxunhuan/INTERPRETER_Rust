use std::fmt::Display;

#[derive(PartialEq,Debug, Default,Clone)]
pub struct Token {
    pub kind:TokenKind, // 表示令牌的类型，用 TokenKind 枚举来定义
    pub literal:String, // 令牌的字面值表示，通常是从源代码中提取出来的字符串
}

#[derive(PartialEq,Debug,Default,Clone)]
pub enum TokenKind {
    #[default]
    Illegal,    // 非法的或未识别的字符
    Eof,        // 文件结束符（End of File）
    
    Ident,      // 标识符（如变量名）
    Int,        // 整数数字类型
    
    Assign,     // 赋值操作符 '='
    Plus,       // 加号操作符 '+'
    Minus,      // 减号操作符 '-'
    Asterisk,   // 乘号操作符 '*'
    Slash,      // 除号操作符 '/'
    Bang,       //  Bang 运算符 '!'
    
    Lt,         // 小于号 '<'
    Gt,         // 大于号 '>'
    Eq,
    NotEq,
    
    Comma,      // 逗号 ','
    Semicolon,  // 分号 ';'
    
    Lparen,     // 左括号 '('
    Rparen,     // 右括号 ')'
    Lbrace,     // 左大括号 '{'
    Rbrace,     // 右大括号 '}'
    
    Fn,         // 函数关键字
    Let,        // 变量声明关键字
    True,       
    False,      
    If,
    Else,
    Return,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Illegal => write!(f, "Illegal"),
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::Ident => write!(f, "IDENT"),
            TokenKind::Int => write!(f, "INT"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Lparen => write!(f, "("),
            TokenKind::Rparen => write!(f, ")"),
            TokenKind::Lbrace => write!(f, "{{"),
            TokenKind::Rbrace => write!(f, "}}"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Eq => write!(f, "=="),
            TokenKind::NotEq => write!(f, "!="),
        }
    }
}

pub fn lookup_ident(ident: &str) -> TokenKind {
    match ident {
        "fn" => TokenKind::Fn,
        "let" => TokenKind::Let,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "return" => TokenKind::Return,
        _ => TokenKind::Ident,
    }
}