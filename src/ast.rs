use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn print_string(&self) -> String;
}
#[derive(Debug)]
pub enum StatementNode {
    Let(LetStatement),
}

impl Node for StatementNode {
    fn token_literal(&self) -> String {
        return match self {
            Self::Let(let_stmt) => let_stmt.token_literal() ,
        }
    }
    fn print_string(&self) -> String {
        return match self {
            Self::Let(let_stmt) => let_stmt.print_string(),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionNode {
    IdentifierNode(Identifier),
}

impl Node for ExpressionNode {
    fn token_literal(&self) -> String {
        return match self {
            Self::IdentifierNode(identifier) => identifier.token_literal(),
        }
    }
    
    fn print_string(&self) -> String {
        return match self {
            Self::IdentifierNode(identifier) => identifier.print_string(),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements:Vec<StatementNode>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        return if self.statements.len() > 0 {
            match &self.statements[0] {
                StatementNode::Let(let_stmt) => let_stmt.token_literal(),
            }
        }else { 
            String::from("")
        };
    }
    
    fn print_string(&self) -> String {
        let mut out = String::from("");
        
        for stmt in self.statements.as_slice(){
            out.push_str(stmt.print_string().as_str());
        }
        out
    }
}

#[derive(Debug,Default)]
pub struct LetStatement {
    pub token:Token,                    // 存储与let语句相关的标记信息
    pub name:Identifier,                // 存储let语句中声明的变量名
    pub value:Option<ExpressionNode>,   // 存储let语句中声明的变量的值
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    
    fn print_string(&self) -> String {
        let mut out = String::from("");
        
        out.push_str(self.token_literal().as_str());
        out.push_str(" ");
        out.push_str(self.name.print_string().as_str());
        out.push_str(" = ");
        
        if let Some(val) = &self.value {
            out.push_str(val.print_string().as_str());
        }
        out.push_str(";");
        out
    }
}

#[derive(Debug,Default)]
pub struct Identifier{
    pub token: Token,   // 存储标记信息
    pub value: String,  // 存储标识符的值
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    
    fn print_string(&self) -> String {
        self.value.clone()
    }
}