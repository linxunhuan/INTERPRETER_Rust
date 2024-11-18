use std::io::{Stdin,Stdout,Write};
use crate::{token::TokenKind,lexer::Lexer};


pub fn start(stdin: Stdin,mut stdout: Stdout) {
    loop {
        // 输出提示符>>,并检查写入是否成功
        write!(stdout,">>").expect("应该有书面提示字符串>>");
        
        // 刷新输出流，确保提示符显示
        stdout.flush().expect("应该有刷新输出");
        
        let mut input = String::new();
        
        // 从标准输入读取一行，并将结果存储在input中
        if let Err(e) = stdin.read_line(&mut input) {
            // 如果读取失败，输出错误信息并返回
            writeln!(stdout,"Error: {e}").expect("应该有错误输出");
            return;
        }
        
        let mut lexer = Lexer::new(input.as_str());

        loop {
            let token = lexer.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }
            writeln!(stdout,"{token:?}").expect("应该有输出");
        }
    }
}