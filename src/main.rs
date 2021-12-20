use lox::Lox;
use std::error::Error;
use std::io::Write;
use std::{env, fs, io, process};

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<_>>();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(args[1].clone()),
        _ => {
            println!("Usage: jlox [script]");
            process::exit(64);
        }
    }
}

fn run_file(path: String) -> Result<(), Box<dyn Error>> {
    let source_code = fs::read_to_string(path)?;
    let mut lox = Lox::new();
    lox.run(source_code);
    if lox.had_error() {
        process::exit(65);
    }
    if lox.had_runtime_error() {
        process::exit(70);
    }
    Ok(())
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut lox = Lox::new();
    let stdin = io::stdin();
    let mut buf = String::new();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let read = stdin.read_line(&mut buf)?;
        if read == 0 {
            break;
        }
        // should be in a separate crate?
        if buf.ends_with('\n') {
            buf.pop();
            if buf.ends_with('\r') {
                buf.pop();
            }
        }
        lox.run(buf.drain(..).collect::<String>());
        lox.set_had_error(false);
    }
    Ok(())
}
