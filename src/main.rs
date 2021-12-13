use std::error::Error;
use std::io::Write;
use std::{env, fs, io, process};

use scanner::Scanner;

mod scanner;

static mut HAD_ERROR: bool = false;

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
    run(source_code)?;
    if unsafe { HAD_ERROR } {
        process::exit(65);
    }
    Ok(())
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
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
        run(buf.drain(..).collect())?;
        unsafe {
            HAD_ERROR = false;
        }
    }
    Ok(())
}

fn run(source: String) -> Result<(), Box<dyn Error>> {
    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }

    Ok(())
}

fn error(line: usize, message: String) {
    report(line, "", message);
}

fn report(line: usize, place: impl AsRef<str>, message: impl AsRef<str>) {
    println!(
        "[line {}] Error{}: {}",
        line,
        place.as_ref(),
        message.as_ref()
    );
    unsafe {
        HAD_ERROR = true;
    }
}
