use lox::Lox;
use std::io::Write;
use std::{env, io, process};

fn main() {
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

fn run_file(path: String) {
    let mut lox = Lox::new();
    lox.run_file(path);
    if lox.had_error() {
        process::exit(65);
    }
    if lox.had_runtime_error() {
        process::exit(70);
    }
}

fn run_prompt() {
    let mut lox = Lox::new();
    let mut buf = String::new();
    loop {
        print!("> ");
        // OS usually flushes written data on a line-feed
        // so, we need to manually flush "> "
        match flush_and_read(&mut buf) {
            Ok(read) => {
                if read == 0 {
                    break;
                }
                // following: io::Lines::next()
                // Rust doesn't strip line-feed
                if buf.ends_with('\n') {
                    buf.pop();
                    if buf.ends_with('\r') {
                        buf.pop();
                    }
                }
                let line = buf.drain(..).collect::<String>();
                lox.run(line);
            }
            Err(e) => lox.scan_error(0, e.to_string()),
        }
        // we discard errors in REPL
        // so that users could continue using it
        lox.set_had_error(false);
    }
}

fn flush_and_read(buf: &mut String) -> io::Result<usize> {
    io::stdout().flush()?;
    io::stdin().read_line(buf)
}
