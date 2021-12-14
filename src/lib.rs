use crate::scanner::Scanner;
use std::error::Error;

mod scanner;

static mut HAD_ERROR: bool = false;

pub fn had_error() -> bool {
    unsafe { HAD_ERROR }
}

pub fn set_had_error(value: bool) {
    unsafe { HAD_ERROR = value };
}

pub fn run(source: impl Into<String>) -> Result<(), Box<dyn Error>> {
    let scanner = Scanner::new(source.into());
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
