use std::error::Error;
use std::error::Error;
use std::io;
use std::io::Read;

#[test]
fn lox_should_detect_unterminated_block_comment() -> Result<(), Box<dyn Error>> {
    let source = r#"
    /* This is a test of
    a multiline comment,
    which is unterminated.
    "#;

    lox::run(source)?;

    assert!(lox::had_error());
    // TODO: capture stderr and verify the error message
    Ok(())
}

#[test]
fn lox_block_comment() -> Result<(), Box<dyn Error>> {
    let source = r#"
    /*
    This is a test of
    a multiline comment.
    */
    "#;

    lox::run(source)?;

    assert!(!lox::had_error());
    Ok(())
}
