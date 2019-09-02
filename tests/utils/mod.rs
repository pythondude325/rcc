#![allow(dead_code)]

use std::io::Error;
use std::path::Path;
use std::process::{Command, Output};

extern crate rcc;
extern crate tempfile;

use rcc::CompileError;

pub fn compile_and_run(program: String, args: &[&str]) -> Result<Output, Vec<CompileError>> {
    let output = tempfile::NamedTempFile::new().unwrap().into_temp_path();
    compile(program, false, &output)?;
    run(&output, args).map_err(|err| vec![CompileError::IO(err)])
}

pub fn compile(program: String, no_link: bool, output: &Path) -> Result<(), Vec<CompileError>> {
    let module = rcc::compile(
        program,
        "<integration-test>".to_string(),
        false,
        false,
        false,
    )?;
    if !no_link {
        let tmp_file = tempfile::NamedTempFile::new().unwrap();
        rcc::assemble(module, tmp_file.as_ref()).map_err(|err| vec![err])?;
        rcc::link(tmp_file.as_ref(), &output).map_err(|err| vec![err.into()])
    } else {
        rcc::assemble(module, output).map_err(|err| vec![err])
    }
}

pub fn run(program: &Path, args: &[&str]) -> Result<Output, Error> {
    Command::new(program).args(args).output()
}

pub fn assert_compile_error(program: &str) {
    let output = tempfile::NamedTempFile::new().unwrap().into_temp_path();
    assert!(
        match compile(program.to_string(), true, &output) {
            Err(mut errs) => match errs.pop() {
                Some(CompileError::Semantic(_)) => true,
                _ => false,
            },
            _ => false,
        },
        "{} should fail to compile",
        program
    );
}

pub fn assert_output(program: &str, output: &str) {
    let output: Vec<u8> = output.into();
    assert!(match compile_and_run(program.into(), &[]) {
        Err(_) => false,
        Ok(actual) => actual.stdout == output,
    })
}

pub fn assert_succeeds(program: &str) {
    assert!(
        match compile_and_run(program.to_string(), &[]) {
            Err(_) => false,
            Ok(output) => output.status.success(),
        },
        "{} should exit successfully",
        program
    );
}

pub fn assert_code(program: &str, code: i32) {
    assert!(
        match compile_and_run(program.to_string(), &[]) {
            Err(_) => false,
            Ok(output) => match output.status.code() {
                Some(actual) => actual == code,
                None => false,
            },
        },
        "{} should exit with code {}",
        program,
        code
    );
}

/// test a program so that if it becomes implemented I remember to add a test for it
pub fn not_implemented(prog: &str) {
    compile(prog.into(), true, "/dev/null".as_ref()).unwrap();
}
