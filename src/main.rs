use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process;

extern crate pico_args;
extern crate rcc;
use pico_args::Arguments;
use rcc::{assemble, compile, link, utils, CompileError};
use std::ffi::OsStr;
use tempfile::NamedTempFile;

const HELP: &str = "compiler 0.1.0
Joshua Nelson <jyn514@gmail.com>
A C compiler written in Rust, with a focus on good error messages.

USAGE:
    compiler [FLAGS] [OPTIONS] [file]

FLAGS:
        --debug-asm    If set, print the intermediate representation of the program in addition to compiling
    -a, --debug-ast    If set, print the parsed abstract syntax tree in addition to compiling
        --debug-lex    If set, print all tokens found by the lexer in addition to compiling.
    -h, --help         Prints help information
    -c, --no-link      If set, compile and assemble but do not link. Object file is machine-dependent.
    -V, --version      Prints version information

OPTIONS:
    -o, --output <output>    The output file to use. [default: a.out]

ARGS:
    <file>    The file to read C source from. \"-\" means stdin (use ./- to read a file called '-').
              Only one file at a time is currently accepted. [default: -]";

#[derive(Debug)]
struct Opt {
    /// The file to read C source from.
    /// "-" means stdin (use ./- to read a file called '-').
    /// Only one file at a time is currently accepted.
    filename: PathBuf,

    /// If set, print all tokens found by the lexer in addition to compiling.
    debug_lex: bool,

    /// If set, print the parsed abstract syntax tree in addition to compiling
    debug_ast: bool,

    /// If set, print the intermediate representation of the program in addition to compiling
    debug_asm: bool,

    /// If set, compile and assemble but do not link. Object file is machine-dependent.
    no_link: bool,

    /// The output file to use.
    output: PathBuf,
}

impl Default for Opt {
    fn default() -> Self {
        Opt {
            filename: "<default>".into(),
            debug_lex: false,
            debug_ast: false,
            debug_asm: false,
            no_link: false,
            output: PathBuf::from("a.out"),
        }
    }
}

// TODO: when std::process::termination is stable, make err_exit an impl for CompilerError
// TODO: then we can move this into `main` and have main return `Result<(), CompileError>`
fn real_main(buf: String, opt: Opt) -> Result<(), Vec<CompileError>> {
    let product = compile(
        buf,
        opt.filename.to_string_lossy().into_owned(),
        opt.debug_lex,
        opt.debug_ast,
        opt.debug_asm,
    )?;
    if opt.no_link {
        return assemble(product, opt.output.as_path()).map_err(|err| vec![err]);
    }
    let tmp_file = NamedTempFile::new().map_err(|err| vec![err.into()])?;
    assemble(product, tmp_file.as_ref()).map_err(|err| vec![err])?;
    link(tmp_file.as_ref(), opt.output.as_path()).map_err(|err| vec![err.into()])
}

fn main() {
    let mut opt = match parse_args() {
        Ok(opt) => opt,
        Err(err) => {
            println!(
                "{}: error parsing args: {}",
                std::env::args().next().unwrap_or_else(|| "compiler".into()),
                err
            );
            std::process::exit(1);
        }
    };
    // NOTE: only holds valid UTF-8; will panic otherwise
    let mut buf = String::new();
    opt.filename = if opt.filename == PathBuf::from("-") {
        io::stdin().read_to_string(&mut buf).unwrap_or_else(|err| {
            eprintln!("Failed to read stdin: {}", err);
            process::exit(1);
        });
        PathBuf::from("<stdin>")
    } else {
        File::open(opt.filename.as_path())
            .and_then(|mut file| file.read_to_string(&mut buf))
            .unwrap_or_else(|err| {
                eprintln!("Failed to read {}: {}", opt.filename.to_string_lossy(), err);
                process::exit(1);
            });
        opt.filename
    };
    // why a closure instead of err_exit?
    // from a conversation in discord#rust-usage:
    //
    // A ! value can be coerced into any type implicitly
    // When you take the function directly you have a value of fn(&'static str) -> ! and that can't be coerced
    // When you call it you get a value of ! which can
    // It's like &String can be coerced to &str, but it's not a subtype of it
    // Likewise a fn(T) -> &String should not be allowed to coerce to fn(T) -> &str
    //
    // What's happening here is the function has type `fn(...) -> !`,
    // but when it's called, that's coerced to `!`,
    // so the closure has type `fn(...) -> i32`
    real_main(buf, opt).unwrap_or_else(|err| err_exit(&err));
}

fn os_str_to_path_buf(os_str: &OsStr) -> Result<PathBuf, bool> {
    Ok(os_str.into())
}

fn parse_args() -> Result<Opt, pico_args::Error> {
    let mut input = Arguments::from_env();
    if input.contains(["-h", "--help"]) {
        println!("{}", HELP);
        std::process::exit(1);
    }
    Ok(Opt {
        debug_lex: input.contains("--debug-lex"),
        debug_asm: input.contains("--debug-asm"),
        debug_ast: input.contains(["-a", "--debug-ast"]),
        no_link: input.contains(["-c", "--no-link"]),
        output: input
            .value_from_os_str(["-o", "--output"], os_str_to_path_buf)?
            .unwrap_or_else(|| "a.out".into()),
        filename: input
            .free_from_os_str(os_str_to_path_buf)?
            .unwrap_or_else(|| "-".into()),
    })
}

fn err_exit(errors: &[CompileError]) -> ! {
    use CompileError::*;
    for err in errors {
        match err {
            Semantic(err) => {
                utils::error(&err.data, &err.location);
            }
            IO(err) => utils::fatal(&format!("{}", err), 3),
            Platform(err) => utils::fatal(&format!("{}", err), 4),
        }
    }
    let (num_warnings, num_errors) = (utils::get_warnings(), utils::get_errors());
    print_issues(num_warnings, num_errors);
    process::exit(2);
}

fn print_issues(warnings: usize, errors: usize) {
    if warnings == 0 && errors == 0 {
        return;
    }
    let warn_msg = if warnings > 1 { "warnings" } else { "warning" };
    let err_msg = if errors > 1 { "errors" } else { "error" };
    let msg = match (warnings, errors) {
        (0, _) => format!("{} {}", errors, err_msg),
        (_, 0) => format!("{} {}", warnings, warn_msg),
        (_, _) => format!("{} {} and {} {}", warnings, warn_msg, errors, err_msg),
    };
    eprintln!("{} generated", msg);
}
