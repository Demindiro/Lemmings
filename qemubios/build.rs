use std::{env, process::Command};

fn main() {
    Command::new("env").status().unwrap();
    Command::new("pwd").status().unwrap();
    println!("cargo::rerun-if-changed=src/main.s");
    println!("cargo::rerun-if-changed=src/main.ld");
    println!("cargo::rustc-link-arg=-Tsrc/main.ld");
    let out = env::var("OUT_DIR").expect("OUT_DIR");
    println!("cargo::rustc-link-arg={out}/main.o");
    Command::new("as")
        .args(&["src/main.s", "-o", &format!("{out}/main.o")])
        .status()
        .expect("failed to assemble src/main.s");
}
