use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=spleen-6x12.bdf");
    println!("cargo::rerun-if-changed=spleen-6x12.bin");
    let res = Command::new("python3")
        .arg("bdf2bin.py")
        .spawn()
        .expect("failed to run bdf2bin.py")
        .wait()
        .expect("failed to wait for bdf2bin.py");
    assert!(res.success(), "bdf2bin.py failed with status {res}");
}
