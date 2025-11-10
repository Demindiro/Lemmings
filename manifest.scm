;; What follows is a "manifest" equivalent to the command line you gave.
;; You can store it in a file that you may then pass to any 'guix' command
;; that accepts a '--manifest' (or '-m') option.

(specifications->manifest
  (list "rust"
        "rust:cargo"
        "rust:rust-src"
        "rust:tools"
        "make"
        "qemu"
        "coreutils"
        "python"
        "python-blake3"
        "python-pymdown-extensions"
        "nss-certs"
        "neovim"
        "tcpdump"))
