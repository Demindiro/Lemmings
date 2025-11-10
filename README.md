Lemmings!
=========

Lemmings is an experimental OS exploring unconventional approaches to
scheduling and resource management.


Building
--------

You need the Rust **1.85** toolchain. Any other version is unlikely to work!

You should first copy `config.env.example` to `config.env` and set it up.

### Guix

```
guix shell -m manifest.scm
make
```

Use `./run.sh` to launch QEMU.


Documentation
-------------

```
make doc
```

Output will be in the `Doc.build/` directory.
You can view it with a web browser.
