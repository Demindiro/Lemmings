IDL = $(wildcard List/*)
IDL_TO_RUST = $(IDL:List/%.idl=rust/%)

all: rust

rust: $(IDL_TO_RUST)

check: check-rust

rust/%: List/%.idl gen_rust.py gen.py | rust/
	python3 gen_rust.py $< $@

check-rust: $(IDL_TO_RUST)
	for p in $<; do (cd $$p; CARGO_BUILD_TARGET=x86_64-unknown-linux-gnu cargo c) || exit; done

%/:
	mkdir -p $@

clean:
	rm -fr $(IDL_TO_RUST)


.PHONY: rust check check-rust
