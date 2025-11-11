build:
	./build.sh

doc: $(patsubst Doc/%.md,Doc.build/%.html,$(wildcard Doc/*.md)) Doc.build/style.css

clean:
	rm -rf Doc.build

Doc.build/%.html: Doc/%.md | Doc.build/
	echo '<!DOCTYPE html><html><head><link rel=stylesheet href=style.css></head><body>' > $@
	python3 -m markdown -x admonition -x wikilinks $< >> $@
	echo '</body></html>' >> $@

Doc.build/%.css: Doc/%.css
	cp $< $@

%/:
	mkdir -p $@

.PHONY: build doc clean

.SECONDARY: Doc.build/
