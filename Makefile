SOURCES = src/Main.elm src/Types.elm src/Bar.elm

debug: ELMFLAGS = --debug
debug: app docs

release: app docs

ELM = elm-make $(ELMFLAGS)

clean:
	rm -f *.js docs.json # -f suppresses missing file errors

app: Main.js index.html

docs: docs.html

documentation.json: $(SOURCES)
	$(ELM) --docs $@
docs.html: src/docs/Main.elm documentation.json
	$(ELM) $< --output $@

Main.js: $(SOURCES)
	$(ELM) $< --output $@
