debug: EM = elm-make --debug
debug: all

release: EM = elm-make
release: all

clean:
	rm *.js

all: Main.js index.html

Main.js: src/Main.elm src/Types.elm src/Bar.elm
	$(EM) src/Main.elm --output=Main.js
