.PHONY: default

default: Main.js index.html

clean:
	rm *.js

Main.js: src/Main.elm src/Types.elm src/Bar.elm
	elm-make src/Main.elm --output=Main.js --debug
