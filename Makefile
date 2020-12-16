OUTPUT = public

pages:
	mkdir -p $(OUTPUT)
	elm make src/Main.elm --output elm.js
	cp elm.js $(OUTPUT)
	cp index.html $(OUTPUT)
	rsync -r --delete src/img public
