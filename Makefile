.PHONY: all elm_js minify clean publish elm_doc_tui

elm_js := static/js/elm.js
css := static/css/elm-doc.css
cli_js := cli.js
main := src/Main.elm
doc_server := lib/elm-doc-server.js
types := lib/elm-doc-server.d.ts
version := lib/version.js
elm_doc_tui := lib/elm-docs-tui.mjs

css_files := static/css/style.css static/highlight/styles/default.css static/css/spinner.css

all: clean $(version) $(doc_server) elm_js minify $(css) elm_doc_tui

$(version): package.json
	npx genversion -se lib/version.js

$(doc_server): lib/elm-doc-server.ts $(version)
	npx tsc

elm_js:
	npx elm make --output=$(elm_js) --optimize $(main)

$(css): $(css_files)
	cat $^ | npx csso -o $@

minify:
	npx uglifyjs $(elm_js) --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | npx uglifyjs --mangle --output $(elm_js)

elm_doc_tui:
	cd script && npx elm-pages bundle-script src/ElmDocs.elm --output ../$(elm_doc_tui)

clean:
	rm -f $(elm_js) $(css) $(doc_server) $(version) $(types) $(elm_doc_tui)

publish: all
	npm publish
