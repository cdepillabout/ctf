
.PHONY: build cabal-clean clean watch

all: build

.cabal-sandbox/bin/site:
	cabal install

_site:
	mkdir -p _site

_site/.git: _site
	cp -r .git _site/.git
	bash -c '(cd _site/ && git checkout gh-pages && git reset --hard HEAD && git clean -f -x)'

deploy: _site build
	bash -c '(cd _site/ && git add -A . && git commit -m "Deploy" && git push)'

build: .cabal-sandbox/bin/site _site
	.cabal-sandbox/bin/site build

watch: .cabal-sandbox/bin/site
	.cabal-sandbox/bin/site watch

clean: 
	rm -rf ./_cache
	rm -rf ./_site/*

cabal-clean: 
	rm -rf ./dist
	rm -rf .cabal-sandbox/bin/site
