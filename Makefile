
.PHONY: build cabal-clean clean deploy super-clean watch

all: build


.cabal-sandbox/bin/site: site.hs
	if [ ! -d ".cabal-sandbox" ] ; then cabal sandbox init ; fi
	cabal install

_site:
	mkdir -p _site

_site/.git: _site
	cp -r .git _site/.git
	bash -c '(cd _site/ && git checkout -f gh-pages && git reset --hard HEAD && git clean -f -x)'



build: .cabal-sandbox/bin/site _site
	.cabal-sandbox/bin/site build

deploy: _site/.git build
	bash -c '(cd _site/ && git add -A . && git commit -m "Deploy" && git push)'

watch: .cabal-sandbox/bin/site
	.cabal-sandbox/bin/site watch -h 0.0.0.0



clean: 
	rm -rf ./_cache
	rm -rf ./_site/*

cabal-clean: 
	rm -rf ./dist
	rm -rf .cabal-sandbox/bin/site

super-clean: clean cabal-clean
