
.PHONY: build cabal-clean clean clean-cache deploy ghci repl super-clean watch .site/.git

all: build



########################
## files we depend on ##
########################

.cabal-sandbox/bin/site: site.hs
	if [ ! -d ".cabal-sandbox" ] ; then cabal sandbox init ; fi
	cabal install

_site:
	mkdir -p _site

_site/.git: _site
	git fetch -p
	rm -rf _site/.git
	cp -r .git _site/
	bash -c '(cd _site/ && git checkout -f origin/gh-pages && git reset --hard HEAD && git clean -f -x)'



######################
## Hakyll functions ##
######################

build: .cabal-sandbox/bin/site _site
	.cabal-sandbox/bin/site build

deploy: _site/.git clean build
	bash -c '(cd _site/ && git add -A . && git commit -m "Deploy" && git push origin HEAD:gh-pages)'

watch: .cabal-sandbox/bin/site
	.cabal-sandbox/bin/site watch -h 0.0.0.0

rebuild: .cabal-sandbox/bin/site _site
	.cabal-sandbox/bin/site rebuild



########################
## Cleaning functions ##
########################

clean-cache: 
	rm -rf ./_cache

clean: clean-cache
	rm -rf ./_site/*

cabal-clean: 
	rm -rf ./dist
	rm -rf .cabal-sandbox/bin/site

super-clean: clean cabal-clean



#####################
## Cabal functions ##
#####################

repl:
	cabal repl

ghci: repl
