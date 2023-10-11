
build:
	npm install
	cd src && spago build
	esbuild src/webpack-module.js --bundle --minify --format=esm --outfile=locoMotion.js

test:
	cd src && spago test

serve:
	npx http-server -c-1

clean:
	rm -rf locoMotion.js
	rm -rf src/output

