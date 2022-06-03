
build:
	cd src && spago bundle-module
	npx webpack

serve:
	npx http-server -c-1

clean:
	rm -rf index.js

three.min.js:
	curl -L -o three.min.js https://raw.githubusercontent.com/mrdoob/three.js/dev/build/three.min.js

GLTFLoader.js:
	curl -L -o GLTFLoader.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/loaders/GLTFLoader.js
