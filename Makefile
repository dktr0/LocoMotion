
build:
	cd src && spago bundle-app
	cd src && cp -f index.js ../index.js

serve:
	python -m SimpleHTTPServer 8000

clean:
	rm -rf index.js

three.min.js:
	curl -L -o three.min.js https://raw.githubusercontent.com/mrdoob/three.js/dev/build/three.min.js

GLTFLoader.js:
	curl -L -o GLTFLoader.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/jsm/loaders/GLTFLoader.js

three.module.js:
	curl -L -o three.module.js https://raw.githubusercontent.com/mrdoob/three.js/dev/build/three.module.js
