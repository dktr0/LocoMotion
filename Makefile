
build:
	cd src && spago bundle-app
	cd src && cp -f index.js ../index.js

serve:
	python -m SimpleHTTPServer 8000

clean:
	rm -rf index.js

three.min.js:
	curl -L -o three.min.js https://raw.githubusercontent.com/mrdoob/three.js/dev/build/three.min.js
