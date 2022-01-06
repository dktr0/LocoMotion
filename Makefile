
build:
	spago bundle-app
	-mkdir staging
	mv index.js staging
	cp index.html staging
	cp style.css staging

serve:
	cd staging; python -m SimpleHTTPServer 8000

clean:
	rm -rf staging
	rm -rf index.js
