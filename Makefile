
build:
	cd src && spago bundle-module
	npx webpack

test:
	cd src && spago test

serve:
	npx http-server -c-1

clean:
	rm -rf index.js

threejs:
	curl -L -o threejs/three.min.js https://raw.githubusercontent.com/mrdoob/three.js/dev/build/three.min.js
	curl -L -o threejs/GLTFLoader.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/loaders/GLTFLoader.js
	curl -L -o threejs/DRACOLoader.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/loaders/DRACOLoader.js
	curl -L -o threejs/draco_decoder.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/libs/draco/draco_decoder.js
	curl -L -o threejs/draco_decoder.wasm https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/libs/draco/draco_decoder.wasm
	curl -L -o threejs/draco_wasm_wrapper.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/libs/draco/draco_wasm_wrapper.js
