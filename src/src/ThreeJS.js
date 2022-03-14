"use strict";

// note the rather weird handling of the callback in the line below...
// ... seems to be necessary when we try to pass a callback with argument from PureScript
exports.loadGLTF = url => cb => () => new THREE.GLTFLoader().load(url,x => cb(x)());
