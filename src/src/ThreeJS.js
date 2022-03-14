"use strict";

// note the rather weird handling of the callback in the line below...
// ... seems to be necessary when we try to pass a callback with argument from PureScript
exports.loadGLTF = url => cb => () => new THREE.GLTFLoader().load(url,x => cb(x)());

exports.addAnythingToScene = scene => anything => () => scene.add(anything);

exports.setPositionOfAnything = thing => x => y => z => () => thing.position.set(x,y,z);

exports.newHemisphereLight = skyColor => groundColor => intensity => () => new THREE.HemisphereLight(skyColor,groundColor,intensity);
