"use strict";

// note the rather weird handling of the callback in the line below...
// ... seems to be necessary when we try to pass a callback with argument from PureScript
exports.loadGLTF = url => cb => () => new THREE.GLTFLoader().load(url,x => cb(x)());

exports.addAnythingToScene = scene => anything => () => scene.add(anything);

exports.setPositionOfAnything = thing => x => y => z => () => thing.position.set(x,y,z);

exports.newHemisphereLight = skyColor => groundColor => intensity => () => new THREE.HemisphereLight(skyColor,groundColor,intensity);

exports.newAmbientLight = rgb => intensity => () => new THREE.AmbientLight(rgb,intensity);

exports.newDirectionalLight = rgb => intensity => () => new THREE.DirectionalLight(rgb,intensity);

exports.newPolarGridHelper = radius => radials => circles => divisions => () => new THREE.PolarGridHelper(radius,radials,circles,divisions)

exports.windowInnerWidth = () => window.innerWidth;

exports.windowInnerHeight = () => window.innerHeight;

exports.newAnimationMixer = object3D => () => new THREE.AnimationMixer(object3D);

exports.updateAnimationMixer = mixer => delta => () => mixer.update(delta);

exports.clipAction = animationMixer => clip => () => animationMixer.clipAction(clip);

exports.setEffectiveTimeScale = action => t => () => action.setEffectiveTimeScale(t);

exports.play = thing => () => thing.play();
