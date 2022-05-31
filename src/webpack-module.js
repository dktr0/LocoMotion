import * as L from './index.js';

export function launch(canvas) {
  return L.launch(canvas)();
}

export function evaluateLocomotion(lm,txt) {
  return L.evaluateLocomotion(lm)(txt)();
}

export function animateLocomotion(lm) {
  return L.animateLocomotion(lm)();
}

export function setTempo(tempo) {
  return L.setTempo(tempo)();
}

export function test() {
  console.log("test");
}
