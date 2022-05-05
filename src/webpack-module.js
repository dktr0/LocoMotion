import PS from './index.js';

export function launch(canvas) {
  return PS.launch(canvas)();
}

export function evaluateLocomotion(lm,txt) {
  return PS.evaluateLocomotion(lm)(txt)();
}

export function animateLocomotion(lm) {
  return PS.animateLocomotion(lm)();
}
