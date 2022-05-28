import * as LocoMotion from './index.js';

export function launch(canvas) {
  return LocoMotion.launch(canvas)();
}

export function evaluateLocomotion(lm,txt) {
  return LocoMotion.evaluateLocomotion(lm)(txt)();
}

export function animateLocomotion(lm) {
  return LocoMotion.animateLocomotion(lm)();
}
