import * as L from './index.js';

export function LocoMotion(canvas) {
  this.lm = L.launch(canvas)();
}

LocoMotion.prototype.evaluate = function(txt) {
  return L.evaluate(this.lm)(txt)();
}

LocoMotion.prototype.animate = function() {
  return L.animate(this.lm)();
}

LocoMotion.prototype.setTempo = function(foreignTempo) {
  return L.setTempo(this.lm)(foreignTempo)();
}
