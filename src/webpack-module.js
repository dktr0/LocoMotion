import * as L from './index.js';

export function LocoMotion(canvas) {
  this.lm = L.launch(canvas)();
}

LocoMotion.prototype.evaluate = function(zone,txt) {
  return L.evaluate(this.lm)(zone)(txt)();
}

LocoMotion.prototype.clearZone = function(zone) {
  return L.clearZone(this.lm)(zone)();
}

LocoMotion.prototype.setTempo = function(foreignTempo) {
  return L.setTempo(this.lm)(foreignTempo)();
}

LocoMotion.prototype.preAnimate = function() {
  return L.preAnimate(this.lm)();
}

LocoMotion.prototype.animateZone = function(zone) {
  return L.animateZone(this.lm)(zone)();
}

LocoMotion.prototype.postAnimate = function() {
  return L.postAnimate(this.lm)();
}
