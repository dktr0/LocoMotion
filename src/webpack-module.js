import * as L from './output/Main/index.js';

export function LocoMotion(canvas) {
  this.lm = L.launch(canvas)();
}

LocoMotion.prototype.define = function(args) {
  return L.define(this.lm)(args.zone)(args.time)(args.text)();
}

LocoMotion.prototype.clear = function(args) {
  return L.clear(this.lm)(args.zone)();
}

LocoMotion.prototype.setTempo = function(foreignTempo) {
  return L.setTempo(this.lm)(foreignTempo)();
}

LocoMotion.prototype.preRender = function(args) {
  if(args.canDraw) { L.preRender(this.lm)(args.nowTime)(); }
}

LocoMotion.prototype.render = function(args) {
  if(args.canDraw) { L.render(this.lm)(args.nowTime)(args.zone)(); }
  return [];
}

LocoMotion.prototype.postRender = function(args) {
  if(args.canDraw) { L.postRender(this.lm)(args.nowTime)(); }
}

export function exoLang(canvas) {
  return new LocoMotion(canvas);
}
