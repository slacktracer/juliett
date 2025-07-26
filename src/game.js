import { ball, initBall } from "./ball.js";
import { Player } from "./player.js";
import { len, max, min, plus, sdBox3, times } from "./utils.js";

let player;

function de(p) {
  let d = Infinity;
  let floor = p[1];
  let walls = -sdBox3(p, [30, 100, 50]);
  let goalSize = [17.5, 20, 10];
  let goals = min(
    sdBox3(plus(p, [0, 0, -50]), goalSize),
    sdBox3(plus(p, [0, 0, 50]), goalSize),
  );
  walls = max(walls, -goals);

  d = min(floor, walls);

  return d;
}

function init(uniforms) {
  initBall([0, 10, 0], uniforms);
  player = new Player(uniforms);
}

function update(dt, input) {
  if (!player) return;
  player.update(dt, input);
  ball.update(dt);
}

export { de, init, player, update };
