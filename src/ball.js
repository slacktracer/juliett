import { de } from "./game.js";
import {
  cross,
  deNormal,
  dot,
  len,
  matTimesMat,
  normalize,
  plus,
  reflect,
  rotAxisMat,
  times,
} from "./utils.js";

export class Ball {
  constructor(pos, uniforms) {
    this.pos = pos;
    this.uniforms = uniforms;
    this.gravity = 50;

    this.rotVel = 0;
    this.rotAxis = [0, 0, 0];
    this.rotMat = [
      [1, 0, 0],
      [0, 1, 0],
      [0, 0, 1],
    ];

    this.yFriction = 0.3;
    this.xFriction = 0.5;

    this.velocity = [0, 0, 0];
    this.size = 3;
  }

  onGround() {
    return de(plus(this.pos, [0, -this.size, 0])) < 0.2;
  }

  update(dt) {
    if (de(this.pos) <= this.size) {
      this.velocity = times(reflect(this.velocity, deNormal(this.pos, de)), 1);
      this.velocity[1] *= 1 - this.yFriction;
      this.rotVel = -Math.hypot(this.velocity[0], this.velocity[2]) / this.size;
      this.rotAxis = cross(
        normalize([this.velocity[0], 0, this.velocity[2]]),
        [0, 1, 0],
      );
    }

    while (de(this.pos) < this.size) {
      this.pos = plus(
        this.pos,
        times(deNormal(this.pos, de), -(de(this.pos) - this.size)),
      );
    }

    if (this.onGround() && Math.abs(this.velocity[1]) < 0.5) {
      this.velocity[1] = 0;
      this.velocity[0] *= 1 - this.xFriction * dt;
      this.velocity[2] *= 1 - this.xFriction * dt;

      this.rotVel = -Math.hypot(this.velocity[0], this.velocity[2]) / this.size;
      this.rotAxis = cross(
        normalize([this.velocity[0], 0, this.velocity[2]]),
        [0, 1, 0],
      );
    } else {
      this.velocity = plus(this.velocity, [0, -this.gravity * dt, 0]);
    }
    this.pos = plus(this.pos, times(this.velocity, dt));

    this.rotAxis = cross(
      normalize([this.velocity[0], 0, this.velocity[2]]),
      [0, 1, 0],
    );

    this.rotMat = matTimesMat(
      rotAxisMat(this.rotVel * dt, this.rotAxis),
      this.rotMat,
    );

    this.updateUniforms();
  }

  updateUniforms() {
    this.uniforms.ballPos.value = this.pos;
    this.uniforms.ballRotMat.value.fromArray(this.rotMat.flat());
  }

  de(p) {
    return len(plus(p, times(this.pos, -1))) - this.size;
  }
}

export let ball;

export function initBall(pos, uniforms) {
  ball = new Ball(pos, uniforms);
}
