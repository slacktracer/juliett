import { Animator } from "./animation.js";
import { ball } from "./ball.js";
import { de } from "./game.js";
import {
  aLerp,
  cross,
  deNormal,
  dirFromAngle,
  dot,
  len,
  march,
  normalize,
  plus,
  rotX,
  rotY,
  sinLerp,
  times,
} from "./utils.js";

export class Player {
  constructor(uniforms) {
    this.pos = [0, 1.5, 0];
    this.uniforms = uniforms;

    this.camPos = [0, 0, 0];

    this.angle = [0, 0];

    this.bodyAngle = [0, 0];

    this.gravity = 50;
    this.jumpVel = 15;
    this.terminalYVel = 30;
    this.speed = 7.5;

    this.velocity = [0, 0, 0];
    this.jumpSpeed = 50;
    this.xFriction = 0.5;

    this.coyoteTime = 0.1;

    this.body2 = 0.5; //radius of body collision

    this.kickState = false;
    this.kickPos = undefined;
    this.kickPower = 50; //going to make kick more powerful w/ longer hold? idk controls but power should be a control

    this.createAnimators();
  }

  update(dt, input) {
    this.kick(dt, input);
    this.jump(dt, input);
    this.move(dt, input);

    this.legAnimator.update(dt);
    this.updateUniforms();
  }

  jump(dt, input) {
    if (input.mouseDown[2] && this.coyoteTime > 0) {
      this.coyoteTime = -1;
      let upOffset = [0, 0.5, 0];
      let dir = dirFromAngle(...this.angle);

      dir = normalize(plus(dir, upOffset));
      dir[0] /= 2;
      dir[2] /= 2;
      this.velocity = times(dir, this.jumpSpeed);
    }
  }

  kick(dt, input) {
    let kickRange = 7;
    if (input.mouseDown[0] && ball.de(this.pos) < kickRange) {
      this.kickState = true;
      dt /= 10;
      let dir = dirFromAngle(...this.angle);
      let p = this.camPos;
      // Bind ball.de to the ball instance
      let marchRes = march(ball.de.bind(ball), p, dir);
      if (marchRes) {
        this.kickPos = marchRes;
      } else {
        this.kickPos = undefined;
      }
    } else if (this.kickState) {
      this.kickState = false;
      if (this.kickPos != undefined) {
        let upOffset = [0, -1, 0];
        let kickDir = normalize(
          plus(ball.pos, times(plus(this.kickPos, upOffset), -1)),
        );
        ball.velocity = times(kickDir, this.kickPower);

        ball.rotVel = -Math.hypot(ball.velocity[0], ball.velocity[2]) /
          ball.size;
        ball.rotAxis = cross(
          normalize([ball.velocity[0], 0, ball.velocity[2]]),
          [0, 1, 0],
        );
      }
    }
  }

  move(dt, input) {
    let vel = [0, 0, 0];

    if (input.keyDown["w"]) {
      vel[2] += Math.cos(this.angle[0]);
      vel[0] += Math.sin(this.angle[0]);
    }
    if (input.keyDown["s"]) {
      vel[2] -= Math.cos(this.angle[0]);
      vel[0] -= Math.sin(this.angle[0]);
    }
    if (input.keyDown["a"]) {
      vel[2] += Math.sin(this.angle[0]);
      vel[0] -= Math.cos(this.angle[0]);
    }
    if (input.keyDown["d"]) {
      vel[2] -= Math.sin(this.angle[0]);
      vel[0] += Math.cos(this.angle[0]);
    }

    if (len(vel) > 0) {
      this.bodyAngle[0] = aLerp(
        this.bodyAngle[0],
        Math.atan2(vel[0], vel[2]),
        0.1 * (dt / (1 / 120)),
      );
      this.uniforms.playerAngle.value = [this.bodyAngle[0], 0];
    }

    if (!this.onGround()) {
      if (this.velocity[1] > 10) {
        this.legAnimator.setState(2, 0.1);
      } else {
        this.legAnimator.setState(3, 1);
      }
    } else {
      if (len(vel) > 0) {
        this.legAnimator.setState(0, 0.1);
      } else {
        this.legAnimator.setState(1, 0.2);
      }
    }

    if (len(vel) == 0) {
      vel = [0, 0, 0];
    } else {
      vel = times(vel, (this.speed * dt) / len(vel));
    }

    this.coyoteTime -= dt;

    if (this.onGround()) {
      this.coyoteTime = 0.2;
      this.velocity[0] *= 0;
      this.velocity[2] *= 0;
    }

    if (input.keyPressed[" "] && this.coyoteTime > 0) {
      this.velocity[1] = this.jumpVel;
      this.coyoteTime = -1;
    }

    this.velocity[1] -= this.gravity * dt;
    vel = plus(times(this.velocity, dt), vel);

    this.pos = plus(vel, this.pos);

    while (de(this.pos) < this.body2) {
      let norm = deNormal(this.pos, de);
      this.pos = plus(this.pos, times(norm, -(de(this.pos) - this.body2)));
      this.velocity = plus(
        this.velocity,
        times(norm, -dot(this.velocity, norm)),
      );
    }
  }

  onGround() {
    return de(plus(this.pos, [0, -this.body2, 0])) < 0.1;
  }

  updateUniforms() {
    this.uniforms.playerPos.value = this.pos;
    this.uniforms.camAngle.value = this.angle;

    let camDir = [0, 0, -5];

    camDir = rotX(camDir, -this.angle[1]);

    camDir = rotY(camDir, this.angle[0]);

    let camPos = plus(plus(this.pos, [0, 1, 0]), camDir);

    let iterations = 1000;

    while (de(camPos) < 0.5 && iterations > 0) {
      iterations--;
      camPos = plus(camPos, times(camDir, -0.005 / 5));
    }

    this.uniforms.camPos.value = camPos;
    this.camPos = camPos;
  }

  createAnimators() {
    let legUnis = [
      "uhip1",
      "ujr1",
      "ujr2",
      "ujr3",
      "uhip2",
      "ujl1",
      "ujl2",
      "ujl3",
      "uHeadPos",
      "uChestPos",
      "uBodyAngle",
    ];

    let legUniValues = [
      [
        [0.1, -0.1, -0.12],
        [0.15, -0.26, 0.05],
        [0.13, -0.35, -0.15],
        [0.13, -0.46, 0],

        [-0.1, -0.1, -0.12],
        [-0.15, -0.26, 0.05],
        [-0.13, -0.35, -0.15],
        [-0.13, -0.46, 0],
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0],
        1.2,
      ],

      //left leg back
      [
        [0.1, -0.1, -0.12],
        [0.15, -0.2, 0.15],
        [0.13, -0.35, -0],
        [0.13, -0.46, 0.15],

        [-0.1, -0.1, -0.12],
        [-0.15, -0.28, -0.05],
        [-0.13, -0.3, -0.2],
        [-0.13, -0.46, -0.15],
        [0, 0, 0],
        [0, 0, 0],
        [0, -0.4, 0],
        1.2,
      ],

      //left leg up
      [
        [0.1, -0.1, -0.12],
        [0.15, -0.26, 0.05],
        [0.13, -0.35, -0.15],
        [0.13, -0.46, 0],

        [-0.1, -0.1, -0.12],
        [-0.15, -0.18, 0.05],
        [-0.13, -0.25, -0.15],
        [-0.13, -0.35, 0],
        [0, 0, 0],
        [0, 0, 0],
        [0, -0.35, 0],
        1.2,
      ],

      //left leg front
      [
        [0.1, -0.1, -0.12],
        [0.15, -0.28, -0.05],
        [0.13, -0.3, -0.2],
        [0.13, -0.46, -0.15],

        [-0.1, -0.1, -0.12],
        [-0.15, -0.2, 0.15],
        [-0.13, -0.35, -0],
        [-0.13, -0.46, 0.15],
        [0, 0, 0],
        [0, 0, 0],
        [0, -0.3, 0],
        1.2,
      ],

      //left leg down
      [
        [0.1, -0.1, -0.12],
        [0.15, -0.18, 0.05],
        [0.13, -0.25, -0.15],
        [0.13, -0.35, 0],

        [-0.1, -0.1, -0.12],
        [-0.15, -0.26, 0.05],
        [-0.13, -0.35, -0.15],
        [-0.13, -0.46, 0],
        [0, 0, 0],
        [0, 0, 0],
        [0, -0.35, 0],
        1.2,
      ],
      //both legs up 1
      [
        [0.1, -0.1, -0.12],
        [0.15, -0.18, 0.05],
        [0.13, -0.25, -0.15],
        [0.13, -0.35, 0],

        [-0.1, -0.1, -0.12],
        [-0.15, -0.18, 0.05],
        [-0.13, -0.25, -0.15],
        [-0.13, -0.35, 0],
        [0, 0, 0],
        [0, 0, 0],
        [0, -0.2, 0],
        1.2,
      ],
      //both legs up 2
      [
        [0.1, -0.1, -0.12],
        [0.15, -0.13, 0.05],
        [0.13, -0.2, -0.15],
        [0.13, -0.3, 0],

        [-0.1, -0.1, -0.12],
        [-0.15, -0.13, 0.05],
        [-0.13, -0.2, -0.15],
        [-0.13, -0.3, 0],
        [0, 0, 0],
        [0, 0, 0],
        [0, -0.2, 0],
        1.2,
      ],
    ];

    this.legAnimator = new Animator(
      this.uniforms,
      [...legUnis, "uLift"],
      sinLerp,
    );

    let stepTime = 0.3;

    this.legAnimator.addKeyFrame(legUniValues[1], 0, 0);
    this.legAnimator.addKeyFrame(legUniValues[2], stepTime / 4, 0);
    this.legAnimator.addKeyFrame(legUniValues[3], (2 * stepTime) / 4, 0);
    this.legAnimator.addKeyFrame(legUniValues[4], (3 * stepTime) / 4, 0);
    this.legAnimator.addKeyFrame(legUniValues[1], stepTime, 0);
    this.legAnimator.addKeyFrame(legUniValues[0], stepTime, 1);
    this.legAnimator.addKeyFrame(legUniValues[5], 0, 2);
    this.legAnimator.addKeyFrame(legUniValues[6], 0, 3);
  }
}
