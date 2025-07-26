import { lerp } from "./utils.js";

export class Animator {
  constructor(uniformsObject, uniformNames, interp) {
    this.uniformsObject = uniformsObject; // This will be the material.uniforms object
    this.uniformNames = uniformNames; // This is an array of uniform names (strings)
    this.interp = lerp;
    this.states = [];
    this.changingStates = false;
    this.state = -1;
    this.stateT = 0;
    this.totalStateT = 0;
    this.stateFrames = [];

    if (interp != undefined) {
      this.interp = interp;
    }

    this.t = 0;
    this.totalAnimTime = 0;

    this.keyFrames = [];
  }

  addKeyFrame(nodes, t, state) {
    if (this.states[state] == undefined) {
      this.states[state] = { keyFrames: [], totalAnimTime: 0 };
    }
    this.states[state].keyFrames.push({ nodes, t: t });
    if (t > this.states[state].totalAnimTime) {
      this.states[state].totalAnimTime = t;
    }
    if (this.states.length == 1) {
      this.keyFrames = this.states[state].keyFrames;
      this.totalAnimTime = this.states[state].totalAnimTime;
    }
  }
  currentPos() {
    return this.getPos(this.t);
  }
  setState(state, changeT) {
    if (this.state != state) {
      this.state = state;
      this.t = 0;
      this.stateFrames = [this.getPos(this.t)];
      this.keyFrames = this.states[state].keyFrames;
      this.totalAnimTime = this.states[state].totalAnimTime;
      this.stateT = changeT;
      this.totalStateT = changeT;
      this.stateFrames.push(this.keyFrames[0].nodes);
    }
  }

  getPos(time) {
    if (this.stateT <= 0) {
      if (this.keyFrames.length == 1) {
        return this.keyFrames[0].nodes;
      }
      time %= this.totalAnimTime;

      let index = 0;

      if (time > this.keyFrames[this.keyFrames.length - 2].t) {
        index = this.keyFrames.length - 2;
      } else {
        while (time >= this.keyFrames[index].t) {
          index++;
        }
        index--;
      }

      let nodePositions = [];

      for (let i in this.keyFrames[index].nodes) {
        nodePositions.push(
          this.interp(
            this.keyFrames[index].nodes[i],
            this.keyFrames[index + 1].nodes[i],
            (time - this.keyFrames[index].t) /
              (this.keyFrames[index + 1].t - this.keyFrames[index].t),
          ),
        );
      }

      return nodePositions;
    } else {
      this.stateT -= this.dt;
      let nodePositions = [];

      for (let i in this.stateFrames[0]) {
        nodePositions.push(
          this.interp(
            this.stateFrames[0][i],
            this.stateFrames[1][i],
            1 - this.stateT / this.totalStateT,
          ),
        );
      }

      this.t = 0;

      return nodePositions;
    }
  }
  update(dt) {
    this.dt = dt;
    this.t += dt;
    let nodePositions = this.getPos(this.t);

    for (let i in this.uniformNames) {
      this.uniformsObject[this.uniformNames[i]].value = nodePositions[i];
    }
  }
}
