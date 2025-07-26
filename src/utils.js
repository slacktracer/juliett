export function rotY(v, a) {
  return [
    v[0] * Math.cos(a) + v[2] * Math.sin(a),
    v[1],
    -v[0] * Math.sin(a) + v[2] * Math.cos(a),
  ];
}
export function rotX(v, a) {
  return [
    v[0],
    v[1] * Math.cos(a) - v[2] * Math.sin(a),
    v[1] * Math.sin(a) + v[2] * Math.cos(a),
  ];
}
export function plus(a1, a2) {
  return [a1[0] + a2[0], a1[1] + a2[1], a1[2] + a2[2]];
}
export function times(a, s) {
  return [a[0] * s, a[1] * s, a[2] * s];
}
export function min() {
  return Math.min(...arguments);
}
export function max() {
  return Math.max(...arguments);
}
export function abs(x) {
  if (x.length != undefined) {
    let temp = [];
    for (let i of x) {
      temp.push(Math.abs(i));
    }
    return temp;
  }
  return Math.abs(x);
}
export function matPlus(a, b) {
  let temp = [];
  for (let i in a) {
    temp[i] = [];
    for (let j in a[i]) {
      temp[i][j] = a[i][j] + b[i][j];
    }
  }

  return temp;
}
export function matTimes(m, v) {
  return [
    m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2],
    m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2],
    m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2],
  ];
}
export function matTimesMat(a, b) {
  let temp = [[], [], []];
  for (let i in b) {
    let col = matTimes(a, [b[0][i], b[1][i], b[2][i]]);
    for (let j in col) {
      temp[j].push(col[j]);
    }
  }
  return temp;
}
export function matTimesS(m, s) {
  let temp = [];
  for (let i in m) {
    temp[i] = [];
    for (let j in m[i]) {
      temp[i][j] = m[i][j] * s;
    }
  }
  return temp;
}
export function I() {
  return [
    [1, 0, 0],
    [0, 1, 0],
    [0, 0, 1],
  ];
}
export function sscpm(v) {
  return [
    [0, -v[2], v[1]],
    [v[2], 0, -v[0]],
    [-v[1], v[0], 0],
  ];
}
export function rotMatFromTwoVectors(a, b) {
  let v = cross(a, b);
  let s = len(v);
  let c = dot(a, b);

  let vx = sscpm(v);
  let vx2 = matTimesS(matTimesMat(vx, vx), 1 / (1 + c));

  let R = matPlus(matPlus(I(), vx), vx2);

  return R;
}
export function rotMatFromVec(v) {
  v = normalize(v);
  let u = [0, 0, 1];
  let k = sscpm(u);
  let c = dot(v, u);
  let s = len(cross(v, u));

  let k2 = matTimesMat(k, k);

  k = matTimesS(k, s);
  k2 = matTimesS(k2, 1 - c);

  let R = matPlus(I(), matPlus(k, k2));

  return R;
}
export function rotAxis(p, a, u) {
  let m = [
    [
      Math.cos(a) + u[0] * u[0] * (1 - Math.cos(a)),
      u[0] * u[1] * (1 - Math.cos(a)) - u[2] * Math.sin(a),
      u[0] * u[2] * (1 - Math.cos(a)) + u[1] * Math.sin(a),
    ],
    [
      u[1] * u[0] * (1 - Math.cos(a)) + u[2] * Math.sin(a),
      Math.cos(a) + u[1] * u[1] * (1 - Math.cos(a)),
      u[1] * u[2] * (1 - Math.cos(a)) - u[0] * Math.sin(a),
    ],
    [
      u[2] * u[0] * (1 - Math.cos(a)) - u[1] * Math.sin(a),
      u[2] * u[1] * (1 - Math.cos(a)) + u[0] * Math.sin(a),
      Math.cos(a) + u[2] * u[2] * (1 - Math.cos(a)),
    ],
  ];

  return matTimes(m, p);
}
export function rotAxisMat(a, u) {
  return [
    [
      Math.cos(a) + u[0] * u[0] * (1 - Math.cos(a)),
      u[0] * u[1] * (1 - Math.cos(a)) - u[2] * Math.sin(a),
      u[0] * u[2] * (1 - Math.cos(a)) + u[1] * Math.sin(a),
    ],
    [
      u[1] * u[0] * (1 - Math.cos(a)) + u[2] * Math.sin(a),
      Math.cos(a) + u[1] * u[1] * (1 - Math.cos(a)),
      u[1] * u[2] * (1 - Math.cos(a)) - u[0] * Math.sin(a),
    ],
    [
      u[2] * u[0] * (1 - Math.cos(a)) - u[1] * Math.sin(a),
      u[2] * u[1] * (1 - Math.cos(a)) + u[0] * Math.sin(a),
      Math.cos(a) + u[2] * u[2] * (1 - Math.cos(a)),
    ],
  ];
}
export function mod(x, m) {
  if (x.length != undefined) {
    let temp = [];
    for (let i of x) {
      temp.push(((i % m) + m) % m);
    }
    return temp;
  }
  return ((x % m) + m) % m;
}
export function cos(x) {
  if (x.length != undefined) {
    let temp = [];
    for (let i of x) {
      temp.push(Math.cos(i));
    }
    return temp;
  }
  return Math.cos(x);
}
export function sin(x) {
  if (x.length != undefined) {
    let temp = [];
    for (let i of x) {
      temp.push(Math.sin(i));
    }
    return temp;
  }
  return Math.sin(x);
}
export function len(v) {
  return Math.hypot(...v);
}
export function normalize(v) {
  if (len(v) == 0) {
    return [0, 0, 0];
  }
  return times(v, 1 / len(v));
}
export function dot(a, b) {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}
export function cross(a, b) {
  return [
    a[1] * b[2] - a[2] * b[1],
    a[2] * b[0] - a[0] * b[2],
    a[0] * b[1] - a[1] * b[0],
  ];
}
export function reflect(d, n) {
  return plus(d, times(n, -2 * dot(d, n)));
}
export function deNormal(p, de) {
  let eps = 0.01;
  eps /= 2;
  return normalize([
    de(plus(p, [eps, 0, 0])) - de(plus(p, [-eps, 0, 0])),
    de(plus(p, [0, eps, 0])) - de(plus(p, [0, -eps, 0])),
    de(plus(p, [0, 0, eps])) - de(plus(p, [0, 0, -eps])),
  ]);
}
export function lerp(a, b, w) {
  if (a.length != undefined) {
    let temp = [];
    for (let i in a) {
      temp.push(a[i] + (b[i] - a[i]) * w);
    }
    return temp;
  }
  return a + (b - a) * w;
}

export function sinLerp(a, b, w) {
  return lerp(a, b, Math.sin(Math.PI * (w - 0.5)) / 2 + 0.5);
}

export function aLerp(a, b, w) {
  a %= 2 * Math.PI;
  b %= 2 * Math.PI;
  if (abs(b - a) < Math.PI) {
    return lerp(a, b, w);
  } else {
    a += Math.sign(b - a) * 2 * Math.PI;
    return lerp(a, b, w);
  }
}

export function sdBox3(p, b) {
  let q = plus(abs(p), times(b, -1));
  return (
    len([max(q[0], 0), max(q[1], 0), max(q[2], 0)]) +
    min(max(q[0], q[1], q[2]), 0)
  );
}

export function dirFromAngle(ax, ay) {
  let dir = [0, 0, 1];
  dir = rotX(dir, -ay);
  dir = rotY(dir, ax);

  return dir;
}

export function march(df, p, dir) {
  let eps = 0.01;
  let range = 10000;
  let totDist = 0;

  let dist = df(p);
  while (dist > eps && totDist < range) {
    p = plus(p, times(dir, dist));
    totDist += dist;
    dist = df(p);
  }

  if (dist < eps) {
    return p;
  }
  return false;
}
