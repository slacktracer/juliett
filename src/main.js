import * as THREE from "three";

import { init, player, update } from "./game.js";

const vertexShader = `
  varying vec2 vUv;
  void main() {
    vUv = uv;
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
  }
`;

const fragmentShader = `
    #define MIN_DIST 0.001
    #define MAX_ITERATIONS 500
    #define RANGE 10000.

    precision highp float;

    varying vec2 vUv;

    uniform vec2 res;
    uniform vec3 camPos;
    uniform vec2 camAngle;
    uniform float t;
    uniform vec3 playerPos;
    uniform vec3 ballPos;
    uniform vec2 ballAngle;
    uniform mat3 ballRotMat;
    uniform vec2 playerAngle;
    
    uniform vec3 uhip1;
    uniform vec3 ujr1;
    uniform vec3 ujr2;
    uniform vec3 ujr3;

    uniform vec3 uhip2;
    uniform vec3 ujl1;
    uniform vec3 ujl2;
    uniform vec3 ujl3;

    uniform vec3 uChestPos;
    uniform vec3 uHeadPos;
    uniform float uLift;
    uniform vec3 uBodyAngle;

    // From libs.js
    float smin( float a, float b, float k )
    {
        //k = 0.;
        float h = max( k-abs(a-b), 0.0 )/k;
        return min( a, b ) - h*h*k*(1.0/4.0);
    }

    vec3 bend(vec3 p, float k)
    {
        float c = cos(k*p.x);
        float s = sin(k*p.x);
        mat2  m = mat2(c,-s,s,c);
        vec3  q = vec3(m*p.xy,p.z);
        return q;
    }
    vec3 rotAxis(vec3 p, float a, vec3 u){
        mat3 m = mat3(
            cos(a) + u.x*u.x*(1.-cos(a)), u.x*u.y*(1.-cos(a))-u.z*sin(a), u.x*u.z*(1.-cos(a)) + u.y*sin(a),
            u.y*u.x*(1.-cos(a))+u.z*sin(a), cos(a) + u.y*u.y*(1.-cos(a)), u.y*u.z*(1.-cos(a))-u.x*sin(a),
            u.z*u.x*(1.-cos(a))-u.y*sin(a), u.z*u.y*(1.-cos(a))+u.x*sin(a), cos(a) + u.z*u.z*(1.-cos(a)) 
        );

        return m*p;
    }
    vec3 rotY(vec3 v, float a){
        return vec3(v.x*cos(a)+v.z*sin(a),v.y,-v.x*sin(a) + v.z*cos(a));
    }

    vec3 rotX(vec3 v, float a){
        return vec3(v.x, v.y*cos(a)-v.z*sin(a), v.y*sin(a)+v.z*cos(a));
    }
    vec3 twist( vec3 p, float k )
    {
        float c = cos(k*p.y);
        float s = sin(k*p.y);
        mat2  m = mat2(c,-s,s,c);
        vec3  q = vec3(m*p.xz,p.y);
        return q;
    }

    vec3 palette( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
    {
        return a + b*cos( 6.28318*(c*t+d) );
    }
    float dot2(in vec3 v ) { return dot(v,v); }

    float sdBox( vec3 p, vec3 b ){
      vec3 q = abs(p) - b;
      return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
    }
    float sdBox( in vec2 p, in vec2 b ){
        vec2 d = abs(p)-b;
        return length(max(d,0.0)) + min(max(d.x,d.y),0.0);
    }
    float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
    {
      vec3 pa = p - a, ba = b - a;
      float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
      return length( pa - ba*h ) - r;
    }
    float sdTorus( vec3 p, vec2 t )
    {
      vec2 q = vec2(length(p.xz)-t.x,p.y);
      return length(q)-t.y;
    }
    float sdRoundCone( vec3 p, vec3 a, vec3 b, float r1, float r2 )
    {
      // sampling independent computations (only depend on shape)
      vec3  ba = b - a;
      float l2 = dot(ba,ba);
      float rr = r1 - r2;
      float a2 = l2 - rr*rr;
      float il2 = 1.0/l2;
        
      // sampling dependant computations
      vec3 pa = p - a;
      float y = dot(pa,ba);
      float z = y - l2;
      float x2 = dot2( pa*l2 - ba*y );
      float y2 = y*y*l2;
      float z2 = z*z*l2;

      // single square root!
      float k = sign(rr)*rr*rr*x2;
      if( sign(z)*a2*z2>k ) return  sqrt(x2 + z2)        *il2 - r2;
      if( sign(y)*a2*y2<k ) return  sqrt(x2 + y2)        *il2 - r1;
                            return (sqrt(x2*a2*il2)+y*rr)*il2 - r1;
    }
    float sdVerticalCapsule( vec3 p, float h, float r )
    {
      p.y -= clamp( p.y, 0.0, h );
      return length( p ) - r;
    }
    float sdCappedCylinder( vec3 p, float h, float r )
    {
      vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(r,h);
      return min(max(d.x,d.y),0.0) + length(max(d,0.0));
    }
    float sdSphere(vec3 p, float r){
        return length(p) - r;
    }

    vec4 mod289(vec4 x) {
        return x - floor(x * (1.0 / 289.0)) * 289.0;
    }
      
    vec4 permute(vec4 x) {
        return mod289(((x * 34.0) + 1.0) * x);
    }
    float noise(vec3 p) {
        vec3 a = floor(p);
        vec3 d = p - a;
        d = d * d * (3.0 - 2.0 * d);
      
        vec4 b = a.xxyy + vec4(0.0, 1.0, 0.0, 1.0);
        vec4 k1 = permute(b.xyxy);
        vec4 k2 = permute(k1.xyxy + b.zzww);
      
        vec4 c = k2 + a.zzzz;
        vec4 k3 = permute(c);
        vec4 k4 = permute(c + 1.0);
      
        vec4 o1 = fract(k3 * (1.0 / 41.0));
        vec4 o2 = fract(k4 * (1.0 / 41.0));
      
        vec4 o3 = o2 * d.z + o1 * (1.0 - d.z);
        vec2 o4 = o3.yw * d.x + o3.xz * (1.0 - d.x);
      
        return o4.y * d.y + o4.x * (1.0 - d.y);
    }
    //stars from https://www.shadertoy.com/view/msdXzl
    vec3 nmzHash33(vec3 q) {
        uvec3 p = uvec3(ivec3(q));
        p = p * uvec3(374761393U, 1103515245U, 668265263U) + p.zxy + p.yzx;
        p = p.yzx * (p.zxy ^ (p >> 3U));
        return vec3(p ^ (p >> 16U)) * (1.0 / vec3(0xffffffffU));
    }
    vec3 stars(in vec3 p) {
        vec3 c = vec3(0.);
        float resX = 500.;
      
        for(float i = 0.; i < 5.; i++) {
            vec3 q = fract(p * (.15 * resX)) - 0.5;
            vec3 id = floor(p * (.15 * resX));
            vec2 rn = nmzHash33(id).xy;
            float c2 = 1. - smoothstep(0., .6, length(q));
            c2 *= step(rn.x, .0005 + i * 0.002);
            c += c2 * (mix(vec3(1.0, 0.49, 0.1), vec3(0.75, 0.9, 1.), rn.y) * 0.25 + 0.75);
            p *= 1.4;
        }
        return c * c;
    }

    // From graphics.js (de)
    float rings(vec3 p){
        float rad = 3.;
        float inRad = 3.;
        float ringThickness = 0.3;
        vec3 r1P = p;
        r1P = twist(r1P, 0.3);

        float ring1 = sdTorus(r1P, vec2(rad, ringThickness));

        vec3 r2P = p;
        r2P = rotY(r2P, 3.141/3.);
        r2P = twist(r2P, 0.3);

        float ring2 = sdTorus(r2P, vec2(rad, ringThickness));

        vec3 r3P = p; // Corrected: changed float to vec3
        r3P = rotY(r3P, 3.141*2./3.);
        r3P = twist(r3P, 0.3);

        float ring3 = sdTorus(r3P, vec2(rad, ringThickness));

        ring1 = min(ring1, ring2);

        ring1 = min(ring1, ring3);

        return ring1;
    }

    float ball(vec3 p){
        if(length(p-ballPos) > 3.5){
            return sdSphere(p-ballPos, 3.);
        }
        p -= ballPos;
        p = ballRotMat*p;
        float rad = 3.;
        float inRad = 3.;

        float innerSphere = sdSphere(p, inRad);


        float rings = rings(p);

        return max(innerSphere, -rings);
    }

    float ball2(vec3 p){
        return sdSphere(p-ballPos,3.);
    }

    float playerFace(vec3 p2){
        p2 -= uHeadPos;
        float head = sdSphere(p2 - vec3(0., 0.4, 0.), 0.2);
        float nose = sdSphere(p2 - vec3(0., 0.37, 0.22), 0.1);
        float ear1 = min(
            sdCapsule(p2, vec3(0.17, 0.45, -0.1), 1.1*vec3(0.15, 0.56, -0.4), 0.03),
            sdCapsule(p2, vec3(0.12, 0.5, -0.1), 1.1*vec3(0.15, 0.56, -0.4), 0.03)
        );
        float ear2 = min(
            sdCapsule(p2, vec3(-0.17, 0.45, -0.1), 1.1*vec3(-0.15, 0.56, -0.4), 0.03),
            sdCapsule(p2, vec3(-0.12, 0.5, -0.1), 1.1*vec3(-0.15, 0.56, -0.4), 0.03)
        );
        ear1 = min(ear1, ear2);

        float cheek1 = sdCapsule(p2, vec3(0.17, 0.36, -0.), vec3(0.07, 0.32, 0.17), 0.02);
        float cheek2 = sdCapsule(p2, vec3(-0.17, 0.36, -0.), vec3(-0.07, 0.32, 0.17), 0.02);
        cheek1 = min(cheek1, cheek2);

        float socket1 = sdSphere(p2 - vec3(0.15, 0.45, 0.08), 0.07);
        float socket2 = sdSphere(p2 - vec3(-0.15, 0.45, 0.08), 0.07);
        socket1 = min(socket1, socket2);

        float noseBone = sdCapsule(p2, vec3(0.035, 0.43, 0.27), vec3(-0.035, 0.43, 0.27), 0.03);

        float d = smin(head, nose, 0.1);
        d = smin(d, ear1, 0.1);


        d = smin(d, cheek1, 0.05);

        d = smin(d, noseBone, 0.05);

        d = max(d, -socket1);
        return d;
    }

    float playerLegs(vec3 p){
        vec3 hip1 = uhip1;
        vec3 jr1 = ujr1;
        vec3 jr2 = ujr2;
        vec3 jr3 = ujr3;

        vec3 hip2 = uhip2;
        vec3 jl1 = ujl1;
        vec3 jl2 = ujl2;
        vec3 jl3 = ujl3;

        float thighGap = 0.05 / 2.;

        float thigh1 = min(
            sdCapsule(p-vec3(0., thighGap, 0.), hip1, jr1, 0.015),
            sdCapsule(p-vec3(0., -thighGap, 0.), hip1, jr1, 0.015)
        );
        float shin1 = sdCapsule(p, jr1, jr2, 0.025);
        float ankle1 = sdCapsule(p, jr2, jr3, 0.025);

        float thigh2 = min(
            sdCapsule(p-vec3(0., thighGap, 0.), hip2, jl1, 0.015),
            sdCapsule(p-vec3(0., -thighGap, 0.), hip2, jl1, 0.015)
        );
        float shin2 = sdCapsule(p, jl1, jl2, 0.025);
        float ankle2 = sdCapsule(p, jl2, jl3, 0.025);

        thigh1 = min(thigh1, shin1);
        thigh1 = min(thigh1, ankle1);
        thigh1 = min(thigh1, thigh2);
        thigh1 = min(thigh1, shin2);
        thigh1 = min(thigh1, ankle2);

        return thigh1;
    }

    float playerBody(vec3 p){

        float rCone = sdRoundCone(p, vec3(0., 0.1, 0.) + uChestPos, vec3(0., -0.1, -0.12), 0.18, 0.1);
        float bottom = p.y - (-0.1);
        float d = max(rCone, -bottom);
        d = rCone;
        return d;
    }

    float playerEyes(vec3 p){
        p -= uHeadPos;
        float eye1 = sdSphere(p - vec3(0.15, 0.45, 0.08), 0.05);
        float eye2 = sdSphere(p - vec3(-0.15, 0.45, 0.08), 0.05);
        eye1 = min(eye1, eye2);

        return eye1;
    }
    float playerJacket(vec3 p, float face){
        vec3 jacketCenter = vec3(0., -0.1, 0.);
        p -= jacketCenter;
        float lift = uLift;
        vec3 p2 = bend(p.zyx, lift);
        p2 = p2.zyx;
        p2 = bend(p2, lift);
        vec3 p1 = p2/vec3(1.,12.,1.);
        float jacket = sdTorus(p1, vec2(.24, 0.04));
        float cutRad = 1.;
        float jacketIntersection = sdSphere(p - cutRad*normalize(vec3(0., -1.,1.)), cutRad);

        jacketIntersection = min(
            jacketIntersection, 
            sdSphere(p-vec3(0., 0.15, 0.2), 0.2)
        );

        jacketIntersection = min(
            jacketIntersection,
            sdSphere(abs(p)-vec3(2., -0.2, 0.), 1.75)
        );

        jacket = max(jacket, -jacketIntersection);

        jacket = max(jacket, -sdSphere(p - vec3(0., 0.4, 0.) + jacketCenter, 0.2));



        float d = max(jacket, -face);

        return d;
    }

    float player(vec3 p){
        vec3 p2 = p-playerPos;

        if(length(p-playerPos) > 1.2){
            return sdSphere(p2, 1.);
        }
        p2 = rotY(p2, -playerAngle.x);

        vec3 p3 = rotX(p2, uBodyAngle.y);

        float face = playerFace(p3);
        float body = playerBody(p3);
        float eyes = playerEyes(p3);
        float legs = playerLegs(p2);
        float jacket = playerJacket(p3, face);

        float d = min(face, body);
        d = min(d, eyes);
        d = min(d, legs);
        d = min(d, jacket);

        return d;
    }

    float groundBone(vec3 p){
        float d = p.y;
        d = max(d, -sdSphere(p-vec3(0.,10.,0.), 12.5));
        return d;
    }

    float ground(vec3 p){
        float base = p.y;
        float tiles = groundBone(p);
        float d = min(base, tiles);
        return base;
    }

    float goals(vec3 p){
        vec3 goalSize = vec3(17.5, 20., 10.);
        return min(sdBox(p - vec3(0.,0.,50.), goalSize), sdBox(p - vec3(0.,0.,-50.), goalSize));
    }

    float walls(vec3 p){
        float walls = -sdBox(p, vec3(30., 100., 50.));
        float goals = goals(p);
        walls = max(walls, -goals);
        return walls;
    }

    float bloomDe(vec3 p){
        return ball2(p);
    }

    float de(vec3 p){
        float d = 100000.;
        float floor = ground(p);
        float walls = walls(p);
        float ball = ball(p);
        float player = player(p);

        d = min(floor, walls);
        d = min(d, ball);
        d = min(d, player);
        return d;
    }

    // From libs.js (after de)
    vec3 grad(vec3 p){
        float eps = 0.01;
        return normalize(vec3((de(p+vec3(eps, 0., 0.)) - de(p-vec3(eps,0.,0.)))/(2.*eps), (de(p+vec3(0., eps, 0.)) - de(p-vec3(0.,eps,0.)))/(2.*eps), (de(p+vec3(0., 0., eps)) - de(p-vec3(0.,0.,eps)))/(2.*eps)));
    }
    float light(vec3 p, vec3 l){
        return clamp(dot(grad(p), l), 0., 1.);
    }

    float specLight(vec3 p, vec3 l){
        vec3 pos = normalize(p-camPos);
        vec3 ray = reflect(l, grad(p));
        return clamp(dot(pos, ray), 0., 1.);
    }

    // From graphics.js (otherfunctions)
    float pattern(vec2 p, float t, float o){
        p /= 1.5;
        
        float sum = 0.;

        for(float i = 0.; i < o; i++){
            p = fract(1.5*p)-.5;
            float d = sin(10.*length(p) + t);
            d = .1/d;
            sum += d;
        }

        return clamp(sum, 0., 1.);
    }

    float pat2(vec3 p){
        float noise1 = noise(p/10.)/2. + noise(p)/2. + noise(p*2.)/4. + noise(p*4.)/4. + noise(p*8.)/4. + noise(p*16.)/4.+noise(p*32.)/8.+noise(p*64.)/16.+noise(p*128.)/32.;

        return pattern(p.xz/200., 45./20., 12.) + noise(2.*p)/200. + noise1/10.;
    }

    float pat3(vec3 p){
        return pattern(p.xz/200., 72./20., 12.);
    }

    float noise2(vec3 p){
        return noise(p/10.)/2. + noise(p)/2. + noise(p*2.)/4. + noise(p*4.)/4. + noise(p*8.)/4. + noise(p*16.)/4.+noise(p*32.)/8.+noise(p*64.)/16.+noise(p*128.)/32.;
    }

    vec3 noise2Norm(vec3 p){
        float eps = 0.01;
        return normalize(vec3(
            noise2(p + vec3(eps, 0., 0.)) - noise2(p + vec3(-eps, 0., 0.)),
            noise2(p + vec3(0., eps, 0.)) - noise2(p + vec3(0., -eps, 0.)),
            noise2(p + vec3(0., 0., eps)) - noise2(p + vec3(0., 0., -eps))
        ));
    }

    void main() {
        vec2 pos = -1.0 + 2.0 * vUv;
        pos.x *= res.x/res.y;

        float fovX = .35;
        float fovY = .35;

        vec3 dir = normalize(vec3(pos.x*fovX, pos.y*fovY, 0.5515));

        dir = rotX(dir, -camAngle.y);
        dir = rotY(dir, camAngle.x);

        vec3 p = camPos;

        float dist = de(p);
        float totDist = dist;
        float bloomDist = bloomDe(p);
        float bloomTemp = bloomDist;

        for(int i = 0; i < MAX_ITERATIONS; i++){
            if(dist < MIN_DIST || totDist > RANGE){
                break;
            }
            p += dir*dist;
            dist = de(p);
            bloomTemp = bloomDe(p);
            if(bloomTemp < bloomDist){
                bloomDist = bloomTemp;
            }
            totDist += dist;
        }

        // From graphics.js (colors)
        vec3 col = vec3(0.,0.,0.);
        if(dist <= MIN_DIST){
            vec3 norm = grad(p);

            col = norm;
            if(length(p - ballPos) <= 3.005){
                vec3 p2 = p-ballPos;
                p2 = ballRotMat*p2;


                col = vec3(0.,0.2,.2) + vec3(vec3(clamp(dot(normalize(vec3(-0.2, .4, -0.5)), norm), 0., 1.)));

                if(length(p2) > 3.){
                    col = vec3(0.1);
                }

                float rDist = rings(p2);
                if(rDist > 0.){
                    col += 1./(1.+10.*rDist);
                }

                if(length(p2) < 3.){
                    col = vec3(1.);
                }

                col += 0.2*vec3(clamp(dot(normalize(vec3(-0.2, .4, -0.5)), norm), 0., 1.));
            }

            if(player(p) < 0.005){
                vec3 p2 = p-playerPos;
                p2 = rotY(p2, -playerAngle.x);
                vec3 p3 = rotX(p2, uBodyAngle.y);

                col = vec3(abs(norm));

                if(playerFace(p3) <= 0.005){
                    vec3 col1 = 2.*vec3(0.1,0.,0.2);

                    vec3 faceNorm = norm + 0.1*noise2Norm(3.*p3);

                    col = vec3(0.);
                    col += col1*clamp(dot(norm, normalize(vec3(1., 1., 1.))), 0., 1.);
                    col += col1*clamp(dot(norm, normalize(vec3(-.7, .8, -.5))), 0., 1.);

                    if(p3.x > 0. && false){
                        col = mix(col, stars(dir)+vec3(0.,0.3,0.3), abs(noise(10.*p3)));
                    }

                    float eyeDist = playerEyes(p3);

                    col += (1./(1.+40.*eyeDist));
                }

                if(playerEyes(p3) <= 0.005){
                    col = vec3(0.8);
                }

                else if(playerBody(p3) <= 0.005){
                    float sternumWidth = 0.02;
                    float ribGap = 0.04;
                    float ribSize = 0.02;
                    vec3 bodyNorm = norm;

                    col = stars(dir) + noise(2.*dir)*vec3(0.1,0.,0.2);
                    if(abs(p3.x) < sternumWidth){
                        col = vec3(.8);
                        col = 4.*vec3(0.1,0.,0.2);
                        col -= (length(p2.x)/(3.*sternumWidth));
                    }
                    float ribNum = mod(p3.y+abs(p3.x)/5., ribGap+ribSize);
                    if(ribNum < ribSize){
                        col = vec3(1.);
                        col = 4.*vec3(0.1,0.,0.2);
                        col -= (1.-(ribNum/ribSize));
                    }
                }

                else if(playerLegs(p2) <= 0.005){
                    vec3 col1 = 4. * vec3(0.1, 0., 0.2);
                    col = vec3(0.);
                    col += col1*clamp(dot(norm, normalize(vec3(1.))), 0., 1.);
                    col += col1*clamp(dot(norm, normalize(vec3(-0.7, 0.5, -0.6))), 0., 1.);
                }

                else if(playerJacket(p3, playerFace(p3)) <= 0.005){
                    vec3 col1 = vec3(0.1, 0.1, 0.2);
                    vec3 p4 = p3 / vec3(1., 12., 1.);
                    vec3 jacketNorm = norm + 0.2*noise2Norm(3.*p4);
                    col = vec3(0.);
                    col += col1*clamp(dot(norm, normalize(vec3(1.))), 0., 1.);
                    col += col1*clamp(dot(norm, normalize(vec3(-0.7,0.9,-0.6))), 0., 1.);
                    if(p3.x > 0. && false){
                        col = mix(col, stars(dir)+vec3(0.,0.3,0.3), 1.-abs(noise(4.*p3)));
                    }
                }
            }


            else if(ground(p) <= 0.002){
                col = vec3(0.1,0.1,0.1);
                float noiseGood = noise(p*50.)/8. + noise(p*25.)/4. + noise(p*12.5)/2.;
                float pat = pat2(p);
                float eps = 0.001/2.;
                col += pat*vec3(0.9,1.,1.);
                vec3 patNorm = normalize(vec3(
                    (pat2(p - vec3(eps, 0., 0.)) - pat2(p + vec3(eps, 0., 0.)))/(2.*eps),
                    1.,
                    (pat2(p - vec3(0., 0., eps)) - pat2(p + vec3(0., 0., eps)))/(2.*eps)
                ));
                vec3 patLightDir = normalize(vec3(1.,1.,1.));
                vec3 patLight2Dir = normalize(vec3(-0.7, .2, -0.3));

                float distNum = (5./(length(p-playerPos + vec3(0., 2., 0.))*length(p-playerPos + vec3(0., 2., 0.))));
                distNum = pow(1.1, -length(p-playerPos + vec3(0., 2., 0.)))/1.1;

                float patLight = clamp(dot(patNorm, patLightDir)*distNum + 0.5*(1.-distNum), 0., 1.);
                patLight = clamp(patLight, 0., 1.);

                patLight = clamp(patLight, 0., 1.);
            
                col = vec3(.1)*(0.6+patLight/2.);
                col += pat2(p)/7.;

                col += pow(clamp(dot(normalize(p-camPos), reflect(normalize(vec3(0.,15.,0.)-p), patNorm)), 0., 1.), 1.)/4.;


                if(pat3(p) == 1. && false){
                    col = vec3(0.);
                    col += pow(clamp(dot(normalize(p-camPos), reflect(patLightDir, patNorm)), 0., 1.), 32.);
                }

                col += (1./(1.+4.*walls(p)));
                col += clamp((.75/(1.+0.5*(ball2(p)*ball2(p)))) * dot(patNorm, normalize(ballPos-p)), 0., 1.);
            }

            else if(walls(p) <= 0.005){
                col = vec3(1.,0.,1.) + dot(norm, normalize(vec3(1.)));


                vec3 dustCol = vec3(0.1, 0., 0.2);
                if(dir.z < 0.){
                    dustCol = mix(dustCol, vec3(0.2,0.,0.), abs(dir.z));
                } else {
                    dustCol = mix(dustCol, vec3(0.,0.2,0.2), abs(dir.z));
                }
                col = stars(dir) + dustCol*noise(dir*1.5);


                if(abs(norm.x) > 0.7){
                    col += pat2(p.yxz)*(1./(p.y+1.));
                }
                if(abs(norm.z) > 0.7){
                    col += pat2(p.xzy)*(1./(p.y+1.));
                }

                float goalDist = goals(p);
                if(goalDist > 0.){
                    goalDist -= noise(p+vec3(t, cos(t), sin(t)))+noise(2.*p+vec3(t, cos(t), sin(t)))/2.;
                }

                vec3 goalCol = vec3(0., .8, .8);
                if(p.z < 0.){
                    goalCol = vec3(.7, 0., 0.);
                }

                goalCol += (noise(2.*p+vec3(t, cos(t), sin(t)))+noise(4.*p+vec3(t, cos(t), sin(t)))/2.)/5.;

                if(goalDist < 0.005){
                    col = vec3(1.,0.,0.);
                    col = stars(dir) + dustCol*noise(dir*1.5);
                }
                if(goalDist > 0.005 && goalDist <= 1.){
                    col = goalCol;
                }
                if(goalDist >= 1. && goalDist <= 1.5){
                    col = mix(goalCol, col, (goalDist-1.)/0.5);
                }
            }

            if(bloomDe(p) > 0.02){
                col += .5/(3.*bloomDist*bloomDist+1.);
            }
        } else {
            gl_FragColor = vec4(0.,0.,0.,1.);
        }

        gl_FragColor = vec4(col, 1.);
        gl_FragColor = pow(gl_FragColor, vec4(1.));
    }
`;

const scene = new THREE.Scene();
const camera = new THREE.OrthographicCamera(-1, 1, 1, -1, 0.1, 1000);

const renderer = new THREE.WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

// Mouse lock on click
renderer.domElement.addEventListener("click", () => {
  renderer.domElement.requestPointerLock();
});

const geometry = new THREE.PlaneGeometry(2, 2);
const material = new THREE.ShaderMaterial({
  vertexShader,
  fragmentShader,
  uniforms: {
    res: { value: new THREE.Vector2(window.innerWidth, window.innerHeight) },
    camPos: { value: new THREE.Vector3(0, 5, 0) },
    camAngle: { value: new THREE.Vector2(0, 0) },
    t: { value: 0 },
    playerPos: { value: new THREE.Vector3(0, 0, 0) },
    ballPos: { value: new THREE.Vector3(0, 0, 0) },
    ballAngle: { value: new THREE.Vector2(0, 0) },
    ballRotMat: { value: new THREE.Matrix3() },
    playerAngle: { value: new THREE.Vector2(0, 0) },
    uhip1: { value: new THREE.Vector3(0.1, -0.1, -0.12) },
    ujr1: { value: new THREE.Vector3(0.15, -0.26, 0.05) },
    ujr2: { value: new THREE.Vector3(0.13, -0.35, -0.15) },
    ujr3: { value: new THREE.Vector3(0.13, -0.46, 0) },
    uhip2: { value: new THREE.Vector3(-0.1, -0.1, -0.12) },
    ujl1: { value: new THREE.Vector3(-0.15, -0.26, 0.05) },
    ujl2: { value: new THREE.Vector3(-0.13, -0.35, -0.15) },
    ujl3: { value: new THREE.Vector3(-0.13, -0.46, 0) },
    uChestPos: { value: new THREE.Vector3(0, 0, 0) },
    uHeadPos: { value: new THREE.Vector3(0, 0, 0) },
    uLift: { value: 1.2 },
    uBodyAngle: { value: new THREE.Vector3(0, 0, 0) },
  },
});

const quad = new THREE.Mesh(geometry, material);
scene.add(quad);

camera.position.z = 1;

let lastTime = performance.now();
let dt = 0;

const input = {
  mouseDown: [],
  mousePressed: [],
  keyDown: [],
  keyPressed: [],
};

window.addEventListener("mousemove", (e) => {
  if (!player) return;
  let sens = [1 / 900, 1 / 900];
  player.angle[0] += e.movementX * sens[0];
  player.angle[1] -= e.movementY * sens[1];

  if (player.angle[1] >= Math.PI / 2) {
    player.angle[1] = Math.PI / 2;
  }
  if (player.angle[1] <= -Math.PI / 2) {
    player.angle[1] = -Math.PI / 2;
  }
});

window.addEventListener("mousedown", (e) => {
  input.mouseDown[e.button] = true;
  input.mousePressed[e.button] = true;
});

window.addEventListener("mouseup", (e) => {
  input.mouseDown[e.button] = false;
});

window.addEventListener("keydown", (e) => {
  input.keyDown[e.key.toLowerCase()] = true;
  input.keyPressed[e.key.toLowerCase()] = true;
});

window.addEventListener("keyup", (e) => {
  input.keyDown[e.key.toLowerCase()] = false;
});

// Handle window resize
const resolutionScale = 0.5; // Render at half resolution for performance
window.addEventListener("resize", () => {
  renderer.setSize(window.innerWidth, window.innerHeight);
  material.uniforms.res.value.set(
    window.innerWidth * resolutionScale,
    window.innerHeight * resolutionScale,
  );
});

// Initial resolution set
material.uniforms.res.value.set(
  window.innerWidth * resolutionScale,
  window.innerHeight * resolutionScale,
);

init(material.uniforms);

function animate() {
  requestAnimationFrame(animate);

  const currentTime = performance.now();
  dt = (currentTime - lastTime) / 1000; // Convert to seconds
  lastTime = currentTime;

  // Cap dt to prevent physics glitches on very slow frames
  if (dt > 1 / 20) {
    dt = 1 / 20;
  }

  update(dt, input);

  // Update uniforms
  material.uniforms.t.value += dt; // Use dt for time uniform

  renderer.render(scene, camera);

  input.keyPressed = [];
  input.mousePressed = [];
}

animate();
