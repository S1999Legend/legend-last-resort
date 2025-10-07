#define PI 3.14159265359
#define MAX_STEPS 180
#define MAX_DIST 120.0
#define SURF_DIST 0.001

mat2 rot2D(float a) {
    float c = cos(a);
    float s = sin(a);
    return mat2(c, -s, s, c);
}

mat3 rotateY(float a) {
    float c = cos(a);
    float s = sin(a);
    return mat3(c, 0.0, s, 0.0, 1.0, 0.0, -s, 0.0, c);
}

mat3 rotateX(float a) {
    float c = cos(a);
    float s = sin(a);
    return mat3(1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c);
}

mat3 rotateZ(float a) {
    float c = cos(a);
    float s = sin(a);
    return mat3(c, -s, 0.0, s, c, 0.0, 0.0, 0.0, 1.0);
}

float hash21(vec2 p) {
    p = fract(p * vec2(234.34, 435.345));
    p += dot(p, p + 34.23);
    return fract(p.x * p.y);
}

float hash31(vec3 p) {
    p = fract(p * vec3(234.34, 435.345, 623.234));
    p += dot(p, p.yzx + 34.23);
    return fract((p.x + p.y) * p.z);
}

float noise2D(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    
    float a = hash21(i);
    float b = hash21(i + vec2(1.0, 0.0));
    float c = hash21(i + vec2(0.0, 1.0));
    float d = hash21(i + vec2(1.0, 1.0));
    
    return mix(mix(a, b, f.x), mix(c, d, f.x), f.y);
}

float noise3D(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    
    float a = hash31(i);
    float b = hash31(i + vec3(1.0, 0.0, 0.0));
    float c = hash31(i + vec3(0.0, 1.0, 0.0));
    float d = hash31(i + vec3(1.0, 1.0, 0.0));
    float e = hash31(i + vec3(0.0, 0.0, 1.0));
    float f1 = hash31(i + vec3(1.0, 0.0, 1.0));
    float g = hash31(i + vec3(0.0, 1.0, 1.0));
    float h = hash31(i + vec3(1.0, 1.0, 1.0));
    
    return mix(mix(mix(a, b, f.x), mix(c, d, f.x), f.y),
               mix(mix(e, f1, f.x), mix(g, h, f.x), f.y), f.z);
}

float fbm2D(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for(int i = 0; i < 5; i++) {
        value += amplitude * noise2D(p);
        p *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}

float fbm3D(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for(int i = 0; i < 5; i++) {
        value += amplitude * noise3D(p);
        p *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}

float sdSphere(vec3 p, float r) {
    return length(p) - r;
}

float sdBox(vec3 p, vec3 b) {
    vec3 q = abs(p) - b;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
}

float sdCapsule(vec3 p, vec3 a, vec3 b, float r) {
    vec3 pa = p - a, ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - ba * h) - r;
}

float sdTorus(vec3 p, vec2 t) {
    vec2 q = vec2(length(p.xz) - t.x, p.y);
    return length(q) - t.y;
}

float smin(float a, float b, float k) {
    float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
    return mix(b, a, h) - k * h * (1.0 - h);
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float sdPlane(vec3 p, float time) {
    float d = 1e10;
    
    vec3 bodyPos = p;
    float body = sdCapsule(bodyPos, vec3(-1.8, 0.0, 0.0), vec3(2.0, 0.0, 0.0), 0.38);
    d = smin(d, body, 0.25);
    
    vec3 nosePos = p - vec3(2.0, 0.0, 0.0);
    float nose = sdSphere(nosePos, 0.42);
    nose = max(nose, -(nosePos.x - 0.35));
    d = smin(d, nose, 0.2);
    
    vec3 canopyPos = p - vec3(0.8, 0.25, 0.0);
    float canopy = sdSphere(canopyPos, 0.35);
    canopy = max(canopy, p.y - 0.3);
    canopy = max(canopy, -p.y + 0.15);
    d = smin(d, canopy, 0.1);
    
    vec3 wingPos = p - vec3(0.0, -0.05, 0.0);
    float wing = sdBox(wingPos, vec3(0.8, 0.06, 2.5));

    wing = max(wing, -(abs(wingPos.z) - 0.4 - wingPos.x * 0.9));

    float wingTaper = smoothstep(2.0, 2.5, abs(wingPos.z));
    wing += wingTaper * 0.15;
    d = smin(d, wing, 0.12);
    

    vec3 aileronPos = wingPos - vec3(-0.3, 0.0, 0.0);
    float aileron = sdBox(aileronPos, vec3(0.25, 0.02, 2.3));
    aileron = max(aileron, -(abs(aileronPos.z) - 0.5 - aileronPos.x * 0.9));
    d = min(d, aileron);

    vec3 tailPos = p - vec3(-1.5, 0.6, 0.0);
    float tail = sdBox(tailPos, vec3(0.5, 0.04, 1.0));
    d = smin(d, tail, 0.12);
    
    vec3 vstabPos = p - vec3(-1.6, 0.35, 0.0);
    float vstab = sdBox(vstabPos, vec3(0.25, 0.7, 0.08));
    d = smin(d, vstab, 0.12);
    
    vec3 enginePos = p - vec3(2.3, 0.0, 0.0);
    float engine = sdTorus(enginePos, vec2(0.32, 0.08));
    d = min(d, engine);
    
    vec3 propPos = p - vec3(2.45, 0.0, 0.0);
    propPos.yz = rot2D(time * 20.0) * propPos.yz;
    float prop = sdBox(propPos, vec3(0.04, 0.1, 1.0));
    d = min(d, prop);
    
    float hub = sdSphere(p - vec3(2.4, 0.0, 0.0), 0.16);
    d = min(d, hub);
    
    vec3 gearPos1 = p - vec3(0.5, -0.5, 0.6);
    float gear1 = sdCapsule(gearPos1, vec3(0.0, 0.0, 0.0), vec3(0.0, 0.4, 0.0), 0.04);
    d = min(d, gear1);
    
    vec3 gearPos2 = p - vec3(0.5, -0.5, -0.6);
    float gear2 = sdCapsule(gearPos2, vec3(0.0, 0.0, 0.0), vec3(0.0, 0.4, 0.0), 0.04);
    d = min(d, gear2);
    
    return d;
}

float sdTerrain(vec3 p) {
    float terrain = p.y + 12.0;
    
    float mountains = fbm2D(p.xz * 0.05) * 35.0;
    mountains += fbm2D(p.xz * 0.1 + vec2(100.0, 50.0)) * 18.0;
    
    mountains += fbm2D(p.xz * 0.2) * 8.0;
    
    mountains += fbm2D(p.xz * 0.5) * 3.0;
    mountains += noise2D(p.xz * 1.0) * 1.0;
    
    terrain += mountains;
    
    return terrain;
}

vec2 map(vec3 p, float time) {

    vec3 planePos = vec3(0.0, 5.0 + sin(time * 1.2) * 0.8 + cos(time * 2.1) * 0.3, 0.0);
    float windRoll = sin(time * 1.8) * 0.12 + cos(time * 3.2) * 0.05;
    float windPitch = sin(time * 1.5) * 0.08 + sin(time * 2.7) * 0.03;
    float windYaw = sin(time * 0.9) * 0.04;
    

    vec3 localP = p - planePos;
    localP = rotateZ(windRoll) * localP;
    localP = rotateX(windPitch) * localP;
    localP = rotateY(windYaw) * localP;
    
    float plane = sdPlane(localP, time);
    float terrain = sdTerrain(p);
    

    if(plane < terrain) {
        return vec2(plane, 1.0);
    }
    return vec2(terrain, 2.0);
}


vec3 getNormal(vec3 p, float time) {
    vec2 e = vec2(0.002, 0.0);
    float d = map(p, time).x;
    vec3 n = d - vec3(
        map(p - e.xyy, time).x,
        map(p - e.yxy, time).x,
        map(p - e.yyx, time).x
    );
    return normalize(n);
}


float clouds(vec3 p, float time) {
    p.x += time * 3.0;
    p.z += sin(time * 0.3) * 2.0;
    
    float c = fbm3D(p * 0.15) * 0.5 + 0.5;
    c += fbm3D(p * 0.3 + vec3(10.0, 5.0, 10.0)) * 0.25;
    
    c = smoothstep(0.35, 0.65, c);
    

    float wispy = fbm3D(p * 0.8) * 0.3;
    c = mix(c, c + wispy, 0.5);
    
    return clamp(c, 0.0, 1.0);
}


vec3 atmosphere(vec3 rd, vec3 sunDir) {
    float sun = pow(max(dot(rd, sunDir), 0.0), 8.0);
    vec3 scatter = vec3(0.3, 0.5, 0.8) * (1.0 - rd.y * 0.5);
    return scatter + vec3(1.0, 0.9, 0.7) * sun * 0.3;
}


float softShadow(vec3 ro, vec3 rd, float mint, float maxt, float k, float time) {
    float res = 1.0;
    float t = mint;
    for(int i = 0; i < 20; i++) {
        float h = map(ro + rd * t, time).x;
        if(h < 0.001) return 0.0;
        res = min(res, k * h / t);
        t += h;
        if(t > maxt) break;
    }
    return clamp(res, 0.0, 1.0);
}


float calcAO(vec3 p, vec3 n, float time) {
    float occ = 0.0;
    float sca = 1.0;
    for(int i = 0; i < 5; i++) {
        float h = 0.01 + 0.12 * float(i) / 4.0;
        float d = map(p + h * n, time).x;
        occ += (h - d) * sca;
        sca *= 0.95;
    }
    return clamp(1.0 - 3.0 * occ, 0.0, 1.0);
}

vec2 rayMarch(vec3 ro, vec3 rd, float time) {
    float dO = 0.0;
    float matID = 0.0;
    
    for(int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ro + rd * dO;
        vec2 res = map(p, time);
        float dS = res.x;
        matID = res.y;
        
        dO += dS * 0.8;
        
        if(dO > MAX_DIST || abs(dS) < SURF_DIST) break;
    }
    
    return vec2(dO, matID);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;
    float time = iTime;
    
    float cameraDistance = 10.0 + sin(time * 0.3) * 2.0;
    float cameraHeight = 6.0 + cos(time * 0.4) * 1.0;
    float cameraOffset = sin(time * 0.25) * 1.5;
    
    vec3 planePos = vec3(0.0, 5.0 + sin(time * 1.2) * 0.8 + cos(time * 2.1) * 0.3, 0.0);
    
    vec3 ro = planePos + vec3(-cameraDistance, cameraHeight, cameraOffset);
    vec3 lookAt = planePos + vec3(4.0, 0.0, 0.0);

    ro.y += sin(time * 8.0) * 0.02;
    ro.z += cos(time * 7.0) * 0.02;
    
    vec3 forward = normalize(lookAt - ro);
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);
    
    vec3 rd = normalize(forward + uv.x * right + uv.y * up);
    
    vec3 sunDir = normalize(vec3(0.6, 0.7, -0.4));
    vec3 skyTop = vec3(0.3, 0.55, 0.95);
    vec3 skyHorizon = vec3(0.6, 0.75, 0.95);
    vec3 col = mix(skyHorizon, skyTop, pow(smoothstep(0.0, 0.6, rd.y), 0.8));
    
    col += atmosphere(rd, sunDir);

    float godRays = pow(max(dot(rd, sunDir), 0.0), 4.0);
    col += vec3(1.0, 0.9, 0.7) * godRays * 0.2;
    
    vec2 res = rayMarch(ro, rd, time);
    float d = res.x;
    float matID = res.y;
    
    if(d < MAX_DIST) {
        vec3 p = ro + rd * d;
        vec3 normal = getNormal(p, time);
        
        vec3 lightDir = sunDir;
        float diff = max(dot(normal, lightDir), 0.0);
        float amb = 0.4;
        
        float ao = calcAO(p, normal, time);
        float shadow = softShadow(p + normal * 0.01, lightDir, 0.1, 20.0, 8.0, time);
        
        if(matID < 1.5) {
            vec3 planeCol = vec3(0.92, 0.15, 0.18);
            
            vec3 localP = p - planePos;
            if(localP.x > 0.5 && localP.x < 1.2 && localP.y > 0.15 && abs(localP.z) < 0.25) {
                planeCol = vec3(0.2, 0.3, 0.4);
                float spec = pow(max(dot(reflect(-lightDir, normal), -rd), 0.0), 64.0);
                planeCol += vec3(0.8, 0.9, 1.0) * spec * 0.8;
            }
            
            if(abs(localP.z) > 1.8 && abs(localP.x) < 1.2) {
                planeCol = vec3(0.95, 0.95, 1.0);
            }
            
            float spec = pow(max(dot(reflect(-lightDir, normal), -rd), 0.0), 32.0);
            float fresnel = pow(1.0 - max(dot(normal, -rd), 0.0), 3.0);
            
            col = planeCol * (diff * 0.8 * shadow + amb * ao);
            col += vec3(1.0, 0.95, 0.9) * spec * 0.6;
            col += vec3(0.6, 0.7, 0.9) * fresnel * 0.3;
            
            float rim = pow(1.0 - abs(dot(normal, -rd)), 4.0);
            col += vec3(0.8, 0.9, 1.0) * rim * 0.4;
        } else {
            vec3 terrainCol = vec3(0.25, 0.65, 0.35);
            if(p.y > 20.0) {
                terrainCol = vec3(0.92, 0.92, 0.96);
            } else if(p.y > 15.0) {
                float blend = (p.y - 15.0) / 5.0;
                terrainCol = mix(vec3(0.35, 0.45, 0.32), vec3(0.92, 0.92, 0.96), blend);
            } else if(p.y > 5.0) {
                terrainCol = mix(vec3(0.25, 0.65, 0.35), vec3(0.35, 0.45, 0.32), (p.y - 5.0) / 10.0);
            } else if(p.y < 0.0) {
                terrainCol = vec3(0.4, 0.6, 0.4);
            }
            
            float texNoise = noise2D(p.xz * 5.0) * 0.15;
            terrainCol *= (1.0 - texNoise);
            float sss = pow(clamp(dot(normal, -lightDir) + 1.0, 0.0, 1.0), 2.0) * 0.3;
            col = terrainCol * (diff * 0.9 * shadow + amb * ao + sss);
            float fogAmount = 1.0 - exp(-d * 0.012);
            fogAmount *= smoothstep(-5.0, 20.0, p.y);
            col = mix(col, skyHorizon * 1.1, fogAmount * 0.7);
        }
    }
    vec3 cloudStart = ro + rd * 15.0;
    float cloudDensity = 0.0;
    for(int i = 0; i < 8; i++) {
        vec3 cloudP = cloudStart + rd * float(i) * 3.0;
        float c = clouds(cloudP, time * 0.5);
        c *= smoothstep(-0.1, 0.4, rd.y);
        cloudDensity += c * (1.0 - cloudDensity);
    }
    cloudDensity *= 0.7;
    vec3 cloudCol = vec3(1.0, 1.0, 1.0) * (0.6 + 0.4 * godRays);
    col = mix(col, cloudCol, cloudDensity);
    vec3 propP = ro + rd * d;
    vec3 propLocal = propP - planePos - vec3(2.45, 0.0, 0.0);
    float propBlur = smoothstep(1.0, 0.0, length(propLocal)) * 0.5;
    col = mix(col, col * 0.5, propBlur * smoothstep(MAX_DIST, 5.0, d));
    float vignette = 1.0 - length(uv * 0.5);
    vignette = pow(vignette, 1.5);
    col *= 0.4 + 0.6 * vignette;
    float grain = hash21(uv * time) * 0.03;
    col += grain;
    col = pow(col, vec3(0.9));
    col = col * 1.08;
    col = mix(col, vec3(dot(col, vec3(0.299, 0.587, 0.114))), -0.1);
    col = clamp(col, 0.0, 1.0);
    float aberration = length(uv) * 0.02;
    col.r = mix(col.r, texture(iChannel0, fragCoord / iResolution.xy + vec2(aberration, 0.0)).r, 0.0);
    
    fragColor = vec4(col, 1.0);
}
