#version 430 core

in vec3 position;
in vec2 complexCoordinate;

uniform int maxIterations=100;

out vec4 outColor;

vec4 mandelbrotColor(int maxIter, vec2 pos) {
     int iter;
     float tempzx, tempzy;
     float r2 = 0.0;

     float zx = pos.x;
     float zy = pos.y;

     float ox = zx;
     float oy = zy;
     for (iter = 0; iter < maxIter; iter++)
     {
          tempzx = zx;
          tempzy = zy;
          zx = (tempzx * tempzx) - (tempzy * tempzy);
          zy = 2 * tempzx * tempzy;
          zx += ox;
          zy += oy;
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     if (r2 < 4) {
          return vec4(0.0, 0.0, 0.0, 0.5);
     }
     float tmpval, tmpval2, red, green, blue, pi, fi;
     red = clamp(cos(0.3*zy) * sin(0.7*zx) + (iter % 7000)/6999.0, 0.0, 1.0);
     green = clamp(abs(sin(6*zx) * cos(0.5*zy)) * (1.0 - (iter % 1000)/999.0), 0.0, 1.0);
     blue = clamp(abs(cos(4*zx + zy)) * (1.0 - (iter % 240)/239.0), 0.0, 1.0);

     return vec4(red, green, blue, 1.0);
}

void main() {
     vec4 diffuseColor = mandelbrotColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
