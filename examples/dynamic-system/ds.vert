#version 460 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec4 in_color;

uniform mat4 view_transform = mat4(1);

out VS_OUT {
     vec4 color;
     mat4 final_transform;
} vs_out;

void main()
{
  vs_out.final_transform = view_transform;
  gl_Position = vec4(in_position, 1.0);
  vs_out.color = in_color;
}
