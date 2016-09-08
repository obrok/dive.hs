#version 330 core

uniform sampler2D tex;
in vec2 texCoordFrag;
out vec4 color;

void main() {
  color = vec4(0.5, 0, 0, 1) + texture(tex, texCoordFrag);
}
