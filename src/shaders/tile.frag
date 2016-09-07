#version 330 core

uniform sampler2D tex;
in vec2 texCoordFrag;
out vec4 fragColor;

void main() {
  fragColor = vec4(1, 0, 0, 1) + texture(tex, texCoordFrag);
}
