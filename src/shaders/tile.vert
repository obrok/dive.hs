#version 330 core

in vec2 vertexCoord;
in vec2 texCoord;
out vec2 texCoordFrag;

void main() {
  texCoordFrag = texCoord;
  gl_Position = vec4(vertexCoord, 1, 1);
}
