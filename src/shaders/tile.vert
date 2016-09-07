#version 330

in vec2 vertexCoord;

void main() {
  gl_Position = vec4(vertexCoord, 1, 1);
}
