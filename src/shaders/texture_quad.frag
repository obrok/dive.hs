#version 120

in vec2 uv;
uniform sampler2D tex;

out vec4 fColor;

void main()
{
  fColor = vec4(texture(tex, uv).rgb, 1.0);
}
