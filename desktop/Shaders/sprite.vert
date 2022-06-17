#version 330 core
//layout (location = 0) in vec4 vertex; // <vec2 position, vec2 texCoords>
layout (location = 0) in vec2 vPos;
layout (location = 1) in vec2 vUv;

out vec2 TexCoords;

uniform mat4 model;
uniform mat4 projection;

void main()
{
    TexCoords = vUv;
    //gl_Position = projection * model * vec4(vPos.xy, 0.0, 1.0);
    gl_Position = projection * model * vec4(vPos.xy, 0.0, 1.0);
}