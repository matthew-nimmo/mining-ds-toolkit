#version 330 core

layout (location = 0) in vec3 aPos;    // Cube vertex
layout (location = 1) in vec3 aNormal; // Cube normal
layout (location = 2) in vec3 aOffset; // Instance position
layout (location = 3) in vec3 aScale;  // Instance Block scale
layout (location = 4) in float aValue; // Instance Block value

uniform mat4 view;
uniform mat4 projection;
uniform vec3 globalScale;
uniform bool useVariableSize;
uniform int selectedID;

out vec3 vNormal;
out vec3 vFragPos;
out float vDataValue;
out float isSelected;

void main() {
    isSelected = (gl_InstanceID == selectedID) ? 1.0 : 0.0;
    vec3 scale = useVariableSize ? aScale : globalScale;

    vec3 localPos = aPos * scale;
    vFragPos = localPos + aOffset;
    vNormal = aNormal;
    vDataValue = aValue;

    gl_Position = projection * view * vec4(vFragPos, 1.0);
}
