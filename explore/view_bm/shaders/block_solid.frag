#version 330 core

in vec3 vNormal;
in float isSelected;
in float vDataValue;

uniform sampler1D palette;
uniform float u_Intensity;
uniform float u_Glow;
uniform float u_DataMin;
uniform float u_DataMax;
uniform float u_FilterMin;

out vec4 FragColor;

void main() {
    if (vDataValue < u_FilterMin) {
        discard; // This block won't be rendered at all
    }

    if (u_Glow > 0) {
        float normalizedValue = (vDataValue - u_DataMin) / (u_DataMax - u_DataMin);
        vec3 baseColor = texture(palette, clamp(normalizedValue, 0.0, 1.0)).rgb;
        float alpha = pow(normalizedValue, 2.0) * u_Intensity;
        //FragColor = vec4(baseColor * u_Intensity, 1.0);
        FragColor = vec4(baseColor * alpha, alpha);
    } else {
        vec3 baseColor = texture(palette, clamp(vDataValue, 0.0, 1.0)).rgb;

        // A light coming from the top-front-right
        vec3 lightDir = normalize(vec3(0.4, 1.0, 0.8));
        vec3 normal = normalize(vNormal);
    
        // Lambertian reflectance
        float diff = max(dot(normal, lightDir), 0.0);
        vec3 ambient = 0.2 * baseColor;
        vec3 diffuse = diff * baseColor;

        if (isSelected > 0.5) {
            FragColor = vec4(1.0, 1.0, 0.0, 1.0); // Make it bright Yellow
        } else {
            FragColor = vec4(ambient + diffuse, 1.0);
        }
    }
}
