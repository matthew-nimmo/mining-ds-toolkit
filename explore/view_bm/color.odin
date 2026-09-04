package main

import "core:math"
import gl "vendor:OpenGL"

rainbow_colors := []f32{
    0.0, 0.0, 1.0, // Dark blue
    0.0, 1.0, 1.0, // Cyan
    0.0, 1.0, 0.0, // Green
    1.0, 1.0, 0.0, // Yellow
    1.0, 0.0, 0.0, // Red
}

magma_colors := []f32{
    0.1, 0.0, 0.2, // Deep purple
    0.5, 0.0, 0.0, // Dark red
    1.0, 0.5, 0.0, // Orange
    1.0, 1.0, 0.5, // Yellow
}

copper_colors := []f32{
    0.0, 0.05, 0.2, // Deep blue
    0.0, 0.4, 0.4,  // Dark teal
    0.8, 0.3, 0.1,  // Burnt orange
    1.0, 0.8, 0.4,  // Pale gold
}

cosmic_colors := []f32{
    0.961, 0.961, 0.961,
    0.804, 0.804, 0.804,
    0.0, 0.0, 0.0,
    0.427, 0.141, 0.094,
    0.984, 0.918, 0.486,
}

create_palette_texture :: proc(colors: []f32, binned: bool, texId: i32) -> u32 {
    tex: u32
    gl.GenTextures(texId, &tex)
    gl.BindTexture(gl.TEXTURE_1D, tex)

    // Upload the 5 colors
    gl.TexImage1D(gl.TEXTURE_1D, 0, gl.RGB, 5, 0, gl.RGB, gl.FLOAT, &colors[0])

    // Binned = Nearest (blocky), Continuous = Linear (smooth)
    filter := i32(binned ? gl.NEAREST : gl.LINEAR)
    gl.TexParameteri(gl.TEXTURE_1D, gl.TEXTURE_MIN_FILTER, filter)
    gl.TexParameteri(gl.TEXTURE_1D, gl.TEXTURE_MAG_FILTER, filter)
    gl.TexParameteri(gl.TEXTURE_1D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)

    return tex
}

generate_heat_palette :: proc() -> []f32 {
    //pixels := make([]u8, 256 * 3)
    pixels := make([]f32, 256 * 3)
    for i in 0..<256 {
        t := f32(i) / 255.0
        
        // Simple Heat Math
        r := math.clamp(t * 1.5, 0.0, 1.0)
        g := math.clamp(t * t * 0.8, 0.0, 1.0)
        b := math.clamp(math.pow(t, 4.0), 0.0, 1.0)

        pixels[i*3 + 0] = r // u8(r * 255)
        pixels[i*3 + 1] = g // u8(g * 255)
        pixels[i*3 + 2] = b // u8(b * 255)
    }
    return pixels
}
