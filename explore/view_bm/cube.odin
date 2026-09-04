package main

import gl "vendor:OpenGL"

init_cube_mesh :: proc() {
    // 36 vertices (6 faces * 2 triangles * 3 vertices)
    // 36 vertices * 6 floats = 216 floats total
    cube_data := [216]f32{
        // Position          // Normal
        -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
         0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
         0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
         0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
        -0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
        -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,

        -0.5, -0.5,  0.5,  0.0,  0.0,  1.0,
         0.5, -0.5,  0.5,  0.0,  0.0,  1.0,
         0.5,  0.5,  0.5,  0.0,  0.0,  1.0,
         0.5,  0.5,  0.5,  0.0,  0.0,  1.0,
        -0.5,  0.5,  0.5,  0.0,  0.0,  1.0,
        -0.5, -0.5,  0.5,  0.0,  0.0,  1.0,

        -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,
        -0.5,  0.5, -0.5, -1.0,  0.0,  0.0,
        -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
        -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
        -0.5, -0.5,  0.5, -1.0,  0.0,  0.0,
        -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,

         0.5,  0.5,  0.5,  1.0,  0.0,  0.0,
         0.5,  0.5, -0.5,  1.0,  0.0,  0.0,
         0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
         0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
         0.5, -0.5,  0.5,  1.0,  0.0,  0.0,
         0.5,  0.5,  0.5,  1.0,  0.0,  0.0,

        -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
         0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
         0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
         0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
        -0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
        -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,

        -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
         0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
         0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
         0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
        -0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
        -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
    }

    stride := i32(6 * size_of(f32))

    cube_vbo: u32
    gl.GenBuffers(1, &cube_vbo)
    gl.BindBuffer(gl.ARRAY_BUFFER, cube_vbo)
    gl.BufferData(gl.ARRAY_BUFFER, size_of(cube_data), &cube_data[0], gl.STATIC_DRAW)

    // Location 0: Position
    gl.EnableVertexAttribArray(0)
    gl.VertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, stride, 0)
    gl.VertexAttribDivisor(0, 0)

    // Location 1: Normal
    gl.EnableVertexAttribArray(1)
    gl.VertexAttribPointer(1, 3, gl.FLOAT, gl.FALSE, stride, 3 * size_of(f32))
    gl.VertexAttribDivisor(1, 0)
}

/*
init_cube_mesh :: proc(r: ^Renderer) {
    // 36 vertices (12 triangles) or indexed version
    cube_vertices := []f32 {
        -0.5, -0.5, -0.5,   0.0,  0.0, -1.0, // 0
         0.5, -0.5, -0.5,   0.0,  0.0, -1.0, // 1
         0.5,  0.5, -0.5,   0.0,  0.0, -1.0, // 2
         0.5,  0.5, -0.5,   0.0,  0.0, -1.0, // 2
        -0.5,  0.5, -0.5,   0.0,  0.0, -1.0, // 3
        -0.5, -0.5, -0.5,   0.0,  0.0, -1.0, // 0

        -0.5, -0.5,  0.5,   0.0,  0.0,  1.0, // 4
         0.5, -0.5,  0.5,   0.0,  0.0,  1.0, // 5
         0.5,  0.5,  0.5,   0.0,  0.0,  1.0, // 6
         0.5,  0.5,  0.5,   0.0,  0.0,  1.0, // 6
        -0.5,  0.5,  0.5,   0.0,  0.0,  1.0, // 7
        -0.5, -0.5,  0.5,   0.0,  0.0,  1.0, // 4

        -0.5,  0.5,  0.5,  -1.0,  0.0,  0.0, // 7
        -0.5,  0.5, -0.5,  -1.0,  0.0,  0.0, // 3
        -0.5, -0.5, -0.5,  -1.0,  0.0,  0.0, // 0
        -0.5, -0.5, -0.5,  -1.0,  0.0,  0.0, // 0
        -0.5, -0.5,  0.5,  -1.0,  0.0,  0.0, // 4
        -0.5,  0.5,  0.5,  -1.0,  0.0,  0.0, // 7

         0.5,  0.5,  0.5,   1.0,  0.0, 0.0, // 6
         0.5,  0.5, -0.5,   1.0,  0.0, 0.0, // 2
         0.5, -0.5, -0.5,   1.0,  0.0, 0.0, // 1
         0.5, -0.5, -0.5,   1.0,  0.0, 0.0, // 1
         0.5, -0.5,  0.5,   1.0,  0.0, 0.0, // 5
         0.5,  0.5,  0.5,   1.0,  0.0, 0.0, // 6

        -0.5, -0.5, -0.5,   0.0, -1.0,  0.0, // 0
         0.5, -0.5, -0.5,   0.0, -1.0,  0.0, // 1
         0.5, -0.5,  0.5,   0.0, -1.0,  0.0, // 5
         0.5, -0.5,  0.5,   0.0, -1.0,  0.0, // 5
        -0.5, -0.5,  0.5,   0.0, -1.0,  0.0, // 4
        -0.5, -0.5, -0.5,   0.0, -1.0,  0.0, // 0

        -0.5,  0.5, -0.5,   0.0,  1.0,  0.0, // 3
         0.5,  0.5, -0.5,   0.0,  1.0,  0.0, // 2
         0.5,  0.5,  0.5,   0.0,  1.0,  0.0, // 6
         0.5,  0.5,  0.5,   0.0,  1.0,  0.0, // 6
        -0.5,  0.5,  0.5,   0.0,  1.0,  0.0, // 7
        -0.5,  0.5, -0.5,   0.0,  1.0,  0.0, // 3
	}

    cube_indices := []u32{
        0,1,2, 2,3,0,
        4,5,6, 6,7,4,
        7,3,0, 0,4,7,
        6,2,1, 1,5,6,
        0,1,5, 5,4,0,
        3,2,6, 6,7,3,
    }

    gl.GenVertexArrays(1, &r.cube_vao)
    gl.GenBuffers(1, &r.cube_vbo)
    //gl.GenBuffers(1, &r.cube_ebo)

    gl.BindVertexArray(r.cube_vao)

    gl.BindBuffer(gl.ARRAY_BUFFER, r.cube_vbo)
    gl.BufferData(gl.ARRAY_BUFFER,
        size_of(cube_vertices),
        &cube_vertices[0],
        gl.STATIC_DRAW)

    //gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, r.cube_ebo)
    //gl.BufferData(gl.ELEMENT_ARRAY_BUFFER,
    //    size_of(cube_indices),
    //    &cube_indices[0],
    //    gl.STATIC_DRAW)

    stride : i32 = 6 * size_of(f32)

    // aPosition → location 0
    gl.EnableVertexAttribArray(0)
    gl.VertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, stride, 0)
    gl.VertexAttribDivisor(0, 0)

    // aNormal → location 1
    gl.EnableVertexAttribArray(1)
    gl.VertexAttribPointer(1, 3, gl.FLOAT, gl.FALSE, stride, 3 * size_of(f32))
    gl.VertexAttribDivisor(1, 0)

    gl.BindVertexArray(0)
}
*/