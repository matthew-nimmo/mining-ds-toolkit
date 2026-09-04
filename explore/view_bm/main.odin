package main

import "core:fmt"
import "core:math"
import "core:math/linalg"
import gl "vendor:OpenGL"
import "vendor:glfw"

/*
Pro-Tip: The Odin "Hot Reload" mindset
As you expand this into a 3D app, keep in mind that Odin is very fast to compile.
You can often keep your shader strings in external .glsl files and write a small
function to re-read and re-compile them while the app is running.
This allows you to tweak visuals without restarting the program!
*/

Bound_State :: struct {
    diagonal: f32
}

Orbit_State :: struct {
    center:       linalg.Vector3f32,
    radius:       f32,
    azimuth:      f32, // Horizontal angle
    polar:        f32, // Vertical angle
    mouse_last_x: f64,
    mouse_last_y: f64,
}

Attribute_State :: struct {
    names:       []string,
    active_idx:  int,
    data_min:    f32,
    data_max:    f32,
    filter_min: f32,
}

Camera_State :: struct {
    position: linalg.Vector3f32,
    distance: f32,
    near: f32,
    far: f32
}

State :: struct {
    camera: Camera_State,
    orbit: Orbit_State,
    attr: Attribute_State,
    bounds: Bound_State
}

state := State {
    orbit = Orbit_State {
        center  = {0, 0, 0},
        radius  = 5.0,
        azimuth = 0.0,
        polar   = math.PI / 4.0,
    }
}

Render_Object :: struct {
    vao: u32,
    vbo: u32,
    count: i32,
    mode: u32, // gl.POINTS, gl.LINES, etc.
}

Instance_Data :: struct {
    pos:   linalg.Vector3f32,
    scale: linalg.Vector3f32,
    value: f32,
}

use_variable_size : bool = true
instance_vbo : u32
instances : []Instance_Data
raw_blocks : Block_Model
glow_blocks : bool = false

main :: proc() {
    if !bool(glfw.Init()) do return
    defer glfw.Terminate()

    window := init_window()
    if window == nil do return

    gl.load_up_to(4, 5, glfw.gl_set_proc_address)

    vertex_source := load_text_file("shaders/block_solid.vert")
    fragment_source := load_text_file("shaders/block_solid.frag")
    shader := create_shader_program(vertex_source, fragment_source)
    check_program_link(shader)

    // Get uniform locations
    //model_loc := gl.GetUniformLocation(shader, "model")
    view_loc  := gl.GetUniformLocation(shader, "view")
    proj_loc  := gl.GetUniformLocation(shader, "projection")

    //--- Block Model

    //bm := create_block_model()
    raw_blocks = load_babbit_block_model()
    state.attr.names = raw_blocks.attribute_keys

    n := len(raw_blocks.centers)
    instances = make([]Instance_Data, n)
    state.attr.data_min =  1e30
    state.attr.data_max = -1e30
    for i in 0..<n {
        instances[i].pos = raw_blocks.centers[i] - raw_blocks.bounds_center
        instances[i].scale = 0.95 * linalg.Vector3f32{raw_blocks.dx, raw_blocks.dy, raw_blocks.dz}
        val := raw_blocks.attributes[state.attr.names[0]][i]
        instances[i].value = val
        state.attr.data_min = math.min(state.attr.data_min, val)
        state.attr.data_max = math.max(state.attr.data_max, val)
        state.attr.filter_min = state.attr.data_min
    }
    current_attr := state.attr.names[state.attr.active_idx]
    fmt.printf("Initial attribute: %s\n", current_attr)

    state.bounds.diagonal = raw_blocks.bounds_diagonal

    initial_x, initial_y := glfw.GetCursorPos(window)
    state.orbit.mouse_last_x = initial_x
    state.orbit.mouse_last_y = initial_y
    state.orbit.center = linalg.Vector3f32{0, 0, 0}
    state.orbit.radius = 1.5 * state.bounds.diagonal

    state.camera.distance = state.orbit.radius
    state.camera.near = 0.1 * state.camera.distance
    state.camera.far = 5 * state.camera.distance

    vao: u32
    gl.GenVertexArrays(1, &vao)
    gl.BindVertexArray(vao)
    gl.EnableVertexAttribArray(0)
    gl.VertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, 3 * size_of(f32), 0)

    // 1. CUBE GEOMETRY BUFFER
    init_cube_mesh()

    // 2. INSTANCE OFFSET BUFFER (The point cloud)
    gl.GenBuffers(1, &instance_vbo)
    gl.BindBuffer(gl.ARRAY_BUFFER, instance_vbo)
    gl.BufferData(gl.ARRAY_BUFFER, len(instances) * size_of(Instance_Data), &instances[0], gl.DYNAMIC_DRAW)

    // Location 2: Offset (where the point is)
    gl.EnableVertexAttribArray(2)
    gl.VertexAttribPointer(2, 3, gl.FLOAT, gl.FALSE, size_of(Instance_Data), offset_of(Instance_Data, pos))
    gl.VertexAttribDivisor(2, 1)

    // Location 3: Scale (Per-axis)
    gl.EnableVertexAttribArray(3)
    gl.VertexAttribPointer(3, 3, gl.FLOAT, gl.FALSE, size_of(Instance_Data), offset_of(Instance_Data, scale))
    gl.VertexAttribDivisor(3, 1)

    // Location 4: Data Value
    gl.EnableVertexAttribArray(4)
    gl.VertexAttribPointer(4, 1, gl.FLOAT, gl.FALSE, size_of(Instance_Data), offset_of(Instance_Data, value))
    gl.VertexAttribDivisor(4, 1)

    gl.Enable(gl.TEXTURE_1D)
    gl.Enable(gl.DEPTH_TEST)
    gl.DepthFunc(gl.LESS)

    boxSize : f32 = 0.4
    palette_tex := create_palette_texture(copper_colors, true, 1)
    //palette_tex2 := create_palette_texture(generate_heat_palette(), true, 2)
    palette_tex2 := create_palette_texture(magma_colors, true, 2)

    for !glfw.WindowShouldClose(window) {
        glfw.PollEvents()
        gl.ClearColor(0.1, 0.1, 0.1, 1.0)
        gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

        cam_x := state.orbit.center.x + state.orbit.radius * math.cos(state.orbit.polar) * math.cos(state.orbit.azimuth)
        cam_y := state.orbit.center.y - state.orbit.radius * math.cos(state.orbit.polar) * math.sin(state.orbit.azimuth)
        cam_z := state.orbit.center.z + state.orbit.radius * math.sin(state.orbit.polar)

        state.camera.position = linalg.Vector3f32{cam_x, cam_y, cam_z}
        view := linalg.matrix4_look_at_f32(state.camera.position, state.orbit.center, {0, 0, 1})

        width, height := glfw.GetWindowSize(window)
        proj := linalg.matrix4_perspective_f32(math.to_radians(f32(45)), f32(width)/f32(height), state.camera.near, state.camera.far)
        gl.Viewport(0, 0, width, height)

        selected_idx := select_block(window, view, proj)
    
        gl.UseProgram(shader)

        gl.UniformMatrix4fv(view_loc, 1, gl.FALSE, &view[0, 0])
        gl.UniformMatrix4fv(proj_loc, 1, gl.FALSE, &proj[0, 0])

        gl.ActiveTexture(gl.TEXTURE0)
        gl.Uniform1i(gl.GetUniformLocation(shader, "palette"), 0)
        gl.Uniform1f(gl.GetUniformLocation(shader, "u_Intensity"), 0.001)
        gl.Uniform1i(gl.GetUniformLocation(shader, "u_Glow"), i32(glow_blocks))
        gl.Uniform1f(gl.GetUniformLocation(shader, "u_DataMin"), state.attr.data_min)
        gl.Uniform1f(gl.GetUniformLocation(shader, "u_DataMax"), state.attr.data_max)
        gl.Uniform1f(gl.GetUniformLocation(shader, "u_FilterMin"), state.attr.filter_min)

        gl.Uniform1i(gl.GetUniformLocation(shader, "useVariableSize"), i32(use_variable_size))
        if !use_variable_size {
            gl.Uniform3f(gl.GetUniformLocation(shader, "globalScale"), 5, 5, 5)
        }

        gl.Uniform1i(gl.GetUniformLocation(shader, "selectedID"), selected_idx)
        gl.BindVertexArray(vao)

        if glow_blocks {
            gl.BindTexture(gl.TEXTURE_1D, palette_tex2)
            gl.Enable(gl.BLEND)
            gl.BlendFunc(gl.ONE, gl.ONE_MINUS_SRC_COLOR)
            gl.DepthMask(false) 

            gl.DrawArraysInstanced(gl.TRIANGLES, 0, 36, i32(len(instances)))

            gl.DepthMask(true)
            gl.Disable(gl.BLEND)
        } else {
            gl.BindTexture(gl.TEXTURE_1D, palette_tex)
            gl.DrawArraysInstanced(gl.TRIANGLES, 0, 36, i32(n))
        }

        glfw.SwapBuffers(window)
    }
}
