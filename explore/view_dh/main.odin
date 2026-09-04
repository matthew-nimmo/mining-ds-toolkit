package main

import "core:strconv"
import "core:os"
import "core:strings"
import "base:runtime"
import "core:fmt"
import "core:math"
import "core:math/linalg"
import gl "vendor:OpenGL"
import "vendor:glfw"
import "core:mem"

/*
Pro-Tip: The Odin "Hot Reload" mindset
As you expand this into a 3D app, keep in mind that Odin is very fast to compile.
You can often keep your shader strings in external .glsl files and write a small
function to re-read and re-compile them while the app is running.
This allows you to tweak visuals without restarting the program!
*/

vertex_source := `#version 330 core
layout (location = 0) in vec3 aPos;
//layout (location = 1) in vec3 aColor;
layout (location = 1) in float aValue;

uniform mat4 view;
uniform mat4 projection;
uniform vec3 fallbackColor;
uniform bool useVertexColor;

//out vec3 vColor;
out float vDataValue;

void main() {
    gl_Position = projection * view * vec4(aPos, 1.0);
    //vColor = useVertexColor ? aColor : fallbackColor;
    vDataValue = aValue;
}`

fragment_source := `#version 330 core
in float vDataValue;
//in vec3 vColor; // Received from vertex shader

uniform sampler1D palette;
//uniform float u_DataMin;
//uniform float u_DataMax;
//uniform float u_FilterMin;

out vec4 FragColor;

void main() {
    //if (vDataValue < u_FilterMin) {
    //    discard; // This block won't be rendered at all
    //}

    vec3 baseColor = texture(palette, clamp(vDataValue, 0.0, 1.0)).rgb;
    FragColor = vec4(baseColor, 1.0);
}`

// Camera State
Orbit_State :: struct {
    center:       linalg.Vector3f32,
    radius:       f32,
    azimuth:      f32, // Horizontal angle
    polar:        f32, // Vertical angle
    mouse_last_x: f64,
    mouse_last_y: f64,
}

state := Orbit_State {
    center  = {0, 0, 0},
    radius  = 5.0,
    azimuth = 0.0,
    polar   = math.PI / 4.0,
}

Render_Object :: struct {
    vao: u32,
    vbo: u32,
    count: i32,
    mode: u32, // gl.POINTS, gl.LINES, etc.
}

Instance_Data :: struct {
    pos:   linalg.Vector3f32,
    value: f32,
}

Drillholes :: struct {
    id: string,
    attributes: map[string][]f32,
    active_mask: []bool,

    centers: []linalg.Vector3f32,
}

Attribute_State :: struct {
    names:       [dynamic]string,
    active_idx:  int,
    data_min:    f32,
    data_max:    f32,
    filter_min: f32,
}

attr_state: Attribute_State
useVertexColor_loc: i32
fallbackColor_loc:  i32
model_diagonal : f32
camera_distance : f32
instances : []Instance_Data
raw_dh : Drillholes

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

main :: proc() {
    if !bool(glfw.Init()) do return
    defer glfw.Terminate()

    glfw.WindowHint(glfw.CONTEXT_VERSION_MAJOR, 3)
    glfw.WindowHint(glfw.CONTEXT_VERSION_MINOR, 3)
    glfw.WindowHint(glfw.OPENGL_PROFILE, glfw.OPENGL_CORE_PROFILE)
    glfw.WindowHint(glfw.OPENGL_FORWARD_COMPAT, gl.TRUE) // Required for macOS

    window := glfw.CreateWindow(1280, 720, "Odin 3D Visualizer", nil, nil)
    if window == nil do return
    glfw.MakeContextCurrent(window)
    gl.load_up_to(4, 5, glfw.gl_set_proc_address)

    shader := create_shader_program(vertex_source, fragment_source)

    useVertexColor_loc = gl.GetUniformLocation(shader, "useVertexColor\x00")
    fallbackColor_loc  = gl.GetUniformLocation(shader, "fallbackColor\x00")
    success: i32
    gl.GetProgramiv(shader, gl.LINK_STATUS, &success)
    if success == 0 {
        log_buf: [512]byte
        gl.GetProgramInfoLog(shader, 512, nil, &log_buf[0])
        fmt.printf("Shader Link Error: %s\n", log_buf)
    }
    gl.UseProgram(shader)

    // Get uniform locations
    model_loc := gl.GetUniformLocation(shader, "model")
    view_loc  := gl.GetUniformLocation(shader, "view")
    proj_loc  := gl.GetUniformLocation(shader, "projection")

    // Register Callbacks
    glfw.SetCursorPosCallback(window, mouse_callback)
    glfw.SetScrollCallback(window, scroll_callback)

    //--- Point Cloud

    // Create a simple point cloud
    //points := create_point_cloud(1000)
    raw_dh = load_babbit_drillhole_samples()
    init_attribute_list(&raw_dh)

    min_bound, max_bound := compute_bounds(raw_dh.centers)
    center := (min_bound + max_bound) / 2
    model_size := max_bound - min_bound
    model_diagonal = math.max(model_size.x, math.max(model_size.y, model_size.z))

    n := len(raw_dh.centers)
    instances = make([]Instance_Data, n)
    attr_state.data_min =  1e30
    attr_state.data_max = -1e30
    for i in 0..<n {
        instances[i].pos = raw_dh.centers[i] - center
        val := raw_dh.attributes[attr_state.names[0]][i]
        instances[i].value = val
        attr_state.data_min = math.min(attr_state.data_min, val)
        attr_state.data_max = math.max(attr_state.data_max, val)
        attr_state.filter_min = attr_state.data_min
    }
    current_attr := attr_state.names[attr_state.active_idx]
    fmt.println(attr_state.data_min, attr_state.data_max)

    state.center = linalg.Vector3f32{0, 0, 0}
    state.radius = 1.5 * model_diagonal
    camera_distance = state.radius
    near := 0.1 * camera_distance
    far  := 5 * camera_distance
    
    // Simple Vertex Buffer Setup
    vbo: u32
    gl.GenBuffers(1, &vbo)
    gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
    gl.BufferData(gl.ARRAY_BUFFER, len(instances) * size_of(Instance_Data), &instances[0], gl.STATIC_DRAW)

    vao: u32
    gl.GenVertexArrays(1, &vao)
    gl.BindVertexArray(vao)

    // Location 0: Location
    gl.EnableVertexAttribArray(0)
    gl.VertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, size_of(Instance_Data), offset_of(Instance_Data, pos))

    // Location 1: Data Value
    gl.EnableVertexAttribArray(1)
    gl.VertexAttribPointer(1, 1, gl.FLOAT, gl.FALSE, size_of(Instance_Data), offset_of(Instance_Data, value))

    gl.Enable(gl.PROGRAM_POINT_SIZE)
    gl.PointSize(3.0)

    initial_x, initial_y := glfw.GetCursorPos(window)
    state.mouse_last_x = initial_x
    state.mouse_last_y = initial_y

    //--- Axes

    //Axis_Vertex :: struct {
    //    pos: linalg.Vector3f32,
    //    col: linalg.Vector3f32,
    //}

    //axes := [6]Axis_Vertex{
    //    {{0,0,0}, {1,0,0}}, {{2,0,0}, {1,0,0}}, // X - Red
    //    {{0,0,0}, {0,1,0}}, {{0,2,0}, {0,1,0}}, // Y - Green
    //    {{0,0,0}, {0,0,1}}, {{0,0,2}, {0,0,1}}, // Z - Blue
    //}

    //axis_vbo, axis_vao: u32
    //gl.GenVertexArrays(1, &axis_vao)
    //gl.GenBuffers(1, &axis_vbo)

    //gl.BindVertexArray(axis_vao)
    //gl.BindBuffer(gl.ARRAY_BUFFER, axis_vbo)
    //gl.BufferData(gl.ARRAY_BUFFER, size_of(axes), &axes[0], gl.STATIC_DRAW)

    // Position Attribute (Location 0)
    //gl.EnableVertexAttribArray(0)
    //gl.VertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, size_of(Axis_Vertex), offset_of(Axis_Vertex, pos))

    // Color Attribute (Location 1)
    //gl.EnableVertexAttribArray(1)
    //gl.VertexAttribPointer(1, 3, gl.FLOAT, gl.FALSE, size_of(Axis_Vertex), offset_of(Axis_Vertex, col))

    //--- Grid

    // Create grid
    //grid := create_grid_data(5, 5)
    //grid_vertex_count : i32 = cast(i32)len(grid)

    // Simple Vertex Buffer Setup
    //grid_vbo: u32
    //gl.GenBuffers(1, &grid_vbo)
    //gl.BindBuffer(gl.ARRAY_BUFFER, grid_vbo)
    //gl.BufferData(gl.ARRAY_BUFFER, len(grid) * size_of(linalg.Vector3f32), &grid[0], gl.STATIC_DRAW)

    //grid_vao: u32
    //gl.GenVertexArrays(1, &grid_vao)
    //gl.BindVertexArray(grid_vao)
    //gl.EnableVertexAttribArray(0)
    //gl.VertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, 3 * size_of(f32), 0)

    //gl.Enable(gl.PROGRAM_POINT_SIZE)
    //gl.PointSize(3.0)
    palette_tex := create_palette_texture(copper_colors, true, 1)
    for !glfw.WindowShouldClose(window) {
        glfw.PollEvents()

        // 1. CLEAR
        gl.ClearColor(0.1, 0.1, 0.1, 1.0)
        gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

        // 2. CALCULATE (Math must happen inside the loop!)
        cam_x := state.center.x + state.radius * math.cos(state.polar) * math.cos(state.azimuth)
        cam_y := state.center.y - state.radius * math.cos(state.polar) * math.sin(state.azimuth)
        cam_z := state.center.z + state.radius * math.sin(state.polar)

        camera_pos := linalg.Vector3f32{cam_x, cam_y, cam_z}
        view := linalg.matrix4_look_at_f32(camera_pos, state.center, {0, 0, 1})

        // Get window size for aspect ratio in case you resize
        width, height := glfw.GetWindowSize(window)
        proj := linalg.matrix4_perspective_f32(math.to_radians(f32(45)), f32(width)/f32(height), near, far)

        // 3. UPLOAD (This is the part that updates the GPU)
        gl.UseProgram(shader)

        // linalg matrices are [4,4]f32, we pass a pointer to the first element [0,0]
        gl.UniformMatrix4fv(view_loc, 1, gl.FALSE, &view[0, 0])
        gl.UniformMatrix4fv(proj_loc, 1, gl.FALSE, &proj[0, 0])

        model_mat := linalg.MATRIX4F32_IDENTITY
        gl.UniformMatrix4fv(model_loc, 1, gl.FALSE, &model_mat[0, 0])

        // --- 1. Draw the Grid ---
        //gl.Uniform1i(useVertexColor_loc, 0) // Tell shader: Use the fallback color
        //gl.Uniform3f(fallbackColor_loc, 0.3, 0.3, 0.3) // Set fallback to dark gray
        //gl.BindVertexArray(grid_vao)
        //gl.DrawArrays(gl.LINES, 0, grid_vertex_count)

        // --- 2. Draw the Axes ---
        //gl.Uniform1i(useVertexColor_loc, 1) // Tell shader: Use the colors from the vertex buffer
        //gl.BindVertexArray(axis_vao)
        //gl.DrawArrays(gl.LINES, 0, 6)

        // --- 3. Draw point cloud ---
        gl.BindTexture(gl.TEXTURE_1D, palette_tex)
        gl.Uniform1i(useVertexColor_loc, 1)
        gl.Uniform3f(fallbackColor_loc, 1.0, 1.0, 1.0)
        gl.BindVertexArray(vao)
        gl.DrawArrays(gl.POINTS, 0, i32(n))

        glfw.SwapBuffers(window)
    }
}

mouse_callback :: proc "c" (window: glfw.WindowHandle, xpos, ypos: f64) {
    // This line "boots up" the Odin runtime environment inside this C-style function
    // To be able to use fmt.println.
    //context = runtime.default_context()

    // Only rotate if the left button is down
    if glfw.GetMouseButton(window, glfw.MOUSE_BUTTON_LEFT) == glfw.PRESS {
        // Calculate the delta (change) in movement
        dx := f32(xpos - state.mouse_last_x)
        dy := f32(ypos - state.mouse_last_y)

        sensitivity : f32 = 0.005
        
        // Update angles
        state.azimuth += dx * sensitivity
        state.polar   += dy * sensitivity

        // CRITICAL: Clamp polar angle to prevent the camera from flipping 
        // upside down at the north/south poles.
        //epsilon : f32 = 0.01
        //state.polar = math.clamp(state.polar, epsilon, math.PI - epsilon)
    }

    // Always update last position so the next delta is small
    state.mouse_last_x = xpos
    state.mouse_last_y = ypos
}

scroll_callback :: proc "c" (window: glfw.WindowHandle, xoff, yoff: f64) {
    // 1. Determine a sensitivity factor (adjust to taste)
    // 0.1 means each notch zooms 10% of the current distance
    zoom_sensitivity : f32 = 0.05

    // 2. Adjust the distance exponentially
    if yoff > 0 {
        // Zoom In: Reduce distance by a percentage
        state.radius *= (1.0 - zoom_sensitivity)
    } else {
        // Zoom Out: Increase distance by a percentage
        state.radius *= (1.0 + zoom_sensitivity)
    }

    // 3. Clamp the distance so you don't zoom through the center 
    // or out into deep space
    state.radius = math.clamp(state.radius, model_diagonal * 0.1, model_diagonal * 2.0)
}

create_shader_program :: proc(v_source, f_source: string) -> u32 {
    vs := gl.CreateShader(gl.VERTEX_SHADER)
    v_src_ptr := cstring(raw_data(v_source))
    gl.ShaderSource(vs, 1, &v_src_ptr, nil)
    gl.CompileShader(vs)

    fs := gl.CreateShader(gl.FRAGMENT_SHADER)
    f_src_ptr := cstring(raw_data(f_source))
    gl.ShaderSource(fs, 1, &f_src_ptr, nil)
    gl.CompileShader(fs)

    program := gl.CreateProgram()
    gl.AttachShader(program, vs)
    gl.AttachShader(program, fs)
    gl.LinkProgram(program)

    gl.DeleteShader(vs)
    gl.DeleteShader(fs)

    return program
}

create_point_cloud :: proc(n: int) -> []linalg.Vector3f32 {
    data := make([]linalg.Vector3f32, n)
    for i in 0..<n {
        data[i] = {
            (f32(i % 10) - 5.0) * 0.5,
            (f32((i / 10) % 10) - 5.0) * 0.5,
            (f32(i / 100) - 5.0) * 0.5,
        }
    }
    return data
}

create_grid_data :: proc(size: f32, divisions: int) -> []linalg.Vector3f32 {
    // 2 points per line * 2 sets of lines (horizontal/vertical)
    line_count := (divisions + 1) * 2
    data := make([]linalg.Vector3f32, line_count * 2)
    
    step := size / f32(divisions)
    half := size / 2.0
    
    curr := 0
    for i in 0..=divisions {
        f_i := -half + f32(i) * step
        
        // Line along X
        data[curr]   = {-half, 0, f_i}; curr += 1
        data[curr]   = { half, 0, f_i}; curr += 1
        
        // Line along Z
        data[curr]   = {f_i, 0, -half}; curr += 1
        data[curr]   = {f_i, 0,  half}; curr += 1
    }
    return data
}

parse_string :: proc(str: string) -> f32 {
    // 1. Trim whitespace (crucial for CSVs)
    trimmed := strings.trim_space(str)
    
    // 2. Parse and check success
    val, ok := strconv.parse_f32(trimmed)
    if !ok {
        // If it fails, print the offending string so you can see the invisible character
        //fmt.printf("Error: Could not parse float from string: '%s'\n", str)
        return math.nan_f32() // Or a sentinel value like math.F32_NAN
    }
    return val
}

read_csv_to_maps :: proc(filepath: string) -> ([]map[string]string, bool) {
    // 1. Read the entire file into memory
    data, err := os.read_entire_file(filepath, context.allocator)
    if err != nil {
        fmt.println("Failed to read file:", filepath)
        return nil, false
    }
    // Ensure we free the raw file data when the function returns
    defer delete(data)

    content := string(data)
    lines := strings.split_lines(content)
    defer delete(lines)

    if len(lines) < 2 do return nil, false

    // 2. Parse Header
    header := strings.split(lines[0], ",")
    defer delete(header)

    // 3. Initialize the result slice
    results := make([]map[string]string, len(lines) - 1)

    for i := 1; i < len(lines); i += 1 {
        if lines[i] == "" do continue // Skip empty lines
        
        row_values := strings.split(lines[i], ",")
        defer delete(row_values)

        row_map := make(map[string]string)
        
        for val, j in row_values {
            if j < len(header) {
                // Use strings.clone to ensure the data persists 
                // after 'data' is deleted
                key := strings.clone(header[j])
                value := strings.clone(val)
                row_map[key] = value
            }
        }
        results[i-1] = row_map
    }

    return results, true
}

load_babbit_drillhole_samples :: proc() -> Drillholes {
    dh: Drillholes

    rows, _ := read_csv_to_maps("babbit_drillholes_xyz_samples.csv")

    count := len(rows)
    dh.centers = make([]linalg.Vector3f32, count)
    cu_attr := make([]f32, count)
    active_mask := make([]bool, count)

    if count > 0 {
        dh.id = rows[0]["BHID"]
    }

    for row, i in rows {
        x := parse_string(row["xm"])
        y := parse_string(row["ym"])
        z := parse_string(row["zm"])
        cu := parse_string(row["CU"])

        pos := linalg.Vector3f32{x, y, z}
        dh.centers[i] = pos
        cu_attr[i] = cu
    }

    dh.attributes[strings.clone("cu")] = cu_attr
    dh.active_mask = active_mask

    // Manual cleanup for strings and maps (essential in Odin)
    for row in rows {
        for key, value in row {
            delete(key)
            delete(value)
        }
        delete(row)
    }
    delete(rows)

    return dh
}

compute_bounds :: proc(data: []linalg.Vector3f32) -> (linalg.Vector3f32, linalg.Vector3f32) {
    min := linalg.Vector3f32{math.INF_F32, math.INF_F32, math.INF_F32}
    max := linalg.Vector3f32{math.NEG_INF_F32, math.NEG_INF_F32, math.NEG_INF_F32}
	for i in 1..<len(data) {
		if data[i].x < min.x {
			min.x = data[i].x
		}
		if data[i].y < min.y {
			min.y = data[i].y
		}
		if data[i].z < min.z {
			min.z = data[i].z
		}

        if data[i].x > max.x {
			max.x = data[i].x
		}
		if data[i].y > max.y {
			max.y = data[i].y
		}
		if data[i].z > max.z {
			max.z = data[i].z
		}
	}

    return min, max
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

init_attribute_list :: proc(dh: ^Drillholes) {
    for name, _ in dh.attributes {
        append(&attr_state.names, strings.clone(name))
    }
}
