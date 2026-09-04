package main

import "core:strings"
import "core:fmt"
import "core:os"
import gl "vendor:OpenGL"

load_text_file :: proc(path: string) -> string {
	// Specify the path to your text file
	filepath := path

	// Read the entire file into a byte slice.
	// The memory is tracked by context.allocator.
	data, ok := os.read_entire_file(filepath, context.allocator)
	if ok != nil {
		fmt.printf("Error reading file: %s\n", filepath)
		return ""
	}

	// Ensure the allocated memory is freed when the procedure returns
    defer delete(data, context.allocator)

	// Transmute the byte slice into a string
    tmp_content := string(data)
	content := strings.clone(tmp_content)

    return content
}

compile_shader :: proc(vs_path: string, fs_path: string) -> u32 {
    vs_src := load_text_file(vs_path)
    vs_cstr_ptr := strings.clone_to_cstring(vs_src, context.allocator)
    defer delete(vs_cstr_ptr)

    fs_src := load_text_file(fs_path)
    fs_cstr_ptr := strings.clone_to_cstring(fs_src, context.allocator)
    defer delete(fs_cstr_ptr)

    vs := gl.CreateShader(gl.VERTEX_SHADER)
    gl.ShaderSource(vs, 1, &vs_cstr_ptr, nil)
    gl.CompileShader(vs)
    check_shader_compile(vs)

    fs := gl.CreateShader(gl.FRAGMENT_SHADER)
    gl.ShaderSource(fs, 1, &fs_cstr_ptr, nil)
    gl.CompileShader(fs)
    check_shader_compile(fs)

    prog := gl.CreateProgram()
    gl.AttachShader(prog, vs)
    gl.AttachShader(prog, fs)
    gl.LinkProgram(prog)
    check_program_link(prog)

    gl.DeleteShader(vs)
    gl.DeleteShader(fs)

    return prog
}

check_shader_compile :: proc(shader: u32) {
    success: i32
    gl.GetShaderiv(shader, gl.COMPILE_STATUS, &success);
    if success == 0 {
        log_buf: [512]byte
        gl.GetProgramInfoLog(shader, 512, nil, &log_buf[0])
        fmt.printf("Shader Compile Error: %s\n", log_buf)
    }
}

check_program_link :: proc(shader: u32) {
    success: i32
    gl.GetProgramiv(shader, gl.LINK_STATUS, &success)
    if success == 0 {
        log_buf: [512]byte
        gl.GetProgramInfoLog(shader, 512, nil, &log_buf[0])
        fmt.printf("Shader Link Error: %s\n", log_buf)
    }
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
