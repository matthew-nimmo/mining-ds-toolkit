package main

import "vendor:glfw"
import gl "vendor:OpenGL"

init_window :: proc() -> (glfw.WindowHandle) {
    glfw.WindowHint(glfw.CONTEXT_VERSION_MAJOR, 3)
    glfw.WindowHint(glfw.CONTEXT_VERSION_MINOR, 3)
    glfw.WindowHint(glfw.OPENGL_PROFILE, glfw.OPENGL_CORE_PROFILE)
    glfw.WindowHint(glfw.OPENGL_FORWARD_COMPAT, gl.TRUE) // Required for macOS

    window := glfw.CreateWindow(1280, 720, "Odin 3D Visualizer", nil, nil)
    if window == nil do return nil
    glfw.MakeContextCurrent(window)

    // Register Callbacks
    glfw.SetCursorPosCallback(window, mouse_callback)
    glfw.SetScrollCallback(window, scroll_callback)
    glfw.SetKeyCallback(window, key_callback)

    return window
}
