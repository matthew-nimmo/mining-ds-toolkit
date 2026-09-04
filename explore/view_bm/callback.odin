package main

import "core:fmt"
import "core:math"
import "base:runtime"
import "vendor:glfw"
import "core:math/linalg"

ray_intersects_aabb :: proc(ray_origin, ray_dir, box_min, box_max: linalg.Vector3f32) -> (hit: bool, distance: f32) {
    t1 := (box_min.x - ray_origin.x) / ray_dir.x
    t2 := (box_max.x - ray_origin.x) / ray_dir.x
    t3 := (box_min.y - ray_origin.y) / ray_dir.y
    t4 := (box_max.y - ray_origin.y) / ray_dir.y
    t5 := (box_min.z - ray_origin.z) / ray_dir.z
    t6 := (box_max.z - ray_origin.z) / ray_dir.z

    tmin := math.max(math.max(math.min(t1, t2), math.min(t3, t4)), math.min(t5, t6))
    tmax := math.min(math.min(math.max(t1, t2), math.max(t3, t4)), math.max(t5, t6))

    if tmax < 0 || tmin > tmax do return false, 0

    return true, tmin
}

get_mouse_ray :: proc(window: glfw.WindowHandle, view, proj: linalg.Matrix4f32) -> (ray_origin, ray_dir: linalg.Vector3f32) {
    xpos, ypos := glfw.GetCursorPos(window)
    width, height := glfw.GetWindowSize(window)

    // 1. Convert Mouse to NDC (-1 to 1)
    x := (2.0 * f32(xpos)) / f32(width) - 1.0
    y := 1.0 - (2.0 * f32(ypos)) / f32(height)

    // 2. Unproject to View Space
    inv_proj := linalg.matrix4_inverse(proj)
    ray_clip := linalg.Vector4f32{x, y, -1.0, 1.0}
    ray_eye := inv_proj * ray_clip
    ray_eye = {ray_eye.x, ray_eye.y, -1.0, 0.0}

    // 3. Unproject to World Space
    inv_view := linalg.matrix4_inverse(view)
    ray_world_4 := inv_view * ray_eye
    ray_world := linalg.vector_normalize(ray_world_4.xyz)

    // Origin is just the camera's position (derived from inv_view)
    cam_pos := (inv_view * linalg.Vector4f32{0, 0, 0, 1}).xyz

    return cam_pos, ray_world
}

select_block :: proc(window: glfw.WindowHandle, view, proj: linalg.Matrix4f32) -> (block_idx: i32) {
    selected_idx: i32 = -1

    if glfw.GetMouseButton(window, glfw.MOUSE_BUTTON_RIGHT) == glfw.PRESS {
        ray_origin, ray_dir := get_mouse_ray(window, view, proj)
        closest_dist := f32(1e30)

        for p, i in instances {
            half := p.scale * 0.5
            b_min := p.pos - half
            b_max := p.pos + half
            hit, dist := ray_intersects_aabb(ray_origin, ray_dir, b_min, b_max)

            if hit && dist < closest_dist {
                closest_dist = dist
                selected_idx = i32(i)
            }
        }
    }

    return selected_idx
}

mouse_callback :: proc "c" (window: glfw.WindowHandle, xpos, ypos: f64) {
    // This line "boots up" the Odin runtime environment inside this C-style function
    // To be able to use fmt.println.
    //context = runtime.default_context()

    // Only rotate if the left button is down
    if glfw.GetMouseButton(window, glfw.MOUSE_BUTTON_LEFT) == glfw.PRESS {
        // Calculate the delta (change) in movement
        dx := f32(xpos - state.orbit.mouse_last_x)
        dy := f32(ypos - state.orbit.mouse_last_y)

        sensitivity : f32 = 0.005

        // Update angles
        state.orbit.azimuth += dx * sensitivity
        state.orbit.polar   += dy * sensitivity

        // CRITICAL: Clamp polar angle to prevent the camera from flipping 
        // upside down at the north/south poles.
        //epsilon : f32 = 0.01
        //polar = math.clamp(polar, epsilon, math.PI - epsilon)
    }

    // Always update last position so the next delta is small
    state.orbit.mouse_last_x = xpos
    state.orbit.mouse_last_y = ypos
}

scroll_callback :: proc "c" (window: glfw.WindowHandle, xoff, yoff: f64) {
    // 1. Determine a sensitivity factor (adjust to taste)
    // 0.1 means each notch zooms 10% of the current distance
    zoom_sensitivity : f32 = 0.05

    // 2. Adjust the distance exponentially
    if yoff > 0 {
        // Zoom In: Reduce distance by a percentage
        state.orbit.radius *= (1.0 - zoom_sensitivity)
    } else {
        // Zoom Out: Increase distance by a percentage
        state.orbit.radius *= (1.0 + zoom_sensitivity)
    }

    // 3. Clamp the distance so you don't zoom through the center 
    // or out into deep space
    state.orbit.radius = math.clamp(state.orbit.radius, state.bounds.diagonal * 0.1, state.bounds.diagonal * 2.0)
}

key_callback :: proc "c" (window: glfw.WindowHandle, key, scancode, action, mods: i32) {
    context = runtime.default_context()

    if action != glfw.PRESS do return

    if key == glfw.KEY_ESCAPE {
        glfw.SetWindowShouldClose(window, true)
    }

    if key == glfw.KEY_TAB {
        // Cycle to next attribute
        state.attr.active_idx = (state.attr.active_idx + 1) % len(state.attr.names)
        update_active_attribute(instances, raw_blocks)
    }

    if key == glfw.KEY_UP {
        // Increase filter minimum.
        if state.attr.filter_min < (state.attr.data_max - 0.1) {
            state.attr.filter_min = (state.attr.filter_min + 0.1)
        }
    }

    if key == glfw.KEY_DOWN {
        // Increase filter minimum.
        if state.attr.filter_min > (state.attr.data_min + 0.1) {
            state.attr.filter_min = (state.attr.filter_min - 0.1)
        }
    }

    if key == glfw.KEY_G {
        glow_blocks = !glow_blocks
    }
}
