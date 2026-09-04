package main

import "core:fmt"
import "core:strings"
import "core:math"
import "core:math/linalg"
import gl "vendor:OpenGL"

Bounds :: struct {
    min : linalg.Vector3f32,
    max : linalg.Vector3f32,
    center : linalg.Vector3f32,
    size : linalg.Vector3f32,
    diagonal : f32
}

Block_Model :: struct {
    origin: linalg.Vector3f32,
    nx, ny, nz: u32,
    dx, dy, dz: f32,
    attributes: map[string][]f32,
    attribute_keys: []string,
    active_mask: []bool,

    centers: []linalg.Vector3f32,

    bounds_min: linalg.Vector3f32,
    bounds_max: linalg.Vector3f32,
    bounds_center: linalg.Vector3f32,
    bounds_size: linalg.Vector3f32,
    bounds_diagonal: f32
}

block_model_index :: proc(i, j, k: u32, nx, ny, nz: u32) -> u32 {
    return i + nx * (j + ny * k)
}

create_block_model :: proc() -> Block_Model {
    // synthetic 20x20x10 model
    nx, ny, nz : u32 = 20, 20, 10
    dx, dy, dz : f32 = 10.0, 10.0, 10.0

    bm : Block_Model
    bm.origin = linalg.Vector3f32{-100, 0, -100}
    bm.nx, bm.ny, bm.nz = nx, ny, nz
    bm.dx, bm.dy, bm.dz = dx, dy, dz

    total := nx * ny * nz
    bm.attributes = make(map[string][]f32)
    cu_attr := make([]f32, total)
    zn_attr := make([]f32, total)
    xyz := make([]linalg.Vector3f32, total)
    bm.active_mask = make([]bool, total)

    for k in 0..<nz {
        for j in 0..<ny {
            for i in 0..<nx {
                idx := block_model_index(i, j, k, nx, ny, nz)
                x := f32(i) * dx + bm.origin.x
                y := f32(j) * dy + bm.origin.y
                z := f32(k) * dz + bm.origin.z
                xyz[idx] = linalg.Vector3f32{x, y, z}

                // toy grade: radial pattern
                dist := linalg.length(xyz[idx] - linalg.Vector3f32{0, 50, 0})
                cu_attr[idx] = math.exp_f32(-dist * 0.01)
                zn_attr[idx] = cu_attr[idx] * 0.67 + 0.32
                bm.active_mask[idx] = true
            }
        }
    }

    bm.attributes["cu"] = cu_attr
    bm.attributes["zn"] = zn_attr
    bm.centers = xyz

    bm.bounds_min, bm.bounds_max = compute_bounds(raw_blocks.centers)
    bm.bounds_center = (bm.bounds_min + bm.bounds_max) / 2
    bm.bounds_size = bm.bounds_min - bm.bounds_max
    bm.bounds_diagonal = math.max(bm.bounds_size.x, bm.bounds_size.y, bm.bounds_size.z)

    bm.attribute_keys = make([]string, 2)
    bm.attribute_keys[0] = "cu"
    bm.attribute_keys[1] = "zn"

    return bm
}

load_babbit_block_model :: proc() -> Block_Model {
    bm : Block_Model

    rows, _ := read_csv_to_maps("babbit_blockmodel.csv")

    count := len(rows)
    bm.centers = make([]linalg.Vector3f32, count)
    cu_attr := make([]f32, count)
    zn_attr := make([]f32, count)
    active_mask := make([]bool, count)

    // dx/dy/dz – assume first row
    if count > 0 {
        bm.dx = parse_string(rows[0]["dx"])
        bm.dy = parse_string(rows[0]["dy"])
        bm.dz = parse_string(rows[0]["dz"])

        bm.nx = u32(parse_string(rows[0]["nx"]))
        bm.ny = u32(parse_string(rows[0]["ny"]))
        bm.nz = u32(parse_string(rows[0]["nz"]))
    }

    min := linalg.Vector3f32{+1e30, +1e30, +1e30}
    max := linalg.Vector3f32{-1e30, -1e30, -1e30}

    for row, i in rows {
        x := parse_string(row["x_center"])
        y := parse_string(row["y_center"])
        z := parse_string(row["z_center"])
        cu := parse_string(row["cu"])
        zn := cu * 0.7 + 0.32

        pos := linalg.Vector3f32{x, y, z}
        bm.centers[i] = pos

        cu_attr[i] = cu
        zn_attr[i] = zn
        active_mask[i] = true

        min = linalg.min_double(min, pos)
        max = linalg.max_double(max, pos)
    }

    bm.origin = min - linalg.Vector3f32{bm.dx*0.5, bm.dy*0.5, bm.dz*0.5}

    bm.attributes  = make(map[string][]f32)
    bm.attributes[strings.clone("cu")] = cu_attr
    bm.attributes[strings.clone("zn")] = zn_attr

    bm.active_mask = active_mask

    bm.bounds_min, bm.bounds_max = compute_bounds(bm.centers)
    bm.bounds_center = (bm.bounds_min + bm.bounds_max) / 2
    bm.bounds_size = bm.bounds_max - bm.bounds_min
    bm.bounds_diagonal = math.max(bm.bounds_size.x, bm.bounds_size.y, bm.bounds_size.z)

    bm.attribute_keys = make([]string, 2)
    bm.attribute_keys[0] = "cu"
    bm.attribute_keys[1] = "zn"

    // Manual cleanup for strings and maps (essential in Odin)
    for row in rows {
        for key, value in row {
            delete(key)
            delete(value)
        }
        delete(row)
    }
    delete(rows)

    return bm
}

update_active_attribute :: proc(instances: []Instance_Data, raw_blocks: Block_Model) {
    if len(state.attr.names) == 0 do return

    current_attr := state.attr.names[state.attr.active_idx]

    state.attr.data_min =  1e30
    state.attr.data_max = -1e30
    // Update the instance array with the new values from the map
    for &inst, i in instances {
        val := raw_blocks.attributes[current_attr][i]
        inst.value = val
        // Track min/max for the shader's normalization
        state.attr.data_min = math.min(state.attr.data_min, val)
        state.attr.data_max = math.max(state.attr.data_max, val)
        state.attr.filter_min = state.attr.data_min
    }

    // Re-upload the entire buffer to the GPU
    gl.BindBuffer(gl.ARRAY_BUFFER, instance_vbo)
    gl.BufferSubData(gl.ARRAY_BUFFER, 0, len(instances) * size_of(Instance_Data), &instances[0])
    
    //fmt.printf("Switched to: %s (Min: %f, Max: %f)\n", current_attr, attr_state.data_min, attr_state.data_max)
    fmt.printf("Switched to: %s\n", current_attr)
}
