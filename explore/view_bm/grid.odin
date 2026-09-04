package main

import "core:math/linalg"

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
