package main

import "core:math/linalg"

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
