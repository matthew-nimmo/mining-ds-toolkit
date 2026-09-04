package main

import "core:fmt"
import "core:strconv"
import "core:strings"
import "core:math"
import "core:os"
import "core:math/linalg"

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

compute_range :: proc(data: []f32) -> (f32, f32) {
    min := math.INF_F32
    max := math.NEG_INF_F32
	for i in 1..<len(data) {
		if data[i] < min {
			min = data[i]
		}
        if data[i] > max {
			max = data[i]
		}
	}

    return min, max
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
