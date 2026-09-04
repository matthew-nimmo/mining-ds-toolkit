package knn

import "core:testing"

@(test)
test_addition :: proc(t: ^testing.T) {
    result := 2 + 2
    testing.expect_value(t, result, 4)
}

test_kdtree3_correctness :: proc() {
    allocator := context.allocator

    n_points := 5000
    n_queries := 200
    k := 5

    pts := make([]Point3, n_points, allocator)
    for i in 0..<n_points {
        pts[i] = Point3{
            id = i,
            pos = [3]f64{rand.f64(), rand.f64(), rand.f64()},
        }
    }
 
    queries := make([]Point3, n_queries, allocator)
    for i in 0..<n_queries {
        queries[i] = Point3{
            id = -1,
            pos = [3]f64{rand.f64(), rand.f64(), rand.f64()},
        }
    }

    bf := bruteforce3_init(allocator, pts)
    kd := kd_tree3_init(allocator, pts)

    for i, q in queries {
        nb_bf := bruteforce3_query(&bf, q, k, allocator)
        nb_kd := kd_tree3_query(&kd, q, k, allocator)

        assert(nb_bf.len == nb_kd.len)

        for j in 0..<k {
            assert(nb_bf[j].id == nb_kd[j].id)
            assert(math.abs(nb_bf[j].distance - nb_kd[j].distance) < 1e-9)
        }
    }
}