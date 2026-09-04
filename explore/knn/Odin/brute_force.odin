package knn

insert_neighbor :: proc(neighbors: ^[]Neighbor, max_k: int, cand: Neighbor) {
    ns := neighbors^

    if ns.len < max_k {
        ns = append(ns, cand)
        for i := ns.len-1; i > 0; i -= 1 {
            if ns[i].distance < ns[i-1].distance {
                tmp := ns[i]
                ns[i] = ns[i-1]
                ns[i-1] = tmp
            } else {
                break
            }
        }
        neighbors^ = ns
        return
    }

    if cand.distance >= ns[ns.len-1].distance {
        return
    }

    ns[ns.len-1] = cand
    for i := ns.len-1; i > 0; i -= 1 {
        if ns[i].distance < ns[i-1].distance {
            tmp := ns[i]
            ns[i] = ns[i-1]
            ns[i-1] = tmp
        } else {
            break
        }
    }
    neighbors^ = ns
}

BruteForce3 :: struct {
    points: []Point3,
}

bruteforce3_init :: proc(allocator: Allocator, pts: []Point3) -> BruteForce3 {
    bf: BruteForce3
    bf.points = clone(pts, allocator)
    return bf
}

bruteforce3_query :: proc(bf: ^BruteForce3, q: Point3, k: int, allocator: Allocator) -> []Neighbor {
    neighbors := make([]Neighbor, 0, k, allocator)

    for p in bf.points {
        d := sq_dist3(q.pos, p.pos)
        insert_neighbor(&neighbors, k, Neighbor{id = p.id, distance = d})
    }

    return neighbors
}

bruteforce3_query_batch :: proc(bf: ^BruteForce3, queries: []Point3, k: int, allocator: Allocator) -> [][]Neighbor {
    results := make([][]Neighbor, queries.len, allocator)
    for i, q in queries {
        results[i] = bruteforce3_query(bf, q, k, allocator)
    }
    return results
}
