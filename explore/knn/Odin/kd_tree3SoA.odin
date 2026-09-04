package knn

import "core:math"
import "base:runtime"

kd_tree3_soa_init :: proc(allocator: Allocator, pts: []Point3) -> KDTree3_SoA {
    tree: KDTree3_SoA
    n := pts.len
    if n == 0 {
        return tree
    }

    tree.xs = make([]f64, n, allocator)
    tree.ys = make([]f64, n, allocator)
    tree.zs = make([]f64, n, allocator)
    tree.ids = make([]int, n, allocator)

    for i, p in pts {
        tree.xs[i] = p.pos[0]
        tree.ys[i] = p.pos[1]
        tree.zs[i] = p.pos[2]
        tree.ids[i] = p.id
    }

    indices := make([]int, n, allocator)
    for i in 0..<n {
        indices[i] = i
    }

    tree.root = kd_tree3_soa_build_rec(&tree, indices, 0, allocator)
    return tree
}

kd_tree3_soa_build_rec :: proc(tree: ^KDTree3_SoA, idxs: []int, depth: int, allocator: Allocator) -> ^KDNode3_SoA {
    if idxs.len == 0 {
        return nil
    }

    axis := depth % 3

    switch axis {
    case 0:
        sort.Slice(idxs, proc(i, j: int) -> bool {
            return tree.xs[idxs[i]] < tree.xs[idxs[j]]
        })
    case 1:
        sort.Slice(idxs, proc(i, j: int) -> bool {
            return tree.ys[idxs[i]] < tree.ys[idxs[j]]
        })
    case 2:
        sort.Slice(idxs, proc(i, j: int) -> bool {
            return tree.zs[idxs[i]] < tree.zs[idxs[j]]
        })
    }

    mid := idxs.len / 2
    idx := idxs[mid]

    node := new(KDNode3_SoA, allocator)
    node.index = idx
    node.axis  = axis

    node.left  = kd_tree3_soa_build_rec(tree, idxs[:mid], depth+1, allocator)
    node.right = kd_tree3_soa_build_rec(tree, idxs[mid+1:], depth+1, allocator)

    return node
}

sq_dist3_idx :: proc(tree: ^KDTree3_SoA, q: [3]f64, idx: int) -> f64 {
    dx := q[0] - tree.xs[idx]
    dy := q[1] - tree.ys[idx]
    dz := q[2] - tree.zs[idx]
    return dx*dx + dy*dy + dz*dz
}

kd_tree3_soa_query :: proc(tree: ^KDTree3_SoA, q: Point3, k: int, allocator: Allocator) -> []Neighbor {
    neighbors := make([]Neighbor, 0, k, allocator)
    kd_tree3_soa_search_rec(tree.root, tree, q.pos, k, &neighbors)
    return neighbors
}

kd_tree3_soa_search_rec :: proc(node: ^KDNode3_SoA, tree: ^KDTree3_SoA, q: [3]f64, k: int, neighbors: ^[]Neighbor) {
    if node == nil {
        return
    }

    d := sq_dist3_idx(tree, q, node.index)
    insert_neighbor(neighbors, k, Neighbor{id = tree.ids[node.index], distance = d})

    axis := node.axis
    coord : f64
    switch axis {
        case 0: coord = tree.xs[node.index]
        case 1: coord = tree.ys[node.index]
        case 2: coord = tree.zs[node.index]
    }

    diff := q[axis] - coord

    near: ^KDNode3_SoA
    far:  ^KDNode3_SoA
    if diff <= 0 {
        near, far = node.left, node.right
    } else {
        near, far = node.right, node.left
    }

    kd_tree3_soa_search_rec(near, tree, q, k, neighbors)

    ns := neighbors^
    worst : f64
    if ns.len < k {
        worst = math.inf_f64(1)
    } else {
        worst = ns[len(ns)-1].distance
    }

    if diff*diff < worst {
        kd_tree3_soa_search_rec(far, tree, q, k, neighbors)
    }
}
