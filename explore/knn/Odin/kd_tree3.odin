package knn

import "core:math"

sq_dist3 :: proc(a, b: [3]f64) -> f64 {
    dx := a[0] - b[0]
    dy := a[1] - b[1]
    dz := a[2] - b[2]
    return dx*dx + dy*dy + dz*dz
}

kd_tree3_init :: proc(allocator: Allocator, pts: []Point3) -> KDTree3 {
    tree: KDTree3
    if pts.len == 0 {
        return tree
    }

    tree.points = clone(pts, allocator)
    tree.root = kd_tree3_build_rec(&tree, tree.points, 0, allocator)
    return tree
}

kd_tree3_build_rec :: proc(tree: ^KDTree3, pts: []Point3, depth: int, allocator: Allocator) -> ^KDNode3 {
    if len(pts) == 0 {
        return nil
    }

    axis := depth % 3

    sort.Slice(pts, proc(i, j: int) -> bool {
        return pts[i].pos[axis] < pts[j].pos[axis]
    })

    mid := len(pts) / 2

    node := new(KDNode3, allocator)
    node.point = pts[mid]
    node.axis  = axis

    node.left  = kd_tree3_build_rec(tree, pts[:mid], depth+1, allocator)
    node.right = kd_tree3_build_rec(tree, pts[mid+1:], depth+1, allocator)

    return node
}

kd_tree3_query :: proc(tree: ^KDTree3, q: Point3, k: int, allocator: Allocator) -> []Neighbor {
    neighbors : []Neighbor = make([]Neighbor, 0, k, allocator)
    kd_tree3_search_rec(tree.root, q, k, &neighbors)
    return neighbors
}

kd_tree3_search_rec :: proc(node: ^KDNode3, q: Point3, k: int, neighbors: ^[]Neighbor) {
    if node == nil {
        return
    }

    d := sq_dist3(q.pos, node.point.pos)
    insert_neighbor(neighbors, k, Neighbor{id = node.point.id, distance = d})

    axis := node.axis
    diff := q.pos[axis] - node.point.pos[axis]

    near: ^KDNode3
    far:  ^KDNode3
    if diff <= 0 {
        near, far = node.left, node.right
    } else {
        near, far = node.right, node.left
    }

    kd_tree3_search_rec(near, q, k, neighbors)

    ns := neighbors^
    worst : f64
    if len(ns) < k {
        worst = math.inf_f64(1)
    } else {
        worst = ns[len(ns)-1].distance
    }

    if diff*diff < worst {
        kd_tree3_search_rec(far, q, k, neighbors)
    }
}
