package knn

Point3 :: struct {
    id:  int,
    pos: [3]f64,
}

Neighbor :: struct {
    id:       int,
    distance: f64,
}

KDNode3 :: struct {
    point: Point3,
    left:  ^KDNode3,
    right: ^KDNode3,
    axis:  int,
}

KDTree3 :: struct {
    root: ^KDNode3,
    points: []Point3,
}

KDNode3_SoA :: struct {
    index: int,
    axis:  int,
    left:  ^KDNode3_SoA,
    right: ^KDNode3_SoA,
}

KDTree3_SoA :: struct {
    xs: []f64
    ys: []f64
    zs: []f64
    ids: []int

    root: ^KDNode3_SoA
}
