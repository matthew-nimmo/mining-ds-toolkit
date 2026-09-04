package knn

type Point struct {
	ID   int           // Optional: useful for tracking original indices
	Pos  []float64     // Coordinates: [x,y,z,... latent dims ...]
}

type Point3 struct {
	ID  int
	Pos [3]float64
}

type Neighbor struct {
	ID       int
	Distance float64
}

type Searcher interface {
	// Build constructs the index from points. Implementations should copy
	// or own the slice as they see fit.
	Build(points []Point) error

	// Query finds k nearest neighbors to q.
	Query(q Point, k int) []Neighbor

	// QueryBatch may use concurrency internally.
	QueryBatch(queries []Point, k int) [][]Neighbor
}

func sqDist(a, b []float64) float64 {
	n := len(a)
	if len(b) < n {
		n = len(b)
	}
	var sum float64
	for i := 0; i < n; i++ {
		d := a[i] - b[i]
		sum += d * d
	}
	return sum
}

func sqDist3(a, b [3]float64) float64 {
	dx := a[0] - b[0]
	dy := a[1] - b[1]
	dz := a[2] - b[2]
	return dx*dx + dy*dy + dz*dz
}
