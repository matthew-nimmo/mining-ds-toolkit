package knn

import "container/heap"

//----------------------
// Brute Force k-nearest neighbour
//----------------------

type bruteForce struct {
	points []Point
}

func NewBruteForce() Searcher {
	return &bruteForce{}
}

func (bf *bruteForce) Build(points []Point) error {
	// Shallow copy is fine since we don’t mutate points
	bf.points = append([]Point(nil), points...)
	return nil
}

func (bf *bruteForce) Query(q Point, k int) []Neighbor {
	if k <= 0 || len(bf.points) == 0 {
		return nil
	}
	if k > len(bf.points) {
		k = len(bf.points)
	}

	h := &maxHeap{}
	heap.Init(h)

	for _, p := range bf.points {
		d := sqDist(q.Pos, p.Pos)

		if h.Len() < k {
			heap.Push(h, Neighbor{ID: p.ID, Distance: d})
		} else if d < (*h)[0].Distance {
			(*h)[0] = Neighbor{ID: p.ID, Distance: d}
			heap.Fix(h, 0)
		}
	}

	// Extract and reverse to get nearest -> farthest
	res := make([]Neighbor, h.Len())
	for i := len(res) - 1; i >= 0; i-- {
		res[i] = heap.Pop(h).(Neighbor)
	}
	return res
}

func (bf *bruteForce) QueryBatch(queries []Point, k int) [][]Neighbor {
	out := make([][]Neighbor, len(queries))
	// Simple serial version; we’ll add concurrency pattern you can reuse
	for i, q := range queries {
		out[i] = bf.Query(q, k)
	}
	return out
}

// --- max-heap implementation on distance ---

type maxHeap []Neighbor

func (h maxHeap) Len() int            { return len(h) }
func (h maxHeap) Less(i, j int) bool  { return h[i].Distance > h[j].Distance } // max-heap
func (h maxHeap) Swap(i, j int)       { h[i], h[j] = h[j], h[i] }
func (h *maxHeap) Push(x interface{}) { *h = append(*h, x.(Neighbor)) }
func (h *maxHeap) Pop() interface{} {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[:n-1]
	return x
}
