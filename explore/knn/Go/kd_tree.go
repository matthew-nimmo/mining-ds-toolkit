package knn

import (
	"container/heap"
	"runtime"
	"sort"
	"sync"
)

//----------------------
// Standard KD-Tree k-nearest neighbour
//----------------------

type kdNode struct {
	point Point
	left  *kdNode
	right *kdNode
	axis  int
}

type kdTree struct {
	root      *kdNode
	dimension int
	points    []Point // Keep a copy if needed for debugging or rebuilding
}

func NewKDTree() Searcher {
	return &kdTree{}
}

func (t *kdTree) Build(points []Point) error {
	if len(points) == 0 {
		t.root = nil
		t.dimension = 0
		t.points = nil
		return nil
	}
	// Assume all points have same dimension as first
	t.dimension = len(points[0].Pos)
	t.points = append([]Point(nil), points...)

	t.root = t.buildRec(t.points, 0)
	return nil
}

func (t *kdTree) buildRec(pts []Point, depth int) *kdNode {
	if len(pts) == 0 {
		return nil
	}
	axis := depth % t.dimension

	sort.Slice(pts, func(i, j int) bool {
		return pts[i].Pos[axis] < pts[j].Pos[axis]
	})

	median := len(pts) / 2

	node := &kdNode{
		point: pts[median],
		axis:  axis,
	}

	node.left = t.buildRec(pts[:median], depth+1)
	node.right = t.buildRec(pts[median+1:], depth+1)

	return node
}

func (t *kdTree) Query(q Point, k int) []Neighbor {
	if t.root == nil || k <= 0 {
		return nil
	}
	if k > len(t.points) {
		k = len(t.points)
	}

	h := &maxHeap{}
	heap.Init(h)

	t.searchRec(t.root, q, k, h)

	res := make([]Neighbor, h.Len())
	for i := len(res) - 1; i >= 0; i-- {
		res[i] = heap.Pop(h).(Neighbor)
	}
	return res
}

func (t *kdTree) searchRec(node *kdNode, q Point, k int, h *maxHeap) {
	if node == nil {
		return
	}

	// Update heap with current node
	d := sqDist(q.Pos, node.point.Pos)
	if h.Len() < k {
		heap.Push(h, Neighbor{ID: node.point.ID, Distance: d})
	} else if d < (*h)[0].Distance {
		(*h)[0] = Neighbor{ID: node.point.ID, Distance: d}
		heap.Fix(h, 0)
	}

	axis := node.axis
	diff := q.Pos[axis] - node.point.Pos[axis]

	var near, far *kdNode
	if diff <= 0 {
		near, far = node.left, node.right
	} else {
		near, far = node.right, node.left
	}

	// Search nearer side first
	t.searchRec(near, q, k, h)

	// Decide whether to search the far side
	// Compare squared distance along axis with worst distance in heap.
	if h.Len() < k || diff*diff < (*h)[0].Distance {
		t.searchRec(far, q, k, h)
	}
}

func (t *kdTree) QueryBatch(queries []Point, k int) [][]Neighbor {
	n := len(queries)
	results := make([][]Neighbor, n)

	if n == 0 {
		return results
	}

	// Number of workers: tuneable; start with NumCPU
	workers := runtime.NumCPU()
	if workers > n {
		workers = n
	}

	type job struct {
		idx int
		q   Point
	}

	jobs := make(chan job, workers)
	var wg sync.WaitGroup
	wg.Add(workers)

	for w := 0; w < workers; w++ {
		go func() {
			defer wg.Done()
			for j := range jobs {
				results[j.idx] = t.Query(j.q, k)
			}
		}()
	}

	for i, q := range queries {
		jobs <- job{idx: i, q: q}
	}
	close(jobs)

	wg.Wait()
	return results
}

/*
// Concurrency batch version

import "runtime"

func (t *kdTree) QueryBatch(queries []Point, k int) [][]Neighbor {
	n := len(queries)
	results := make([][]Neighbor, n)

	if n == 0 {
		return results
	}

	// Number of workers: tuneable; start with NumCPU
	workers := runtime.NumCPU()
	if workers > n {
		workers = n
	}

	type job struct {
		idx int
		q   Point
	}

	jobs := make(chan job, workers)
	var wg sync.WaitGroup
	wg.Add(workers)

	for w := 0; w < workers; w++ {
		go func() {
			defer wg.Done()
			for j := range jobs {
				results[j.idx] = t.Query(j.q, k)
			}
		}()
	}

	for i, q := range queries {
		jobs <- job{idx: i, q: q}
	}
	close(jobs)

	wg.Wait()
	return results
}
*/