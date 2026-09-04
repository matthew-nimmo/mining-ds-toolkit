package knn

import (
	"container/heap"
	"runtime"
	"sort"
	"sync"
)

//----------------------
// Optimized 3D KD-Tree k-nearest neighbour
// With SoA optimization
//----------------------

type kdTree3SoA struct {
	xs   []float64
	ys   []float64
	zs   []float64
	ids  []int
	root *kdNode3SoA

	minParallelSize int
}

type kdNode3SoA struct {
	index int           // index into xs/ys/zs/ids
	axis  int          // 0,1,2
	left  *kdNode3SoA
	right *kdNode3SoA
}

type Point3SoA struct {
	ID  int
	Pos [3]float64
}

func NewKDTree3SoA() *kdTree3SoA {
	return &kdTree3SoA{
		minParallelSize: 1 << 14,
	}
}

func (t *kdTree3SoA) Build(points []Point3SoA) error {
	n := len(points)
	if n == 0 {
		t.root = nil
		t.xs, t.ys, t.zs, t.ids = nil, nil, nil, nil
		return nil
	}

	t.xs = make([]float64, n)
	t.ys = make([]float64, n)
	t.zs = make([]float64, n)
	t.ids = make([]int, n)

	for i, p := range points {
		t.xs[i] = p.Pos[0]
		t.ys[i] = p.Pos[1]
		t.zs[i] = p.Pos[2]
		t.ids[i] = p.ID
	}

	indices := make([]int, n)
	for i := range indices {
		indices[i] = i
	}

	var wg sync.WaitGroup
	t.root = t.buildRec(indices, 0, &wg)
	wg.Wait()

	return nil
}

func (t *kdTree3SoA) buildRec(indices []int, depth int, wg *sync.WaitGroup) *kdNode3SoA {
	n := len(indices)
	if n == 0 {
		return nil
	}
	axis := depth % 3

	// sort indices based on coordinate along axis
	switch axis {
	case 0:
		sort.Slice(indices, func(i, j int) bool {
			return t.xs[indices[i]] < t.xs[indices[j]]
		})
	case 1:
		sort.Slice(indices, func(i, j int) bool {
			return t.ys[indices[i]] < t.ys[indices[j]]
		})
	case 2:
		sort.Slice(indices, func(i, j int) bool {
			return t.zs[indices[i]] < t.zs[indices[j]]
		})
	}

	mid := n / 2
	idx := indices[mid]

	node := &kdNode3SoA{
		index: idx,
		axis:  axis,
	}

	if n >= t.minParallelSize {
		wg.Add(2)
		go func() {
			defer wg.Done()
			node.left = t.buildRec(indices[:mid], depth+1, wg)
		}()
		go func() {
			defer wg.Done()
			node.right = t.buildRec(indices[mid+1:], depth+1, wg)
		}()
	} else {
		node.left = t.buildRec(indices[:mid], depth+1, wg)
		node.right = t.buildRec(indices[mid+1:], depth+1, wg)
	}

	return node
}

func (t *kdTree3SoA) sqDistIndex(q [3]float64, idx int) float64 {
	dx := q[0] - t.xs[idx]
	dy := q[1] - t.ys[idx]
	dz := q[2] - t.zs[idx]
	return dx*dx + dy*dy + dz*dz
}

func (t *kdTree3SoA) Query(q Point3SoA, k int) []Neighbor {
	n := len(t.xs)
	if t.root == nil || k <= 0 || n == 0 {
		return nil
	}
	if k > n {
		k = n
	}

	h := &maxHeap{}
	heap.Init(h)

	t.searchRec(t.root, q.Pos, k, h)

	res := make([]Neighbor, h.Len())
	for i := len(res) - 1; i >= 0; i-- {
		res[i] = heap.Pop(h).(Neighbor)
	}
	return res
}

func (t *kdTree3SoA) searchRec(node *kdNode3SoA, q [3]float64, k int, h *maxHeap) {
	if node == nil {
		return
	}

	d := t.sqDistIndex(q, node.index)
	if h.Len() < k {
		heap.Push(h, Neighbor{ID: t.ids[node.index], Distance: d})
	} else if d < (*h)[0].Distance {
		(*h)[0] = Neighbor{ID: t.ids[node.index], Distance: d}
		heap.Fix(h, 0)
	}

	var coord float64
	switch node.axis {
	case 0:
		coord = t.xs[node.index]
	case 1:
		coord = t.ys[node.index]
	case 2:
		coord = t.zs[node.index]
	}

	diff := q[node.axis] - coord

	var near, far *kdNode3SoA
	if diff <= 0 {
		near, far = node.left, node.right
	} else {
		near, far = node.right, node.left
	}

	t.searchRec(near, q, k, h)

	if h.Len() < k || diff*diff < (*h)[0].Distance {
		t.searchRec(far, q, k, h)
	}
}

func (t *kdTree3SoA) QueryBatch(queries []Point3SoA, k int) [][]Neighbor {
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
		q   Point3SoA
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
