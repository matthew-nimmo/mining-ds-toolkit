package knn

import (
	"container/heap"
	"runtime"
	"sort"
	"sync"
)

//----------------------
// Optimized 3D KD-Tree k-nearest neighbour
// With concurrency build
//----------------------

type kdTree3cc struct {
	root   *kdNode3
	points []Point3
	// tuneable parameter: min size of slice to split in parallel
	minParallelSize int
}

func NewKDTree3cc() *kdTree3cc {
	return &kdTree3cc{
		minParallelSize: 1 << 14, // e.g. 16k; tune experimentally
	}
}

func (t *kdTree3cc) Build(points []Point3) error {
	if len(points) == 0 {
		t.root = nil
		t.points = nil
		return nil
	}
	t.points = append([]Point3(nil), points...)

	var wg sync.WaitGroup
	t.root = t.buildRec(t.points, 0, &wg)
	wg.Wait()

	return nil
}

func (t *kdTree3cc) buildRec(pts []Point3, depth int, wg *sync.WaitGroup) *kdNode3 {
	n := len(pts)
	if n == 0 {
		return nil
	}
	axis := depth % 3

	sort.Slice(pts, func(i, j int) bool {
		return pts[i].Pos[axis] < pts[j].Pos[axis]
	})

	mid := n / 2
	node := &kdNode3{
		point: pts[mid],
		axis:  axis,
	}

	// Decide whether to build children in parallel
	if n >= t.minParallelSize {
		wg.Add(2)
		go func() {
			defer wg.Done()
			node.left = t.buildRec(pts[:mid], depth+1, wg)
		}()
		go func() {
			defer wg.Done()
			node.right = t.buildRec(pts[mid+1:], depth+1, wg)
		}()
	} else {
		node.left = t.buildRec(pts[:mid], depth+1, wg)
		node.right = t.buildRec(pts[mid+1:], depth+1, wg)
	}

	return node
}

func (t *kdTree3cc) searchRec(node *kdNode3, q Point3, k int, h *maxHeap) {
	if node == nil {
		return
	}

	d := sqDist3(q.Pos, node.point.Pos)
	if h.Len() < k {
		heap.Push(h, Neighbor{ID: node.point.ID, Distance: d})
	} else if d < (*h)[0].Distance {
		(*h)[0] = Neighbor{ID: node.point.ID, Distance: d}
		heap.Fix(h, 0)
	}

	axis := node.axis
	diff := q.Pos[axis] - node.point.Pos[axis]

	var near, far *kdNode3
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

func (t *kdTree3cc) Query(q Point3, k int) []Neighbor {
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

func (t *kdTree3cc) QueryBatch(queries []Point3, k int) [][]Neighbor {
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
		q   Point3
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
