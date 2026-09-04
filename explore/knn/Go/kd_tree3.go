package knn

import (
	"container/heap"
	"runtime"
	"sort"
	"sync"
)

//----------------------
// Optimized 3D KD-Tree k-nearest neighbour
//----------------------

type kdNode3 struct {
	point Point3
	left  *kdNode3
	right *kdNode3
	axis  int // 0,1,2
}

type kdTree3 struct {
	root   *kdNode3
	points []Point3
}

func NewKDTree3() *kdTree3 {
	return &kdTree3{}
}

func (t *kdTree3) Build(points []Point3) error {
	if len(points) == 0 {
		t.root = nil
		t.points = nil
		return nil
	}
	t.points = append([]Point3(nil), points...)
	t.root = t.buildRec(t.points, 0)
	return nil
}

func (t *kdTree3) buildRec(pts []Point3, depth int) *kdNode3 {
	if len(pts) == 0 {
		return nil
	}
	axis := depth % 3

	sort.Slice(pts, func(i, j int) bool {
		return pts[i].Pos[axis] < pts[j].Pos[axis]
	})

	mid := len(pts) / 2

	node := &kdNode3{
		point: pts[mid],
		axis:  axis,
	}

	node.left = t.buildRec(pts[:mid], depth+1)
	node.right = t.buildRec(pts[mid+1:], depth+1)

	return node
}

func (t *kdTree3) Query(q Point3, k int) []Neighbor {
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

func (t *kdTree3) searchRec(node *kdNode3, q Point3, k int, h *maxHeap) {
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

func (t *kdTree3) QueryBatch(queries []Point3, k int) [][]Neighbor {
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
