package knn

import (
	"math"
	"math/rand"
	"testing"
)

/*
Benchmark:
• Small datasets (≤10k): KD‑Tree build cost dominates, so brute force wins or ties.
• Medium datasets (~100k): KD‑Tree starts to pay off, but build cost still matters.
• Large datasets (≥1M): KD‑Tree pruning becomes extremely effective and brute force collapses 
*/

func randomPoints(n, dim int) []Point {
	points := make([]Point, n)
	for i := 0; i < n; i++ {
		pos := make([]float64, dim)
		for d := 0; d < dim; d++ {
			pos[d] = rand.Float64()
		}
		points[i] = Point{
			ID:  i,
			Pos: pos,
		}
	}
	return points
}

func randomPoints3(n int) []Point3 {
	points := make([]Point3, n)
	for i := 0; i < n; i++ {
		pos := [3]float64{rand.Float64(), rand.Float64(), rand.Float64()}
		points[i] = Point3{
			ID:  i,
			Pos: pos,
		}
	}
	return points
}

func BenchmarkBruteForce(b *testing.B) {
	points := randomPoints(10000, 3)
	queries := randomPoints(1000, 3)

	s := NewBruteForce()
	if err := s.Build(points); err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = s.QueryBatch(queries, 8)
	}
}

func BenchmarkKDTree(b *testing.B) {
	points := randomPoints(10000, 3)
	queries := randomPoints(1000, 3)

	s := NewKDTree()
	if err := s.Build(points); err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = s.QueryBatch(queries, 8)
	}
}

func BenchmarkKDTree3D(b *testing.B) {
	points := randomPoints3(10000)
	queries := randomPoints3(1000)

	s := NewKDTree3()
	if err := s.Build(points); err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = s.QueryBatch(queries, 8)
	}
}

func TestKDTreeCorrectness(t *testing.T) {
	nPoints := 5000
	nQueries := 200
	k := 5

	points := make([]Point, nPoints)
	for i := 0; i < nPoints; i++ {
		points[i] = Point{
			ID: i,
			Pos: []float64{
				rand.Float64(),
				rand.Float64(),
				rand.Float64(),
			},
		}
	}

	queries := make([]Point, nQueries)
	for i := 0; i < nQueries; i++ {
		queries[i] = Point{
			ID: -1,
			Pos: []float64{
				rand.Float64(),
				rand.Float64(),
				rand.Float64(),
			},
		}
	}

	bf := NewBruteForce()
	kd := NewKDTree()

	if err := bf.Build(points); err != nil {
		t.Fatal(err)
	}
	if err := kd.Build(points); err != nil {
		t.Fatal(err)
	}

	for i, q := range queries {
		nbBF := bf.Query(q, k)
		nbKD := kd.Query(q, k)

		if len(nbBF) != len(nbKD) {
			t.Fatalf("query %d: mismatched lengths", i)
		}

		for j := 0; j < k; j++ {
			if nbBF[j].ID != nbKD[j].ID {
				t.Fatalf("query %d: mismatch at neighbor %d: brute=%d kd=%d",
					i, j, nbBF[j].ID, nbKD[j].ID)
			}

			if math.Abs(nbBF[j].Distance-nbKD[j].Distance) > 1e-9 {
				t.Fatalf("query %d: distance mismatch at %d", i, j)
			}
		}
	}
}
