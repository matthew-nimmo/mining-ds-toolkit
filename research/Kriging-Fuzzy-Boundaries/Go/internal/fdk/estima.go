package fdk

import (
	"math"
	"sort"

	"gonum.org/v1/gonum/mat"
)

type Sample struct {
	X, Y float64
	Rock int
	Cd   float64
}

type GridNode struct {
	X, Y float64
}

// 10 Nearest Neighbors Simple Search
func Get10Nearest(target GridNode, samples []Sample) []Sample {
	type distPair struct {
		sample Sample
		dist   float64
	}
	pairs := make([]distPair, len(samples))
	for i, s := range samples {
		pairs[i] = distPair{sample: s, dist: math.Hypot(s.X-target.X, s.Y-target.Y)}
	}
	sort.Slice(pairs, func(i, j int) bool { return pairs[i].dist < pairs[j].dist })

	res := make([]Sample, 10)
	for i := 0; i < 10; i++ {
		res[i] = pairs[i].sample
	}
	return res
}

// Standard Ordinary Kriging (OK)
func EstimateOK(target GridNode, neighbors []Sample, covFunc CovarianceFunc) float64 {
	n := len(neighbors)
	KSysData := make([]float64, (n+1)*(n+1))
	kRHS := make([]float64, n+1)

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			h := math.Hypot(neighbors[i].X-neighbors[j].X, neighbors[i].Y-neighbors[j].Y)
			KSysData[i*(n+1)+j] = covFunc(h)
		}
		KSysData[i*(n+1)+n] = 1.0
		KSysData[n*(n+1)+i] = 1.0

		hTarget := math.Hypot(neighbors[i].X-target.X, neighbors[i].Y-target.Y)
		kRHS[i] = covFunc(hTarget)
	}
	KSysData[n*(n+1)+n] = 0.0
	kRHS[n] = 1.0

	KSys := mat.NewDense(n+1, n+1, KSysData)
	rhs := mat.NewVecDense(n+1, kRHS)

	var w mat.VecDense
	if err := w.SolveVec(KSys, rhs); err != nil {
		return 0.0
	}

	var est float64
	for i := 0; i < n; i++ {
		est += w.AtVec(i) * neighbors[i].Cd
	}
	return est
}

// Indicator Kriging (IK) for a target Rock category
func EstimateIK(target GridNode, neighbors []Sample, rockCat int, covFunc CovarianceFunc) float64 {
	n := len(neighbors)
	KSysData := make([]float64, (n+1)*(n+1))
	kRHS := make([]float64, n+1)

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			h := math.Hypot(neighbors[i].X-neighbors[j].X, neighbors[i].Y-neighbors[j].Y)
			KSysData[i*(n+1)+j] = covFunc(h)
		}
		KSysData[i*(n+1)+n] = 1.0
		KSysData[n*(n+1)+i] = 1.0

		hTarget := math.Hypot(neighbors[i].X-target.X, neighbors[i].Y-target.Y)
		kRHS[i] = covFunc(hTarget)
	}
	KSysData[n*(n+1)+n] = 0.0
	kRHS[n] = 1.0

	KSys := mat.NewDense(n+1, n+1, KSysData)
	rhs := mat.NewVecDense(n+1, kRHS)

	var w mat.VecDense
	if err := w.SolveVec(KSys, rhs); err != nil {
		return 0.0
	}

	var prob float64
	for i := 0; i < n; i++ {
		ind := 0.0
		if neighbors[i].Rock == rockCat {
			ind = 1.0
		}
		prob += w.AtVec(i) * ind
	}
	return prob
}

// FDK Variant B (with Inverse Variance Penalization Matrix R)
func EstimateFDKVariantB(target GridNode, neighbors []Sample, memberships map[int]float64, covFunc CovarianceFunc) float64 {
	n := len(neighbors)
	sigma2R := 1.0
	KSysData := make([]float64, (n+1)*(n+1))
	kRHS := make([]float64, n+1)

	for i := 0; i < n; i++ {
		uI := memberships[neighbors[i].Rock]
		uI = math.Max(1e-4, math.Min(1.0, uI))

		for j := 0; j < n; j++ {
			h := math.Hypot(neighbors[i].X-neighbors[j].X, neighbors[i].Y-neighbors[j].Y)
			covVal := covFunc(h)
			if i == j {
				// Apply R_ii penalty
				covVal += sigma2R * (1.0 - uI) / uI
			}
			KSysData[i*(n+1)+j] = covVal
		}
		KSysData[i*(n+1)+n] = 1.0
		KSysData[n*(n+1)+i] = 1.0

		hTarget := math.Hypot(neighbors[i].X-target.X, neighbors[i].Y-target.Y)
		kRHS[i] = covFunc(hTarget)
	}
	KSysData[n*(n+1)+n] = 0.0
	kRHS[n] = 1.0

	KSys := mat.NewDense(n+1, n+1, KSysData)
	rhs := mat.NewVecDense(n+1, kRHS)

	var w mat.VecDense
	if err := w.SolveVec(KSys, rhs); err != nil {
		return 0.0
	}

	var est float64
	for i := 0; i < n; i++ {
		est += w.AtVec(i) * neighbors[i].Cd
	}
	return est
}
