package fdk

import (
	"errors"
	"math"
)

// Point represents a 2D spatial coordinate with a value and fuzzy membership.
type Point struct {
	X, Y       float64
	Value      float64
	Membership float64 // u_i in [0, 1]
}

// CovarianceFunc returns covariance for a given Euclidean distance h.
type CovarianceFunc func(h float64) float64

// Linear Solver using Gaussian Elimination with Partial Pivoting (A * x = b)
func solveLinearSystem(A [][]float64, b []float64) ([]float64, error) {
	n := len(b)
	// Clone A and b to avoid modifying inputs
	aMat := make([][]float64, n)
	for i := range aMat {
		aMat[i] = make([]float64, n)
		copy(aMat[i], A[i])
	}
	bVec := make([]float64, n)
	copy(bVec, b)

	for i := 0; i < n; i++ {
		// Pivot selection
		maxRow := i
		for k := i + 1; k < n; k++ {
			if math.Abs(aMat[k][i]) > math.Abs(aMat[maxRow][i]) {
				maxRow = k
			}
		}
		aMat[i], aMat[maxRow] = aMat[maxRow], aMat[i]
		bVec[i], bVec[maxRow] = bVec[maxRow], bVec[i]

		if math.Abs(aMat[i][i]) < 1e-12 {
			return nil, errors.New("singular matrix")
		}

		// Elimination
		for k := i + 1; k < n; k++ {
			c := aMat[k][i] / aMat[i][i]
			for j := i; j < n; j++ {
				if i == j {
					aMat[k][j] = 0
				} else {
					aMat[k][j] -= c * aMat[i][j]
				}
			}
			bVec[k] -= c * bVec[i]
		}
	}

	// Back substitution
	x := make([]float64, n)
	for i := n - 1; i >= 0; i-- {
		sum := 0.0
		for j := i + 1; j < n; j++ {
			sum += aMat[i][j] * x[j]
		}
		x[i] = (bVec[i] - sum) / aMat[i][i]
	}
	return x, nil
}

func distance(p1, p2 Point) float64 {
	dx := p1.X - p2.X
	dy := p1.Y - p2.Y
	return math.Sqrt(dx*dx + dy*dy)
}

// OrdinaryKriging performs standard OK without fuzzy penalties.
func OrdinaryKriging(pts []Point, target Point, cov CovarianceFunc) (float64, float64, error) {
	n := len(pts)
	K := make([][]float64, n+1)
	for i := range K {
		K[i] = make([]float64, n+1)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			K[i][j] = cov(distance(pts[i], pts[j]))
		}
		K[i][n] = 1.0
		K[n][i] = 1.0
	}
	K[n][n] = 0.0

	k := make([]float64, n+1)
	for i := 0; i < n; i++ {
		k[i] = cov(distance(pts[i], target))
	}
	k[n] = 1.0

	res, err := solveLinearSystem(K, k)
	if err != nil {
		return 0, 0, err
	}

	weights := res[:n]
	mu := res[n]

	estimate := 0.0
	kDotW := 0.0
	for i := 0; i < n; i++ {
		estimate += weights[i] * pts[i].Value
		kDotW += weights[i] * k[i]
	}

	variance := cov(0.0) - kDotW + mu
	return estimate, variance, nil
}

// VariantA: Multiplicative Covariance Mask (w = k * (D * K)^-1)
func KrigingVariantA(pts []Point, target Point, cov CovarianceFunc) (float64, float64, error) {
	n := len(pts)
	K := make([][]float64, n+1)
	for i := range K {
		K[i] = make([]float64, n+1)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			baseCov := cov(distance(pts[i], pts[j]))
			if i != j {
				// Scale off-diagonal cross-covariance by sample interaction mask
				f1 := 1.0 - pts[i].Membership
				f2 := 1.0 - pts[j].Membership
				//dMask := 1.0 - (f1 * pts[j].Membership + f2 * pts[i].Membership)
				dMask := math.Sqrt(f1 * f2)
				baseCov *= dMask
			}
			K[i][j] = baseCov
		}
		K[i][n] = 1.0
		K[n][i] = 1.0
	}
	K[n][n] = 0.0

	k := make([]float64, n+1)
	for i := 0; i < n; i++ {
		mask := math.Sqrt(pts[i].Membership * target.Membership)
		k[i] = cov(distance(pts[i], target)) * mask
	}
	k[n] = 1.0

	res, err := solveLinearSystem(K, k)
	if err != nil {
		return 0, 0, err
	}

	weights := res[:n]
	mu := res[n]

	estimate := 0.0
	kDotW := 0.0
	for i := 0; i < n; i++ {
		estimate += weights[i] * pts[i].Value
		kDotW += weights[i] * k[i]
	}

	variance := cov(0.0) - kDotW + mu
	return estimate, variance, nil
}

// VariantB: Additive Variance Penalty (w = k * (K + R)^-1)
func KrigingVariantB(pts []Point, target Point, cov CovarianceFunc, sill float64) (float64, float64, error) {
	n := len(pts)
	K := make([][]float64, n+1)
	for i := range K {
		K[i] = make([]float64, n+1)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			K[i][j] = cov(distance(pts[i], pts[j]))
		}
		// Apply diagonal inverse variance penalty R_ii
		if pts[i].Membership < 1.0 {
			if pts[i].Membership <= 1e-6 {
				K[i][i] += 1e9 // Simulate infinite penalty
			} else {
				K[i][i] += ((1.0 - pts[i].Membership) / pts[i].Membership) * sill
			}
		}
		K[i][n] = 1.0
		K[n][i] = 1.0
	}
	K[n][n] = 0.0

	k := make([]float64, n+1)
	for i := 0; i < n; i++ {
		k[i] = cov(distance(pts[i], target))
	}
	k[n] = 1.0

	res, err := solveLinearSystem(K, k)
	if err != nil {
		return 0, 0, err
	}

	weights := res[:n]
	mu := res[n]

	estimate := 0.0
	kDotW := 0.0
	for i := 0; i < n; i++ {
		estimate += weights[i] * pts[i].Value
		kDotW += weights[i] * k[i]
	}

	variance := cov(0.0) - kDotW + mu
	return estimate, variance, nil
}

// VariantC: Post-Projection Fast Scaling (w_fast = (w_fixed * M) / sum(w_fixed * M))
// Designed for fast SGS simulation bypassing system solving per node.
func KrigingVariantC(fixedWeights []float64, pts []Point) (float64, error) {
	n := len(pts)
	if len(fixedWeights) != n {
		return 0, errors.New("weights and samples length mismatch")
	}

	scaledSum := 0.0
	scaledWeights := make([]float64, n)

	for i := 0; i < n; i++ {
		w := fixedWeights[i] * pts[i].Membership
		scaledWeights[i] = w
		scaledSum += w
	}

	if math.Abs(scaledSum) < 1e-12 {
		return 0, errors.New("all memberships zeroed out")
	}

	estimate := 0.0
	for i := 0; i < n; i++ {
		normWeight := scaledWeights[i] / scaledSum
		estimate += normWeight * pts[i].Value
	}

	return estimate, nil
}
