package fdk

import (
	"math"
	"testing"
)

// Exponential Covariance Model: C(h) = Sill * exp(-3h / Range)
func exponentialCovariance(h float64) float64 {
	sill := 10.0
	aRange := 30.0
	return sill * math.Exp(-3.0*h/aRange)
}

func getSyntheticData() ([]Point, Point) {
	pts := []Point{
		{X: 10, Y: 10, Value: 100.0, Membership: 1.0},
		{X: 20, Y: 15, Value: 105.0, Membership: 1.0},
		{X: 15, Y: 25, Value: 98.0, Membership: 1.0},
		{X: 30, Y: 20, Value: 102.0, Membership: 1.0},
		{X: 25, Y: 30, Value: 101.0, Membership: 1.0},
		// Out-of-domain sample with high grade (potential dilution source)
		{X: 45, Y: 22, Value: 250.0, Membership: 0.25},
	}
	target := Point{X: 35, Y: 20}
	return pts, target
}

func TestOrdinaryKriging(t *testing.T) {
	pts, target := getSyntheticData()
	est, varK, err := OrdinaryKriging(pts, target, exponentialCovariance)
	if err != nil {
		t.Fatalf("OK failed: %v", err)
	}
	t.Logf("Standard OK Estimate: %.2f, Variance: %.2f", est, varK)
}

func TestVariantB(t *testing.T) {
	pts, target := getSyntheticData()
	sill := 10.0

	// Standard OK (Includes point 6 unweighted -> diluted high estimate)
	estOK, varOK, _ := OrdinaryKriging(pts[:4], target, exponentialCovariance)

	// Variant B (Additive Penalty)
	pts[5].Membership = 0.0
	estB, varB, errB := KrigingVariantB(pts, target, exponentialCovariance, sill)
	if errB != nil {
		t.Fatalf("Variant B failed (0): %v", errB)
	}

	t.Logf("[Comparison] Unconditioned OK Est: %.2f, Var: %.2f", estOK, varOK)
	t.Logf("[Variant B] Est: %.2f, Var: %.2f", estB, varB)

	// Approximately the same
	if estB / estOK < 0.0001 {
		t.Errorf("FDK variant B failed (0)")
	}

	// ----

	// Standard OK (Includes point 6 unweighted -> diluted high estimate)
	estOK, varOK, _ = OrdinaryKriging(pts, target, exponentialCovariance)

	// Variant B (Additive Penalty)
	pts[5].Membership = 1.0
	estB, varB, errB = KrigingVariantB(pts, target, exponentialCovariance, sill)
	if errB != nil {
		t.Fatalf("Variant B failed (1): %v", errB)
	}

	t.Logf("[Comparison] Unconditioned OK Est: %.2f, Var: %.2f", estOK, varOK)
	t.Logf("[Variant B] Est: %.2f, Var: %.2f", estB, varB)

	// Approximately the same
	if estB / estOK < 0.0001 {
		t.Errorf("FDK variant B failed (1)")
	}
}

func TestVariants(t *testing.T) {
	pts, target := getSyntheticData()
	sill := 10.0

	// Standard OK (Includes point 6 unweighted -> diluted high estimate)
	estOK, _, _ := OrdinaryKriging(pts, target, exponentialCovariance)

	// Variant A (Multiplicative Mask)
	estA, varA, errA := KrigingVariantA(pts, target, exponentialCovariance)
	if errA != nil {
		t.Fatalf("Variant A failed: %v", errA)
	}

	// Variant B (Additive Penalty)
	estB, varB, errB := KrigingVariantB(pts, target, exponentialCovariance, sill)
	if errB != nil {
		t.Fatalf("Variant B failed: %v", errB)
	}

	t.Logf("[Comparison] Unconditioned OK Est: %.2f", estOK)
	t.Logf("[Variant A] Est: %.2f, Var: %.2f", estA, varA)
	t.Logf("[Variant B] Est: %.2f, Var: %.2f", estB, varB)

	// Both variants should suppress the high-grade outlier (250.0) compared to unconditioned OK
	if estA >= estOK || estB >= estOK {
		t.Errorf("FDK variants failed to dampen high-grade boundary outlier")
	}
}

func TestVariantC_FastScaling(t *testing.T) {
	pts, target := getSyntheticData()
	n := len(pts)

	// Step 1: Compute master unconditioned weights once
	K := make([][]float64, n+1)
	for i := range K {
		K[i] = make([]float64, n+1)
	}
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			K[i][j] = exponentialCovariance(distance(pts[i], pts[j]))
		}
		K[i][n] = 1.0
		K[n][i] = 1.0
	}

	k := make([]float64, n+1)
	for i := 0; i < n; i++ {
		k[i] = exponentialCovariance(distance(pts[i], target))
	}
	k[n] = 1.0

	res, _ := solveLinearSystem(K, k)
	masterWeights := res[:n]

	// Step 2: Perform fast O(N) scaling
	estC, err := KrigingVariantC(masterWeights, pts)
	if err != nil {
		t.Fatalf("Variant C failed: %v", err)
	}

	t.Logf("[Variant C Fast SGS] Est: %.2f", estC)
}

// ==============================================================================
// BENCHMARKING SUITE
// ==============================================================================

func BenchmarkOrdinaryKriging(b *testing.B) {
	pts, target := getSyntheticData()
	for b.Loop() {
		_, _, _ = OrdinaryKriging(pts, target, exponentialCovariance)
	}
}

func BenchmarkVariantA(b *testing.B) {
	pts, target := getSyntheticData()
	for b.Loop() {
		_, _, _ = KrigingVariantA(pts, target, exponentialCovariance)
	}
}

func BenchmarkVariantB(b *testing.B) {
	pts, target := getSyntheticData()
	for b.Loop() {
		_, _, _ = KrigingVariantB(pts, target, exponentialCovariance, 10.0)
	}
}

func BenchmarkVariantC_FastScaling(b *testing.B) {
	pts, _ := getSyntheticData()
	// Pre-calculated weights
	weights := []float64{0.25, 0.25, 0.20, 0.15, 0.10, 0.05}
	for b.Loop() {
		_, _ = KrigingVariantC(weights, pts)
	}
}
