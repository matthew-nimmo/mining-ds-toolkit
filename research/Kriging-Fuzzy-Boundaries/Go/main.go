package main

import (
	"fmt"
	"log"
	"math"

	"fdk/internal/fdk"
)

// Standard Jura Exponential Covariance Model
func covFunc(h float64) float64 {
	nug := 0.4595549
	sill := 0.3850553
	rng := 0.3308479
	if h == 0 {
		return nug + sill
	}
	e := math.Exp(-3.0*h/rng)

	return sill*e
}

// Jura Spherical Covariance Model
func covFunc2(h float64) float64 {
	nug := 0.4777354
	sill := 0.3371204
	rng := 0.6687083

	if h == 0 {
		return nug + sill
	}
	if h > rng {
		return 0.0
	}
	r := h / rng
	e := 1 - (3/2)*r + 0.5*math.Pow(r, 3.0)

	return sill*e
}

func main() {
	// Load Prediction & Grid Datasets
	samples := fdk.LoadSamples("jura_pred.csv")
	grid := fdk.LoadGrid("jura_grid.csv")
	rocks := []int{1, 2, 3, 4, 5}

	log.Printf("Loaded %d sample points and %d grid nodes.", len(samples), len(grid))

	// Perform Indicator Kriging (IK) on Grid for each Rock type
	// Estimate Grid via OK and FDK Variant B
	gridMemberships := make([]map[int]float64, len(grid))
	okEstimates := make([]float64, len(grid))
	fdkEstimates := make([]float64, len(grid))
	for i, gNode := range grid {
		neighbors := fdk.Get10Nearest(gNode, samples)

		gridMemberships[i] = make(map[int]float64)
		for _, r := range rocks {
			prob := fdk.EstimateIK(gNode, neighbors, r, covFunc)
			gridMemberships[i][r] = math.Max(0.0, math.Min(1.0, prob))
		}

		// Standard OK
		okEstimates[i] = fdk.EstimateOK(gNode, neighbors, covFunc)

		// FDK Variant B (using IK membership penalties)
		fdkEstimates[i] = fdk.EstimateFDKVariantB(gNode, neighbors, gridMemberships[i], covFunc)
	}

	// Generate CDF Comparison Plot
	fdk.PlotCDF(okEstimates, fdkEstimates, "jura_cdf_comparison.png")
	fmt.Println("Validation completed. Output generated: jura_cdf_comparison.png")

	// Generate map
	gridSpacing := 0.05 // Grid cell size
	fdk.PlotImage(grid, okEstimates, gridSpacing, "Ordinary Kriging (OK) - Cd Estimate", "jura_ok_map.png")
	fdk.PlotImage(grid, fdkEstimates, gridSpacing, "Fuzzy Domain Kriging (Variant B) - Cd Estimate", "jura_fdk_map.png")

	// Export combined estimates to CSV
	err := fdk.ExportEstimatesCSV(grid, okEstimates, fdkEstimates, "jura_kriging_estimates.csv")
	if err != nil {
    	log.Fatalf("Error exporting CSV: %v", err)
	}
	fmt.Println("Estimates exported to jura_kriging_estimates.csv")
}
