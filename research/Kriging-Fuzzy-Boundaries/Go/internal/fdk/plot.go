package fdk

import (
	"image/color"
	"math"
	"sort"

	"gonum.org/v1/plot"
	"gonum.org/v1/plot/plotter"
	"gonum.org/v1/plot/vg"
)

func PlotCDF(okEst, fdkEst []float64, filename string) {
	sortedOkEst := make([]float64, len(okEst))
	copy(sortedOkEst, okEst)
	sort.Float64s(sortedOkEst)

	sortedFdkEst := make([]float64, len(fdkEst))
	copy(sortedFdkEst, fdkEst)
	sort.Float64s(sortedFdkEst)

	n := len(sortedOkEst)
	ptsOK := make(plotter.XYs, n)
	ptsFDK := make(plotter.XYs, n)

	for i := 0; i < n; i++ {
		p := float64(i) / float64(n-1)
		ptsOK[i].X = sortedOkEst[i]
		ptsOK[i].Y = p
		ptsFDK[i].X = sortedFdkEst[i]
		ptsFDK[i].Y = p
	}

	p := plot.New()
	p.Title.Text = "Jura Dataset Cd Estimate CDF: OK (Black) vs FDK Variant B (Red)"
	p.X.Label.Text = "Cd Estimate"
	p.Y.Label.Text = "Cumulative Probability"

	lOK, _ := plotter.NewLine(ptsOK)
	lOK.Color = color.Black
	lOK.Width = vg.Points(2)

	lFDK, _ := plotter.NewLine(ptsFDK)
	lFDK.Color = color.RGBA{R: 255, G: 0, B: 0, A: 255}
	lFDK.Width = vg.Points(2)

	p.Add(lOK, lFDK)
	p.Save(6*vg.Inch, 4*vg.Inch, filename)
}

func PlotImage(grid []GridNode, estimates []float64, gridSpacing float64, title, filename string) error {
	p := plot.New()
	p.Title.Text = title
	p.X.Label.Text = "X"
	p.Y.Label.Text = "Y"

	// 1. Compute min/max estimates for min-max color normalization
	minEst, maxEst := estimates[0], estimates[0]
	for _, v := range estimates {
		if v < minEst {
			minEst = v
		}
		if v > maxEst {
			maxEst = v
		}
	}

	halfCell := gridSpacing / 2.0

	// 2. Render each grid point as a centered square polygon
	for i, node := range grid {
		val := estimates[i]
		norm := 0.0
		if maxEst > minEst {
			norm = (val - minEst) / (maxEst - minEst)
		}

		cellColor := sampleColorMap(norm)

		// Define boundary vertices centered at (node.X, node.Y)
		pts := plotter.XYs{
			{X: node.X - halfCell, Y: node.Y - halfCell},
			{X: node.X + halfCell, Y: node.Y - halfCell},
			{X: node.X + halfCell, Y: node.Y + halfCell},
			{X: node.X - halfCell, Y: node.Y + halfCell},
			{X: node.X - halfCell, Y: node.Y - halfCell},
		}

		poly, err := plotter.NewPolygon(pts)
		if err != nil {
			return err
		}
		poly.Color = cellColor
		poly.LineStyle.Width = 0 // borderless tiles for continuous plan map display

		p.Add(poly)
	}

	// 3. Save output image
	return p.Save(8*vg.Inch, 8*vg.Inch, filename)
}

// sampleColorMap maps a normalized [0, 1] scalar to an RGBA Viridis-style gradient
func sampleColorMap(t float64) color.RGBA {
	t = math.Max(0.0, math.Min(1.0, t))

	// Polynomial Viridis RGB approximation
	r := uint8(255 * math.Max(0, math.Min(1, 0.280+0.700*t-0.400*t*t)))
	g := uint8(255 * math.Max(0, math.Min(1, 0.150+0.850*math.Sqrt(t))))
	b := uint8(255 * math.Max(0, math.Min(1, 0.480+0.200*t-0.650*t*t)))

	return color.RGBA{R: r, G: g, B: b, A: 255}
}
