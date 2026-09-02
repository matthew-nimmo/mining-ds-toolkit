package fdk

import (
	"encoding/csv"
	"fmt"
	"os"
	"strconv"
)

func LoadSamples(filename string) []Sample {
	f, _ := os.Open(filename)
	defer f.Close()
	r := csv.NewReader(f)
	r.Read() // header
	records, _ := r.ReadAll()

	rfact := map[string]int{"Sequanian":1, "Kimmeridgian":2, "Quaternary":3, "Argovian":4, "Portlandian":5}

	var samples []Sample
	for _, rec := range records {
		// Header: Xloc,Yloc,long,lat,Landuse,Rock,Cd,Co,Cr,Cu,Ni,Pb,Zn
		x, _ := strconv.ParseFloat(rec[0], 64)
		y, _ := strconv.ParseFloat(rec[1], 64)
		rk, _ := rfact[rec[5]]
		cd, _ := strconv.ParseFloat(rec[6], 64)
		samples = append(samples, Sample{X: x, Y: y, Rock: rk, Cd: cd})
	}
	return samples
}

func LoadGrid(filename string) []GridNode {
	f, _ := os.Open(filename)
	defer f.Close()
	r := csv.NewReader(f)
	r.Read() // header
	records, _ := r.ReadAll()

	var grid []GridNode
	for _, rec := range records {
		// Header: Xloc,Yloc,long,lat,Landuse,Rock
		x, _ := strconv.ParseFloat(rec[0], 64)
		y, _ := strconv.ParseFloat(rec[1], 64)
		grid = append(grid, GridNode{X: x, Y: y})
	}
	return grid
}

// exportEstimatesCSV writes the grid locations along with OK and FDK estimates to a CSV file.
// Format: Xloc, Yloc, OK, FDK
func ExportEstimatesCSV(grid []GridNode, okEst, fdkEst []float64, filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("failed to create output CSV file: %w", err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	// 1. Write CSV Header
	header := []string{"Xloc", "Yloc", "OK", "FDK"}
	if err := writer.Write(header); err != nil {
		return fmt.Errorf("failed to write CSV header: %w", err)
	}

	// 2. Write Data Rows
	for i, node := range grid {
		row := []string{
			strconv.FormatFloat(node.X, 'f', 4, 64),
			strconv.FormatFloat(node.Y, 'f', 4, 64),
			strconv.FormatFloat(okEst[i], 'f', 4, 64),
			strconv.FormatFloat(fdkEst[i], 'f', 4, 64),
		}
		if err := writer.Write(row); err != nil {
			return fmt.Errorf("failed to write row at index %d: %w", i, err)
		}
	}

	return nil
}
