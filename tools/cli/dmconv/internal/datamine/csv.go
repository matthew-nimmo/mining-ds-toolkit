package datamine

import (
	"encoding/csv"
	"fmt"
	"log"
	"os"
)

func WriteCsv(data *Data, filename string) {
	file, err := os.Create(filename)
	if err != nil {
		log.Fatal("Could not create file", err)
	}
	defer file.Close()

	// Initialize the CSV writer
	writer := csv.NewWriter(file)
	defer writer.Flush() // Ensure all buffered data is written

	// Original dimensions
	nrows := len(data.Data)
    ncols := len(data.Data[0])

	// Initialize result with swapped dimensions (cols x rows)
    result := make([][]string, ncols)
    for i := range result {
        result[i] = make([]string, nrows)
    }

    // Fill the transposed matrix
    for i := 0; i < nrows; i++ {
        for j := 0; j < ncols; j++ {
            result[j][i] = fmt.Sprint(data.Data[i][j])
        }
    }

	// Write header
	if err := writer.Write(data.Names); err != nil {
		log.Fatal("Error writing header to CSV:", err)
	}

	// Write data
	if err := writer.WriteAll(result); err != nil {
		log.Fatal("Error writing to CSV:", err)
	}
	log.Println("Write Finished")
}