package cmd

import (
	"fmt"
	"os"
	"regexp"

	"dmconv/internal/datamine"

	"github.com/spf13/cobra"
)

// scenarioCmd represents the scenario command
var outCmd = &cobra.Command{
	Use:   "out <filename.dm>",
	Short: "Export a Datamine binary file to Parquet",
	Long: `Converts Datamine Studio binary .dm files into open,
analysis-ready formats. No Datamine license required.
Suitable for reproducible mining data science workflows.`,
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) == 0 {
        	fmt.Fprintln(os.Stderr, "Usage: out filename.dm")
        	os.Exit(1)
    	}
		myfile := args[0]

		// Load data
		data, err := datamine.ReadDM(myfile)
		if err != nil {
			panic(err)
		}
		fmt.Println("Data read successfully!")

		re, _ := regexp.Compile(`[.]dm$`)
		if toggleCsv {
			// Write data to csv
			outfile := re.ReplaceAllString(myfile, ".csv")
			datamine.WriteCsv(data, outfile)
			fmt.Println("Writen to", outfile)
		} else {
			// Write data to parquet
			outfile := re.ReplaceAllString(myfile, ".parquet")
			datamine.WriteParquet(data, outfile)
			fmt.Println("Writen to", outfile)
		}
	},
}
