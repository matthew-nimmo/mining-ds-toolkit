package cmd

import (
	"os"

	"github.com/spf13/cobra"
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "dmconv",
	Short: "File conversion utility for Datamine files.",
	Long: `Convert Datamine Studio .dm files into open, analysis-ready formats.
Run 'datamine-convert --help' for usage.`,
}

func Execute() {
	err := rootCmd.Execute()
	if err != nil {
		os.Exit(1)
	}
}

var toggleCsv bool

func init() {
	rootCmd.Version = "2026.5"
	rootCmd.AddCommand(aboutCmd)
	rootCmd.AddCommand(outCmd)

	outCmd.Flags().BoolVar(&toggleCsv, "csv", false, "Description of the flag")
}
