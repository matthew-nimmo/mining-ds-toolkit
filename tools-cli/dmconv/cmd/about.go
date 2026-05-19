package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var aboutCmd = &cobra.Command{
    Use:   "about",
    Short: "Display information about this CLI tool",
    Long:  `Provide a detailed description of the tool, its authors, and version.`,
    Run: func(cmd *cobra.Command, args []string) {
        fmt.Println("dmconv 2026.5")
        fmt.Println(`Datamine Studio Binary File Converter
Part of the mining-ds-toolkit

This tool parses Datamine .dm binary files and exports them to open formats
for analysis, automation, and reproducible workflows.

Examples:
  dmconv out model.dm
  dmconv help

Documentation:
  https://github.com/matthew-nimmo/mining-ds-vault

Toolkit:
  https://github.com/matthew-nimmo/mining-ds-toolkit

Author:
  Matthew Nimmo — mining data science, reproducible workflows, Go-based tooling`)
    },
}
