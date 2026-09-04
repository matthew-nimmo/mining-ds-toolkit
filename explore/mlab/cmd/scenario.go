/*
Copyright © 2026 NAME HERE <EMAIL ADDRESS>
*/
package cmd

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"svolab/internal/core"
)

// scenarioCmd represents the scenario command
var scenarioCmd = &cobra.Command{
	Use:   "scenario",
	Short: "A brief description of your command",
	Long: `A longer description that spans multiple lines and likely contains examples
and usage of using your command. For example:

Cobra is a CLI library for Go that empowers applications.
This application is a tool to generate the needed files
to quickly create a Cobra application.`,
	Run: func(cmd *cobra.Command, args []string) {
    	if len(args) == 0 {
        	fmt.Fprintln(os.Stderr, "Usage: simulator --scenario scenario.json")
        	os.Exit(1)
    	}
		scenarioPath := args[0]

   		f, err := os.Open(scenarioPath)
    	if err != nil {
        	fmt.Fprintln(os.Stderr, "error opening scenario:", err)
        	os.Exit(1)
    	}
    	defer f.Close()

    	var sc core.Scenario
    	if err := json.NewDecoder(f).Decode(&sc); err != nil {
        	fmt.Fprintln(os.Stderr, "error parsing scenario:", err)
        	os.Exit(1)
    	}

    	res, _, err := core.RunSimulation(sc)
    	if err != nil {
        	fmt.Fprintln(os.Stderr, "error running simulation:", err)
        	os.Exit(1)
    	}

    	enc := json.NewEncoder(os.Stdout)
    	enc.SetIndent("", "  ")
    	if err := enc.Encode(res); err != nil {
        	fmt.Fprintln(os.Stderr, "error writing result:", err)
        	os.Exit(1)
    	}
	},
}

func init() {
	rootCmd.AddCommand(scenarioCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// scenarioCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// scenarioCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}
