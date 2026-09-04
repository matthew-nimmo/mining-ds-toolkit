/*
Copyright © 2026 NAME HERE <EMAIL ADDRESS>
*/
package cmd

import (
	"fmt"
	"log"
	"os"
	"time"

	"github.com/fsnotify/fsnotify"
	"github.com/spf13/cobra"
	"svolab/internal/core"
)

// watchCmd represents the watch command
var watchCmd = &cobra.Command{
	Use:   "watch",
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
		excelPath := args[0]

    	watcher, err := fsnotify.NewWatcher()
    	if err != nil {
        	log.Fatal(err)
    	}
    	defer watcher.Close()

   		if err := watcher.Add(excelPath); err != nil {
        	log.Fatal(err)
    	}

    	fmt.Println("Live Lab running.")
    	fmt.Println("Open", excelPath, "in Excel, edit, and save to trigger simulation.")

   		var lastEvent time.Time

    	for {
        	select {
        	case event := <-watcher.Events:
            	if event.Op&(fsnotify.Write|fsnotify.Create) != 0 {
                	now := time.Now()
                	// basic debounce: ignore events that come too quickly
                	if now.Sub(lastEvent) < 300*time.Millisecond {
                    	continue
                	}
                	lastEvent = now

                	go func() {
                    	fmt.Println("Change detected, running simulation...")

                    	sc, err := core.LoadScenarioFromExcel(excelPath)
                    	if err != nil {
                        	fmt.Println("Error loading scenario:", err)
                        	return
                    	}

                    	res, ctx, err := core.RunSimulation(sc)
                    	if err != nil {
                        	fmt.Println("Simulation error:", err)
                        	return
                    	}

                    	if err := core.WriteResultsToExcel(excelPath, res, sc, ctx); err != nil {
                        	fmt.Println("Error writing results:", err)
                        	return
                    	}

                    	fmt.Println("Simulation complete. Results updated.")
                	}()
            	}
        	case err := <-watcher.Errors:
            	fmt.Println("Watcher error:", err)
        	}
    	}
	},
}

func init() {
	rootCmd.AddCommand(watchCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// watchCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// watchCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}
