# Datamine Converter — R Prototype

This folder contains the original R implementation of the Datamine Studio binary file converter. It was developed to allow direct ingestion of Datamine `.dm` files into R without requiring access to the Datamine software.

There is also an R `Rdmread.R` with C `DLL` in the `specification/` folder. This was create to speed up the pure R version. For an added bonus, there is also an equivalent reader for Vulcan data files (this was not used very frequently or tested thoroughly).

## Purpose

- Remove dependency on proprietary software for data export
- Enable reproducible data science workflows in R
- Provide a reference implementation for the later Go version

## Usage

The test_datamine.qmd Quarto markdown document demonstrates the pure R version. Warning: it can take a while to load large files.

# Notes

- The pure R implementation was the first working version
- It demonstrates the parsing logic and binary structure
- It is slower than the C or Go versions but remains useful for validation and reference
- It reflects the early DS‑first workflow before the transition to Go

# Repository Context

This R version is preserved for completeness and reproducibility. It shows the evolution from prototype → production and remains a useful reference for understanding the binary format and the design of the Go implementation.
