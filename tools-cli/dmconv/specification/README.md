# Datamine Converter — R Prototype

This folder contains the original C implementation of the Datamine Studio binary file converter. It was developed to speed up the R version by compiling it into a `DLL` and wrapping it via an R function.

There is also a pure R version `read.datamine.R`, the original version in the `legacy/` folder

## Purpose

- Speed up the original R version
- This version is very similar to the current Go version

## Usage

For those who wish to experiment with the C implementation, the compilation step—though not guaranteed to work on modern systems—was originally performed as follows:

```r
R CMD SHLIB Rdmread.c
```

# Repository Context

This C version is preserved for completeness and reproducibility. It remains a useful reference for understanding the binary format and the design of the Go implementation.
