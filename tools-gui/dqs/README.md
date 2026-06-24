# Data Quality Scoring (DQS)

![R](https://img.shields.io/badge/R-4.0+-276DC3?style=flat&logo=r)
![Quarto](https://img.shields.io/badge/Quarto-1.0+-75AADB?style=flat&logo=quarto)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

This project was initially prototyped to help collate data and automate (as much as possible) data preparation for data science projects in the mining industry. The intent was to create a profiling tool capable of processing a large number of projects to:

- **Categorize data** — Automatically identify file types and data characteristics
- **Select datasets** — Assess and compare data quality across projects  
- **Combine for modeling** — Select and merge high-quality datasets for generating a global ML model
- **Query across projects** — Enable seamless cross-project analysis and aggregation

The core requirement was building a profiling tool that could:
1. Identify the **type of file** (CSV, Excel, etc.)
2. Identify the **type of data** within the file (columns, units, methods, limits)
3. Identify the **quality of the data** (completeness, accuracy, consistency, etc.)

## Background

---

## Overview

A comprehensive R-based system for profiling and scoring the quality of datasets from mining, geology, and metallurgy domains. DQS automatically extracts metadata, detects column properties, clusters similar characteristics, and generates detailed quality assessments across 11 scoring dimensions.

**Table of Contents**
- [Overview](#overview)
- [Features](#features)
- [Quick Start](#quick-start)
- [Installation](#installation)
- [Usage](#usage)
- [Architecture](#architecture)
- [Project Structure](#project-structure)
- [Development](#development)
- [Future Roadmap](#future-roadmap)
- [References](#references)

---

## Overview

The Data Quality Scoring system is designed to help understand, assess, and improve dataset quality in the specialized domains of geology, mining, and metallurgy. The system could easily be adapted to other domains. By analyzing raw data files (CSV, Excel), extracting metadata, and applying statistical profiles, DQS produces actionable quality insights stored in a structured SQLite database and visualized through an interactive Shiny dashboard.

**Key Workflow:**
```
Dataset Discovery → Load Files → Extract Metadata → Cluster Properties 
→ Profile Statistics → Score Quality → Export to SQLite
```

**Output:** An interactive Shiny dashboard for exploring quality assessments and detailed quality scores across multiple dimensions.

---

## Features

### 11 Scoring Dimensions

DQS evaluates datasets across these quality dimensions:

| Dimension | Description |
|-----------|-------------|
| **Accessibility** | How easily data can be accessed and understood |
| **Accuracy** | Degree of correctness and precision in the data |
| **Completeness** | Extent of missing values and data coverage |
| **Complexity** | Structural complexity and feature relationships |
| **Consistency** | Uniformity and agreement across the dataset |
| **Coverage** | Breadth of data types and value ranges |
| **Freshness** | Age and timeliness of the data |
| **Metadata** | Quality and completeness of metadata documentation |
| **Skewness** | Distribution characteristics and data balance |
| **Uniqueness** | Distinctiveness and duplication patterns |
| **Usability** | How well suited data is for analysis |

### Intelligent Metadata Detection

Automatic detection of:
- Column names and descriptions
- Physical units (SI, imperial, domain-specific)
- Detection methods and measurement techniques
- Row names and identifiers
- Detection limits and bounds

### Format Support

- **CSV** — Delimited text files with auto-detection
- **XLS/XLSX** — Excel workbooks with multi-sheet handling
- **Unknown formats** — Graceful handling with fallback processing

### S4 Object System

Professional OOP design using R's S4 class system:
- `datafile` — Base class for data file abstraction
- `metadata` — Structured metadata extraction results
- `score` — Individual quality dimension scores
- `dqs` — Complete quality assessment object

### Reproducible Pipeline

Built on the **targets** package for:
- Declarative pipeline definitions
- Automatic dependency tracking
- Incremental builds
- Pipeline introspection and debugging

---

## Quick Start

### Prerequisites
- R 4.0+
- RStudio (recommended)
- Required packages: `targets`, `quarto`, `tidyverse`, `DBI`, `RSQLite`, `readr`, `readxl`, `writexl`, `dplyr`, `ggplot2`, `univariateML`

### Running the Pipeline

Or run from RStudio with the Quarto notebook interface in this order:
- `quarto/run_dqs.qmd` (assumes ./data and ./output/catalogue)
- `dqs_report.qmd` (can render)
- `dqs_shiny.qmd` (run document)

`run_dqs.qmd` contains a setup block (lines 51-58) that initializes the targets pipeline. In that block you can set `dqs_client_files` (path to data) and `dqs_db_file` (path to output catalogue) to any directory, e.g. `"/path/to/dataset"`. The paths do not have to be inside the project.

Use the runner script for targets-only convenience:
- Run in R script mode (targets-only):
```bash
Rscript run.r /path/to/data /path/to/output/catalogue
```

Alternative targets-only workflow:
```R
# Load pipeline and all modules
source("_targets.R")

# Execute the full pipeline
tar_make()

# Inspect specific targets
tar_read(dqs_profile)           # Profile results
tar_read(dqs_score)             # Scoring results
tar_read(dqs_export)            # Export summary

# List all available targets
tar_targets()
```

### Viewing Results

```R
# Read data from the SQLite database
library(DBI)
connection <- dbConnect(RSQLite::SQLite(), "output/catalogue")
results <- dbReadTable(connection, "quality_scores")
dbDisconnect(connection)
```

### Interactive Dashboard

```R
# Launch the Shiny dashboard
quarto::quarto_run("dqs_shiny.qmd")
```

### PDF Report

For a static PDF summary of scoring and profiling results:

```R
# Render PDF report
quarto::quarto_render("dqs_report.qmd")
```

Or from the command line:
```bash
quarto render doc/dqs_report.qmd
```

---

## Installation

### 1. Clone or Download the Repository
```bash
cd /path/to/Data-Quality-Scoring
```

### 2. Install Required R Packages

```R
# Core dependencies
packages <- c(
  "targets", "quarto", "tidyverse", "DBI", "RSQLite",
  "readxl", "shiny", "ggplot2", "plotly"
)

install.packages(packages)
```

### 3. Set Up Data Directory

```bash
mkdir -p data
# Place your CSV/XLS files in the data/ directory
```

---

## Usage

### Processing Your Data

1. **Add data files** to the `data/` directory (CSV or XLS format)
2. **Run the pipeline**:
   ```R
   source("_targets.R")
   tar_make()
   ```
3. **Inspect outputs**:
   - SQLite database: `output/catalogue`
   - Generated reports: `output/` directory
4. **Explore results** via the interactive dashboard

### Accessing Quality Scores

```R
# Load results from SQLite
library(DBI)
db <- dbConnect(RSQLite::SQLite(), "output/catalogue")

# Get all scores
scores <- dbReadTable(db, "quality_scores")

# Get metadata
metadata <- dbReadTable(db, "dataset_metadata")

# Query specific datasets
library(dplyr)
high_quality <- scores %>%
  filter(overall_score > 0.8) %>%
  arrange(desc(overall_score))

dbDisconnect(db)
```

### Customizing Metadata Detection

Edit files in `r/detect_*.R` to:
- Add domain-specific unit definitions
- Improve column name recognition
- Tune detection thresholds
- Handle special data patterns

### Adding New Scoring Dimensions

1. Create `r/score_new_dimension.R` following the S4 class pattern
2. Implement the `score()` method
3. Add to pipeline in `_targets.R`
4. Update dashboard in `quarto/`

---

## Architecture

### Core Components

#### Data File Handling (`r/datafile*.R`)
- `datafile` — Abstract base class
- `datafile_csv` — CSV-specific implementation
- `datafile_xls` — Excel-specific implementation
- `datafile_class` — Class/structured format handling
- `datafile_unkwn` — Fallback for unknown formats

#### Metadata Extraction (`r/detect_*.R`)
- `detect_colnames()` — Column identification and naming
- `detect_units()` — Physical unit recognition
- `detect_method()` — Measurement method detection
- `detect_rownames()` — Row identifier detection
- `detect_limit()` — Detection limit and range bounds

#### Quality Scoring (`r/score_*.R`)
Individual scoring functions for each quality dimension. Each returns a `score` object containing:
- Numeric score (0-1 scale)
- Component scores
- Metadata and reasoning
- Recommendations for improvement

#### Pipeline Orchestration
- `dqs()` — Main orchestration function in `r/dqs.R`
- `dqs_datafiles()` — Dataset discovery and loading
- `dqs_extract()` — Metadata and feature extraction
- `dqs_metadata()` — Metadata aggregation
- `dqs_cluster()` — Property clustering (CLOPE algorithm)
- `dqs_profile()` — Statistical profiling
- `dqs_score()` — Quality scoring
- `dqs_export()` — SQLite database export

#### Utilities
- `clope.R` — CLOPE clustering algorithm for grouping similar properties
- `value_format.R` — Data value formatting and standardization
- `encode_format.R` — Format encoding and normalization
- `plot_donut.R` — Visualization helpers for score summaries

### Data Flow

```
Raw Data Files (CSV/XLS)
    ↓
File Loading & Validation (datafile_*)
    ↓
Metadata Detection (detect_*)
    ↓
Metadata Clustering (CLOPE)
    ↓
Statistical Profiling (profile_*)
    ↓
Quality Scoring (score_* × 11 dimensions)
    ↓
Score Aggregation
    ↓
SQLite Export
    ↓
Shiny Dashboard Visualization
```

---

## Project Structure

```
Data-Quality-Scoring/
├── README.md                      # This file
├── _targets.R                     # Pipeline definition
├── _quarto.yml                    # Quarto configuration
├── dqs.Rproj                      # RStudio project file
├── dqs_shiny.qmd                  # Interactive Shiny dashboard
├── dqs_report.qmd                 # Basic static report
├── run.r                          # R run script
│
├── r/                             # R modules (32 files)
│   ├── dqs.R                      # Main orchestration function
│   ├── datafile*.R                # File format implementations
│   ├── detect_*.R                 # Metadata detection (5 files)
│   ├── score_*.R                  # Scoring dimensions (11 files)
│   ├── dqs_*.R                    # Pipeline functions
│   ├── clope.R                    # Clustering algorithm
│   ├── metadata.R                 # Metadata class
│   ├── new_score.R                # Score class definition
│   └── [utilities]                # Formatting, plotting helpers
│
├── quarto/
│   ├── run_dqs.qmd                # Run DQS
│   └── app_*.qmd                  # Dashboard screens
│
├── data/                          # Input datasets (user-provided)
│   └── [your data files]
│
├── output/                        # Pipeline outputs
│   ├── catalogue                  # SQLite database
│   └── [reports and artifacts]
```

---

## Development

### Adding a New Scoring Dimension

1. **Create the scoring module:**
   ```R
   # r/score_newdimension.R
   setClass("score", ...)
   
   score_newdimension <- function(data, metadata) {
     # Calculate score
     # Return score object with results
   }
   ```

2. **Update the pipeline** (`_targets.R`):
   ```R
   tar_target(new_score_results, score_newdimension(data, metadata))
   ```

3. **Add dashboard visualization** (`quarto/app_score.qmd`)

### Improving Metadata Detection

Edit relevant `r/detect_*.R` file:
- Add domain-specific patterns to column name detection
- Extend unit dictionaries for specialized domains
- Tune detection thresholds based on your data

### Running Tests

```R
# Load and inspect pipeline
source("_targets.R")

# Check specific target
tar_read(target_name)

# Inspect pipeline graph
tar_visnetwork()
```

### Code Conventions

- **Naming**: `dqs_*` (pipeline), `score_*` (dimensions), `detect_*` (metadata), `datafile_*` (formats)
- **R Pipe**: Uses modern `|>` operator throughout
- **OOP**: S4 classes for extensibility
- **Documentation**: Roxygen comments above functions
- **Functional Design**: Minimize side effects; use pure functions where possible

---

## Database Schema

The exported SQLite database (`output/catalogue`) contains:

### Tables
- `quality_scores` — Final quality assessments for each dataset
- `dataset_metadata` — Extracted metadata and properties
- `dimension_scores` — Individual scores for each 11 dimensions
- `column_profiles` — Statistical profiles per column
- `detection_results` — Meta-detection outcomes

### Querying Examples

```R
# High-quality datasets
SELECT * FROM quality_scores WHERE overall_score > 0.8;

# Datasets by dimension strength
SELECT dataset_id, dimension, score FROM dimension_scores 
ORDER BY score DESC;

# Completeness analysis
SELECT dataset_id, SUM(is_complete) / COUNT(*) as completeness
FROM column_profiles GROUP BY dataset_id;
```

---

## Future Roadmap

Potential features for future development:

- **UMAP Visualization** — t-SNE/UMAP projection of dataset similarity
- **Enhanced Header Detection** — Multi-language column name recognition
- **Discipline Classification** — Domain-specific quality assessment models
- **End-to-End PDF Pipeline** — Automated extraction from research documents
- **Cloud Data Bank** — Hosted service for centralized assessment and exploration

---

## Troubleshooting

### Pipeline fails on data loading
- Verify CSV/XLS files are in `data/` directory
- Check for encoding issues (UTF-8 recommended)
- Ensure Excel files have consistent structure

### Metadata detection not working well
- Review detected patterns in pipeline output
- Edit `src/r/detect_*.R` files for your domain
- Consider manual metadata annotation

### Dashboard not rendering
- Ensure Quarto is installed: `install.packages("quarto")`
- Check Shiny dependency: `install.packages("shiny")`
- Review error logs in R console

### Database locked errors
- Close other connections to `output/catalogue`
- Ensure pipeline completed successfully
- Check file permissions in `output/` directory

---

## References

- **targets R package**: https://books.ropensci.org/targets/
- **Quarto**: https://quarto.org/
- **S4 Classes in R**: https://adv-r.hadley.nz/S4.html
- **Shiny**: https://shiny.rstudio.com/
- **DBI (Database Interface)**: https://dbi.r-dbi.org/

---

## License

Distributed under the MIT License. See `LICENSE` for more information.

---

**Last Updated:** April 2026  
**Current Version:** 1.0
