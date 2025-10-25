# 🏃‍♂️ Garmin Run Visualszer

This project parses and visualises **Garmin `.tcx` activity files**, producing clean and informative charts of your **running performance** — including heart rate zones, pace splits, and combined visual summaries.  

You can use this package in **two ways**:
1. 📊 From the **command line** using `Plot_Runs.R`
2. 🌐 Interactively via the **Shiny web app** (`app.R`)

## 📊 Example Output
![Combined](docs/combined.png)
---

## ⚡️ Quick Start

```bash
# Run from the command line (generate charts)
Rscript Plot_Runs.R ./activities/*.tcx ./Charts

# Or launch interactive Shiny app
R -e "shiny::runApp('app.R')"
```

---

## 🎨 Features

1. **Kilometre Split Chart**: Bar chart displaying pace (duration per kilometre).
2. **Average Heart Rate Line Chart**: Line chart showing average heart rate per kilometre with points.
3. **Heart Rate Zones Stacked Chart**: Stacked bar chart showing proportion of time spent in different heart rate zones:

   * Zone 5: Red
   * Zone 4: Orange
   * Zone 3: Yellow
   * Other: Gray
4. **Combined Chart**: All three charts stacked vertically.

## 📁 Project Structure

```
fitnessPlotter/
├── R/ # Contains function library
│ └── Plot_Runs.R # Core functions for parsing and plotting
│ └── init.R # Initialization script for installing/loading dependencies
├── activities/ # Sample Garmin .tcx activities
│ └── 2025-10-01T11_23_52+00_00_20556010525.tcx
├── app.R # Shiny app for interactive visualisation
├── README.md # This file
├── .Rproj.user/ # RStudio project files (usually ignored in Git)
├── Plot_Runs.R # CLI script to generate plots from .tcx files
├── GarminPlots.Rproj # RStudio project file
└── .gitignore # Recommended to ignore .Rproj
```

* `R/Plot_Runs.R` - functions for parsing TCX, summarising, and plotting.
* `Plot_Runs.R` - command-line script for batch processing activities.
* `app.R` - Shiny app to interactively visualise a single TCX file.
* `activities/` - sample Garmin TCX files.
* `R/init.R` - helper to install and load required packages.

## ⚙ Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/yourusername/fitnessPlotter.git
cd fitnessPlotter
```

### 2. Install dependencies

Open R or RStudio in the project directory and run:

```r
source("R/init.R")  # loads the init() function
init()               # installs and loads all required packages
```

### 3. Example Activity

A sample activity is included for testing:

```
activities/2025-10-01T11/23/52+00/00_20556010525.tcx
```

## 📘 Notes

* `R/init.R` ensures all packages including `shiny`, `ggplot2`, `patchwork`, `xml2`, and `fontawesome` are installed.
* Plots dynamically adjust sizes based on the total distance of the run.
* Font Awesome is used for icons in plot subtitles (install locally if needed).

---

## 🧩 Usage Options

### 1️⃣ Command-line Script (`Plot_Runs.R`)

Run the batch visualisation script directly from your terminal or RStudio Console:

```bash
Rscript Plot_Runs.R ./activities/*.tcx ./Charts
```

**Arguments:**
| Argument | Description |
|-----------|-------------|
| `<tcx_files>` | One or more `.tcx` files, or a wildcard (e.g. `./activities/*.tcx`) |
| `<output_root>` | Directory where the generated plots will be saved |

Each `.tcx` file produces:
- `*_pace.png` – kilometre splits (pace duration)
- `*_hr_line.png` – average heart rate by kilometre
- `*_hr_stacked.png` – time spent in HR zones
- `*_combined.png` – combined view of all three

---

### 2️⃣ Shiny Web App (`app.R`)

For an interactive experience, launch the Shiny dashboard:

```r
shiny::runApp("app.R")
```

This opens a browser interface where you can:
- Upload a **.tcx** file  
- View 4 visualisations (pace, HR line, HR zones, combined)
- Download any chart as a `.png` file

#### 🧭 Navigation

| Tab | Description |
|------|-------------|
| **Pace Splits** | Visualises pace per kilometre |
| **Heart Rate** | Average HR by distance |
| **Heart Rate Zones** | Stacked proportion of time in each HR zone |
| **Combined** | All three charts arranged together |

---

## 📊 Sample Output

Below are sample output charts generated from a sample `.tcx` file:

| Chart Type | Example |
|-------------|----------|
| Pace Splits | ![Pace Splits](docs/pace.png) |
| HR Line | ![HR Line](docs/hr_line.png) |
| HR Zones | ![HR Zones](docs/hr_stacked.png) |
| Combined View | ![Combined](docs/combined.png) |

---

## 🔍 Modular Design

The plotting logic is defined in **`R/Plot_Runs.R`**, which provides the following key functions:

| Function | Purpose |
|-----------|----------|
| `read_tcx_data(file)` | Parses a `.tcx` file into a tidy `tibble` |
| `get_plot_sizes(max_km)` | Dynamically adjusts font and point sizes by distance |
| `summarize_run_data(run_data)` | Computes HR summaries and kilometre splits |
| `plot_hr_stacked(...)` | Creates stacked HR zone bar chart |
| `plot_hr_line(...)` | Creates HR average line chart |
| `plot_km_splits(...)` | Creates pace-per-kilometre chart |
| `combine_run_plots(...)` | Combines the three plots vertically |

---

## 🧠 Tips

- Ensure `.tcx` files include **latitude, longitude, heart rate**, and **distance** data.  
- Non-running activities are automatically skipped.  
- The app uses **Font Awesome** for icons — ensure it’s available via your system fonts or embedded through `{fontawesome}`.

---

## ✅ Example Workflow

```bash
# 1. Generate charts for all activities
Rscript Plot_Runs.R ./activities/*.tcx ./Charts

# 2. View charts
open Charts/2025-10-01T11_23_52+00_00_20556010525/

# 3. Launch the Shiny app
R -e "shiny::runApp('app.R')"
```

---

## 🚀 Initialisation

This repository includes an **`init.R`** script that automatically installs all dependencies.

Simply run this once from your R console:

```r
source("init.R")
```

This ensures all required libraries are available before using the CLI tool or launching the Shiny app.

---

## 📜 License

MIT License © 2025 [Your Name]
