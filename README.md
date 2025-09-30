# $N_{2}O$ Analysis

Code and data repository for paper *Hot or Not? An Evaluation of Methods for Identifying Hot Moments of Nitrous Oxide Emissions From Soils*. Paper is published at Journal of Geophysical Research: Biogeosciences 130.1 (2025): e2024JG008138.

## Overview

This repository contains the code and data used for analyzing nitrous oxide (N₂O) emissions from agricultural soils and evaluating different methods for identifying "hot moments" - periods of exceptionally high N₂O flux. The study compares four statistical methods for threshold determination: 1.5×IQR (Interquartile Range), Isolation Forest, 2 Standard Deviations (2SD), and 4 Standard Deviations (4SD).

## Repository Structure

```
N2O-analysis/
├── Data/                          # Raw and processed data files
│   └── finalizeddata_all_data repository.xlsx
├── Python Code/                   # Python analysis scripts
│   ├── data_processing.py        # Core data processing and hot moment calculations
│   ├── figure_1.py               # Main figure generation
│   ├── figure_s3.py              # Supplementary figure S3 (threshold heatmaps)
│   ├── figure_s4.py              # Supplementary figure S4
│   ├── figure_s6.py              # Supplementary figure S6 (seasonal vs yearly)
│   └── figure_s8.py              # Supplementary figure S8 (time series)
├── R Code/                        # R analysis scripts
│   ├── Reinhart_N2O flux distributions and skew 12172024.R
│   ├── Reinhart_comparing among hot moment contributions_12172024.R
│   └── Reinhart_comparing among hot moment thresholds_12172024.R
├── LICENSE                        # MIT License
└── README.md                      # This file
```

## Data Description

The repository includes N₂O flux measurements from:
- **Study period**: May 13, 2022 - Post-harvest 2022
- **Measurement chambers**: 4 chambers across 4 nodes (16 chamber-node combinations)
- **Seasons analyzed**:
  - Early growing season: May 13 - July 7, 2022
  - Late growing season: July 8 - October 31, 2022
  - Post-harvest season: November 1, 2022 onwards

The data file (`finalizeddata_all_data repository.xlsx`) contains multiple sheets with:
- N₂O flux measurements
- Threshold calculations
- Hot moment percentages
- Statistical comparisons
- Skewness analysis

## Methods

### Hot Moment Identification Methods

Four statistical methods are evaluated for identifying hot moments:

1. **1.5×IQR (Interquartile Range)**
   - Threshold = Q3 + 1.5 × IQR
   - Traditional outlier detection method

2. **Isolation Forest**
   - Machine learning anomaly detection algorithm
   - Parameters: n_estimators=100, contamination='auto', random_state=40

3. **2 Standard Deviations (2SD)**
   - Threshold = Mean + 2 × SD
   - Identifies values >2 standard deviations above mean

4. **4 Standard Deviations (4SD)**
   - Threshold = Mean + 4 × SD
   - More conservative threshold for extreme values

## Dependencies

### Python Requirements
- numpy
- pandas
- matplotlib
- scikit-learn (for IsolationForest)
- scipy
- openpyxl (for Excel file handling)

### R Requirements
- tidyverse
- readxl
- lubridate
- esquisse
- dplyr
- data.table
- anytime
- tidyr
- ggpubr
- gridExtra
- emmeans
- multcompView
- multcomp
- car
- plyr
- ggplot2

## Usage

### Python Analysis

1. Ensure all dependencies are installed
2. Place the data file (`N2O_Fluxes.xlsx` or equivalent) in the same directory as the scripts
3. Run the analysis scripts:

```bash
python "Python Code/data_processing.py"      # Process data and calculate thresholds
python "Python Code/figure_1.py"             # Generate main figure
python "Python Code/figure_s3.py"            # Generate threshold heatmaps
```

### R Analysis

1. Update file paths in the R scripts to match your local directory structure
2. Run the R scripts to perform statistical analyses and generate figures:

```r
source("R Code/Reinhart_N2O flux distributions and skew 12172024.R")
source("R Code/Reinhart_comparing among hot moment contributions_12172024.R")
source("R Code/Reinhart_comparing among hot moment thresholds_12172024.R")
```

## Key Findings

The code enables analysis of:
- Percentage of time identified as hot moments by each method
- Cumulative N₂O flux contribution from hot moments
- Seasonal vs. yearly threshold calculations
- Relationship between flux distribution skewness and method performance
- Comparison of threshold values across methods and seasons

## Citation

If you use this code or data in your research, please cite:

> Hot or Not? An Evaluation of Methods for Identifying Hot Moments of Nitrous Oxide Emissions From Soils. *Journal of Geophysical Research: Biogeosciences* 130.1 (2025): e2024JG008138.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Authors

- Jiacheng X.
- Contributors listed in the published paper

## Contact

For questions or issues, please open an issue on this GitHub repository.
