# DNA-Invasion-Protocol
Companion R script to a published protocol (see below for DOI). Handles data visualization and C50 calculations for Dose-Response Invasion Assay experiments. 

This repository contains R code used for data analysis in the protocol:
**An electrophoretic mobility shift assay with chemiluminescent readout to evaluate DNA-targeting oligonucleotide-based probes**  
Available at: [DOI coming soon]  
Author: Michaela E. Everly  
ORCID: https://orcid.org/0009-0003-5689-0700

## Contents
- `DR_Script.R`: Main analysis script
- `README.md`: Project overview
- `LICENSE`: License for usage and distribution

## Beginner-Friendly
This script is designed to be accessible to users who are new to R. It includes extensive comments explaining the functions of most lines, with clear indications of where to modify the code for your own data.

## Requirements
- R version >= 4.x
- Required packages:
  - `ggplot2`
  - `minpack.lm`
  - `dplyr`
  - `colorspace` (optional)
  - `ggsci` (optional)

## Usage
1. Clone or download this repository.
2. Open `DR_Script.R` in R or RStudio.
3. Modify the input paths if needed.
4. Run the script.

## Citation
Please credit the author and refer to this repository when using or adapting the code.

## License
This project is licensed under the MIT License (https://opensource.org/licenses/MIT). See the LICENSE file for details.

## Keywords
C50 value, data-visualization, DNA invasion, DNA hybridization, dose-response, double-duplex invasion, duplex invasion, electrophoretic mobility shift assay, EMSA, non-linear line fitting, oligonucleotide probe
