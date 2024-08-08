# AUBIE-Analytics

## Overview

AUBIE-Analytics is a project dedicated to analyzing bird classification data. The dataset used in this analysis can be accessed [here](https://drive.google.com/drive/folders/1pL_Z3m3C46_QE_e6pnS16rzUSP6cUaBW). This repository includes various components for exploratory data analysis (EDA) of this dataset.

## Dataset

The dataset, `bird_classification_v1.csv`, is available through the provided link. For the analysis, only detections with a rank of 1 will be considered.

## Repository Structure

- **RMD/**: Contains:
  - `analysis.Rmd`: R Markdown file documenting the exploratory data analysis (EDA) report.
  - `analysis.html`: HTML version of the EDA report generated from the R Markdown file.
  - Markdown files with interactive plots not yet included in the main report.

- **plots/**: Contains:
  - Various plots generated during the exploratory data analysis.

- **R/**: Contains:
  - R scripts used to create the plots in the `plots/` folder.

## Usage

1. **Download the Dataset**: Obtain the `bird_classification_v1.csv` file from the provided Google Drive link.
2. **Run Analysis**:
   - Navigate to the `RMD/` directory and execute the `analysis.Rmd` file to view the EDA report.
   - Review and utilize the plots in the `plots/` directory for further insights.
   - Refer to the R scripts in the `R/` folder if you need to regenerate any of the plots or customize the analysis.

For additional information or questions, please refer to the documentation within the respective folders.

