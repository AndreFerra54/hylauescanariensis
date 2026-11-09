# Effects of Honey Bee Apiaries on *Hyla canariensis*

## Analyses

The script is organized into five main analytical sections:

### 1. Productivity
- Analysis of male, female, and total individual counts in relation to distance from apiaries.
- Generalized linear models (GLMs) with Poisson distributions.
- Binomial GLMs to investigate sex ratio variation.
- Visualization of productivity patterns across distances and years (2020–2021).

### 2. Development and Emergence
- Analysis of brood cell completion dynamics.
- Generalized linear mixed models (GLMMs) using negative binomial distributions.
- Evaluation of the effects of distance from apiaries and the number of active honey bee hives.
- Accounting for hierarchical structure through random effects.

### 3. Morphology
- Analysis of morphological traits in relation to distance and sex.
- Linear mixed-effects models.
- Non-metric multidimensional scaling (NMDS).
- PERMANOVA and multivariate dispersion analyses.
- Functional trait space analyses and functional richness (FRic) estimation.

### 4. Cuticular Hydrocarbon (CHC) Profiles
- Analysis of CHC diversity and composition.
- Linear mixed-effects models investigating distance and sex effects.
- NMDS and PERMANOVA to assess differences in chemical profiles.
- SIMPER analyses to identify compounds contributing to dissimilarities.
- Additional models for the most influential compounds.

### 5. Floral Competition and Resource Overlap
- Construction of plant–insect interaction networks for 2019 and 2020.
- Estimation of niche overlap using Pianka's index.
- Assessment of resource overlap in relation to distance from apiaries and the number of active honey bee hives.

## Repository Structure

```text
.
├── README.md
├── Script.R
├── Dataset_Hcanariensis.xlsx
├── figures/
└── results/
```

The `figures/` and `results/` directories are automatically created by the script if they do not already exist.

## Requirements

The analyses are performed in R and require the following packages:

- `openxlsx`
- `readxl`
- `writexl`
- `dplyr`
- `tidyr`
- `purrr`
- `tibble`
- `scales`
- `broom.mixed`
- `stats`
- `glmmTMB`
- `vegan`
- `spaa`
- `bipartite`
- `funspace`
- `car`
- `performance`
- `ggplot2`
- `viridis`
- `beepr`
- `here`

The script automatically installs missing packages and loads them at the beginning of the analysis.

## How to Run

1. Install [R](https://www.r-project.org/) and preferably [RStudio](https://posit.co/download/rstudio/).
2. Clone or download this repository.
3. Ensure that `Script.R` and `Dataset_Hcanariensis.xlsx` are located in the same directory.
4. Open the script in RStudio.
5. Run the script from the beginning.

The script automatically prepares the working environment, loads the datasets, performs the analyses, and exports the results.

## Input Data

The script requires the file:

`Dataset_Hcanariensis.xlsx`

The Excel workbook contains the following sheets:

- `Metadata`
- `Hives`
- `Emergence`
- `Productivity`
- `Morphology`
- `CHC`
- `Network`

## Outputs

The analyses generate figures and statistical results, which are saved in the following directories:

### Figures
PDF files containing:
- Productivity and sex-specific patterns.
- Developmental dynamics.
- Morphological trait relationships and NMDS ordinations.
- Functional richness comparisons.
- CHC diversity and composition patterns.
- Plant–insect interaction networks.

### Results
Statistical outputs in `.txt` and `.xlsx` formats, including:
- GLM and GLMM summaries.
- Morphological and CHC model results.
- PERMANOVA and multivariate dispersion results.
- SIMPER outputs.
- Summary statistics.

## Reproducibility

A random seed (`set.seed(123)`) is specified for multivariate ordination analyses to improve reproducibility.

All analyses are conducted using the data contained in the provided Excel workbook.

## Citation

If you use this code, please cite the associated scientific publication:

*Citation details to be added.*

## License

A license has not yet been specified for this repository.


