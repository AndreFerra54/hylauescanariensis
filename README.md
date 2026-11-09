# Effects of Honey Bee Apiaries on *Hyla canariensis*

# Honey Bee Apiaries and *Hyla canariensis*

This repository contains the R script and dataset used to analyse productivity, development, morphological traits, cuticular hydrocarbon (CHC) profiles, and floral resource overlap in *Hyla canariensis* in relation to distance from honey bee apiaries.

---

## Associated publication

**Title:** *Proximity to apiaries affect fitness, morphology, and cuticular hydrocarbon profiles of an insular, endemic wild bee species*

**Authors:** *To be added*

**Journal:** *To be added*

**Year:** *To be added*

**DOI:** *To be added*

---

## Necessary files to run the analyses

* `Script.R` → Main script for the complete analytical workflow
* `Dataset_Hcanariensis.xlsx` → Dataset containing metadata, hive activity, emergence, productivity, morphology, CHC profiles, and plant–insect interaction data

When the script is executed, the following folders will be automatically created:

* `results/` → Statistical outputs, model summaries, and supplementary tables
* `figures/` → Graphical outputs and publication-ready figures

---

## Workflow overview

1. Download the repository files
2. Place `Script.R` and `Dataset_Hcanariensis.xlsx` in the same folder
3. Open the script in RStudio
4. Run the script sequentially following the numbered sections

The analysis pipeline is organized into modular sections.

---

* **0 – Prepare the environment** → Installs and loads required packages, creates output directories, imports the datasets, prepares variables, and associates the number of active apiaries with the closest recorded sampling day.

* **1 – Productivity analyses** → Analyses male, female, and total productivity in relation to distance from apiaries using Poisson GLMs and binomial GLMs for sex ratio. Generates productivity plots for 2020 and 2021.

* **2 – Evolution and emergence analyses** → Investigates brood cell completion dynamics in relation to distance from apiaries and the number of active honey bee hives using negative binomial GLMMs.

* **3 – Morphological analyses** → Investigates morphological traits in relation to distance and sex using linear mixed models, NMDS, PERMANOVA, multivariate dispersion analyses, and functional trait space analyses.

* **4 – Cuticular hydrocarbon (CHC) analyses** → Analyses CHC diversity, composition, and individual compounds using linear mixed models, NMDS, PERMANOVA, SIMPER, and additional models on the most important compounds.

* **5 – Floral competition analyses** → Constructs plant–insect interaction networks for 2019 and 2020, estimates niche overlap using Pianka's index, and investigates resource overlap in relation to distance from apiaries and the number of active honey bee hives.

---

## Main outputs

### Results

* Productivity GLM summaries
* Sex ratio GLM results
* Emergence GLMM summaries
* Morphological linear mixed model outputs
* Morphological summary statistics
* Morphological PERMANOVA results
* Functional richness summaries
* CHC linear mixed model outputs
* CHC summary statistics
* CHC PERMANOVA results
* SIMPER analyses
* Linear mixed model results for selected CHC compounds
* Floral resource overlap statistics

### Figures

* Male and female productivity plots
* Total productivity plots
* Brood cell evolution plots
* Morphological trait regression plots
* Morphological NMDS ordinations
* Functional richness plots
* CHC diversity and composition plots
* CHC NMDS ordinations
* Selected CHC compound plots
* Plant–insect interaction networks for 2019 and 2020

---

## Requirements

The script automatically installs missing CRAN packages.

The following packages are required:

* `openxlsx`
* `readxl`
* `writexl`
* `dplyr`
* `tidyr`
* `purrr`
* `tibble`
* `scales`
* `broom.mixed`
* `glmmTMB`
* `vegan`
* `spaa`
* `bipartite`
* `funspace`
* `car`
* `performance`
* `ggplot2`
* `viridis`
* `beepr`
* `here`

The script was developed for R and is intended to be run using a recent version of R and RStudio.

---

## Contact

For questions regarding the analyses, data, or code: *a.ferrari.research@gmail.com*

---

## Citation

If you use this code or dataset, please cite the associated publication.
