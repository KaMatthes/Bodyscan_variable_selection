# Associations between 3D surface scanner derived anthropometric measurements and body composition in a cross-sectional sample.

## Paper

<br >
Guarnieri Lopez M, Matthes KL, Sob C, Bender N, Staub K (2023) Associations between 3D surface scanner derived anthropometric measurements and body composition in a cross-sectional study. <i>  Eur J Clin Nutr  </i>  https://doi.org/10.1038/s41430-023-01309-4
<br >

## Data

The data is public available via Zenodo:
<br >
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7108758.svg)](https://doi.org/10.5281/zenodo.7108758)


## Content of this repository

### Structure

```
.
+-- R
+-- data
+-- output

```

### `R` folder 

This folder contains all R scripts.

  - `data.R` : prepares the data for the analysis
  - `Bootstrapping.R` : function of the variable selection
  - `function_data.R` : function to define data for analysis
  - `lineareRegression.R` : code to calculate the adjusted R-square values of the respective models
  - `lineareRegression_bootstrapping.R` : code of the validated boot strapping models (Table 3)
  - `Table1_wilcox_chi2tests.R` : code to create Table 1
  - `bland_altman.R` : code of scatterplot comparisons and bland-altman plots (Figure 1 and 2)
  - `plots_body_composition.R` : code to create Supplement Figure 1
  - `Appendix_Table1.R` : code to create Supplement Table 1
    
### `data` folder

This folder contains the data.
  - `data_study.csv` : contains socio-demographic, health- and lifestyle factors, and body scan variables of each participant
  - `healthscore.csv` : contains "healthy score" from the food frequency questions calculated from from five food categories: fruits, vegetables, wholegrain
products, meat, and sweet/salty snacks. For each category the officially recommended minimum or maximum amount of weekly intake was used as the cut-off value and a point was assigned if the recommendation was met. A score from 0 to 5 was built to reflect the overall healthiness of the diet.

### `output` folder

This folder contains the bootstrapping results of each outcome.

### `master.R` 

This skript contains information of the used R packages, number of bootstrapping, set seed etc.. To run the function `bootsptrapping.R`, please run first `data.R`.

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
