# Associations between 3D surface scanner derived anthropometric measurements and body composition in a cross-sectional age-heterogenous sample of 201 men and women

## Paper

Link später einfügen

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
  - `lineareRegression.R` : functions to calculate the adjusted R-square values of the respective models

### `data` folder

This folder contains the data.
  - `data_study.csv` : contains socio-demographic, health- and lifestyle Factors, and body scan variables of each participant
  - `healthscore.csv` : contains "healthy score" from the food frequency questions calculated from from five food categories: fruits, vegetables, wholegrain
products, meat, and sweet/salty snacks. For each category the officially recommended minimum or maximum amount of weekly intake was used as the cut-off value and a point was assigned if the recommendation was met. A score from 0 to 5 was built to reflect the overall healthiness of the diet (Ref Sob einfügen)

### `output` folder

This folder contains the bootstrapping results of each outcome.

### `master.R` 

This skript contains information of the used R packages, number of bootstrapping, set seed etc.. To run the function `bootsptrapping.R`, please run first `data.R`.

