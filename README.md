# NBA Prediction Project

This project aims to predict the outcomes of NBA playoff series, including the winner of each series and the number of games played. Using historical player and team data, a game-level modeling approach is used and abstracted to predict series outcomes. The XG boosted tree is the classification model of choice that incorporates rolling window, advanced and simple box score metrics from the previous 5 games, a measure of skill rating difference via the Glicko-2 rating system and various time series proxies for injury and form.

## Usage

To view the content of this project please visit this [link](https://jordanh-49.github.io/nba_playoff_predictor).

## Packages

### Guide

-   **`tidyverse`:** Collection of packages for transformation and cleaning data.
-   **`tidymodels`:** Collection of packages for modeling and machine learning using tidyverse principles.
-   **`PlayerRatings`:** Implement dynamic updating methods for player ratings estimation.
-   **`ggplot2`**: Implement data visualisations using the grammar of graphics.
-   **`gt/gtExtras`:** Used to create presentation-ready display tables
-   **`vip`: Used to create** variable importance plots

### Installation

To install the packages run the following command in R:

``` r
packages <- c("tidyverse", "tidymodels", "ggplot2", "RcppRoll", "vip", "doParallel",
              "xgboost", "lme4", "finetune", "PlayerRatings", "gt", "gtExtras")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
```
