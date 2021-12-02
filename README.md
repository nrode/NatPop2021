---
output: 
  html_document:
    keep_md: true
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# Description


This research compendium describes how to analyze data from experiment conducted in D. suzukii [here](https://github.com/nrode/NatPop2021).


## Contents

The [:open_file_folder: **data**](data/) directory contains the data sets used in the analyses. 

The [:open_file_folder: **R**](R/) directory contains the functions used in the analyses.

The [:open_file_folder: **man**](man/) directory contains the documentation for the functions.
  
The [:open_file_folder: **reports**](reports/) directory contains the `.Rmd` files used to build each part of the analyses and produce the final figures. They also have rendered versions and `html` suitable for reading in a web browser.

### Markdown versions

 - [:clipboard: Analysis_main_text](reports/main_text.Rmd) : All the analyses that can be found in the main text.

 - [:clipboard: Simuls_powertest_nongenet](reports/simuls_powertest_nongenet.Rmd) : Simulations of fitness data with different values of SA plastic.
 
 - [:clipboard: Simuls_fitness](reports/simul_fitness.Rmd) : Simulations of fitness data.
 
 - [:clipboard: Supplements_WT3](reports/supplements_WT3.Rmd) : All the analyses of the WT3 data.
 
 - [:clipboard: Supplements_WT3](reports/supplements_WT3.Rmd) : All the figures with real data. 
 
 
### HTML versions

 - [:clipboard: Analysis_main_text](reports/main_text.html) : All the analyses that can be found in the main text.

 - [:clipboard: Simuls_powertest_nongenet](reports/simuls_powertest_nongenet.html) : Simulations of fitness data with different values of SA plastic.
 
 - [:clipboard: Simuls_fitness](reports/simul_fitness.html) : Simulations of fitness data.
 
 - [:clipboard: Supplements_WT3](reports/supplements_WT3.html) : All the analyses of the WT3 data.
 
 - [:clipboard: Supplements_WT3](reports/supplements_WT3.html) : All the figures with real data. 

The [:open_file_folder: **figures**](figures/) directory contains the figures generated during the analyses.

The [:hammer: dev_history.R](dev_history.R) file allow to rebuild our research compendium from scratch for maximum reproducibility.


## How to run it ?

This research compendium has been developed using the statistical programming language R. To work with the compendium, you will need
installed on your computer the [R software](https://cloud.r-project.org/)
itself and optionally [RStudio Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium by cloning this [repository](https://https://github.com/nrode/AnalysisEvolExp2020):
  
  - open the `.Rproj` file in RStudio

  - open scripts `.Rmd` in reports folder and run it to produce all the analyses and associated reports.
  
  - launch the [`README.html`](README.html) to be able to explore the contents on your web browser


