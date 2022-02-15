---
output: 
  html_document:
    keep_md: true
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



## GENERAL INFORMATION

####  1. Description
This research compendium describes how to analyze fecundity, egg-to-adult viability and oviposition preference data from an experiment conducted in D. suzukii in 2018, in South of France (CBGP, Montpellier, France). [here](https://github.com/nrode/NatPop2021).

The analyses of this research compendium are published in JOURNAL NAME in DATE. 
Citation - Link to come. 

####  2. Author information and main investigators
	Corresponding Investigator and first author:
		Name: Laure Olazcuaga
		Institution: CBGP, INRAE, France and Colorado State University, USA
		Email: olaz.laure@gmail.com
		ORCID: "0000-0001-9100-1305

	Co-investigator and last author: 
		Name: Nicolas Rode
		Institution: CBGP, INRAE, France
    Email: nicolas.rode@inrae.fr
    ORCID: "0000-0002-1121-4202

For the complete list of authors of the manuscript, see the manuscript. 

####  3. Date and Geopgraphic locationof data collection
2018 - South of France. 
The exact date and location data is here.[:clipboard: TableS6](data/TableS6.csv). 

####  4. Funding sources that supported the collection of the data
Data collection was supported by the Languedoc-Roussillon region (France) through the European Union program FEDER FSE IEJ 2014-2020 (project CPADROL), the INRAE scientific department SPE (AAP-SPE 2016), the National Science Foundation (DEB-0949619), the USDA Agriculture and Food Research Initiative award (2014-67013-21594), Hatch project 1012868, the French Agropolis Fondation (LabEx Agro–Montpellier) through the AAP “International Mobility” (CfP 2015-02), the French programme investissement d'avenir, and the LabEx CEMEB through the AAP "invited scientist 2016", the CeMEB LabEx/University of Montpellier (ANR-10-LABX-04-01) and the INRAE scientific department SPE (AAP-SPE 2021PestAdapt).

####  5. Recommended citation for this dataset:
Olazcuaga et al. (2022), Data from: Analysis Nat Pop 2021, Dryad, Dataset


## CONTENTS

The [:hammer: dev_history.R](dev_history.R) file allow to rebuild our research compendium from scratch for maximum reproducibility.

The [:open_file_folder: **data**](data/) directory contains the data sets used in the analyses. 

### Data et file overview
 - [:clipboard: Data_Pref](data/DATACOMPLET_PREF.csv) : Dataset containing oviposition preference data..

 - [:clipboard: Data_Perf](data/DATACOMPLET_PERF.csv) : Dataset containing offsrping performance and oviposition stimulation data.
 
 - [:clipboard: TableS6](data/TableS6.csv) : Database containing location and sampling date for each population. 


The [:open_file_folder: **figures**](figures/) directory contains the figures generated during the analyses.

The [:open_file_folder: **R**](R/) directory contains the functions used in the analyses.

The [:open_file_folder: **man**](man/) directory contains the documentation for the functions.
  
The [:open_file_folder: **reports**](reports/) directory contains the `.Rmd` files used to build each part of the analyses and produce the final figures. They also have rendered versions and `html` suitable for reading in a web browser.

### Markdown versions

 - [:clipboard: Analysis_main_text](reports/maintext.Rmd) : All the analyses that can be found in the main text.

 - [:clipboard: Simuls_powertest_nongenet](reports/simuls_powertest_nongenet.Rmd) : Simulations of fitness data with different values of SA plastic.
 
 - [:clipboard: Simuls_fitness](reports/simuls_fitness.Rmd) : Simulations of fitness data.
 
 - [:clipboard: Supplements_WT3](reports/supplements_WT3.Rmd) : All the analyses of the WT3 data.
 
 - [:clipboard: Supplements_realdata](reports/supplements_realdata.Rmd) : All the analyses with real data.
 
 - [:clipboard: Supplements_MKB](reports/supplements_MotherKnowsBest.Rmd) : All the Mother knows best analyses. 
 
 - [:clipboard: Supplements_acp](reports/supplements_acp.Rmd) : All the acp analyses. 

 - [:clipboard: Supplements_MKB](reports/supplements_correlation_generation.Rmd) : All the analyses of correlation between generations. 
 
 
### HTML versions

 - [:clipboard: Analysis_main_text](reports/maintext.html) : All the analyses that can be found in the main text.

 - [:clipboard: Simuls_powertest_nongenet](reports/simuls_powertest_nongenet.html) : Simulations of fitness data with different values of SA plastic.
 
 - [:clipboard: Simuls_fitness](reports/simuls_fitness.html) : Simulations of fitness data.
 
 - [:clipboard: Supplements_WT3](reports/supplements_WT3.html) : All the analyses of the WT3 data.
 
 - [:clipboard: Supplements_realdata](reports/supplements_realdata.html) : All the analyses with real data.
 
 - [:clipboard: Supplements_MKB](reports/supplements_MotherKnowsBest.html) : All the Mother knows best analyses. 
 
 - [:clipboard: Supplements_acp](reports/supplements_acp.html) : All the acp analyses. 

 - [:clipboard: Supplements_MKB](reports/supplements_correlation_generation.html) : All the analyses of correlation between generations. 


## HOW TO RUN IT?

This research compendium has been developed using the statistical programming language R. To work with the compendium, you will need
installed on your computer the [R software](https://cloud.r-project.org/)
itself and optionally [RStudio Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium by cloning this [repository](https://github.com/nrode/NatPop2021.git):
  
  - open the `.Rproj` file in RStudio

  - open scripts `.Rmd` in reports folder and run it to produce all the analyses and associated reports.
  
  - launch the [`README.html`](README.html) to be able to explore the contents on your web browser


