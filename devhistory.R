# Create project on Github

## Create compendium
rrtools::use_compendium("/Users/rodenico/Documents/Pro/Articles/2020_popnat/NatPop2021", open = FALSE)

## Add to .gitignore
usethis::use_git_ignore(".DS_Store")
usethis::use_build_ignore(".DS_Store")
usethis::use_git(message = ":see_no_evil: Ban .DS_Store files")

## Modify DESCRIPTION file
usethis::edit_file("DESCRIPTION")
usethis::use_git(message = ":bulb: Update documentation")

## Create directories
dir.create("data")
dir.create("reports")

## Update DESCRIPTION file
usethis::use_package("here")
usethis::use_package("lme4")
usethis::use_package("MASS")
usethis::use_package("reshape")
usethis::use_package("ggplot2")
usethis::use_package("cowplot")



## Create a R directory and a file for functions
usethis::use_r("simulmodel")
usethis::use_r("simul_fitnessdata")
usethis::use_r("simul_fitnessdata_unbalanced")
usethis::use_r("add_indic_sign") 
usethis::use_r("add_sim_number_SA")
usethis::use_r("select_sample_simul")
usethis::use_r("theme_LO_sober")


## Update NAMESPACE file
devtools::document()

## Load all required packages
devtools::load_all()

