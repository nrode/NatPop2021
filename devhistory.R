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

## Create a R directory and a file for functions
usethis::use_r("loadfitnessdata")