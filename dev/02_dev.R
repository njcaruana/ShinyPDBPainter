# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()
## Add one line by package you want to add as dependency
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjs")
usethis::use_package("readr")
usethis::use_package("tools")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("shinydashboardPlus") #download Github version for boxUpdate() function
usethis::use_package("stringr")
usethis::use_package("colourpicker")
usethis::use_package("bsplus")
usethis::use_package("r2d3")
usethis::use_dev_package("NGLVieweR", remote = "njcaruana/NGLVieweR")
#usethis::use_package("NGLVieweR")
usethis::use_package("shinyjqui")
usethis::use_package("uuid")
usethis::use_package("httr")
usethis::use_package("jsonlite")
#usethis::use_package("AnnotationDbi")
#usethis::use_package("Homo.sapiens")
usethis::use_package("RcppSimdJson")


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "fileInput" ) # Name of the module
golem::add_module( name = "structure" )
golem::add_module( name = "selection" )
golem::add_module( name = "label" )
golem::add_module( name = "fcPainter" )
golem::add_module( name = "stage" )
golem::add_module( name = "snapshot" )
golem::add_module( name = "sidebarcontrols" )
golem::add_module( name = "sequenceOutput" )
golem::add_module( name = "NGLVieweROutput" )
golem::add_module( name = "labelcontrols" )


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" )
golem::add_utils( "inputs")
golem::add_fct( "readFile" )
golem::add_fct( "loadLigand" )
golem::add_fct( "loadSelections")
golem::add_fct( "loadLabels" )
golem::add_fct( "loadStructure" )
golem::add_fct( "loadSurface" )
golem::add_fct( "fcPainter" )
golem::add_fct( "loadStage" )
golem::add_fct( "sequence_df" )
golem::add_fct( "ngl_to_position" ) #sequenceOutput
golem::add_fct( "aa_clicked_to_ngl" ) #sequenceOutput
golem::add_fct( "selection_to_ngl" ) # selectionInput
golem::add_fct( "insertUI_component" )
golem::add_fct( "removeUI_component" )
golem::add_fct( "loadUI_component" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler( "handlers" )
golem::add_css_file( "styles" )
golem::add_sass_file("custom")
golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("ShinyPDBPainter")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
