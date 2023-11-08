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
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "randomplot", with_test = FALSE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

### Studium
golem::add_module(name = "studium_studienzahl", with_test = TRUE) # Name of the module

golem::add_module(name = "studium_studienzahl_einstieg", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_studienzahl_einstieg_gender", with_test = TRUE) # Name of the module

golem::add_module(name = "studium_studienzahl_einstieg_verlauf", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_studienzahl_einstieg_verlauf_gender", with_test = TRUE) # Name of the module

golem::add_module(name = "studium_studienzahl_einstieg_comparison", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_studienzahl_einstieg_comparison_gender", with_test = TRUE) # Name of the module



golem::add_module(name = "studium_studienzahl_verlauf", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_studienzahl_verlauf_bl", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_studienzahl_verlauf_bl_subject", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_studienzahl_choice_1", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_studienzahl_choice_2", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_abschluss", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_abschluss_choice_1", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_abschluss_choice_2", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_compare", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_compare_choice", with_test = TRUE) # Name of the module
golem::add_module(name = "studium_compare_choice_updates", with_test = FALSE) # Name of the module

### Beruf
golem::add_module(name = "beruf_arbeitsmarkt", with_test = TRUE) # Name of the module
golem::add_module(name = "beruf_arbeitsmarkt_einstieg", with_test = TRUE) # Name of the module
golem::add_module(name = "beruf_arbeitsmarkt_multiple", with_test = TRUE) # Name of the module
golem::add_module(name = "beruf_arbeitsmarkt_verlauf", with_test = TRUE) # Name of the module
golem::add_module(name = "beruf_arbeitsmarkt_verlauf_bl", with_test = TRUE) # Name of the module

### Schule
golem::add_module(name = "schule_kurse", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_multiple", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_multiple_mint", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_einstieg", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_einstieg_verlauf", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_einstieg_comparison", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_pie_gender", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_verlauf_gender", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_comparison_gender", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_comparison_subjects", with_test = TRUE) # Name of the module

golem::add_module(name = "schule_kurse_comparison_bl", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_map", with_test = TRUE) # Name of the module

golem::add_module(name = "schule_kurse_map_gender", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_verlauf_multiple", with_test = TRUE) # Name of the module

golem::add_module(name = "schule_kurse_ranking", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_ranking_gender", with_test = TRUE) # Name of the module


golem::add_module(name = "schule_kurse_verlauf", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_verlauf_bl", with_test = TRUE) # Name of the module
golem::add_module(name = "schule_kurse_verlauf_bl_subjects", with_test = TRUE) # Name of the module


## Internationale Daten
golem::add_module(name = "international", with_test = TRUE)
golem::add_module(name = "international_start", with_test = TRUE)

golem::add_module(name = "international_map", with_test = TRUE)
golem::add_module(name = "international_top10_mint", with_test = TRUE)
golem::add_module(name = "international_top10_mint_gender", with_test = TRUE)

golem::add_module(name = "international_schule_map", with_test = TRUE)
golem::add_module(name = "international_schule_item", with_test = TRUE)
golem::add_module(name = "international_schule_migration", with_test = FALSE)

golem::add_module(name = "international_arbeitsmarkt_map", with_test = FALSE)
golem::add_module(name = "international_arbeitsmarkt_top10", with_test = FALSE)
golem::add_module(name = "international_arbeitsmarkt_vergleich", with_test = FALSE)


## Fachkraft Daten
golem::add_module(name = "fachkraft", with_test = FALSE)
golem::add_module(name = "fachkraft_start", with_test = FALSE)
golem::add_module(name = "fachkraft_item_epa", with_test = FALSE)
golem::add_module(name = "fachkraft_item_mint", with_test = FALSE)
golem::add_module(name = "fachkraft_item_detail", with_test = FALSE)


### Home
golem::add_module(name = "home_start", with_test = TRUE) # Name of the module
golem::add_module(name = "home_start_comparison", with_test = TRUE) # Name of the module
golem::add_module(name = "home_start_comparison_mint", with_test = TRUE) # Name of the module
golem::add_module(name = "home_start_comparison_mint_gender", with_test = TRUE) # Name of the module
golem::add_module(name = "home_start_leaky", with_test = TRUE) # Name of the module
golem::add_module(name = "home_start_multiple", with_test = TRUE) # Name of the module
golem::add_module(name = "home_start_einstieg", with_test = TRUE) # Name of the module
golem::add_module(name = "home_start_einstieg_gender", with_test = TRUE) # Name of the module

### Ausbildung
golem::add_module(name = "ausbildung_vertraege", with_test = TRUE) # Name of the module
golem::add_module(name = "ausbildung_vertraege_multiple", with_test = TRUE) # Name of the module
golem::add_module(name = "ausbildung_vertraege_verlauf", with_test = TRUE) # Name of the module

### Quellen
golem::add_module(name = "quellen", with_test = TRUE) # Name of the module


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)

# Studium
golem::add_fct(name = "preprocess_studium", module = NULL,with_test = TRUE)
golem::add_fct(name = "plots_studium", module = NULL,with_test = TRUE)
golem::add_fct(name = "valueBoxes_studium", module = NULL,with_test = TRUE)

# Beruf
golem::add_fct(name = "preprocess_beruf", module = NULL,with_test = TRUE)
golem::add_fct(name = "plots_beruf", module = NULL,with_test = TRUE)
golem::add_fct(name = "valueBoxes_beruf", module = NULL,with_test = TRUE)

# Schule
golem::add_fct(name = "preprocess_schule", module = NULL,with_test = TRUE)
golem::add_fct(name = "plots_schule", module = NULL,with_test = TRUE)
golem::add_fct(name = "valueBoxes_schule", module = NULL,with_test = TRUE)

# Home
golem::add_fct(name = "preprocess_home", module = NULL,with_test = TRUE)
golem::add_fct(name = "plots_home", module = NULL,with_test = TRUE)
golem::add_fct(name = "valueBoxes_home", module = NULL,with_test = TRUE)

# Ausbildung
golem::add_fct(name = "plots_ausbildung", module = NULL,with_test = TRUE)

# International
golem::add_fct(name = "plots_international", module = NULL,with_test = TRUE)


golem::add_utils("ui_body", with_test = FALSE)
golem::add_utils(name = "helpers")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("datalab")
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

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

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
