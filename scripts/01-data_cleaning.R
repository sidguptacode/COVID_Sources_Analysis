#### Preamble ####
# Purpose: Download and clean survey data that records COVID-19 cases in Toronto, and neighbourhood profiles in Toronto.
#           This cleaned dataset will be processed in ../outputs/paper/unpacking_covid_sources.Rmd
# Author: Sidharth Gupta
# Data: 31 January 2022
# Contact: sid.gupta@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - RStudio, or an equivalent environment to compile and execute .R and .Rmd files.
# - The following libraries are installed:
# -- knitr
# -- lubridate
# -- opendatatoronto
# -- tidyverse
# -- tidyr

#### Workspace setup ####
# Use R Projects, not setwd().
library(tidyverse)
library(opendatatoronto)
library(knitr)
library(lubridate)
library(tidyr)
library(janitor)
library(dplyr)
#### Data download ####
# Toronto COVID-19 cases dataset is from: https://open.toronto.ca/dataset/covid-19-cases-in-toronto/
# Toronto neighbourhood profiles dataset is from: https://open.toronto.ca/dataset/neighbourhood-profiles/

# First we download the COVID data.
# Datasets are grouped into 'packages' that have multiple datasets
# also called 'resources' that are relevant to that topic. So we first look at the package
# using a unique key that we obtain from the datasets webpage (see above).
covid_package <- show_package("64b54586-6180-4485-83eb-81e8fae3b8fe")
# get all resources for our packages
covid_resources <- list_package_resources("64b54586-6180-4485-83eb-81e8fae3b8fe")
# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
covid_datastore_resources <- filter(covid_resources, tolower(format) %in% c('csv', 'geojson'))
# We need the unique key from a list of resources
# There is only one resource and so get_resource() will load that.
# If there is more than one resource then need to either filter or specify
covid_cases <-
  covid_datastore_resources %>%
  get_resource
write_csv(covid_cases, here::here("inputs/data/covid_cases.csv"))

# Now we download the neighbourhood data, using the same process.
neighb_package <- show_package("6e19a90f-971c-46b3-852c-0c48c436d1fc")
neighb_resources <- list_package_resources("6e19a90f-971c-46b3-852c-0c48c436d1fc")
neighb_datastore_resources <- filter(neighb_resources, tolower(format) %in% c('csv', 'geojson'))
neighb_profiles <-
  neighb_datastore_resources[0:1,] %>%
  get_resource
write_csv(neighb_profiles, here::here("inputs/data/neighb_profiles.csv"))

#### Clean the COVID and neighborhood datasets. ####
covid_cases <- read_csv(here::here("inputs/data/covid_cases.csv"))
neighb_profiles <- read_csv(here::here("inputs/data/neighb_profiles.csv"))
# Clean the COVID dataset and convert the dates to R datetime objects that we can bin and compare.
covid_cases_clean <-
  covid_cases |>
  clean_names() |>
  mutate(reported_date = as_date(episode_date)) |>
  mutate(reported_date = format(reported_date, "%Y-%m"))
# Clean the neighborhood dataset and filter only the income index records for ages between (18, 64).
# Those are the only records relevant to this study.
income_data <-
  neighb_profiles |>
  clean_names() |>
  filter(topic == "Low income in 2015") |>
  filter(characteristic == "18 to 64 years (%)")
income_data

####  Split the dataset into groups with and without information about the source of infection. ####
cases_no_info <-
  covid_cases_clean |>
  filter(source_of_infection == 'No Information')

cases_with_info <-
  covid_cases_clean |>
  filter(source_of_infection != 'No Information')

         