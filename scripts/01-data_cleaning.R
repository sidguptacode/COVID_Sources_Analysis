#### Preamble ####
# Purpose: Clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander [CHANGE THIS TO YOUR NAME!!!!]
# Data: 3 January 2021
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
#install.packages("opendatatoronto")
#install.packages("haven")
library(tidyverse)
library(opendatatoronto)


#### Data download ####
# From https://open.toronto.ca/dataset/toronto-shelter-system-flow/

# Datasets are grouped into 'packages' that have multiple datasets
# also called 'resources' that are relevant to that topic. So we first look at the package
# using a unique key that we obtain from the datasets webpage (see above).

# get all resources for this package
resources <- list_package_resources("ac77f532-f18b-427c-905c-4ae87ce69c93")

# We need the unique key from that list of resources
# There is only one resource and so get_resource() will load that.
# If there is more than one resource then need to either filter or specify
monthly_shelter_usage <- 
  resources %>% 
  get_resource()


#### Save data ####
write_csv(monthly_shelter_usage, "inputs/data/monthly_shelter_usage.csv")

#### What's next? ####



         