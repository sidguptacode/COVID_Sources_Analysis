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

####  Split the dataset into groups with and without information about the source of infection. ####
cases_no_info <-
  covid_cases_clean |>
  filter(source_of_infection == 'No Information')

cases_with_info <-
  covid_cases_clean |>
  filter(source_of_infection != 'No Information')

covid_cases_all_sources <- get_col_proportions(covid_cases_clean, "source_of_infection")
covid_cases_fields <- data.frame(field = names(covid_cases))