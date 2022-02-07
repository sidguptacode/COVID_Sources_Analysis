reported_data_noinfo_prop <- get_prop_against_total(covid_cases_clean, cases_no_info, 'reported_date', TRUE)

counts_for_reported_date <-
  covid_cases_clean |>
  count(reported_date)

counts_for_reported_date <- sort_df_by_date(counts_for_reported_date, "reported_date")
reported_data_noinfo_prop_sorted <- sort_df_by_date(reported_data_noinfo_prop, "reported_date")

# Remove data from the month of February
counts_for_reported_date <- head(counts_for_reported_date,-1)
reported_data_noinfo_prop_sorted <- head(reported_data_noinfo_prop_sorted,-1)

reported_data_noinfo_prop_sorted
counts_for_reported_date