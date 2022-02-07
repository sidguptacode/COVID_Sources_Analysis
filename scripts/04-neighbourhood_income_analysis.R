neighborhood_props <- get_prop_against_total(covid_cases_clean, cases_no_info, 'neighbourhood_name', TRUE)
# Fixing some formatting errors with clean_names()
neighborhood_props <-
  neighborhood_props |>
  drop_na() |>
  mutate(neighbourhood_name = tolower(str_replace_all(neighbourhood_name, " ", "_"))) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "-", "_")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "\\.", "_")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "'", "")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "__", "_")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "lamoreaux", "l_amoreaux")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "wexford/maryvale", "wexford_maryvale")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "tam_oshanter_sullivan", "tam_o_shanter_sullivan")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "oconnor", "o_connor")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "mimico_(includes_humber_bay_shores)", "mimico_includes_humber_bay_shores")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "briar_hill__belgravia", "briar_hill_belgravia")) |>
  mutate(neighbourhood_name = str_replace_all(neighbourhood_name, "weston_pellam_park", "weston_pelham_park"))


neighborhood_props_names <- neighborhood_props$neighbourhood_name
neighborhood_props_names <- neighborhood_props_names[neighborhood_props_names != "mimico_(includes_humber_bay_shores)"]
top_noinfo_neighbs <- top_n(neighborhood_props, 30)
top_noinfo_neighbs_names <- top_noinfo_neighbs$neighbourhood_name
botton_noinfo_neighbs <- top_n(neighborhood_props, -30)
botton_noinfo_neighbs_names <-botton_noinfo_neighbs$neighbourhood_name

top_noinfo_neighbs <- normalize_wrt_other_df(top_noinfo_neighbs, "prop", neighborhood_props, "prop")
bottom_noinfo_neighbs <- normalize_wrt_other_df(botton_noinfo_neighbs, "prop", neighborhood_props, "prop")

income_data_of_allneighbs <-
  income_data[, neighborhood_props_names] |>
  mutate_all(function(x) as.numeric(as.character(x))) |>
  colMeans() |>
  as.data.frame.list()
income_values_of_allneighbs <- unlist(income_data_of_allneighbs[1,], use.names = FALSE)
income_data_of_allneighbs_df <- 
  data.frame(neighbourhood_name = c(neighborhood_props_names), income_index=(income_values_of_allneighbs))

income_data_of_topneighbs_df <-
  income_data_of_allneighbs_df |>
  filter(neighbourhood_name %in% top_noinfo_neighbs_names)
income_data_of_topneighbs_df <- normalize_wrt_other_df(income_data_of_topneighbs_df, "income_index", income_data_of_allneighbs_df, "income_index")

income_data_of_botneighbs_df <-
  income_data_of_allneighbs_df |>
  filter(neighbourhood_name %in% botton_noinfo_neighbs_names)
income_data_of_botneighbs_df <- normalize_wrt_other_df(income_data_of_botneighbs_df, "income_index", income_data_of_allneighbs_df, "income_index")