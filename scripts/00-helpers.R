#### Preamble ####
# Purpose: A library of R helper functions that are re-used in this study.
# Author: Sidharth Gupta
# Data: 31 January 2022
# Contact: sid.gupta@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - RStudio, or an equivalent environment to compile and execute .R and .Rmd files.
# - The following libraries are installed:
# -- ggmap
# -- dplyr
# -- forcats
# -- knitr
# -- janitor
# -- lubridate
# -- opendatatoronto
# -- tidyverse
# -- tidyr
# -- wrapr

get_col_proportions <- function(table, col) {
  ### Takes in a table and a column, and returns a new table that holds 
  ### the proportion each value takes in that column.
  ###
  ### Arguments and preconditions:
  ###   table: a tibble dataframe
  ###   col: a character string representing a column in `table`.
  table <- 
    table |>
    count(.data[[col]]) |>
    mutate(n = n / sum(n)) |>
    rename(prop = n) |>
    arrange(desc(prop))
  return(table)
}


get_prop_against_total <- function(total_table, sub_table, col, should_sort) {
  ### Takes in two tables and a column. The second table is a subset of the first.
  ### This function counts the values in each table at the provided column, and returns
  ### a new table that divides the counts in the second table by the first
  ###
  ### Example:
  ###     col = B
  ###     total_table     sub_table   
  ###       A | B           A | B
  ###     ... | ø         ... | ø
  ###     ... | ø         ... | †
  ###     ... | † 
  ###     ... | π 
  ###
  ###     with col = B, should_sort = TRUE, this function returns:
  ###       B | prop          
  ###       † | 1.0       
  ###       ø | 0.5      
  ###       π | 0
  ###
  ### Arguments and preconditions:
  ###   total_table: a tibble dataframe
  ###   sub_table: a character string representing a column in `table`
  ###   col: a character string representing a column in `total_table`.
  ###   should_sort: a boolean flag that determines if the results should be sorted in descending order.
  total_col_count <-     
    total_table |>
    count(.data[[col]])
  
  sub_table_prop_of_total <-
    sub_table |>
    count(.data[[col]])
  
  # In some cases, total_col_count has values that do not exist in sub_table_prop_of_total.
  # We want to add those values sub_table_prop_of_total with an n=0.
  # CITATION: These three lines are inspired by: https://stackoverflow.com/questions/38266167/add-missing-rows-to-dataframe-from-other-dataframe-in-r
  sub_res1 <- anti_join(total_col_count,sub_table_prop_of_total,by=c(col))
  sub_res1$n <- 0
  sub_table_prop_of_total <- full_join(sub_table_prop_of_total,sub_res1,by=c(col,"n"))
  
  # Use match_order to make  sure the `col`s are aligned in these two dataframes.
  idx_sub_table <- match_order(sub_table_prop_of_total[[col]], total_col_count[[col]])
  sub_table_prop_of_total <- sub_table_prop_of_total[idx_sub_table,]
 
  # Divide the two dataframes. 
  sub_table_prop_of_total <- cbind(sub_table_prop_of_total[1],round(sub_table_prop_of_total[-1]/total_col_count[-1],3))
  
  if (should_sort) {
    sub_table_prop_of_total <-
      sub_table_prop_of_total |>
      arrange(desc(n))
  }
  sub_table_prop_of_total <-
    sub_table_prop_of_total |>
    rename(prop = n)
  return(sub_table_prop_of_total)
}


single_df_barplot <- function(df, xcol, ycol, title, xlab, ylab) {
    ###   Intakes a dataframe with two columns representing x, y values, and plots a bar plot.
    ###   Arguments and preconditions:
    ###   df: a tibble dataframe
    ###   xcol: a character string denoting the column for the x-values in the df
    ###   ycol:  a character string denoting the column for the y-values in the df
    ###   title: a character string denoting the title of the plot
    ###   xlab:  a character string denoting the x-label of the plot
    ###   ylab:  a character string denoting the y-label of the plot
    ###   xcol and ycol must be columns in df.
    barplot <- ggplot(data=df, aes(.data[[xcol]])) +
      geom_bar(aes(y=df[[ycol]]), stat="identity", position ="identity", alpha=.5, fill='lightblue', color='lightblue4') + 
      ggtitle(title) +
      xlab(xlab) + 
      ylab(ylab) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    return(barplot)
}


single_df_table <- function(df, xcol, ycol, title) {
  ###   Intakes a dataframe with two columns representing x, y values, and creates a table.
  ###   Arguments and preconditions:
  ###   df: a tibble dataframe
  ###   xcol: a character string denoting the column for the x-values in the df
  ###   ycol:  a character string denoting the column for the y-values in the df
  ###   title: a character string denoting the title of the table
  table <-
    kable(df,
    booktabs=TRUE,
    caption=title,
    col.names = c(xcol, ycol),
    digits=2
    )
  return(table)
}



sort_df_by_date <- function(df, col) {
  ###   Sorts a dataframe by a (year, month) date column.
  ###   Arguments and preconditions:
  ###   table: a tibble dataframe
  ###   col: a character string representing a column in `table`.
  ###   col must denote a (year, month) date column.
  sorted_df_by_date <- df
  # To make the '(year, month)' date objects, we just let the day = 1.
  sorted_df_by_date[[col]] = as_date(paste(df[[col]],"-01",sep=""))
  sorted_df_by_date <-
    sorted_df_by_date |>
    # This sorts the dataframe by the date.
    arrange(reported_date)
  # Now that the dataframe is sorted, we can remove the 'day' information.
  sorted_df_by_date[[col]] = format(sorted_df_by_date[[col]], "%Y-%m")
  return(sorted_df_by_date)
}


normalize_wrt_other_df <- function(df1, ycol1, df2, ycol2) {
  ###   Intakes two dataframes, and normalizes the first one based off of the min/max of the second.
  ###   Arguments and preconditions:
  ###   df1: a tibble dataframe
  ###   ycol1: a character string denoting the column for the y-values in the df1
  ###   df1: a tibble dataframe
  ###   ycol1: a character string denoting the column for the y-values in the df2
  df1_normalized <- df1
  df2_min = min(df2[[ycol2]])
  df2_max = max(df2[[ycol2]])
  df1_normalized[[ycol1]] = (df1_normalized[[ycol1]] - df2_min) / df2_max
  return(df1_normalized)
}
  
normalize_dfs_and_barplot <- function(df1, ycol1, df2, ycol2, xcol, xlab, ylab,title, should_normalize_df1, should_normalize_df2) {
  ###   Intakes two dataframes, normalizes their values, and plots a stacked-bar-chart.
  ###   Arguments and preconditions:
  ###   df1: a tibble dataframe
  ###   ycol1: a character string denoting the column for the y-values in the df1
  ###   df1: a tibble dataframe
  ###   ycol1: a character string denoting the column for the y-values in the df2
  ###   xcol: a character string representing the column for the x-values in both df1 and df2.
  ###   xcol must be a column in both df1 and df2.
  # First we normalize the y-values in both dataframes
  df1_normalized <- df1
  df2_normalized <- df2
  if (should_normalize_df1) {
    df1_min = min(df1[[ycol1]])
    df1_max = max(df1[[ycol1]])
    df1_normalized[[ycol1]] = (df1_normalized[[ycol1]] - df1_min) / df1_max
  }
  if (should_normalize_df2) {
    df2_min = min(df2[[ycol2]])
    df2_max = max(df2[[ycol2]])
    df2_normalized[[ycol2]] = (df2_normalized[[ycol2]] - df2_min) / df2_max
  }
  
  # Now that both dfs are comparable, we plot a stacked-bar-chart
  ggplot(data=df1_normalized, aes(.data[[xcol]])) +
    geom_bar(aes(y=df1_normalized[[ycol1]]), stat="identity", position ="identity", alpha=.5, fill='lightblue', color='lightblue4') +
    geom_bar(aes(y=df2_normalized[[ycol2]]), stat="identity", position="identity", alpha=.3, fill='pink', color='red') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(title) +
    xlab(xlab) + 
    ylab(ylab)
    ylim(0,1)
}






