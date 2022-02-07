---
  title: "Toronto homeless shelter usage"
author: "Sid Gupta"
date: '2022-01-12'
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
#install.packages("opendatatoronto")
library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
library(tidyr)

```

```{r}
set.seed(853)
rcauchy(n = 10,
        location=5,
        scale=1)

simulate_data <-
  tibble(
    date = rep(x = as.Date("2021-07-01") + c(0:183), times=3),
    shelter = c(rep(x="Shelter 1", times=184),
                rep(x="Shelter 2", times=184),
                rep(x="Shelter 3", times=184)),
    number_occupied =
      rpois(n=184*3,
            lambda=150)
  )

```


```{r}
toronto_shelters <- 
  # Each package is associated with a unique id which can be found in 'For Developers':
  # https://open.toronto.ca/dataset/daily-shelter-overnight-service-occupancy-capacity/
  list_package_resources("21c83b32-d5a8-4106-a54f-010dbe49f6f2") |> 
  filter(name == "daily-shelter-overnight-service-occupancy-capacity-2021") |> 
  get_resource()

write_csv(
  x = toronto_shelters, 
  file = "toronto_shelters.csv"
)

head(toronto_shelters)
```

```{r}
toronto_shelters_clean <- 
  toronto_shelters |> 
  clean_names() |>
  select(occupancy_date, id, occupied_beds) |> 
  mutate(occupancy_date = as_date(occupancy_date)) |>
  filter(occupancy_date >= as_date("2021-07-01"))
head(toronto_shelters_clean)
```


```{r}
write_csv(
  x = toronto_shelters_clean, 
  file = "cleaned_toronto_shelters.csv"
)

```

```{r}
#### Explore ####
toronto_shelters_clean <- 
  read_csv(
    "cleaned_toronto_shelters.csv",
    show_col_types = FALSE
  )
```


```{r}
toronto_shelters_clean |>
  mutate(occupancy_month = month(occupancy_date, 
                                 label = TRUE, 
                                 abbr = FALSE)) |>
  drop_na(occupied_beds) |> # We only want rows that have data
  group_by(occupancy_month) |> # We want to know the occupancy by month
  summarize(number_occupied = mean(occupied_beds)) |>
  kable()
```


```{r}
toronto_shelters_clean |>
  mutate(occupancy_month = month(occupancy_date, 
                                 label = TRUE, 
                                 abbr = FALSE)) |>
  drop_na(occupied_beds) |> # We only want rows that have data
  group_by(occupancy_month) |> # We want to know the occupancy by month
  summarize(number_occupied = mean(occupied_beds)) |> 
  kable(caption = "Homeless shelter usage in Toronto in 2021", 
        col.names = c("Month", "Average daily number of occupied beds"),
        digits = 1,
        booktabs = TRUE,
        linesep = ""
  )
```


