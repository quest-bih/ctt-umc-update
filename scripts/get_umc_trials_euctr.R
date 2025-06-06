# This script gets EUCTR trial ids and title for a given German sponsor

library(jsonlite)
library(httr)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(vroom)
library(here)
library(readr)

# get data (source: https://github.com/ebmdatalab/euctr-tracker-data)
all_trials <- read_json("https://raw.githubusercontent.com/ebmdatalab/euctr-tracker-data/master/all_trials.json")

# get lookup table with German UMC and relevant name(s) in EUTT
DE_UMCs <- vroom::vroom(here("data","umc_search_terms", "umc_sponsors_euctr.csv")) |>
  mutate(sponsor = str_split(sponsor, "; ")) |>
  unnest(sponsor)

# create tibble and extract information from all_trials
trials_tidy <- tibble(id = map_chr(all_trials, pluck("trial_id")),
                      sponsor = map_chr(all_trials, pluck("normalized_name")),
                      title = map_chr(all_trials, pluck("trial_title")),
                      results_due_euctr = map_lgl(all_trials, pluck("results_expected")),
                      has_results_euctr = map_lgl(all_trials, pluck("has_results")))


# get title and id for EUCTR registered trial with German sponsor
euctr_trials_german_umc <- trials_tidy |>
  filter(sponsor %in% DE_UMCs$sponsor) |>
  left_join(DE_UMCs, by = "sponsor") |>
  mutate(title_len = str_length(title)) # title cuts off at 200 and an ellipsis is added if title too long

#write_csv(euctr_trials_german_umc, here("data", "processed", "umc_trials_euctr.csv"))
