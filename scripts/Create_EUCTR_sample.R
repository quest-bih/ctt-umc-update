library(tidyverse)
library(here)
library(ctregistries)
library(furrr)
library(janitor)

source(here("scripts", "utils.R"))
# regexes <- yaml::read_yaml(here("inst", "extdata", "keywords_patterns.yaml"))
regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

# dedupe trial tracker data
euctr_umc <- read_csv(here("data", "processed", "umc_trials_euctr.csv")) |> 
  rename(eudract_number = id) |> 
  group_by(eudract_number) |> 
  summarise(across(everything(), \(x) unique(x) |> paste0(collapse = ";")))


euctr_tib <- read_csv(here("data", "raw", "euctr_euctr_dump-2024-09-07-092059.csv")) |> 
  select(eudract_number, everything())
euctr_results <- read_csv(here("data", "raw", "euctr_data_quality_results_scrape_sept_2024.csv")) |> 
  select(-`...1`) |> 
  rename(eudract_number = trial_id,
         actual_enrollment = global_subjects,
         last_updated = this_version_date) |> 
  rename_with(\(x) paste0("results_", x), !starts_with("eudract"))

euctr_combined <- euctr_tib |> 
  full_join(euctr_results, by = "eudract_number") |> 
  left_join(euctr_umc, by = "eudract_number")

#one_off <- euctr_results |> 
#  filter(!eudract_number %in% euctr_tib$eudract_number) ?

euctr_combined |> 
  saveRDS(here("data", "raw", "euctr_combined.rds"))

euctr_2018_2021 <- euctr_combined |> 
  mutate(completion_date = case_when(
    !is.na(results_global_end_of_trial_date) ~ results_global_end_of_trial_date,
    !is.na(date_of_the_global_end_of_the_trial) &
      str_detect(eudract_number_with_country, "DE") ~ date_of_the_global_end_of_the_trial,
    .default = NA
  )) |> 
  select(contains("eudract_number"), completion_date, contains("global"),
         everything()) |> 
  filter(between(completion_date, as_date("2018-01-01"), as_date("2021-12-31")))

