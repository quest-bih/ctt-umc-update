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
  select(-sponsor, -contains("title")) |> 
  rename(eudract_number = id) |> 
  group_by(eudract_number) |> 
  summarise(across(everything(), \(x) unique(x) |> paste0(collapse = ";"))) |> 
  mutate(umc = ifelse(eudract_number == "2009-012198-36", "Giessen", umc))


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

euctr_filtered <- euctr_combined |> 
  mutate(completion_date = case_when(
    !is.na(results_global_end_of_trial_date) ~ results_global_end_of_trial_date,
    !is.na(date_of_the_global_end_of_the_trial) &
      str_detect(eudract_number_with_country, "DE") ~ date_of_the_global_end_of_the_trial,
    .default = NA
  )) |> 
  select(contains("eudract_number"), completion_date, contains("global"), umc, 
         everything()) |> 
  filter(!is.na(umc),
    between(completion_date, as_date("2018-01-01"), as_date("2021-12-31"))) |> 
  group_by(eudract_number) |> 
  mutate(trial_de_protocol = any(str_detect(eudract_number_with_country, "DE"), na.rm = TRUE)) |> 
  ungroup()

# number of new TRNs from EUCTR
euctr_combined |> 
  select(results_global_end_of_trial_date,
         date_of_the_global_end_of_the_trial,
         umc, eudract_number, eudract_number_with_country, everything()) |> 
  # filter(eudract_number == "2016-002673-35")
  distinct(eudract_number) |> 
  nrow()

write_excel_csv(euctr_filtered, here("data", "processed", "EUCTR_sample.csv"), na = "")

# sanity check results without german protocols
# collapse by trial remove dupes, preferably by german protocol! exclude otherwise, but sanity check
# sequence umc > completion_date > german protocol

# there are 22 TRNs in all of EUCTR with a German UMC but no German protocol
qa_missing_de_protocols <- euctr_combined |> 
  filter(!is.na(umc)) |> 
  group_by(eudract_number, umc) |> 
  summarise(n_german_protocol = str_detect(eudract_number_with_country, "DE") |> sum(),
            ) |> 
  filter(n_german_protocol < 1)

missing_de_protocols <- euctr_combined |> 
  filter(eudract_number %in% qa_missing_de_protocols$eudract_number) |> 
  rowwise() |> 
  mutate(in_sampling_period = any(between(results_global_end_of_trial_date, as_date("2018-01-01"), as_date("2021-12-31")),
    between(date_of_the_global_end_of_the_trial, as_date("2018-01-01"), as_date("2021-12-31")), na.rm = TRUE)) |> 
  ungroup() |> 
  select(contains("eudract"), umc, in_sampling_period, contains("global"), everything()) |> 
  arrange(desc(eudract_number))


missing_de_protocols |> 
  write_excel_csv(here("data", "processed", "euctr_missing_de_protocols.csv"))


### add these with a flag for trial_id and linked_id_with_de_protocol, etc.
