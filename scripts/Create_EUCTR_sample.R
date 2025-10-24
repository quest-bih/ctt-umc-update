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
  mutate(umc = ifelse(eudract_number == "2009-012198-36", "Giessen", umc)) |> 
  ungroup()

euctr_tib <- read_csv(here("data", "raw", "euctr_euctr_dump-2025-07-05-072411.csv")) |> 
  select(eudract_number, everything())
euctr_results <- read_csv(here("data", "raw", "euctr_data_quality_results_scrape_jul_2025.csv")) |> 
  select(-`...1`) |> 
  rename(eudract_number = trial_id,
         actual_enrollment = global_subjects,
         last_updated = this_version_date) |> 
  rename_with(\(x) paste0("results_", x), !starts_with(c("eudract", "results_"))) |> 
  mutate(results_reporting = results_type %in% c("Tabular", "Mixed", "Document"))

euctr_combined <- euctr_tib |> 
  full_join(euctr_results, by = "eudract_number") |> 
  left_join(euctr_umc, by = "eudract_number") |> 
  group_by(eudract_number) |> 
  mutate(completion_date = case_when(
    !is.na(results_global_end_of_trial_date) ~ results_global_end_of_trial_date,
    !is.na(date_of_the_global_end_of_the_trial) &
      str_detect(eudract_number_with_country, "DE") ~ date_of_the_global_end_of_the_trial,
    !is.infinite(max(date_of_the_global_end_of_the_trial, na.rm = TRUE)) ~ max(date_of_the_global_end_of_the_trial, na.rm = TRUE),  
    .default = max(date_of_the_global_end_of_the_trial)
        ),
    has_trial_de_protocol = any(str_detect(eudract_number_with_country, "DE"), na.rm = TRUE),
    umc_validated = !is.na(umc)) |> 
  ungroup() |> 
  select(contains("eudract_number"), contains("global"), umc, has_trial_de_protocol,
         umc_validated,
         everything()) 

#one_off <- euctr_results |> 
#  filter(!eudract_number %in% euctr_tib$eudract_number) ?

euctr_combined |> 
  saveRDS(here("data", "raw", "euctr_combined.rds"))

euctr_inex <- euctr_combined |> 
  mutate(is_interventional = TRUE,
         is_completed_2018_2021 = between(as_date(completion_date), as_date("2018-01-01"), as_date("2021-12-31")),
         is_german_umc = !is.na(umc),
         results_reporting = replace_na(results_reporting, FALSE)) |> 
  # filter(str_detect(eudract_number_with_country, "DE")) |> # exclude any without a DE protocol 
  select(trial_id = eudract_number, status = trial_status, last_updated = results_last_updated,
         registration_date = date_on_which_this_record_was_first_entered_in_the_eudract_data,
         # actual_completion_date = results_global_end_of_trial_date,
         results_completion_date = results_global_end_of_trial_date,
         estimated_completion_date = date_of_the_global_end_of_the_trial,
         # protocol_completion_date = date_of_the_global_end_of_the_trial,
         is_interventional, is_completed_2018_2021, is_german_umc, has_trial_de_protocol,
         eudract_number_with_country, results_reporting, completion_date) 

euctr_inex |> 
  write_excel_csv(here("data", "processed", "inclusion_exclusion_euctr.csv"))

euctr_filtered <- euctr_combined |> 
  select(trial_id = eudract_number, everything(), -has_trial_de_protocol,
         -completion_date, -results_reporting) |> 
  right_join(euctr_inex, by = c("trial_id", "eudract_number_with_country")) |> 
  filter(is_interventional == TRUE, is_completed_2018_2021 == TRUE, is_german_umc == TRUE)

# # number of new TRNs from EUCTR
# euctr_combined |> 
#   select(results_global_end_of_trial_date,
#          date_of_the_global_end_of_the_trial,
#          umc, eudract_number, eudract_number_with_country, everything()) |> 
#   # filter(eudract_number == "2016-002673-35")
#   distinct(eudract_number) |> 
#   nrow()

euctr_filtered |> 
  # filter(has_trial_de_protocol == TRUE) |> # here we exclude TRNs without DE protocols
  write_excel_csv(here("data", "processed", "EUCTR_sample.csv"), na = "")


# sanity check results without german protocols
# collapse by trial remove dupes, preferably by german protocol! exclude otherwise, but sanity check
# sequence umc > completion_date > german protocol

# there are 22 TRNs in all of EUCTR with a German UMC but no German protocol
qa_missing_de_protocols <- euctr_inex |> 
  filter(is_german_umc == TRUE,
         has_trial_de_protocol == FALSE) # here we exclude TRNs with DE protocols!

qa_missing_de_protocols |> 
  distinct(trial_id) |> 
  count()

missing_de_protocols <- euctr_combined |> 
  select(trial_id = eudract_number, everything(), -has_trial_de_protocol) |> 
  filter(trial_id %in% qa_missing_de_protocols$trial_id) |> 
  left_join(euctr_inex, by = c("trial_id", "eudract_number_with_country")) |>  
  select(contains("eudract"), umc, is_completed_2018_2021, contains("global"), everything()) |> 
  arrange(desc(trial_id))

missing_de_protocols |> 
  count(is_completed_2018_2021)

missing_de_protocols |> 
  filter(is_completed_2018_2021) |> 
  distinct(trial_id)

missing_de_protocols |> 
  write_excel_csv(here("data", "processed", "euctr_missing_de_protocols.csv"))

############# prepare export with crossreg data and additional transparency practices measures? 
# validated_crossreg_ids <- read_csv(here("data", "processed", "crossreg_ids.csv"))
# euctr_export <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
# 
# euctr_export <- euctr_export |> 
#   rename(trial_id = eudract_number) |> 
#   left_join(validated_crossreg_ids, by = "trial_id") |> 
#   select(trial_id, eudract_number_with_country, umc, completion_date, contains("date"),
#          results_reporting, contains("eutt"))
# 
# euctr_results |> 
#   count(results_actual_enrollment >= 88888)
# qa_enrollment <- euctr_results |> 
#   filter( eudract_number %in% euctr_export$trial_id) 



