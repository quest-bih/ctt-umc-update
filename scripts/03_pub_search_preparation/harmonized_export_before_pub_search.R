library(tidyverse)
library(progressr)
library(furrr)
library(here)
library(janitor)
# library(skimr)
# library(flextable)
# library(flowchart)
# library(ggVennDiagram)

source(here("scripts", "utils.R"))


plan(multisession)
handlers(global = TRUE)

unfiltered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_unfiltered.csv"))
filtered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_filtered.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv")) 
euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))
clean_pub_search_results <- read_csv(here("data", "processed", "results_clean_2026-04-09.csv"))

  # read_csv(here("data", "processed", "EUCTR_sample.csv"))

# # EUCTR inclusion and exclusion criteria
euctr_inex <- read_csv(here("data", "processed", "inclusion_exclusion_euctr.csv"))
euctr_inex_deduped <- euctr_inex |>
  group_by(trial_id) |>
  mutate(is_trial_de_protocol = str_detect(eudract_number_with_country, "DE")) |>
  arrange(desc(is_trial_de_protocol)) |>
  summarise(across(everything(), first)) |> # this takes first also for is_trial_de_protocol == FALSE
  select(-eudract_number_with_country, -is_trial_de_protocol, has_euctr_results = results_reporting) |>
  ungroup() |>
  mutate(completion_date = ymd(completion_date),
         estimated_completion_date = ymd(estimated_completion_date))


# # DRKS inclusion and exclusion criteria
drks_inex <- read_csv(here("data", "processed", "inclusion_exclusion_drks.csv")) |>
  mutate(completion_date = ymd(completion_date))
# # Ct.gov inclusion and exclusion criteria
ctgov_inex <- read_csv(here("data", "processed", "inclusion_exclusion_ctgov.csv")) 

combined_inex <- bind_rows(euctr_inex_deduped, drks_inex, ctgov_inex)
  
combined_inex <- combined_inex |> 
  mutate(is_withdrawn = status == "WITHDRAWN",
         is_crossreg = trial_id %in% unfiltered_crossreg_ids$trial_id,
         registry = get_registry_name(trial_id),
         has_german_umc = case_when(
           is_crossreg == FALSE ~ is_german_umc,
           .default = FALSE
         ),
         has_completion_2018_2021 = case_when(
           is_crossreg == FALSE ~ is_completed_2018_2021,
           .default = FALSE
         ),
         has_interventional = is_interventional,
         has_withdrawn_status = is_withdrawn)


combined_inex_plus_crossreg <- combined_inex |> 
  mutate(crossreg_id = trial_id,
         has_premature = NA,
         hierarchical_completion_date = NA_Date_,
         last_updated_actual = NA_Date_,
         only_estimated = NA,
         is_last_updated = NA,
         n_last_updated = NA_integer_,
         is_latest_actual = NA,
         is_latest_estimated = NA,
         n_latest_estimated = NA_integer_,
         is_index_reg = FALSE,
         n_index_reg = 0,
         many_to_many = FALSE,
         trns_in_cluster = 1,
         mtm_validated = FALSE) |> 
  rows_upsert(falling_clusters, by = "trial_id")


combined_data_filtered <- combined_inex_plus_crossreg |> 
  filter(has_german_umc, has_completion_2018_2021, has_interventional,
         !has_withdrawn_status) |> 
  mutate(is_index_reg = if_else(is_crossreg == FALSE, TRUE, is_index_reg)) |> 
  select(trial_id, crossreg_id, everything())

combined_data_filtered |> 
  write_csv(here("data", "processed", "harmonized_data_filtered.csv"))

distinct(combined_data_filtered, crossreg_id) |> 
  nrow()


qa_ctgov_estimated <- combined_data_filtered |> 
  filter(is_index_reg | is_crossreg == FALSE,
         registry == "ClinicalTrials.gov",
         completion_date_type == "ESTIMATED")

qa_ctgov_estimated |> 
  count(is_crossreg)

# which trns to exclude to simplify cross-reg clusters
combined_data_filtered |> 
  get_dupes(crossreg_id) |> 
  distinct(crossreg_id) |> 
  nrow()
# n unique trials within cross-reg + non-crossreg trials =
# n total unique trials 
329 + 1499 


# missing_in_extractions <- combined_data_filtered |> 
#   filter(!trial_id %in% extractions_filtered_exclusions$trial_id)
