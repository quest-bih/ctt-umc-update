library(tidyverse)
library(here)
library(yaml)
library(janitor)
library(furrr)
library(progressr)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

source(here("scripts", "utils.R"))
# the complete AACT was downloaded with the timestamp given and saved in the data/raw folder

AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_260606")

AACT_datasets <- load_AACT_datasets(AACT_folder, "studies")
updated_sumres_ctgov <- AACT_datasets$studies |> 
  mutate(is_sumres = if_else(
    !is.na(results_first_submitted_date),
    TRUE,
    FALSE)) |> 
  select(trial_id = nct_id, is_sumres,
         summary_results_date = results_first_submitted_date) |> 
  filter(is_sumres == TRUE)

#----------------------------------------------------------------------------------------------------------------------
# Loading of EUCTR dataset
#----------------------------------------------------------------------------------------------------------------------

updated_sumres_euctr <- read_csv(here("data", "raw", "euctr_data_quality_results_scrape_jun_2026.csv")) |> 
  select(-`...1`) |> 
  rename_with(\(x) paste0("results_", x), !starts_with(c("eudract", "results_", "trial_"))) |> 
  mutate(is_sumres = results_type %in% c("Tabular", "Mixed", "Document"),
         is_sumres = replace_na(is_sumres, FALSE)) |> 
  select(trial_id, is_sumres, summary_results_date = results_first_version_date) |> 
  filter(is_sumres == TRUE)

updated_sumres_2026 <- bind_rows(updated_sumres_ctgov, updated_sumres_euctr)

updated_sumres_2026 |> 
  filter(!is.na(summary_results_date)) |> 
  write_csv(here("data", "processed", "updated_sumres_2026.csv"))
