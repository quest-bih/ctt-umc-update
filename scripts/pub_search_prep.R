library(tidyverse)
library(ctregistries)
library(progressr)
library(furrr)

plan(multisession)
handlers(global = TRUE)

validated_crossreg_ids <- read_csv(here("data", "processed", "crossreg_ids.csv"))
euctr_export <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv"))


sample_ids <- c(euctr_export$eudract_number, drks_export$drksId, ctgov_export$nct_id) |> 
  unique()

### double-check linked_ids

pub_search_table_crossreg <- validated_crossreg_ids |> 
  # rowwise() |> 
  mutate(registry_url = get_registry_url(trial_id),
         registry = get_registry_name(trial_id),
         noselfref = str_remove(crossreg_id, trial_id), 
         crossreg_euctr = which_trns(noselfref, registry = "EudraCT") ,
         crossreg_drks = which_trns(noselfref, registry = "DRKS"),
         crossreg_ctgov = which_trns(noselfref, registry = "ClinicalTrials.gov"),
         many_to_many = is_mtm(crossreg_id),
         trial_id_meets_inclusion = trial_id %in% sample_ids) |>
  select(-noselfref)


pub_search_table_euctr <- euctr_export |> 
  select(trial_id = eudract_number) |> 
  filter(!trial_id %in% pub_search_table_crossreg$trial_id)

pub_search_table_drks <- drks_export |> 
  select(trial_id = drksId) |> 
  filter(!trial_id %in% pub_search_table_crossreg$trial_id)

pub_search_table_ctgov <- ctgov_export |> 
  select(trial_id = nct_id) |> 
  filter(!trial_id %in% pub_search_table_crossreg$trial_id)

pub_search_table <- pub_search_table_euctr |> 
  bind_rows(pub_search_table_drks) |> 
  bind_rows(pub_search_table_ctgov) |> 
  mutate(registry_url = get_registry_url(trial_id),
         registry = get_registry_name(trial_id),
         crossreg_id = NA,
         crossreg_euctr = NA,
         crossreg_drks = NA,
         crossreg_ctgov = NA,
         many_to_many = FALSE,
         trial_id_meets_inclusion = trial_id %in% sample_ids) |> 
  bind_rows(pub_search_table_crossreg)


