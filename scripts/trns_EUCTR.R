library(tidyverse)
library(janitor)
library(progressr)
library(furrr)
library(here)

euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))
source(here("scripts", "utils.R"))

AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_250513")

ctgov_id_info <- file.path(AACT_folder, "id_information.txt") |> 
  read_delim(delim = "|")
ctgov_aliases <- read_csv(here("data", "processed", "ctgov_aliases.csv"))

drks_ids <- read_csv(here("data", "raw", "drks_ids.csv")) |> 
  pull(drksId)

regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

euctr_trns <- euctr_combined |> 
  select(contains("eudract_number"), contains("isrctn"),
         contains("nct_"), contains("who\\b"), other_identifiers, results_other_ids) |> 
  rename(isrctn_number = isrctn_international_standard_randomised_controlled_trial_numbe,
         nct_number = us_nct_clinicaltrials_gov_registry_number,
         other_ids = other_identifiers)

# dupes <- euctr_trns |> 
#   get_dupes(meta_id)

plan(multisession)
#plan(sequential)
handlers(global = TRUE)

na_regexes <- c("\\b[Nn]\\W?[Aa]\\b",
                "00000000",
                "99999999",
                "12345678",
                "^[\\W\\s]+$",
                "\\: -") |> 
  paste0(collapse = "|")
# examples <- c("NA", "na", "NA: ", "China", "N/A", "n/a", "n.a.", "NCT00000000",
#               "ISRCTN00000000", "-", "Name: - Number: -")
# str_view(examples, na_regexes)

extract_valid <- function(trn_vec, valid_regex, na_regex) {
  dplyr::case_when(
    stringr::str_detect(trn_vec, na_regex) ~ NA_character_,
    .default = stringr::str_extract(trn_vec, valid_regex)
  )
}

euctr_trns_clean <- euctr_trns |> 
  mutate(across(everything(), \(x) if_else(str_detect(x, na_regexes), NA_character_, x))) |> 
  filter(!if_all(-contains("eudract"), is.na))  |> # Remove fully NA rows (if any)
  mutate(ctgov_clean = extract_valid(nct_number, regexes$ClinicalTrials.gov, na_regexes),
         ctgov_clean_results = extract_valid(results_nct_number, regexes$ClinicalTrials.gov, na_regexes),
         other_clean = which_trns(other_ids),
         other_clean_results = which_trns(results_other_ids)
         )

euctr_trns_clean_registries <- euctr_trns_clean |> 
  mutate(ctgov_clean_other = which_trns(other_clean, registry = "ClinicalTrials.gov"),
         ctgov_clean_other_results = which_trns(other_clean_results, registry = "ClinicalTrials.gov"),
         drks_clean = which_trns(other_clean, registry = "DRKS"),
         drks_clean_results = which_trns(other_clean_results, registry = "DRKS"),
         euctr_clean = which_trns(other_clean, registry = "EudraCT"),
         euctr_clean_results = which_trns(other_clean_results, registry = "EudraCT"))


euctr_trns_updated <- euctr_trns_clean_registries |> 
  mutate(euctr_clean = case_when(
    str_detect(euctr_clean, eudract_number) ~ str_remove(euctr_clean, paste0(";?", eudract_number)) |>
      na_if(""),
    .default = euctr_clean
  ),
  euctr_clean_results = case_when(
    str_detect(euctr_clean_results, eudract_number) ~ str_remove(euctr_clean_results, paste0(";?", eudract_number)) |>
      na_if(""),
    .default = euctr_clean_results
  )) |>
  mutate(across(contains("ctgov_"), \(x) future_map_chr(x, \(y) update_ctgov_alias(y, id_aliases))))
  

euctr_trns_updated <- euctr_trns_updated |> 
  mutate(ctgov_exists = ctgov_clean %in% ctgov_id_info$nct_id,
         ctgov_results_exists = ctgov_clean_results %in% ctgov_id_info$nct_id,
         ctgov_other_exists = ctgov_clean_other %in% ctgov_id_info$nct_id,
         ctgov_other_results_exists = case_when(
           ctgov_clean_other_results %in% ctgov_id_info$nct_id ~ TRUE,
           str_detect(ctgov_clean_other_results, ";") ~ TRUE, # many-to-many not pruned
           .default = FALSE
         ),
         drks_exists = drks_clean %in% drks_ids,
         drks_results_exists = drks_clean_results %in% drks_ids,
         euctr_exists = case_when(
           euctr_clean %in% euctr_trns$eudract_number ~ TRUE,
           str_detect(euctr_clean, ";") ~ TRUE, # many-to-many not pruned
          .default = FALSE
         ),
         #2017-003426-17 is an older version of (alias for) 2017-003426-32
         euctr_results_exists = case_when(
           euctr_clean_results %in% euctr_trns$eudract_number ~ TRUE,
           str_detect(euctr_clean_results, ";") ~ TRUE, # many-to-many not pruned
           .default = FALSE
         )) |> 
  mutate(ctgov_clean = ifelse(ctgov_exists, ctgov_clean, NA_character_),
         ctgov_clean_results = ifelse(ctgov_results_exists, ctgov_clean_results, NA_character_),
         ctgov_clean_other = ifelse(ctgov_other_exists, ctgov_clean_other, NA_character_),
         ctgov_clean_other_results = ifelse(ctgov_other_results_exists, ctgov_clean_other_results,
                                            NA_character_),
         drks_clean = ifelse(drks_exists, drks_clean, NA_character_),
         drks_clean_results = ifelse(drks_results_exists, drks_clean_results, NA_character_),
         euctr_clean = ifelse(euctr_exists, na_if(euctr_clean, eudract_number), NA_character_),
         euctr_clean_results = ifelse(euctr_results_exists,
                                      na_if(euctr_clean_results, eudract_number), NA_character_))

euctr_trns_clean_all <- euctr_trns_updated |> 
  group_by(eudract_number) |> 
  summarise(ctgov_all = deduplicate_collapsed(
    c(ctgov_clean, ctgov_clean_results, ctgov_clean_other, ctgov_clean_other_results)),
            drks_all = deduplicate_collapsed(c(drks_clean, drks_clean_results)),
            euctr_all = deduplicate_collapsed(c(euctr_clean, euctr_clean_results))
         ) |>
  mutate(n_ctgov = str_count(ctgov_all, ";") + 1 * as.numeric(ctgov_all != "") |> as.numeric(),
         n_drks = str_count(drks_all, ";") + 1 * as.numeric(drks_all != "") |> as.numeric(),
         n_euctr = str_count(euctr_all, ";") + 1 * as.numeric(euctr_all != "") |> as.numeric(),
         across(where(is.numeric), \(x) replace_na(x, 0)))

# quality check on many-to-many relationships (multiple TRNs in results either for EUCTR or CT.gov)

qa_mtm <- euctr_trns_clean_all |> 
  filter(n_ctgov > 1 | n_drks > 1 | n_euctr > 1) |> 
  select(contains("eudract"), contains("_all"), contains("other_ids"))

# is protocol_sponsor_code unique for countries? does it matter if collapsed to 1?

qa_trns <- euctr_trns_clean_all |>  
  filter(n_ctgov > 0 | n_drks > 0 | n_euctr > 0) |>  
  pull(eudract_number)

# check if some euctr stragglers were missed:
# all are self-references, aliases. or false positives, so no
#2017-003426-17 is an older version of (alias for) 2017-003426-32

qa_euctr_stragglers <- euctr_trns |> 
  filter(!eudract_number %in% qa_trns,
         
         str_detect(other_ids, paste(c(regexes$ClinicalTrials.gov,
                                       regexes$DRKS,
                                       regexes$EudraCT), collapse = "|")),
         !str_detect(other_ids, stringr::fixed(eudract_number)))

euctr_trns_export <- euctr_trns_clean_all |>
  select(contains("eudract_number"), contains("_all"), n_ctgov, n_drks, n_euctr)

euctr_trns_export |>
  write_excel_csv(here("data", "processed", "euctr_ids.csv"))
# 
# euctr_ids <- read_csv((here("data", "processed", "euctr_ids.csv")))

crossreg_euctr <- euctr_trns_export |>  
  filter(n_ctgov > 0 | n_drks > 0 | n_euctr > 0) |> 
  mutate(triad = n_ctgov > 0 & n_drks > 0,
         many_to_many = n_ctgov > 1 | n_drks > 1 | n_euctr > 0) |> 
  pivot_longer(cols = contains("_all"), values_to = "linked_id") |> 
  filter(linked_id != "") |> 
  select(trial_id = eudract_number, linked_id, triad, many_to_many) |> 
  mutate(linked_id = na_if(linked_id, ""), via_id = TRUE, bidirectional = NA) |> 
  unite("binary_id", c(trial_id, linked_id), na.rm = TRUE, remove = FALSE)

crossreg_euctr |> 
  write_excel_csv(here("data", "processed", "crossreg_euctr.csv"))

