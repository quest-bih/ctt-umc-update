library(tidyverse)
library(janitor)
library(progressr)
library(furrr)
library(here)

euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))
source(here("scripts", "utils.R"))


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

# columns_to_clean <- c("results_isrctn_number", "results_nct_number", "results_other_ids")
# invalid_ncts <- c("NCT00000000", "NCT99999999", "NCT12345678")
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

euctr_trns_clean <- euctr_trns |> 
  # select(all_of(columns_to_clean), everything()) |> 
  mutate(across(everything(), \(x) if_else(str_detect(x, na_regexes), NA_character_, x))) |> 
  filter(!if_all(-contains("eudract"), is.na))  |> # Remove fully NA rows (if any)
  
  
mutate(ctgov_clean = ifelse(nct_number %in% invalid_ncts, NA_character_, nct_number), 
         # remove potential self-references here
         other_clean = other_ids |> str_remove(eudract_number) |> which_trns()) |> 
  rowwise() |> 
  mutate(ctgov_other = str_extract_all(other_clean, regexes$ClinicalTrials.gov) |> 
           unlist() |> 
           paste(collapse = ";") |> 
           na_if("NA") |> 
           na_if(""),
         ctgov_all = deduplicate_collapsed(c(ctgov_clean, ctgov_other)),
         n_ctgov = str_count(ctgov_all, ";") + 1 * as.numeric(ctgov_all != ""),
         drks_clean = str_extract_all(other_clean, regexes$DRKS) |> 
           unlist() |> 
           paste(collapse = ";") |> 
           na_if("NA") |> 
           na_if(""),
         n_drks = str_count(drks_clean, ";") + 1 * as.numeric(drks_clean != ""),
         euctr_clean = str_extract_all(other_clean, regexes$EudraCT)|> 
           unlist() |> 
           paste(collapse = ";") |> 
           na_if("NA") |> 
           na_if(""),
         n_euctr = str_count(euctr_clean, ";") + 1 * as.numeric(euctr_clean != "")) |> 
  ungroup()

qa_results_clean <- euctr_results_trns_clean |> 
  filter(str_detect(other_ids, eudract_number)) |> 
  select(eudract_number, other_ids, other_clean, euctr_clean, everything())

euctr_trns <- euctr_tib |> 
  select(eudract_number,
         protocol_sponsor_code = sponsor_s_protocol_code_number,
         isrctn_number = isrctn_international_standard_randomised_controlled_trial_numbe,
         nct_number = us_nct_clinicaltrials_gov_registry_number,
         who_utn_number = who_universal_trial_reference_number_utrn,
         other_ids = other_identifiers) 

# is protocol_sponsor_code unique for countries? does it matter if collapsed to 1?

euctr_protocols <- euctr_trns |> 
  # select(-protocol_sponsor_code) |> 
  # janitor::get_dupes(eudract_number) |>
  # filter(eudract_number %in% dupe_tests$eudract_number) |> 
  mutate(across(everything(),
                \(x) if_else(str_detect(x, "0000\\W?0000|1234-1234|12345678"),
                             NA_character_, as.character(x)))) |> 
  filter(str_detect(nct_number, regexes$ClinicalTrials.gov) |
           str_detect(other_ids, paste(c(regexes$ClinicalTrials.gov,
                                         regexes$DRKS,
                                         regexes$EudraCT), collapse = "|"))) |> 
  distinct(pick(everything()))

euctr_protocols_trns_clean <- euctr_protocols |> 
  mutate(ctgov_clean = ifelse(nct_number %in% invalid_ncts, NA_character_, nct_number),
         other_clean = other_ids |> str_remove(eudract_number) |> which_trns()) |> 
  rowwise() |> 
  mutate(ctgov_other = str_extract_all(other_clean, regexes$ClinicalTrials.gov) |> 
           unlist() |> 
           paste(collapse = ";") |> 
           na_if("NA") |> 
           na_if(""),
         ctgov_all = deduplicate_collapsed(c(ctgov_clean, ctgov_other)),
         n_ctgov = str_count(ctgov_all, ";") + 1 * as.numeric(ctgov_all != ""),
         drks_clean = str_extract_all(other_clean, regexes$DRKS) |> 
           unlist() |> 
           paste(collapse = ";") |> 
           na_if("NA") |> 
           na_if(""),
         n_drks = str_count(drks_clean, ";") + 1 * as.numeric(drks_clean != ""),
         euctr_clean = str_extract_all(other_clean, regexes$EudraCT)|> 
           unlist() |> 
           paste(collapse = ";") |> 
           na_if("NA") |> 
           na_if(""),
         n_euctr = str_count(euctr_clean, ";") + 1 * as.numeric(euctr_clean != "")) |> 
  ungroup()

# deduplicate and filter out NAs

euctr_protocols_trns_clean <- euctr_protocols_trns_clean |> 
  group_by(eudract_number) |> 
  summarise(across(c(ctgov_all, drks_clean, euctr_clean), deduplicate_collapsed)) |> 
  # remove self-references from euctr_clean
  mutate(euctr_clean = ifelse(euctr_clean == eudract_number, NA_character_, euctr_clean)) |> 
  filter(!is.na(ctgov_all) |
           !is.na(drks_clean) |
           !is.na(euctr_clean)) |> 
  ungroup()

# check if stragglers were still missed:
# all NCTs are non-resolving and malformed
qa_protocols <- euctr_trns |> 
  filter(!eudract_number %in% euctr_protocols_trns_clean$eudract_number,
         !nct_number %in% invalid_ncts,
         str_detect(other_ids, paste(c(regexes$ClinicalTrials.gov,
                                       regexes$DRKS,
                                       regexes$EudraCT), collapse = "|")),
         !str_detect(other_ids, eudract_number))
