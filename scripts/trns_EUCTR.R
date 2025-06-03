library(tidyverse)
library(janitor)
library(progressr)
library(furrr)
library(here)

euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))
source(here("scripts", "utils.R"))

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

extract_valid <- function(trn_vec, valid_regex, na_regex, clean = FALSE) {
  dplyr::case_when(
    stringr::str_detect(trn_vec, na_regex) ~ NA_character_,
    clean == TRUE & stringr::str_detect(trn_vec, valid_regex) ~ 
      stringr::str_extract(trn_vec, valid_regex) |> ctregistries::which_trns(),
    .default = stringr::str_extract(trn_vec, valid_regex)
  )
}

euctr_trns_clean <- euctr_trns |> 
  # select(all_of(columns_to_clean), everything()) |> 
  mutate(across(everything(), \(x) if_else(str_detect(x, na_regexes), NA_character_, x))) |> 
  filter(!if_all(-contains("eudract"), is.na))  |> # Remove fully NA rows (if any)
  mutate(ctgov_clean  = extract_valid(nct_number, regexes$ClinicalTrials.gov, na_regexes),
         ctgov_clean_results = extract_valid(results_nct_number, regexes$ClinicalTrials.gov, na_regexes),
         other_clean = which_trns(other_ids),
         other_clean_results = which_trns(results_other_ids)
         )

euctr_trns_clean_registries <- euctr_trns_clean |> 
  mutate(ctgov_clean_other = extract_valid(other_clean, regexes$ClinicalTrials.gov, na_regexes),
         ctgov_clean_other_results = extract_valid(other_clean_results, regexes$ClinicalTrials.gov, na_regexes),
         drks_clean = extract_valid(other_clean, regexes$DRKS, na_regexes),
         drks_clean_results = extract_valid(other_clean_results, regexes$DRKS, na_regexes),
         euctr_clean = extract_valid(other_clean, regexes$EudraCT, na_regexes),
         euctr_clean_results = extract_valid(other_clean_results, regexes$EudraCT, na_regexes))

euctr_trns_clean_all <- euctr_trns_clean_registries |> 
  mutate(euctr_clean = na_if(euctr_clean, eudract_number),
         euctr_clean_results = na_if(euctr_clean_results, eudract_number)) |> 
  rowwise() |> 
  mutate(ctgov_all = deduplicate_collapsed(c(ctgov_clean, ctgov_clean_results, ctgov_clean_other, ctgov_clean_other_results)),
         drks_all = deduplicate_collapsed(c(drks_clean, drks_clean_results)),
         euctr_all = deduplicate_collapsed(c(euctr_clean, euctr_clean_results)),
         n_ctgov = str_count(ctgov_all, ";") + 1 * as.numeric(ctgov_all != ""),
         n_drks = str_count(drks_all, ";") + 1 * as.numeric(drks_clean != ""),
         n_euctr = str_count(euctr_all, ";") + 1 * as.numeric(euctr_clean != "")) |> 
  ungroup()


qa_results_clean <- euctr_trns_clean_all |> 
  filter(str_detect(results_other_ids, eudract_number)) |> 
  select(eudract_number, results_other_ids, other_clean_results, euctr_all, everything())

# is protocol_sponsor_code unique for countries? does it matter if collapsed to 1?

qa_trns <- euctr_trns_clean_all |>  
  filter(!is.na(ctgov_all) | !is.na(drks_all) | !is.na(euctr_all)) |> 
  pull(eudract_number)

# check if stragglers were still missed:
# all are self-references, so no
qa_protocols <- euctr_trns |> 
  filter(!eudract_number %in% qa_trns,
         str_detect(other_ids, paste(c(regexes$ClinicalTrials.gov,
                                       regexes$DRKS,
                                       regexes$EudraCT), collapse = "|")),
         !str_detect(other_ids, eudract_number))

euctr_trns_export <- euctr_trns_clean_all |> 
  select(contains("eudract_number"), contains("_all"), n_ctgov, n_drks, n_euctr) |> 
  mutate(across(contains("n_"), \(x) replace_na(x, 0)))

euctr_trns_export |> 
  write_excel_csv(here("data", "processed", "euctr_ids.csv"))
