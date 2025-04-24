library(tidyverse)
library(here)
library(ctregistries)
library(furrr)

source(here("scripts", "utils.R"))
# regexes <- yaml::read_yaml(here("inst", "extdata", "keywords_patterns.yaml"))
regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

euctr_tib <- read_csv(here("data", "raw", "euctr_euctr_dump-2024-09-07-092059.csv"))
euctr_results <- read_csv(here("data", "raw", "euctr_data_quality_results_scrape_sept_2024.csv"))

# Take only trial start date and secondary ids from results
euctr_results <- euctr_results |>
  select(trial_id, trial_start_date, global_subjects, this_version_date, nct_number, isrctn_number, other_ids) |>
  rename(eudract_number = trial_id,
         actual_enrollment = global_subjects,
         last_updated = this_version_date)


# Cut down table to only variables we're interested in
euctr_narrow <- euctr_tib |>
  select(eudract_number_with_country,
         date_on_which_this_record_was_first_entered_in_the_eudract_data,
         eudract_number,
         member_state_concerned,
         subject_in_the_member_state,
         subject_in_the_whole_clinical_trial,
         full_title_of_the_trial,
         imps,
         date_on_which_this_record_was_first_entered_in_the_eudract_data,
         end_of_trial_status,
         trial_the_trial_involves_multiple_member_states,
         sponsors,
         trial_medical_condition_in_easily_understood_language,
         trial_therapeutic_area,
         trial_system_organ_class,
         trial_medical_condition_s_being_investigated,
         trial_therapeutic_use_phase_iv,
         trial_therapeutic_exploratory_phase_ii,
         trial_human_pharmacology_phase_i,
         trial_therapeutic_confirmatory_phase_iii,
         trial_therapy,
         trial_prophylaxis,
         trial_safety,
         trial_diagnosis,
         trial_bioequivalence_study,
         trial_efficacy,
         trial_pharmacogenomic,
         trial_bioequivalence,
         trial_pharmacodynamic,
         trial_pharmacogenetic,
         trial_placebo,
         trial_pharmacoeconomic,
         trial_pharmacokinetic,
         trial_other_trial_design_description,
         trial_other_trial_type_description,
         trial_other_scope_of_the_trial_description,
         trial_other,
         trial_secondary_end_point_s,
         trial_primary_end_point_s,
         trial_main_objective_of_the_trial,
         trial_principal_exclusion_criteria,
         subject_male,
         subject_female,
         trial_randomised,
         trial_single_blind,
         trial_double_blind,
         trial_cross_over,
         trial_parallel_group,
         date_of_the_global_end_of_the_trial,
         trial_results,
         date_of_ethics_committee_opinion,
         date_of_competent_authority_decision
        ) 



plan(multisession)
#plan(sequential)

columns_to_clean <- c("isrctn_number", "nct_number", "other_ids")
invalid_ncts <- c("NCT00000000", "NCT99999999", "NCT12345678")

euctr_results_trns_clean <- euctr_results |> 
  filter(!if_all(everything(), is.na)) |> # Remove fully NA rows (if any)
  select(all_of(columns_to_clean), everything()) |> 
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
