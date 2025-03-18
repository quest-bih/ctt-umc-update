library(tidyverse)
library(here)

euctr_tib <- read_csv(here("data", "raw", "euctr_euctr_dump-2024-09-07-092059.csv"))
euctr_results <- read_csv(here("data", "raw", "euctr_data_quality_results_scrape_sept_2024.csv"))

regexes <- yaml::read_yaml(here("inst", "extdata", "keywords_patterns.yaml"))


# Take only trial start date and secondary ids from results
euctr_results <- euctr_results |>
  select(trial_id, trial_start_date, global_subjects, this_version_date, nct_number, other_ids) |>
  rename(eudract_number = trial_id,
         actual_enrollment = global_subjects,
         last_updated = this_version_date)

euctr_ids <- euctr_results |> 
  select(eudract_number,
         # trial_start_date,
         # global_subjects,
         # actual_enrollment = global_subjects,
         #last_updated = this_version_date,
         other_identifiers = other_ids) |> 
  mutate(other_identifiers = na_if(other_identifiers, "")) |> 
  filter(!is.na(other_identifiers)) |> 
  rowwise() |> 
  mutate(ctgov_clean = str_extract_all(other_identifiers, regexes$ctgov) |> 
           unlist() |> 
           paste(collapse = ";"),
         n_ctgov = str_count(ctgov_clean, ";") + 1 * as.numeric(ctgov_clean != ""),
         drks_clean = str_extract_all(other_identifiers, regexes$drks) |> 
           unlist() |> 
           paste(collapse = ";"),
         n_drks = str_count(drks_clean, ";") + 1 * as.numeric(drks_clean != ""),
         euctr_clean = str_extract_all(other_identifiers, regexes$euctr)|> 
           unlist() |> 
           paste(collapse = ";"),
         n_euctr = str_count(euctr_clean, ";") + 1 * as.numeric(euctr_clean != "")) |> 
  ungroup()


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

# Remove fully NA rows (if any)
euctr_narrow <- euctr_narrow %>%
  filter(rowSums(is.na(.)) < ncol(.))

