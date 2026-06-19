library(tidyverse)
library(here)
library(skimr)
library(flextable)
library(furrr)
library(progressr)
library(ggVennDiagram)

plan(multisession)
handlers(global = TRUE)

source(here("scripts", "utils.R"))


# unfiltered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_unfiltered.csv"))
# filtered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_filtered.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv")) 
euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))
# euctr_export <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
# clean_pub_search_results <- read_csv(here("data", "processed", "results_clean_2026-04-09.csv"))
combined_data_filtered <- read_csv(here("data", "processed", "harmonized_data_filtered.csv"))

excluded_ct <- ctgov_export |> 
  filter(!trial_id %in% combined_data_filtered$trial_id)

euctr_combined_deduped <- euctr_combined |>
  rename(trial_id = eudract_number) |> 
  filter(trial_id %in% combined_data_filtered$trial_id) |> 
  group_by(trial_id) |>
  mutate(is_trial_de_protocol = str_detect(eudract_number_with_country, "DE")) |>
  arrange(desc(is_trial_de_protocol)) |>
  summarise(across(everything(), first)) |> 
  select(-eudract_number_with_country) |> 
  ungroup()

euctr_combined_deduped |> 
  write_csv(here("data", "processed", "export_euctr_deduped.csv"))

euctr_trial_characteristics <- euctr_combined_deduped |> 
  mutate(trial_id,
         is_randomized = trial_randomised,
         is_blinded = trial_single_blind | trial_double_blind,
         primary_sponsor = future_map_chr(sponsors, extract_sponsor_classifications,
                                          .progress = TRUE),
         sponsor_name = future_map_chr(sponsors, extract_sponsor_name,
                                       .progress = TRUE),
         phase = case_when(
           trial_human_pharmacology_phase_i &
             trial_therapeutic_exploratory_phase_ii ~ "I-II",
           trial_therapeutic_exploratory_phase_ii &
             trial_therapeutic_confirmatory_phase_iii ~ "II-III",
           trial_human_pharmacology_phase_i ~ "I",
           trial_therapeutic_exploratory_phase_ii ~ "II",
           trial_therapeutic_confirmatory_phase_iii &
             trial_therapeutic_use_phase_iv ~ "III-IV",
           trial_therapeutic_confirmatory_phase_iii ~ "III",
           trial_therapeutic_use_phase_iv ~ "IV",
           .default = NA_character_
         ),
         is_multicentric = case_when(
           trial_the_trial_involves_single_site_in_the_member_state_concer ~ FALSE,
           trial_the_trial_involves_multiple_sites_in_the_member_state_con ~ TRUE,
           trial_the_trial_involves_multiple_member_states ~ TRUE,
           .default = NA
         ),
         is_multinational = trial_the_trial_involves_multiple_member_states,
         planned_enrolment = subject_in_the_whole_clinical_trial,
         actual_enrolment = results_actual_enrollment,
         completion_date = ymd(completion_date),
         status = trial_status,
         .keep = "none")

### trial_characteristics
trial_characteristics <- drks_export |> 
  bind_rows(ctgov_export) |> 
  bind_rows(euctr_trial_characteristics) |> 
  # left_join(combined_inex_plus_crossreg |> select(trial_id, is_index_reg,
  # crossreg_id)) |>
  right_join(combined_data_filtered |> select(trial_id, is_index_reg,
                                              crossreg_id)) |>
  # mutate(is_index_reg = if_else(is_crossreg == FALSE, TRUE, is_index_reg)) |>
  select(trial_id, crossreg_id, is_index_reg, contains("has_"), everything()) |> 
  # filter(is.na(is_index_reg))
  filter(is_index_reg == TRUE) |>
  mutate(Registry = get_registry_name(trial_id),
         Randomization = if_else(is_randomized == TRUE, "Randomized", "Not randomized") |>
           factor(levels = c("Randomized", "Not randomized")),
         Blinding = if_else(is_blinded == TRUE, "Blinded", "Not blinded"),
         Multinational = if_else(is_multinational == TRUE, "Yes", "No"),
         Multicentric = if_else(is_multicentric == TRUE, "Yes", "No"),
         `Primary sponsor` = primary_sponsor |> 
           str_to_sentence(),
         Phase = phase,
         Status = recode_status(status),
         Enrolment = actual_enrolment,
         `Completion year` = year(completion_date) |> factor()
  ) 
# trial_characteristics |> count(Status, status)
trial_characteristics |>
  select(Registry, Randomization, Blinding,
         `Primary sponsor`, Phase,
         Multicentric, Multinational, Enrolment, 
         `Completion year`, Status
  ) |> 
  summarizor() |> 
  as_flextable() |>
  footnote(i = c(1,2,3), j = 2,
           value = as_paragraph("may also be cross-registered in another registry"),
           ref_symbols = "a") |> 
  footnote(i = c(28, 32), j = 1,
           value = as_paragraph("actual or anticipated, if actual not available"),
           ref_symbols = "b")


in_table_not_fc <- drks_export |> 
  bind_rows(ctgov_export) |> 
  bind_rows(euctr_trial_characteristics) |> 
  left_join(combined_data_filtered |> select(trial_id, is_index_reg,
                                             crossreg_id)) |> 
  select(trial_id, crossreg_id, is_index_reg, everything()) |> 
  filter(is_index_reg == TRUE) |> 
  # left_join(combined_inex, by = "trial_id") |> 
  # select(trial_id, crossreg_id, contains("is_"), everything()) 
  anti_join(combined_data_filtered, by = "trial_id")

qa_missing_characteristics <- combined_data_filtered |> 
  filter(!trial_id %in% trial_characteristics$trial_id) |> 
  select(trial_id, crossreg_id, contains("has_"), contains("is_"),
         everything())

umc_info_drks <- drks_export |> 
  select(trial_id, contains("umc"))
# |> 
#   rename_with(\(x) paste0("dks_", x), .cols = contains("umc"))

umc_info_ctgov <- ctgov_export |> 
  select(trial_id, contains("umc"))

umc_euctr <- euctr_combined_deduped |> 
  select(trial_id, umc)

umc_info <- bind_rows(umc_info_drks, 
                      umc_info_ctgov,
                      umc_euctr)


qa_umc <- combined_data_filtered |> 
  left_join(umc_info, by = "trial_id") |>
  mutate(is_umc_a = case_when(
    registry == "EUCTR" & is_german_umc == TRUE ~ TRUE,
    !is.na(umc_sponsor) ~ TRUE,
    !is.na(umc_resp_party) ~ TRUE,
    .default = FALSE
  ),
  is_any_umc = !is.na(umc) | (registry == "EUCTR" & is_german_umc == TRUE),
  is_umc_sponsor_resp_party = if_else(is_german_umc == TRUE & registry == "EUCTR", TRUE, !is.na(umc_sponsor) | 
                                        !is.na(umc_resp_party)),
  ) |> 
  group_by(crossreg_id) |> 
  mutate(has_umc_a = any(is_umc_a, na.rm = TRUE),
         has_any_umc = any(is_any_umc, na.rm = TRUE),
         has_umc_sponsor_resp_party = any(is_umc_sponsor_resp_party, na.rm = TRUE),
         has_only_umc_pi = any(!is.na(umc_pi), na.rm = TRUE) &
           !any(is_umc_sponsor_resp_party, na.rm = TRUE)) |> 
  ungroup() |> 
  select(trial_id, crossreg_id, contains("umc"), everything())

qa_umc |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(is_crossreg, has_only_umc_pi, has_umc_sponsor_resp_party)

euctr_sponsor_names <- euctr_trial_characteristics |> 
  select(trial_id, sponsor_name)

# add original sponsor field to euctr
qa_euctr_umc <- qa_umc |> 
  filter(is_crossreg, has_only_umc_pi, str_detect(crossreg_id, "-")) |> 
  arrange(crossreg_id) |> 
  left_join(euctr_sponsor_names, by = "trial_id") |> 
  select(trial_id, crossreg_id, is_german_umc, has_german_umc,
         umc, sponsor_name, umc_pi, umc_sponsor, umc_resp_party)

qa_euctr_umc |> 
  write_excel_csv(here("data", "processed", "euctr_umc_validations.csv"))


#### Upset plot
# 1 distinguish between straightforward inclusion
# and registrations only included because of being linked to
# a straightforwardly included registration

upset_info <- combined_data_filtered |> 
  select(crossreg_id) |> 
  mutate(registry_names = map_chr(crossreg_id, get_registry_names))

list(
  DRKS = upset_info |> filter(str_detect(registry_names, "DRKS")) |> 
    pull(crossreg_id),
  `ClinicalTrials.gov` = upset_info |> filter(str_detect(registry_names, "Clin")) |> 
    pull(crossreg_id),
  EUCTR = upset_info |> filter(str_detect(registry_names, "Eud")) |> 
    pull(crossreg_id)
) |> 
  ggVennDiagram(force_upset = TRUE)


# 
# missing_in_pub_search <- combined_data_filtered |> 
#   filter(!trial_id %in% pub_search_table$trial_id)
