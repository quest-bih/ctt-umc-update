library(tidyverse)
library(progressr)
library(furrr)
library(here)
library(janitor)
library(skimr)
library(flextable)
library(flowchart)
library(ggVennDiagram)

source(here("scripts", "utils.R"))


plan(multisession)
handlers(global = TRUE)

unfiltered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_unfiltered.csv"))
filtered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_filtered.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv")) 
euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))
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


fc_simple <- combined_inex |>
  
  as_fc(label = "All TRNS from all registries")


inex_per_registry <- function(tib) {
  fc <- flowchart::as_fc(tib, label = "All TRNs")
  
  fc |> 
    flowchart::fc_split(registry) |>
    flowchart::fc_filter(is_german_umc, label = "German UMC", show_exc = TRUE) |> 
    flowchart::fc_filter(is_completed_2018_2021, label = "Completed 2018-2021", show_exc = TRUE) |>
    flowchart::fc_filter(is_interventional, label = "Interventional", show_exc = TRUE) |> 
    flowchart::fc_filter(!is_crossreg, label = "Not crossregistered", show_exc = TRUE) |>
    flowchart::fc_filter(!is_withdrawn, label = "Not withdrawn", show_exc = TRUE) |>
    flowchart::fc_draw()
}


combined_inex |> 
  inex_per_registry()

falling_clusters <- read_csv(here("data", "processed", "crossreg_unfiltered.csv")) |> 
  mutate(registry = get_registry_name(trial_id),
         is_crossreg = TRUE) |> 
  select(-contains("premature"), -contains("filled"), -registries,
         -has_cluster_de_protocol, -recent_status, -actual_completion_date)


falling_clusters |> 
  as_fc(label = "Crossreg TRNs (at least one TRN in cluster is included according to 'simple' criteria") |> 
  fc_split(registry) |> 
  fc_filter(has_completion_2018_2021, label = "Completed 2018-2021", show_exc = TRUE) |>
  fc_filter(has_interventional, label = "Interventional", show_exc = TRUE) |> 
  fc_filter(has_german_umc, label = "German UMC", show_exc = TRUE) |> 
  fc_filter(!has_withdrawn_status, label = "Not withdrawn", show_exc = TRUE) |>
  flowchart::fc_draw()

gumc <- falling_clusters |> 
  filter(!has_german_umc)


falling_clusters

setdiff(names(combined_inex), names(falling_clusters))

setdiff(names(falling_clusters), names(combined_inex))

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


combined_inex_plus_crossreg |> 
  as_fc(label = "All TRNs") |> 
  # fc_split(registry) |>
  fc_filter(has_interventional, label = "Interventional", show_exc = TRUE) |>
  
  fc_filter(has_german_umc, label = "German UMC", show_exc = TRUE) |> 
  fc_filter(has_completion_2018_2021, label = "Completed 2018-2021", show_exc = TRUE) |>
  # fc_filter(!is_crossreg, label = "Not crossregistered", show_exc = TRUE) |>
  fc_filter(!has_withdrawn_status, label = "Not withdrawn", show_exc = TRUE) |>
  # fc_split(is_crossreg) |> # add n trials for crossreg
  fc_draw()


combined_data_filtered <- combined_inex_plus_crossreg |> 
  filter(has_german_umc, has_completion_2018_2021, has_interventional,
         !has_withdrawn_status) |> 
  mutate(is_index_reg = if_else(is_crossreg == FALSE, TRUE, is_index_reg)) |> 
  select(trial_id, crossreg_id, everything())

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


ecluded_ct <- ctgov_export |> 
  filter(!nct_id %in% combined_data_filtered$trial_id)

euctr_combined_deduped <- euctr_combined |>
  rename(trial_id = eudract_number) |> 
  filter(trial_id %in% combined_data_filtered$trial_id) |> 
  group_by(trial_id) |>
  mutate(is_trial_de_protocol = str_detect(eudract_number_with_country, "DE")) |>
  arrange(desc(is_trial_de_protocol)) |>
  summarise(across(everything(), first)) |> 
  select(-eudract_number_with_country) |> 
  ungroup()

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
  bind_rows(ctgov_export |> rename(trial_id = nct_id)) |> 
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
         Status = case_when(
           status %in% c("COMPLETE_FOLLOW_UP_COMPLETE",
                         "COMPLETED",
                         "Completed") ~ "Completed",
           status %in% c("INVITE_ONLY",
                         "PENDING",
                         "COMPLETE_FOLLOW_UP_CONTINUING",
                         "RECRUITING",
                         "Ongoing") ~ "Ongoing",
           status %in% c("DISCONTINIUED",
                         "Prematurely Ended",
                         "TERMINATED") ~ "Terminated",
           status == "WITHDRAWN" ~ "Withdrawn",
           str_detect(status,
                      regex("suspended",
                            ignore_case = TRUE)
                      ) ~ "Suspended",
           status == "UNKNOWN" ~ "Unknown",
           # status == "\\N" |
           #    ~ "Other",
           .default = NA
           )
         ,
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
           ref_symbols = "a")


in_table_not_fc <- drks_export |> 
  bind_rows(ctgov_export |> rename(trial_id = nct_id)) |> 
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
  select(trial_id = nct_id, contains("umc"))

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

