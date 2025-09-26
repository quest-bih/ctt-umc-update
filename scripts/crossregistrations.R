# Script to title match trials in GTL analysis set

library(tidyverse)
library(here)
library(ctregistries)
library(jsonlite)
library(tictoc)
library(furrr)
library(progressr)
library(stringdist)
library(janitor)
library(readxl)

##########################################################
source(here("scripts", "utils.R"))

# Ct.gov preprocessing
AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_250513")
AACT_datasets <- load_AACT_datasets(AACT_folder, "studies")

# Decided not to salvage bogus official titles by looking into the brief_title instead,
# this is a job for the registries and the registrants to ensure better (meta)data quality
ct_titles <- AACT_datasets$studies |> 
  select(trial_id = nct_id, title = official_title, brief_title) |> 
  tidyr::drop_na(title) |>
  mutate(title_processed = process_title(title),
         title_length = str_length(title_processed))

qa_titles <- ct_titles |> 
  select(trial_id, contains("title"))
# 
# 
# ### remove bogus titles from ct
# bogus_titles_ct <- ct_titles |> 
#   filter(str_detect(title, "^(Principal Investigator|Corresponding)|\\bDirector") |
#            str_detect(title, "University(?! \\w+s\\b)") & title_length <= 50,
#          trial_id != "NCT02038387",
#          !str_detect(title, "C(ohort|OHORT)|Study|\\bin\\b|Program")
#   )
# 
# ct_cleaned <- ct_titles |> 
#   filter(!trial_id %in% bogus_titles_ct$trial_id)
##########################################################

# DRKS preprocessing
drks_tib <- fromJSON(here("data", "raw", "DRKS_search_20250513.json"))

drks_titles <- drks_tib |> 
  select(trial_id = drksId, trialDescriptions) |> 
  unnest(trialDescriptions) |> 
  unnest(idLocale) |> 
  filter(locale == "en") |> 
  select(trial_id, title) |> 
  tidyr::drop_na(title) |>
  mutate(title_processed = process_title(title),
         title_length = str_length(title_processed))


###########################################################

# Extract EU IDs plus process EU titles to be used in title matching
euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))

euctr_de_protocols <- euctr_combined |> 
  group_by(eudract_number) |> 
  summarise(has_trial_de_protocol = any(has_trial_de_protocol, na.rm = TRUE), .groups = "drop")

# Process EU titles
euctr_titles <- euctr_combined |>
  tidyr::drop_na(full_title_of_the_trial) |>
  mutate(title_processed = str_remove_all(full_title_of_the_trial, "\\n.*") |> 
           process_title(),
         title_length = str_length(title_processed)) |> 
  select(trial_id = eudract_number, eudract_number_with_country, 
         title = full_title_of_the_trial,
         title_processed, title_length, has_trial_de_protocol)

# multiline_titles <- euctr_updated_titles |> 
#   # filter(eudract_number %in% c("2021-000369-34")) |> 
#   filter(str_detect(full_title_of_the_trial, "\\n \\n")) |> 
#   mutate(first_title = str_remove_all(full_title_of_the_trial, "\\n.*")) |> 
#   select(full_title_of_the_trial, first_title, eudract_number, eudract_number_with_country) 

title_matches_euctr_drks <- euctr_titles |> 
  inner_join(drks_titles, by = "title_processed",
             relationship = "many-to-many") |> 
  arrange(trial_id.x, desc(has_trial_de_protocol)) |> 
  distinct(trial_id.x, trial_id.y) |> 
  mutate(via_title = TRUE)

#only a single case of many-to-many with drks and euctr
dupes_euctr_drks_titles.x <- get_dupes(title_matches_euctr_drks, trial_id.x)
dupes_euctr_drks_titles.y <- get_dupes(title_matches_euctr_drks, trial_id.y)
mtm_euctr_drks <- dupes_euctr_drks_titles.y |> 
  select(contains("trial_id")) |> 
  unlist() |> 
  unique()

title_matches_euctr_ctgov <- euctr_titles |> 
  inner_join(ct_titles, by = "title_processed",
             relationship = "many-to-many") |> 
  arrange(trial_id.x, desc(has_trial_de_protocol)) |> 
  distinct(trial_id.x, trial_id.y) |> 
  mutate(via_title = TRUE)

### many-to-many with euctr and ct.gov!
dupes_euctr_ctgov_titles.x <- get_dupes(title_matches_euctr_ctgov, trial_id.x)
dupes_euctr_ctgov_titles.y <- get_dupes(title_matches_euctr_ctgov, trial_id.y)
mtm_euctr_ctgov <- dupes_euctr_ctgov_titles.x |> 
  bind_rows(dupes_euctr_ctgov_titles.y) |> 
  select(contains("trial_id")) |> 
  unlist() |> 
  unique()

title_matches_drks_ctgov <- drks_titles |> 
  inner_join(ct_titles, by = "title_processed",
             relationship = "many-to-many") |>
  distinct(trial_id.x, trial_id.y) |> 
  mutate(via_title = TRUE)

### only one many-to-many with drks and ct.gov
dupes_drks_ctgov_titles.x <- get_dupes(title_matches_drks_ctgov, trial_id.x)
dupes_drks_ctgov_titles.y <- get_dupes(title_matches_drks_ctgov, trial_id.y)
mtm_drks_ctgov <- dupes_drks_ctgov_titles.x |> 
  bind_rows(dupes_drks_ctgov_titles.y) |> 
  select(contains("trial_id")) |> 
  unlist() |> 
  unique()

mtm_ids <- c(mtm_drks_ctgov, mtm_euctr_ctgov, mtm_euctr_drks) |> 
  unique()

ctgov_id_info <- file.path(AACT_folder, "id_information.txt") |> 
  read_delim(delim = "|")

### all title matches between all registries
title_matches <- title_matches_euctr_drks |> 
  bind_rows(title_matches_euctr_ctgov) |> 
  bind_rows(title_matches_drks_ctgov) |> 
  rename(trial_id = trial_id.x, linked_id = trial_id.y) |>
  mutate(many_to_many = trial_id %in% mtm_ids | linked_id %in% mtm_ids)

title_matches |> 
  saveRDS(here("data", "processed", "all_title_matches.rds"))

EUCTR_sample <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
DRKS_sample <- read_csv(here("data", "processed", "DRKS_sample.csv"))
CTgov_sample <- read_csv(here("data", "processed", "CTgov_sample.csv"))

# by taking crossregistrations to only involve at least one trial in-sample
# we avoid having to refilter here yet again and getting filter criteria potentially out-of-sync
sample_ids <- c(EUCTR_sample$eudract_number, DRKS_sample$drksId, CTgov_sample$nct_id) |>
  unique()

crossreg_euctr_drks_ctgov <- read_csv(here("data", "processed", "crossreg_euctr_drks_ctgov.csv"))

crossreg_long <- crossreg_euctr_drks_ctgov |>
  select(trial_id, linked_id) |>
  separate_longer_delim(linked_id, ";") |>
  mutate(via_id = TRUE)
  
trn_clusters <- title_matches |>
  select(trial_id, linked_id) |>
  bind_rows(crossreg_long) |>
  get_cluster_names_from_pairs()

title_matches_clusters <- title_matches |>
  left_join(trn_clusters, by = "trial_id") |>
  group_by(cluster_unique_id) |>
  mutate(in_sample = any(trial_id %in% sample_ids |
                           linked_id %in% sample_ids),
         pair_in_sample = trial_id %in% sample_ids |
           linked_id %in% sample_ids) |>
  rowwise() |>
  mutate(many_to_many_overall = is_mtm(cluster_unique_id),
         binary_id = get_binary_id(c(trial_id, linked_id))) |>
  ungroup()

title_matches_included <- title_matches_clusters |>
  filter(in_sample == TRUE)
  
crossreg_clusters <- crossreg_euctr_drks_ctgov |>
  left_join(trn_clusters, by = "trial_id") |>
  separate_longer_delim(linked_id, ";") |>
  group_by(cluster_unique_id) |>
  mutate(in_sample = any(trial_id %in% sample_ids |
                           linked_id %in% sample_ids),
         pair_in_sample = trial_id %in% sample_ids |
           linked_id %in% sample_ids) |>
  ungroup()

crossreg_included <- crossreg_clusters |>
  filter(in_sample == TRUE) |>
  rowwise() |>
  mutate(many_to_many_overall = is_mtm(cluster_unique_id),
         binary_id = get_binary_id(c(trial_id, linked_id))) |>
  ungroup() |>
  update_bidirectionality()

title_matches_only <- title_matches_included |>
  mutate(via_id = cluster_unique_id %in% crossreg_included$cluster_unique_id) |>
  filter(via_id == FALSE)

title_matched_id_matched <- title_matches_included |>
  filter(!binary_id %in% title_matches_only$binary_id)

# add the title match info for cases that have TRN crossreg (still missing cases that only have a title match)
crossreg_w_and_wo_title <- crossreg_included |>
  mutate(via_title = FALSE) |>
  rows_upsert(title_matched_id_matched, by = c("binary_id", "cluster_unique_id", "trial_id", "linked_id")) |>
  # update via_title for the cases where the binary_id is the same but the linked_id and trial_id are swapped
  mutate(via_title = case_when(
    !binary_id %in% title_matches_only$binary_id ~ TRUE,
    .default = via_title)) |>
  # flag redundant title rows
  group_by(binary_id) |>
  mutate(n_binary = n(),
         remove_title = n_binary > 1 & is.na(bidirectional)) |>
  ungroup() |>
  filter(remove_title == FALSE) |>
  select(-n_binary, -remove_title)

# finally add the cases with only a title match to the rest of the crossregs
crossreg_title_ids <- crossreg_w_and_wo_title |>
  bind_rows(title_matches_only) |>
  arrange(cluster_unique_id, binary_id) |>
  select(cluster_unique_id, binary_id, trial_id, linked_id, everything()) |>
  mutate(triad = is_triad(cluster_unique_id),
         trns_in_cluster = str_count(cluster_unique_id, "_") + 1,
         via_id = replace_na(via_id, FALSE)) |> 
  left_join(euctr_de_protocols, by = c("trial_id" = "eudract_number")) |> 
  left_join(euctr_de_protocols |> rename(has_linked_de_protocol = has_trial_de_protocol), by = c("linked_id" = "eudract_number")) 

# Number unique TRNs?= unique clusters
crossreg_title_ids |>
  distinct(cluster_unique_id) |>
  nrow()

crossreg_title_ids_mtm <- crossreg_title_ids |>
  filter(many_to_many_overall)

crossreg_title_ids |> 
  count(has_trial_de_protocol)

# crossreg_title_ids <- crossreg_title_ids_new_clusters |>
crossreg_title_ids |> 
  write_csv(here("data", "processed", "crossreg_titles_ids.csv"))

# how many links (not triads, not many_to_many)
qa_crossreg_title_ids <- crossreg_title_ids |> 
  filter(many_to_many == FALSE, triad == FALSE)

nrow(qa_crossreg_title_ids)

qa_mtm <- crossreg_title_ids |> 
  filter(many_to_many == TRUE)

#### add EUCTR and CTgov grant numbers
other_info <- file.path(AACT_folder, "id_information.txt") |>
  read_delim(delim = "|") |>
  filter(id_type %in% c(NA, "OTHER")) |>
  select(trial_id = nct_id, other_nr_ctgov = id_value) |>
  group_by(trial_id) |>
  summarise(other_nr_ctgov = deduplicate_collapsed(other_nr_ctgov)) |>
  ungroup()

other_info_euctr <- read_csv(here("data", "raw", "euctr_euctr_dump-2024-09-07-092059.csv")) |>
  # select(contains("number"), everything())
  select(contains("eudract_number"), sponsor_s_protocol_code_number)

other_info_euctr_collapsed <- other_info_euctr |>
  rename(trial_id = eudract_number, other_nr_euctr = sponsor_s_protocol_code_number) |>
  group_by(trial_id) |>
  summarise(other_nr_euctr = deduplicate_collapsed(other_nr_euctr)) |>
  ungroup()

mtm_other <- qa_mtm  |>
  left_join(other_info, by = "trial_id") |>
  left_join(other_info, by = c("linked_id" = "trial_id")) |>
  left_join(other_info_euctr_collapsed, by = "trial_id") |>
  left_join(other_info_euctr_collapsed, by = c("linked_id" = "trial_id")) |>
  rowwise() |>
  mutate(trial_registry = get_registry_name(trial_id),
         linked_registry = get_registry_name(linked_id),
         other_nr_ctgov = deduplicate_collapsed(c(other_nr_ctgov.x, other_nr_ctgov.y)),
         other_nr_euctr = deduplicate_collapsed(c(other_nr_euctr.x, other_nr_euctr.y))) |>
  ungroup() |>
  select(-contains(c(".x", ".y"))) |>
  mutate(other_match = str_detect(other_nr_ctgov, other_nr_euctr) |
           str_detect(other_nr_euctr, other_nr_ctgov),
         euctr_ctgov = trial_registry == "EUCTR" & linked_registry == "ClinicalTrials.gov" |
           trial_registry == "ClinicalTrials.gov" & linked_registry == "EUCTR") |>
  group_by(cluster_unique_id, euctr_ctgov) |>
  mutate(n_reg = n(),
         n_other_matched = sum(other_match == TRUE, na.rm = TRUE),
         false_positive = n_reg > 1 &
           n_other_matched > 0 & other_match == FALSE)

mtm_other |>
  write_csv(here("data", "processed", "mtm_othernr.csv"))

validated_mtm_resolved <- read_xlsx(here("data", "processed", "validations",
                                         "mtm_othernr_manually_validated – 20250724.xlsx"))

crossreg_tp <- validated_mtm_resolved |>
  filter(combinations_validated != "NA") |>
  select(binary_id, cluster_unique_id = combinations_validated)

mtm_resolved <- crossreg_title_ids |>
  filter(many_to_many_overall == TRUE,
         binary_id %in% c(crossreg_tp$binary_id)) |>
  rename(old_cluster = cluster_unique_id) |>
  # left_join(simplified_clusters, by = "trial_id") |>
  # left_join(simplified_clusters, by = c("linked_id" = "trial_id")) |>
  left_join(crossreg_tp, by = "binary_id") |>
  mutate(
  #   cluster_unique_id = case_when(
  #   !is.na(cluster_unique_id) ~ cluster_unique_id,
  #   cluster_unique_id.x == binary_id ~ binary_id,
  #   .default = NA
  # ),
  many_to_many = is_mtm(cluster_unique_id),
  trns_in_cluster = str_count(cluster_unique_id, "_") + 1) |>
  select(-contains(".x"), -contains(".y"), -many_to_many_overall)
  

# EUCTR inclusion and exclusion criteria
euctr_inex <- read_csv(here("data", "processed", "inclusion_exclusion_euctr.csv"))
euctr_inex_deduped <- euctr_inex |> 
  group_by(trial_id) |> 
  mutate(is_trial_de_protocol = str_detect(eudract_number_with_country, "DE"),
         has_premature = any(status == "Prematurely Ended", na.rm = TRUE),
         de_premature = status == "Prematurely Ended" & str_detect(eudract_number_with_country, "DE"),
         has_de_premature = any(de_premature, na.rm = TRUE)) |> 
  arrange(desc(is_trial_de_protocol)) |> 
  summarise(across(everything(), first)) |> # this takes first also for is_trial_de_protocol == FALSE
  select(-eudract_number_with_country, -is_trial_de_protocol, -de_premature, has_euctr_results = results_reporting) |> 
  ungroup() |> 
  mutate(completion_date = ymd(completion_date),
         estimated_completion_date = case_when(
           has_euctr_results == FALSE ~ completion_date,
           .default = NA_Date_
         ),
         actual_completion_date = case_when(
           has_euctr_results == TRUE ~ completion_date,
           .default = NA_Date_
         ))

# DRKS inclusion and exclusion criteria
drks_inex <- read_csv(here("data", "processed", "inclusion_exclusion_drks.csv")) |> 
  mutate(completion_date = ymd(completion_date),
         estimated_completion_date = ymd(estimated_completion_date),
         actual_completion_date = case_when(
           is.na(estimated_completion_date) ~ completion_date,
           .default = NA_Date_
         ))
# Ct.gov inclusion and exclusion criteria
ctgov_inex <- read_csv(here("data", "processed", "inclusion_exclusion_ctgov.csv")) |> 
  mutate(estimated_completion_date = case_when(
    completion_date_type == "ESTIMATED" ~ completion_date,
    .default = NA_Date_
  ),
  actual_completion_date = case_when(
    completion_date_type == "ACTUAL" ~ completion_date,
    .default = NA_Date_
  ))


combined_inex <- bind_rows(euctr_inex_deduped, drks_inex, ctgov_inex)

qa_de_clusters <- crossreg_clusters |> 
  left_join(euctr_de_protocols  |> rename(has_linked_de_protocol = has_trial_de_protocol),
            join_by("linked_id" == "eudract_number")) |> 
  left_join(combined_inex, by = "trial_id") |> 
  left_join(combined_inex |> select(linked_id = trial_id, contains("is_")) |> 
              rename_with(\(x) paste0(x, "_linked"), starts_with("is_")),
            by = "linked_id") |> 
  filter(has_trial_de_protocol == FALSE | has_linked_de_protocol == FALSE,
         is_german_umc | is_german_umc_linked)

# number of german_umc but no DE protocol and many_to_many
# thus no effect on has_xxxx_de_protocol on the many_to_many validations
qa_de_clusters |> 
  filter(many_to_many) |> 
  nrow()

qa_de_title_matches <- title_matches_clusters |> 
  left_join(euctr_de_protocols  |> rename(has_linked_de_protocol = has_trial_de_protocol),
            join_by("linked_id" == "eudract_number")) |> 
  left_join(combined_inex, by = "trial_id") |> 
  left_join(combined_inex |> select(linked_id = trial_id, contains("is_")) |> 
              rename_with(\(x) paste0(x, "_linked"), starts_with("is_")),
            by = "linked_id") |> 
  filter(has_trial_de_protocol == FALSE | has_linked_de_protocol == FALSE,
         is_german_umc | is_german_umc_linked)

# number of german_umc but no DE protocol and many_to_many
# Effect on has_xxxx_de_protocol on the many_to_many validations : a single new cluster
# was declustered, to allow sensitivity analysis of including or exlcuding
# has_xxxx_de_protocol data
qa_de_title_matches |> 
  filter(many_to_many_overall) |> 
  nrow()


crossreg_validated <- crossreg_title_ids |>
  filter(many_to_many_overall == FALSE) |>
  select(-many_to_many_overall) |> 
  ### add only validated  mtm here
  bind_rows(mtm_resolved |> select(-old_cluster))

  
#### this is still unfiltered at this point, except for dropping some clusters in validation!
validated_crossreg_ids <- crossreg_validated |>
  select(trial_id, linked_id, crossreg_id = cluster_unique_id) |>    
  pivot_longer(-crossreg_id, names_to = NULL, values_to = "trial_id") |>
  distinct(trial_id, .keep_all = TRUE) |>
  write_excel_csv(here("data", "processed", "crossreg_ids_unfiltered.csv"))

falling_clusters <- validated_crossreg_ids |> 
  left_join(combined_inex, by = "trial_id") |>
  mutate(
    # completion_date_type = case_when(
    # has_euctr_results == FALSE & str_detect(trial_id, "-") ~ "ESTIMATED",
    # has_euctr_results == TRUE & str_detect(trial_id, "-") ~ "ACTUAL",
    # str_detect(trial_id, "DRKS") ~ "ACTUAL",
    # .default = completion_date_type),
    filled_actual_completion_dates = case_when(
      is.na(estimated_completion_date) & !is.na(completion_date) ~ completion_date,
      .default = NA_Date_),
    filled_euctr_completion_date = if_else(has_euctr_results == TRUE, completion_date, NA_Date_)) |>
  group_by(crossreg_id) |> 
  arrange(crossreg_id, desc(last_updated), desc(registration_date)) |> 
  fill(c(filled_actual_completion_dates, filled_euctr_completion_date), .direction = "up") |> 
  mutate(has_euctr_results = any(has_euctr_results, na.rm = TRUE),
         has_interventional = any(is_interventional, na.rm = TRUE),
         has_german_umc = any(is_german_umc, na.rm = TRUE),
         registries = get_registry_names(crossreg_id),
         has_premature = any(has_premature, na.rm = TRUE),
         has_de_premature = any(has_de_premature, na.rm = TRUE),
         has_cluster_de_protocol = any(has_trial_de_protocol, na.rm = TRUE),
         # recent_completion_2018_2021 = first(is_completed_2018_2021),
         # max_completion_date = max(estimated_completion_date, na.rm = TRUE),
         # has_only_estimated_cd = all(is.na(only_actual_completion_dates)),
         hierarchical_completion_date = case_when(
           str_detect(crossreg_id, "-") &
             any(has_euctr_results == TRUE, na.rm = TRUE) &
             !is.na(first(filled_euctr_completion_date)) ~
             first(filled_euctr_completion_date),
           any(!is.na(filled_actual_completion_dates), na.rm = TRUE) ~
             first(filled_actual_completion_dates),
           all(is.na(filled_actual_completion_dates), na.rm = TRUE) ~
             max(estimated_completion_date, na.rm = TRUE),
           # .default = first(only_actual_completion_dates)
           .default = NA_Date_
         ),
         has_completion_2018_2021 = any(between(hierarchical_completion_date,
                                               as_date("2018-01-01"), as_date("2021-12-31")),
                                       na.rm = TRUE),
         recent_status = first(status),
         has_withdrawn_status = any(str_detect(status, regex("withdrawn", ignore_case = TRUE))),
         many_to_many = is_mtm(crossreg_id),
         trns_in_cluster = str_count(crossreg_id, "_") + 1,
         mtm_validated = trial_id %in% mtm_resolved$trial_id |
           trial_id %in% mtm_resolved$linked_id) |> 
  ungroup()


falling_clusters |> 
  write_csv(here("data", "processed", "crossreg_unfiltered.csv"))

qa_hier <- falling_clusters |> 
  filter(has_euctr_results == FALSE, 
         hierarchical_completion_date != filled_actual_completion_dates)

# falling_clusters |>
#   select(-completion_date_type) |> 
#   filter(has_interventional, has_german_umc, has_completion_2018_2021,
#          !has_withdrawn_status) |> 
#   write_csv(here("data", "processed", "qa_completion_dates.csv"))
  

fallen_clusters <- falling_clusters |>   
  filter(has_interventional == FALSE |
           has_german_umc == FALSE |
           has_completion_2018_2021 == FALSE |
           has_withdrawn_status == TRUE)

falling_clusters |> 
  count(status, sort = TRUE)

fallen_clusters |> count(status, sort = TRUE)
#### status exploration
status_euctr_main <- euctr_inex |> 
  filter(status == "Prematurely Ended") |>
  select(contains("eudract"), contains("results_actual_enrollment"), contains("status"), everything())

status_euctr_crossreg <- euctr_inex |> 
  filter(trial_id %in% validated_crossreg_ids$trial_id) |> 
  left_join(validated_crossreg_ids, by = "trial_id") |> 
  group_by(crossreg_id) |> 
  mutate(has_premature = any(status == "Prematurely Ended", na.rm = TRUE),
         de_premature = status == "Prematurely Ended" & str_detect(eudract_number_with_country, "DE"),
         has_de_premature = any(de_premature, na.rm = TRUE),
         has_interventional = any(is_interventional, na.rm = TRUE),
         has_german_umc = any(is_german_umc, na.rm = TRUE)) |>
  filter(has_premature == TRUE, has_german_umc == TRUE, has_interventional == TRUE) |> 
  ungroup() |> 
  select(contains("eudract"), contains("results_actual_enrollment"), contains("status"), everything())


premature_status <- status_euctr_crossreg |>
  distinct(trial_id, .keep_all = TRUE) |> 
  select(trial_id, has_premature, has_de_premature)

fc <- falling_clusters |> 
  filter(crossreg_id %in% premature_cases$crossreg_id) |> 
  left_join(pcases, by = "crossreg_id") |> 
  group_by(crossreg_id) |> 
  mutate(euctr_most_recent = last_updated == max(last_updated, na.rm = TRUE) & str_detect(trial_id, "-")) 
  

non_de <- fc |> 
  filter(euctr_most_recent == TRUE, has_de_premature == FALSE)

fc |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  ungroup() |> 
  count(de_premature, euctr_most_recent)

status_euctr <- euctr_combined |>
  filter(end_of_trial_status == "Prematurely Ended", eudract_number %in% pub_search_table_crossreg$trial_id) |> 
  select(contains("eudract"), contains("results_actual_enrollment"), contains("status"))


### remove EUCTR TRNs without a DE protocol 
crossreg_title_ids_only_de <- crossreg_title_ids |>
  filter(has_trial_de_protocol != FALSE | is.na(has_trial_de_protocol),
         has_linked_de_protocol != FALSE | is.na(has_linked_de_protocol))


# recalculate the clusters
new_clusters <- get_cluster_names_from_pairs(crossreg_title_ids_only_de |>
                                               select(trial_id, linked_id)) |>
  mutate(many_to_many = is_mtm(cluster_unique_id),
         trns_in_cluster = str_count(cluster_unique_id, "_") + 1)


simplified_clusters <- new_clusters |>
  filter(many_to_many == FALSE) |>
  select(-many_to_many)
# 
new_mtm <- new_clusters |>
  filter(many_to_many == TRUE)  |>
  select(-many_to_many)
# 
crossreg_title_ids_new_clusters <- crossreg_title_ids |>
  select(-cluster_unique_id, -many_to_many, -trns_in_cluster) |>
  left_join(new_clusters, by = "trial_id") |>
  select(contains("cluster"), everything(), -many_to_many_overall)
# 
# #unique clusters
crossreg_title_ids_new_clusters |>
  filter(many_to_many == TRUE) |>
  count(cluster_unique_id) |>
  nrow()

crossreg_validated_included <- crossreg_validated |> 
  filter(cl_meets_inclusion == TRUE)|>
  select(-cl_meets_inclusion) |>
  write_excel_csv(here("data", "processed", "crossreg_validated_mtm.csv"))

filtered_clusters <- falling_clusters |>
  filter(has_completion_2018_2021, has_interventional, has_german_umc, !has_withdrawn_status) |> 
  write_excel_csv(here("data", "processed", "crossreg_filtered.csv"))

# cluster check
qa_clusters <- validated_crossreg_ids |> 
  filter(trial_id %in% filtered_clusters$trial_id)

setdiff(qa_clusters$crossreg_id, filtered_clusters$crossreg_id)

setdiff(filtered_clusters$crossreg_id, qa_clusters$crossreg_id)

##########################################################
# old scripts here for potential re-use

# min_title_length <- 5
# 
# # function for chunked title match to be able to process e.g. full registries
# 
# chunked_title_match <- function(tib_1, tib_2, title_col, chunksize, output_filename, max_dist, min_length) {
#   
#   if (nrow(tib_1) > nrow(tib_2)) {
#     tib_large <- tib_1
#     tib_small <- tib_2
#   } else {
#     tib_large <- tib_2
#     tib_small <- tib_1
#   }
#   
#   tib_large <- tib_large |>
#     filter({{ title_col }} >= min_length)
#   
#   tib_small <- tib_small |> 
#     filter({{ title_col }} >= min_length)
#   
#   col_name <- rlang::as_label(rlang::enquo(title_col))
#   # return(col_name)
#   
#   assertthat::assert_that(chunksize <= 50000, msg = "Chunksize should not exeed 50,000!")
#   
#   nrows <- nrow(tib_large)
#   
#   
#   if (nrows > chunksize) { # if the total number of rows exceeds the chunksize
#     chunks <- 1:nrows%%chunksize == 0 # build the chunk numbers
#     chunks <- chunks |>
#       as.numeric() |>
#       cumsum()
#     
#     tib_chunked <- split(tib_large, chunks) # split vector into chunks
#     p <- progressr::progressor(along = tib_chunked)
#     return(furrr::future_map(tib_chunked, \(chunk) {
#       p()
#       fuzzyjoin::stringdist_inner_join(
#         tib_small,
#         chunk,
#         by = col_name,
#         max_dist = max_dist,
#         distance_col = "dist"
#       )
#     }))
#   }
# }
# 
# plan(multisession)
# # plan(sequential)
# handlers(global = TRUE)
# 
# ct_cleaned_bogus <- bogus_titles_ct |> 
#   select(trial_id, official_title = title, title = brief_title) |> 
#   tidyr::drop_na(title) |>
#   mutate(title_processed = tolower(title) |>
#            stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
#            stringr::str_remove_all("[:punct:]") |>
#            stringr::str_remove_all(" "),
#          title_length = str_length(title_processed))
#   
# 
# #QA title matches in the bogus sample using brief_title instead
# # not a single match, so ignore bogus title sample
# tic() # took 5.5 hours with no glitches
# title_matches_ct_drks <- chunked_title_match(ct_cleaned,
#                                                    drks_titles,
#                                                    title_col = title_processed, chunksize = 5000, max_dist = 20, min_length = 5)
# toc()
# 
# matches_ct_drks <- title_matches_ct_drks |> 
#   list_rbind() |> 
#     mutate(rel_match = 1 - dist * 2/ (title_length.x + title_length.y),
#            is_match = rel_match > 0.6 # to be empirically dediced
#     )  |>
#     select(is_match, rel_match, dist, contains("title"), contains("length"), contains("processed"), everything())  |>
#     mutate(x_longer = title_length.x > title_length.y,
#            y_longer = title_length.x < title_length.y,
#            x_in_y = str_detect(title_processed.x, title_processed.y),
#            y_in_x = str_detect(title_processed.y, title_processed.x)
#     ) |> 
#     arrange(desc(rel_match)) |>
#     filter(title_length.x >= min_title_length,
#            title_length.y >= min_title_length)
# 
# matches_ct_drks |> 
#   saveRDS(here("data", "processed", "title_matches_ct_drks.rds"))
# 
# matches_ct_drks <- readRDS(here("data", "processed", "title_matches_ct_drks.rds")) |> 
#   filter(!str_detect(title_processed.y, "bio\\|")) |> 
#   mutate(jarowinkler = stringdist(title_processed.x, title_processed.y, method = "jw"),
#          lcs = stringdist(title_processed.x, title_processed.y, method = "lcs"),
#          is_match = jarowinkler < 0.2) |> 
#   select(rel_match, jarowinkler, lcs, is_match, everything())
# 
# counts <- matches_ct_drks |> 
#   filter(is_match) |> 
#   count(dist, sort = TRUE)
# 
# matches_ct_drks |> 
#   filter(is_match) |>
#   ggplot(aes(dist)) +
#   geom_density()
# 
# embeded_titles <- matches_ct_drks |> 
#   filter(x_in_y | y_in_x,
#          rel_match < 1)
# # tic("joining euctr and drks")
# # titles_joined_euctr_drks <- euctr_updated_titles |> 
# #   fuzzyjoin::stringdist_inner_join(drks_updated_titles, by = "title_processed", max_dist = 20, distance_col = "dist")
# # toc()
# 
# # 
# # title_matches <- titles_joined_euctr_drks |> 
# #   # bind_rows(titles_joined_ct_drks) |> 
# #   mutate(rel_match = 1 - dist * 2/ (title_length.x + title_length.y),
# #          is_match = rel_match > 0.6 # to be empirically dediced
# #   )  |> 
# #   select(is_match, rel_match, dist, contains("official"), contains("length"), contains("processed"), everything())  |> 
# #   mutate(x_longer = title_length.x > title_length.y,
# #          y_longer = title_length.x < title_length.y,
# #          x_in_y = str_detect(title_processed.x, title_processed.y),
# #          y_in_x = str_detect(title_processed.y, title_processed.x)
# #   )
# 
# 
# # title_matches_drks_euctr <- title_matches |> 
# #   arrange(desc(rel_match)) |> 
# #   filter(title_length.x >= min_title_length,
# #          title_length.y >= min_title_length)
# # title_matches_drks_euctr |> 
# #   saveRDS(here("data", "processed", "title_matches_drks_euctr_20.rds"))
# 
# 
# # 
# # bogus_titles_euctr <- euctr_updated_titles |>
# #   filter(str_detect(official_title, "^(N|n)(o|O|a)[nNtT]?|see") & title_length < 20
# #   )
# # 
# # euctr_cleaned <- euctr_updated_titles |> 
# #   filter(!trial_id %in% bogus_titles_euctr$trial_id)
# 
# tic()
# ct_subresult <- chunked_title_match(ct_cleaned,
#                                     drks_updated_titles,
#                                     title_col = title_processed, chunksize = 10000, max_dist = 20, min_length = 5)
# toc()
# 
# ct_sbr <- ct_subresult |> 
#   list_rbind() 
# 
# title_matches_ct_drks <- ct_sbr |> 
#   mutate(rel_match = 1 - dist * 2/ (title_length.x + title_length.y),
#          is_match = rel_match > 0.6 # to be empirically dediced
#   )  |> 
#   select(is_match, rel_match, dist, contains("official"), contains("length"), contains("processed"), everything())  |> 
#   mutate(x_longer = title_length.x > title_length.y,
#          y_longer = title_length.x < title_length.y,
#          x_in_y = str_detect(title_processed.x, title_processed.y),
#          y_in_x = str_detect(title_processed.y, title_processed.x)
#   )
# 
# title_matches_ct_drks <- title_matches_ct_drks |> 
#   arrange(desc(rel_match)) |> 
#   filter(title_length.x >= min_title_length,
#          title_length.y >= min_title_length)
# title_matches_ct_drks |> 
#   saveRDS(here("data", "processed", "title_matches_ct_drks_20.rds"))
### as doing the ct_cleaned in one go fails, split the dataset into two chunks
# ct_cleaned_first_half <- ct_cleaned |> 
#   slice(1:230000)
# ct_cleaned_second_half <- ct_cleaned |> 
#   filter(!trial_id %in% ct_cleaned_first_half$trial_id)


# tic() 
# title_matches_ct_euctr_ls <- chunked_title_match(ct_cleaned_first_half,
#                                                  euctr_cleaned,
#                                                  title_col = title_processed, chunksize = 5000, max_dist = 20, min_length = 5)
# toc()

# tic()
# title_matches_ct_euctr_ls2 <- chunked_title_match(ct_cleaned_second_half,
#                                                   euctr_cleaned,
#                                                   title_col = title_processed, chunksize = 5000, max_dist = 20, min_length = 5)
# toc()

# title_matches_ct_euctr <- list_rbind(title_matches_ct_euctr_ls) |> 
#   bind_rows(list_rbind(title_matches_ct_euctr_ls2))
# 
# title_matches_ct_euctr <- title_matches_ct_euctr |> 
#   mutate(rel_match = 1 - dist * 2/ (title_length.x + title_length.y),
#          is_match = rel_match > 0.6 # to be empirically dediced
#   )  |> 
#   select(is_match, rel_match, dist, contains("official"), contains("length"), contains("processed"), everything())  |> 
#   mutate(x_longer = title_length.x > title_length.y,
#          y_longer = title_length.x < title_length.y,
#          x_in_y = str_detect(title_processed.x, title_processed.y),
#          y_in_x = str_detect(title_processed.y, title_processed.x)
#   )
# 
# title_matches_ct_euctr <- title_matches_ct_euctr |> 
#   arrange(desc(rel_match)) |> 
#   filter(title_length.x >= min_title_length,
#          title_length.y >= min_title_length)
# title_matches_ct_euctr |> 
#   saveRDS(here("data", "processed", "title_matches_ct_euctr_20.rds"))
