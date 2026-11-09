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
euctr_processed <- readRDS(here("data", "processed", "euctr_processed.rds"))

euctr_de_protocols <- euctr_processed |> 
  group_by(eudract_number) |> 
  summarise(has_trial_de_protocol = any(has_trial_de_protocol, na.rm = TRUE), .groups = "drop")

# Process EU titles
euctr_titles <- euctr_processed |>
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
sample_ids <- c(EUCTR_sample$trial_id, DRKS_sample$trial_id, CTgov_sample$trial_id) |>
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
  write_csv(here("data", "processed", "crossreg_title_ids.csv"))

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
  filter(!is.na(other_nr_ctgov),
             !is.na(other_nr_euctr)) |> 
  mutate(other_match = str_detect(other_nr_ctgov, coll(other_nr_euctr)) |
           str_detect(other_nr_euctr, coll(other_nr_ctgov)),
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
  arrange(desc(is_trial_de_protocol), desc(registration_date)) |> 
  fill(estimated_completion_date, .direction = "up") |> 
  summarise(across(everything(), first)) |> # this takes first also for is_trial_de_protocol == FALSE
  select(-eudract_number_with_country, -is_trial_de_protocol, -de_premature, has_euctr_results = results_reporting) |> 
  ungroup() |>
  mutate(across(contains("completion_date"),  ymd))

# DRKS inclusion and exclusion criteria
drks_inex <- read_csv(here("data", "processed", "inclusion_exclusion_drks.csv")) |> 
  mutate(completion_date = ymd(completion_date),
         estimated_completion_date = ymd(estimated_completion_date),
         actual_completion_date = case_when(
           !is.na(completion_date) ~ completion_date,
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
#validated_crossreg_ids <- read_csv(here("data", "processed", "crossreg_ids_unfiltered.csv"))
# falling_clusters <- validated_crossreg_ids |> 
#   left_join(combined_inex, by = "trial_id") |>
#   mutate(
#     filled_actual_completion_date = case_when(
#       !is.na(actual_completion_date) ~ actual_completion_date,
#       .default = NA_Date_),
#     filled_euctr_completion_date = if_else(has_euctr_results == TRUE, results_completion_date, NA_Date_)) |>
#   group_by(crossreg_id) |> 
#   arrange(crossreg_id, desc(last_updated), desc(registration_date)) |> 
#   fill(c(filled_actual_completion_date, filled_euctr_completion_date), .direction = "up") |> 
#   mutate(has_euctr_results = any(has_euctr_results, na.rm = TRUE),
#          has_interventional = any(is_interventional, na.rm = TRUE),
#          has_german_umc = any(is_german_umc, na.rm = TRUE),
#          registries = get_registry_names(crossreg_id),
#          has_premature = any(has_premature, na.rm = TRUE),
#          has_de_premature = any(has_de_premature, na.rm = TRUE),
#          has_cluster_de_protocol = any(has_trial_de_protocol, na.rm = TRUE),
#          # recent_completion_2018_2021 = first(is_completed_2018_2021),
#          # max_completion_date = max(estimated_completion_date, na.rm = TRUE),
#          # has_only_estimated_cd = all(is.na(only_actual_completion_dates)),
#          hierarchical_completion_date = case_when(
#            str_detect(crossreg_id, "-") &
#              any(has_euctr_results == TRUE, na.rm = TRUE) &
#              !is.na(first(filled_euctr_completion_date)) ~
#              first(filled_euctr_completion_date),
#            any(!is.na(filled_actual_completion_date), na.rm = TRUE) ~
#              first(filled_actual_completion_date),
#            all(is.na(filled_actual_completion_date), na.rm = TRUE) ~
#              max(estimated_completion_date, na.rm = TRUE),
#            .default = NA_Date_
#          ),
#          last_updated_actual = if_else(!is.na(actual_completion_date), last_updated, NA_Date_),
#          only_estimated = all(is.na(results_completion_date), na.rm = TRUE) &
#            all(is.na(actual_completion_date), na.rm = TRUE),
#          is_last_updated = last_updated_actual == max(last_updated_actual, na.rm = TRUE),
#          n_last_updated = sum(is_last_updated, na.rm = TRUE),
#          is_latest_actual = actual_completion_date == max(actual_completion_date, na.rm = TRUE),
#          is_latest_estimated = estimated_completion_date == max(estimated_completion_date, na.rm = TRUE),
#          n_latest_estimated = sum(is_latest_estimated, na.rm = TRUE),
#          is_index_reg = case_when(
#            !is.na(results_completion_date) ~ TRUE,
#            all(is.na(results_completion_date)) & 
#              n_last_updated == 1 &
#              is_last_updated & only_estimated == FALSE ~ TRUE,
#            all(is.na(results_completion_date)) & 
#              n_last_updated > 1 &
#              is_latest_actual & only_estimated == FALSE ~ TRUE,
#            only_estimated & is_latest_estimated & n_latest_estimated == 1 ~ TRUE,
#            only_estimated & is_last_updated & n_latest_estimated > 1 ~ TRUE,
#            .default = FALSE
#          ),
#          n_index_reg = sum(is_index_reg),
#          has_completion_2018_2021 = any(between(hierarchical_completion_date,
#                                                 as_date("2018-01-01"), as_date("2021-12-31")),
#                                         na.rm = TRUE),
#          recent_status = first(status),
#          has_withdrawn_status = any(str_detect(status, regex("withdrawn", ignore_case = TRUE))),
#          many_to_many = is_mtm(crossreg_id),
#          trns_in_cluster = str_count(crossreg_id, "_") + 1,
#          mtm_validated = trial_id %in% mtm_resolved$trial_id |
#            trial_id %in% mtm_resolved$linked_id) |> 
#   ungroup()
# 

falling_clusters <- validated_crossreg_ids |> 
  left_join(combined_inex, by = "trial_id") |> 
  get_hierarchical_completion_date() |> 
  group_by(crossreg_id) |> 
  mutate(has_premature = any(has_premature, na.rm = TRUE),
         has_de_premature = any(has_de_premature, na.rm = TRUE),
         mtm_validated = trial_id %in% mtm_resolved$trial_id |
           trial_id %in% mtm_resolved$linked_id) |> 
  ungroup()

qa_index <- falling_clusters |> 
  filter(is_index_reg == TRUE,
         hierarchical_completion_date != completion_date)

test_cases <- tribble(~crossreg_id, ~verified_cluster_completion_date,
                      "2017-005032-42_NCT04057261", ymd("2022-11-30"),
                      "2013-001884-21_NCT01829347", ymd("2015-09-16"), # because EUCTR results first
                      "2013-001884-21_NCT01829347", ymd("2015-09-16"), # because EUCTR results first
                      "2010-019885-10_NCT01315990", ymd("2017-07-09"), # because EUCTR results first
                      "2020-003503-34_NCT04631666", ymd("2021-08-11"),
                      "2014-003647-34_NCT02310243", ymd("2020-06-03"), 
                      "2006-006962-41_NCT02543749", ymd("2022-07-31"),
                      "2017-002468-41_NCT03645980", ymd("2022-08-31"),
                      "2007-007262-38_NCT00777244", ymd("2020-12-31"),
                      "2012-000620-17_DRKS00005503_NCT02531841", ymd("2022-02-25"),
                      "2016-004396-51_DRKS00011374_NCT03669185", ymd("2022-02-07"),
                      "2013-003714-40_DRKS00014559_NCT02615938", ymd("2025-04-30"),
                      "2016-001179-60_DRKS00017467_NCT02961257", ymd("2021-08-31"),
                      "2016-001815-19_DRKS00013701_NCT03362359", ymd("2020-07-03"), # because EUCTR results first
                      "2015-000465-31_DRKS00012657", ymd("2019-11-25"),
                      "2016-001554-18_DRKS00008018", ymd("2020-09-29"),
                      "2015-005219-34_DRKS00009451", ymd("2021-08-03"),
                      "2011-004228-37_DRKS00004186", ymd("2021-03-08"),
                      "2017-002429-39_DRKS00014953_NCT03437226", ymd("2021-12-31")
)

cluster_check <- falling_clusters |>
  inner_join(test_cases, by = "crossreg_id", relationship = "many-to-many")

cluster_check |> 
  filter(hierarchical_completion_date != verified_cluster_completion_date) |>
  nrow() |> 
  testthat::expect_equal(0)

falling_clusters |> 
  write_csv(here("data", "processed", "crossreg_unfiltered.csv"))

qa_hierarchical <- falling_clusters |> 
  filter(has_euctr_results == FALSE, 
         # is_index_reg == TRUE,
         hierarchical_completion_date != filled_actual_completion_date)

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


# status_euctr <- euctr_processed |>
#   filter(end_of_trial_status == "Prematurely Ended", eudract_number %in% pub_search_table_crossreg$trial_id) |> 
#   select(contains("eudract"), contains("results_actual_enrollment"), contains("status"))


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

# crossreg_validated_included <- crossreg_validated |> 
#   filter(cl_meets_inclusion == TRUE)|>
#   select(-cl_meets_inclusion) |>
#   write_excel_csv(here("data", "processed", "crossreg_validated_mtm.csv"))

filtered_clusters <- falling_clusters |>
  filter(has_completion_2018_2021, has_interventional, has_german_umc, !has_withdrawn_status) |> 
  write_excel_csv(here("data", "processed", "crossreg_filtered.csv"))

# cluster check
qa_clusters <- validated_crossreg_ids |> 
  filter(trial_id %in% filtered_clusters$trial_id)


setdiff(qa_clusters$crossreg_id, filtered_clusters$crossreg_id)

setdiff(filtered_clusters$crossreg_id, qa_clusters$crossreg_id)

