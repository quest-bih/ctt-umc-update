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
  summarise(trial_de_protocol = any(str_detect(eudract_number_with_country, "DE"), na.rm = TRUE))

# Process EU titles
euctr_titles <- euctr_combined |>
  tidyr::drop_na(full_title_of_the_trial) |>
  mutate(title_processed = str_remove_all(full_title_of_the_trial, "\\n.*") |> 
           process_title(),
         title_length = str_length(title_processed)) |> 
  select(trial_id = eudract_number, eudract_number_with_country, 
         title = full_title_of_the_trial,
         title_processed, title_length)

# multiline_titles <- euctr_updated_titles |> 
#   # filter(eudract_number %in% c("2021-000369-34")) |> 
#   filter(str_detect(full_title_of_the_trial, "\\n \\n")) |> 
#   mutate(first_title = str_remove_all(full_title_of_the_trial, "\\n.*")) |> 
#   select(full_title_of_the_trial, first_title, eudract_number, eudract_number_with_country) 

title_matches_euctr_drks <- euctr_titles |> 
  inner_join(drks_titles, by = "title_processed",
             relationship = "many-to-many") |> 
  mutate(trial_de_protocol = str_detect(eudract_number_with_country, "DE$")) |> 
  arrange(trial_id.x, desc(trial_de_protocol)) |> 
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
  mutate(trial_de_protocol = str_detect(eudract_number_with_country, "DE$")) |> 
  arrange(trial_id.x, desc(trial_de_protocol)) |> 
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
  ungroup()

title_matches_included <- title_matches_clusters |>
  filter(in_sample == TRUE) |>
  rowwise() |>
  mutate(many_to_many_overall = is_mtm(cluster_unique_id),
         binary_id = get_binary_id(c(trial_id, linked_id))) |>
  ungroup()

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
  left_join(euctr_de_protocols |> rename(linked_de_protocol = trial_de_protocol), by = c("linked_id" = "eudract_number")) 


# Number unique TRNs?= unique clusters
crossreg_title_ids |>
  distinct(cluster_unique_id) |>
  nrow()

crossreg_title_ids_mtm <- crossreg_title_ids |>
  filter(many_to_many_overall)

crossreg_title_ids |> 
  count(trial_de_protocol)

### remove EUCTR TRNs without a DE protocol
crossreg_title_ids <- crossreg_title_ids |> 
  filter(trial_de_protocol != FALSE | is.na(trial_de_protocol),
         linked_de_protocol != FALSE | is.na(linked_de_protocol))

# and recalculate the clusters
new_clusters <- get_cluster_names_from_pairs(crossreg_title_ids |>
                                               select(trial_id, linked_id)) |>
  mutate(many_to_many = is_mtm(cluster_unique_id),
         trns_in_cluster = str_count(cluster_unique_id, "_") + 1)


simplified_clusters <- new_clusters |>
  filter(many_to_many == FALSE) |>
  select(-many_to_many)

new_mtm <- new_clusters |>
  filter(mtm == TRUE)  |>
  select(-mtm)

crossreg_title_ids_new_clusters <- crossreg_title_ids |> 
  select(-cluster_unique_id, -many_to_many, -trns_in_cluster) |> 
  left_join(new_clusters, by = "trial_id") |> 
  select(contains("cluster"), everything(), -many_to_many_overall)

#unique clusters
crossreg_title_ids_new_clusters |>
  filter(many_to_many == TRUE) |> 
  count(cluster_unique_id) |>
  nrow()

crossreg_title_ids <- crossreg_title_ids_new_clusters |> 
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
         binary_id %in% c(crossreg_tp$binary_id) |
           trial_id %in% simplified_clusters$trial_id |
           linked_id %in% simplified_clusters$trial_id) |>
  select(-cluster_unique_id) |>
  left_join(simplified_clusters, by = "trial_id") |>
  left_join(simplified_clusters, by = c("linked_id" = "trial_id")) |>
  left_join(crossreg_tp, by = "binary_id") |>
  mutate(cluster_unique_id = case_when(
    !is.na(cluster_unique_id) ~ cluster_unique_id,
    cluster_unique_id.x == binary_id ~ binary_id,
    .default = NA
  ),
  many_to_many = is_mtm(cluster_unique_id),
  trns_in_cluster = str_count(cluster_unique_id, "_") + 1) |>
  filter(!is.na(cluster_unique_id)) |>
  select(-contains(".x"), -contains(".y"))


crossreg_validated <- crossreg_title_ids |>
  filter(many_to_many_overall == FALSE) |>
  ### add only validated  mtm here
  bind_rows(mtm_resolved) |>
  mutate(trial_id_meets_inclusion = trial_id %in% sample_ids,
         linked_id_meets_inclusion = linked_id %in% sample_ids) |>
  group_by(cluster_unique_id) |> 
  mutate(cl_meets_inclusion = any(trial_id_meets_inclusion) |
           any(linked_id_meets_inclusion)) |>
  ungroup() |> 
  select(
    -many_to_many_overall, -contains("in_sample"), -selfref)

falling_clusters <- crossreg_validated |> 
  filter(cl_meets_inclusion == FALSE)

crossreg_validated_included <- crossreg_validated |> 
  filter(cl_meets_inclusion == TRUE)|>
  select(-cl_meets_inclusion) |> 
  write_excel_csv(here("data", "processed", "crossreg_validated_mtm.csv"))

validated_crossreg_ids <- crossreg_validated |>
  select(trial_id, linked_id, crossreg_id = cluster_unique_id) |>
  pivot_longer(-crossreg_id, names_to = NULL, values_to = "trial_id") |>
  distinct(trial_id, .keep_all = TRUE) |> 
  write_excel_csv(here("data", "processed", "crossreg_ids.csv"))


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
