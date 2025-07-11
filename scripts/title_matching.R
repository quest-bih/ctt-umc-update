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
  mutate(has_german_protocol = str_detect(eudract_number_with_country, "DE$")) |> 
  arrange(trial_id.x, desc(has_german_protocol)) |> 
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
  mutate(has_german_protocol = str_detect(eudract_number_with_country, "DE$")) |> 
  arrange(trial_id.x, desc(has_german_protocol)) |> 
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


EUCTR_sample <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
DRKS_sample <- read_csv(here("data", "processed", "DRKS_sample.csv"))
CTgov_sample <- read_csv(here("data", "processed", "CTgov_sample.csv"))


sample_ids <- c(EUCTR_sample$eudract_number, DRKS_sample$drksId, CTgov_sample$nct_id) |> 
  unique()

# filter for TRNs in the validated umc sample inclusion filter here 
title_matches_included <- title_matches |> 
  filter(trial_id %in% sample_ids |
           linked_id %in% sample_ids | trial_id == "2016-002673-35")

title_matches_included |> 
  write_excel_csv(here("data", "processed", "title_matches.csv"))


crossreg_euctr_drks_ctgov <- read_csv(here("data", "processed", "crossreg_euctr_drks_ctgov.csv"))
  
crossreg_euctr_drks_ctgov_included <- crossreg_euctr_drks_ctgov |> 
  mutate(linked_id = strsplit(linked_id, ";")) |> 
  unnest(linked_id) |> 
  filter(trial_id %in% sample_ids |
           linked_id %in% sample_ids) |> 
  distinct(binary_id, .keep_all = TRUE)

included_by_reference <- crossreg_euctr_drks_ctgov |> 
  filter(trial_id %in% crossreg_euctr_drks_ctgov_included$trial_id |
           linked_id %in% crossreg_euctr_drks_ctgov_included$trial_id |
           trial_id %in% crossreg_euctr_drks_ctgov_included$linked_id |
           linked_id %in% crossreg_euctr_drks_ctgov_included$linked_id) |> 
  mutate(linked_id = strsplit(linked_id, ";")) |> 
  unnest(linked_id) |>
  distinct(binary_id, .keep_all = TRUE)

title_matched_not_id_matched <- title_matches_included |> 
  filter(!trial_id %in% included_by_reference$trial_id,
         !trial_id %in% included_by_reference$linked_id,
         !linked_id %in% included_by_reference$trial_id,
         !linked_id %in% included_by_reference$linked_id) 

mtm_title_not_id <- title_matched_not_id_matched |> 
  filter(many_to_many == TRUE) |> 
  group_by(trial_id) |> 
  mutate(linked_id = deduplicate_collapsed(linked_id)) |> 
  distinct(trial_id, .keep_all = TRUE)

title_matched_not_id_matched <- title_matched_not_id_matched |> 
  filter(!trial_id %in% mtm_title_not_id$trial_id) |> 
  bind_rows(mtm_title_not_id) |> 
  rowwise() |> 
  mutate(binary_id = case_when(
    str_detect(trial_id, "\\-") ~ paste(c(trial_id, linked_id), collapse = "_"),
    str_detect(linked_id, "NCT") ~ paste(c(trial_id, linked_id), collapse = "_"),
    .default = paste(c(linked_id, trial_id), collapse = "_")
  ), .before = 1) |> 
  ungroup()

title_matched_id_matched <- title_matches_included |> 
  filter(!trial_id %in% title_matched_not_id_matched$trial_id)

mtm_title_id <- title_matched_id_matched |> 
  filter(many_to_many == TRUE) |> 
  group_by(trial_id) |> 
  mutate(linked_id = deduplicate_collapsed(linked_id)) |> 
  distinct(trial_id, .keep_all = TRUE)

mtm_title <- mtm_title_id |> 
  bind_rows(mtm_title_not_id) |> 
  mutate(binary_id = paste(trial_id, "_", linked_id))

title_matched_id_matched <- title_matched_id_matched |> 
  filter(!trial_id %in% mtm_title_id$trial_id) |> 
  bind_rows(mtm_title_id) |> 
  rowwise() |> 
  mutate(binary_id = case_when(
    str_detect(trial_id, "\\-") ~ paste(c(trial_id, linked_id), collapse = "_"),
    str_detect(linked_id, "NCT") ~ paste(c(trial_id, linked_id), collapse = "_"),
    .default = paste(c(linked_id, trial_id), collapse = "_")
  ), .before = 1) |> 
  ungroup()

title_matches_only <- title_matched_id_matched |> 
  anti_join(crossreg_euctr_drks_ctgov, by = "binary_id") |> 
  bind_rows(title_matched_not_id_matched) |> 
  mutate(via_id = FALSE, many_to_many = trial_id %in% mtm_title$trial_id)

crossreg_title_ids <- included_by_reference |> 
  semi_join(title_matched_id_matched, by = "binary_id") |> 
  mutate(via_title = TRUE, many_to_many = trial_id %in% mtm_title$trial_id) |> 
  rows_upsert(title_matches_only, by = "binary_id")

crossreg_title_ids <- included_by_reference |>
  mutate(via_title = binary_id %in% crossreg_title_ids$binary_id) |>
  rows_upsert(crossreg_title_ids, by = "binary_id") |>
  group_by(trial_id) |>
  mutate(
    many_to_many = case_when(
      n() > 2 ~ TRUE,
      sum(str_detect(linked_id, "DRKS")) > 1 ~ TRUE,
      sum(str_detect(linked_id, "NCT")) > 1 ~ TRUE,
      # sum(str_detect(linked_id, "-")) > 1 ~ TRUE,
      .default = any(many_to_many, na.rm = TRUE)
    ),
    triad = any(triad, na.rm = TRUE)) |>
  ungroup()

crossreg_title_ids |> 
  write_csv(here("data", "processed", "crossreg_titles_ids.csv"))

# how many links (not triads, not many_to_many)
qa_crossreg_title_ids <- crossreg_title_ids |> 
  filter(many_to_many == FALSE, triad == FALSE) |> 
  mutate(in_sample = trial_id %in% sample_ids |
                               linked_id %in% sample_ids)
nrow(qa_crossreg_title_ids)

# how many many_to_many
qa_mtm <- crossreg_title_ids |> 
  filter(many_to_many == TRUE) |> 
  mutate(in_sample = trial_id %in% sample_ids |
           linked_id %in% sample_ids)
nrow(qa_mtm)
# how many triads
qa_triads <- crossreg_title_ids |> 
  distinct(binary_id, .keep_all = TRUE) |> 
  filter(triad == TRUE) |> 
  group_by(trial_id) |> 
  mutate(n_id = n()) |> 
  group_by(linked_id) |> 
  mutate(n_linked = n()) |> 
  ungroup() |> 
  mutate(in_sample = trial_id %in% sample_ids |
           linked_id %in% sample_ids)
nrow(qa_triads) / 3


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
