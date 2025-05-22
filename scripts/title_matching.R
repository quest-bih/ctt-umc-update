# Script to title match trials in GTL analysis set

library(tidyverse)
library(here)
library(ctregistries)
library(jsonlite)
library(tictoc)
library(furrr)
library(progressr)
library(stringdist)

##########################################################

# Ct.gov preprocessing
AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_250513")
AACT_datasets <- load_AACT_datasets(AACT_folder, "studies")

# TODO: shall we salvage bogus official titles by looking into the brief_title instead?
ct_titles <- AACT_datasets$studies |> 
  select(trial_id = nct_id, title = official_title, brief_title) |> 
  tidyr::drop_na(title) |>
  mutate(title_processed = tolower(title) |>
           stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
           stringr::str_remove_all("[:punct:]") |>
           stringr::str_remove_all(" "),
         title_length = str_length(title_processed))

qa_titles <- AACT_datasets$studies |> 
  select(nct_id, contains("title"))

### remove bogus titles from ct
bogus_titles_ct <- ct_titles |> 
  filter(str_detect(title, "^(Principal Investigator|Corresponding)|\\bDirector") |
           str_detect(title, "University(?! \\w+s\\b)") & title_length <= 50,
         trial_id != "NCT02038387",
         !str_detect(title, "C(ohort|OHORT)|Study|\\bin\\b|Program")
  )

ct_cleaned <- ct_titles |> 
  filter(!trial_id %in% bogus_titles_ct$trial_id)
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
  mutate(title_processed = tolower(title) |>
           stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
           stringr::str_remove_all("[:punct:]") |>
           stringr::str_remove_all(" "),
         title_length = str_length(title_processed))


###########################################################

# Extract EU IDs and country data (for filtering), plus process EU titles to be used in title matching algorithm

# Process EU titles
# euctr_updated_titles <- euctr_flagged |>
#   filter(in_mdr == 0) |> 
#   tidyr::drop_na(official_title) |>
#   distinct(trial_id, official_title, .keep_all = TRUE) |>
#   mutate(title_processed = tolower(official_title) |>
#            stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
#            stringr::str_remove_all("[:punct:]") |>
#            stringr::str_remove_all(" "),
#          title_length = str_length(title_processed))


##########################################################


min_title_length <- 5

# function for chunked title match to be able to process e.g. full registries

chunked_title_match <- function(tib_1, tib_2, title_col, chunksize, output_filename, max_dist, min_length) {
  
  if (nrow(tib_1) > nrow(tib_2)) {
    tib_large <- tib_1
    tib_small <- tib_2
  } else {
    tib_large <- tib_2
    tib_small <- tib_1
  }
  
  tib_large <- tib_large |>
    filter({{ title_col }} >= min_length)
  
  tib_small <- tib_small |> 
    filter({{ title_col }} >= min_length)
  
  col_name <- rlang::as_label(rlang::enquo(title_col))
  # return(col_name)
  
  assertthat::assert_that(chunksize <= 50000, msg = "Chunksize should not exeed 50,000!")
  
  nrows <- nrow(tib_large)
  
  
  if (nrows > chunksize) { # if the total number of rows exceeds the chunksize
    chunks <- 1:nrows%%chunksize == 0 # build the chunk numbers
    chunks <- chunks |>
      as.numeric() |>
      cumsum()
    
    tib_chunked <- split(tib_large, chunks) # split vector into chunks
    p <- progressr::progressor(along = tib_chunked)
    return(furrr::future_map(tib_chunked, \(chunk) {
      p()
      fuzzyjoin::stringdist_inner_join(
        tib_small,
        chunk,
        by = col_name,
        max_dist = max_dist,
        distance_col = "dist"
      )
    }))
  }
}

plan(multisession)
# plan(sequential)
handlers(global = TRUE)

ct_cleaned_bogus <- bogus_titles_ct |> 
  select(trial_id, official_title = title, title = brief_title) |> 
  tidyr::drop_na(title) |>
  mutate(title_processed = tolower(title) |>
           stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
           stringr::str_remove_all("[:punct:]") |>
           stringr::str_remove_all(" "),
         title_length = str_length(title_processed))
  

#QA title matches in the bogus sample using brief_title instead
# not a single match, so ignore bogus title sample
tic() # took 5.5 hours with no glitches
title_matches_ct_drks <- chunked_title_match(ct_cleaned,
                                                   drks_titles,
                                                   title_col = title_processed, chunksize = 5000, max_dist = 20, min_length = 5)
toc()

matches_ct_drks <- title_matches_ct_drks |> 
  list_rbind() |> 
    mutate(rel_match = 1 - dist * 2/ (title_length.x + title_length.y),
           is_match = rel_match > 0.6 # to be empirically dediced
    )  |>
    select(is_match, rel_match, dist, contains("title"), contains("length"), contains("processed"), everything())  |>
    mutate(x_longer = title_length.x > title_length.y,
           y_longer = title_length.x < title_length.y,
           x_in_y = str_detect(title_processed.x, title_processed.y),
           y_in_x = str_detect(title_processed.y, title_processed.x)
    ) |> 
    arrange(desc(rel_match)) |>
    filter(title_length.x >= min_title_length,
           title_length.y >= min_title_length)

matches_ct_drks |> 
  saveRDS(here("data", "processed", "title_matches_ct_drks.rds"))

matches_ct_drks <- readRDS(here("data", "processed", "title_matches_ct_drks.rds")) |> 
  filter(!str_detect(title_processed.y, "bio\\|")) |> 
  mutate(jarowinkler = stringdist(title_processed.x, title_processed.y, method = "jw"),
         lcs = stringdist(title_processed.x, title_processed.y, method = "lcs"),
         is_match = jarowinkler < 0.2) |> 
  select(rel_match, jarowinkler, lcs, is_match, everything())

counts <- matches_ct_drks |> 
  filter(is_match) |> 
  count(dist, sort = TRUE)

matches_ct_drks |> 
  filter(is_match) |>
  ggplot(aes(dist)) +
  geom_density()

embeded_titles <- matches_ct_drks |> 
  filter(x_in_y | y_in_x,
         rel_match < 1)
# tic("joining euctr and drks")
# titles_joined_euctr_drks <- euctr_updated_titles |> 
#   fuzzyjoin::stringdist_inner_join(drks_updated_titles, by = "title_processed", max_dist = 20, distance_col = "dist")
# toc()

# 
# title_matches <- titles_joined_euctr_drks |> 
#   # bind_rows(titles_joined_ct_drks) |> 
#   mutate(rel_match = 1 - dist * 2/ (title_length.x + title_length.y),
#          is_match = rel_match > 0.6 # to be empirically dediced
#   )  |> 
#   select(is_match, rel_match, dist, contains("official"), contains("length"), contains("processed"), everything())  |> 
#   mutate(x_longer = title_length.x > title_length.y,
#          y_longer = title_length.x < title_length.y,
#          x_in_y = str_detect(title_processed.x, title_processed.y),
#          y_in_x = str_detect(title_processed.y, title_processed.x)
#   )


# title_matches_drks_euctr <- title_matches |> 
#   arrange(desc(rel_match)) |> 
#   filter(title_length.x >= min_title_length,
#          title_length.y >= min_title_length)
# title_matches_drks_euctr |> 
#   saveRDS(here("data", "processed", "title_matches_drks_euctr_20.rds"))


# 
# bogus_titles_euctr <- euctr_updated_titles |>
#   filter(str_detect(official_title, "^(N|n)(o|O|a)[nNtT]?|see") & title_length < 20
#   )
# 
# euctr_cleaned <- euctr_updated_titles |> 
#   filter(!trial_id %in% bogus_titles_euctr$trial_id)

tic()
ct_subresult <- chunked_title_match(ct_cleaned,
                                    drks_updated_titles,
                                    title_col = title_processed, chunksize = 10000, max_dist = 20, min_length = 5)
toc()

ct_sbr <- ct_subresult |> 
  list_rbind() 

title_matches_ct_drks <- ct_sbr |> 
  mutate(rel_match = 1 - dist * 2/ (title_length.x + title_length.y),
         is_match = rel_match > 0.6 # to be empirically dediced
  )  |> 
  select(is_match, rel_match, dist, contains("official"), contains("length"), contains("processed"), everything())  |> 
  mutate(x_longer = title_length.x > title_length.y,
         y_longer = title_length.x < title_length.y,
         x_in_y = str_detect(title_processed.x, title_processed.y),
         y_in_x = str_detect(title_processed.y, title_processed.x)
  )

title_matches_ct_drks <- title_matches_ct_drks |> 
  arrange(desc(rel_match)) |> 
  filter(title_length.x >= min_title_length,
         title_length.y >= min_title_length)
title_matches_ct_drks |> 
  saveRDS(here("data", "processed", "title_matches_ct_drks_20.rds"))


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
