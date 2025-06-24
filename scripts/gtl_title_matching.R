library(here)
library(tidyverse)
library(janitor)
library(stringdist)


source(here("scripts", "utils.R"))

title_matches_gtl_validation <- readRDS(here("data", "processed", "title_matches_gtl_20.rds"))
ctgov_aliases <- read_csv(here("data", "processed", "ctgov_aliases.csv"))

title_matches_gtl_validation <- title_matches_gtl_validation |> 
  select(rel_match:trial_id.x, trial_id.y, x_in_y, y_in_x) |> 
  mutate(jarowinkler = stringdist(title_processed.x, title_processed.y, method = "jw", p = 0.1),
         is_match = jarowinkler <= 0.1, shorter_title_in_longer_title = x_in_y | y_in_x) |> 
  arrange(jarowinkler) |> 
  group_by(is_match) |> 
  mutate(is_match = jarowinkler <= 0.1, pair_n = case_when(
    is_match == TRUE ~ -n():-1,
    .default = 0:(n() - 1)
  ), .before = 1)


multiples <- title_matches_gtl_validation |> 
  # filter(is_match) |> 
  rowwise() |> 
  mutate(trial_id.y = update_ctgov_alias(trial_id.y, ctgov_aliases)) |> 
  ungroup() |> 
  get_dupes(trial_id.x) |> 
  select(contains("trial_id"), everything())

multiples_y <- title_matches_gtl_validation |> 
  # filter(is_match) |>
  rowwise() |> 
  mutate(trial_id.y = update_ctgov_alias(trial_id.y, ctgov_aliases)) |> 
  ungroup() |> 
  get_dupes(trial_id.y) |> 
  select(contains("trial_id"), everything())

multiples_all <- multiples |> 
  bind_rows(multiples_y) |>
  write_excel_csv(here("data", "processed", "many_to_many_title_matches_gtl.csv"))


title_matches_gtl_validation_multiples <- title_matches_gtl_validation |> 
  mutate(many_to_many = trial_id.x %in% multiples_all$trial_id.x) |> 
  write_excel_csv(here("data", "processed", "title_matches_gtl_20_mtm.csv"))


title_matches_gtl_validation |> 
  ggplot(aes(jaro, jarowinkler)) + geom_point()

process_title <- function(str_title) {
  stringr::str_remove_all(str_title,
                 stringr::regex("\\b(an?)\\b(?!\\-)|\\bN\\/?A\\b", ignore_case = TRUE)) |> 
    tolower() |> 
    stringr::str_remove_all("[:punct:]|(^aa\\s)|\\W")
}
### simplified approach with removal of articles


title_matches_gtl_validation2 <- title_matches_gtl_validation |> 
  select(rel_match:trial_id.x, trial_id.y, x_in_y, y_in_x) |> 
  mutate(title_postprocessed.x = process_title(official_title.x),
         title_postprocessed.y = process_title(official_title.y),
         dl_dist = stringdist(title_postprocessed.x, title_postprocessed.y, method = "dl"),
         dist_x = stringdist(title_postprocessed.x, title_processed.x, method = "dl"),
         dist_y = stringdist(title_postprocessed.y, title_processed.y, method = "dl"),
         diff = dl_dist != dist) |> 
  ungroup() |> 
  select(dl_dist, title_postprocessed.x, title_postprocessed.y, contains("official_title"),
         contains("length"), contains("trial_id"), contains("_in_"))


title_matches_gtl_validation_new <- title_matches_gtl_validation2 |> 
  rowwise() |> 
  mutate(trial_id.y = update_ctgov_alias(trial_id.y, ctgov_aliases)) |> 
  ungroup() |> 
  write_excel_csv(here("data", "processed", "title_matches_gtl_20_new.csv"))
