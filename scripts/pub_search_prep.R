library(tidyverse)
library(ctregistries)
library(progressr)
library(furrr)
library(here)
library(janitor)


source(here("scripts", "utils.R"))

plan(multisession)
handlers(global = TRUE)
# 
# # EUCTR inclusion and exclusion criteria
# euctr_inex <- read_csv(here("data", "processed", "inclusion_exclusion_euctr.csv"))
# euctr_inex_deduped <- euctr_inex |> 
#   group_by(trial_id) |> 
#   mutate(is_trial_de_protocol = str_detect(eudract_number_with_country, "DE")) |> 
#   arrange(desc(is_trial_de_protocol)) |> 
#   summarise(across(everything(), first)) |> # this takes first also for is_trial_de_protocol == FALSE
#   select(-eudract_number_with_country, -is_trial_de_protocol, has_euctr_results = results_reporting) |> 
#   ungroup() |> 
#   mutate(completion_date = ymd(completion_date))
# 
# # DRKS inclusion and exclusion criteria
# drks_inex <- read_csv(here("data", "processed", "inclusion_exclusion_drks.csv")) |> 
#   mutate(completion_date = ymd(completion_date))
# # Ct.gov inclusion and exclusion criteria
# ctgov_inex <- read_csv(here("data", "processed", "inclusion_exclusion_ctgov.csv"))
# combined_inex <- bind_rows(euctr_inex_deduped, drks_inex, ctgov_inex)


filtered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_filtered.csv"))
euctr_export <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv"))

# 
# sample_ids <- c(euctr_export$trial_id, drks_export$drksId, ctgov_export$nct_id) |> 
#   unique()
pub_table_vars <- c("trial_id", "registry_url", "registry", "status",
                    "crossreg_id", "crossreg_euctr", "crossreg_drks", "crossreg_ctgov")


pub_search_table_crossreg <- filtered_crossreg_ids |> 
  # rowwise() |> 
  mutate(registry_url = get_registry_url(trial_id),
         registry = get_registry_name(trial_id),
         noselfref = str_remove(crossreg_id, trial_id) |> str_replace_all("_", " "), 
         crossreg_euctr = which_trns(noselfref, registry = "EudraCT") ,
         crossreg_drks = which_trns(noselfref, registry = "DRKS"),
         crossreg_ctgov = which_trns(noselfref, registry = "ClinicalTrials.gov")) |>
  select(all_of(pub_table_vars), has_premature, has_de_premature, has_cluster_de_protocol) |> 
  group_by(crossreg_id) |> 
  arrange(crossreg_id, desc(trial_id)) |> # sorting !
  ungroup()

pub_search_table_euctr <- euctr_export |> 
  filter(!trial_id %in% pub_search_table_crossreg$trial_id) |> 
  distinct(trial_id, .keep_all = TRUE)

pub_search_table_drks <- drks_export |> 
  filter(!trial_id %in% pub_search_table_crossreg$trial_id)

pub_search_table_ctgov <- ctgov_export |> 
  select(trial_id = nct_id, everything()) |> 
  filter(!trial_id %in% pub_search_table_crossreg$trial_id)

pub_search_table <- pub_search_table_euctr |> 
  bind_rows(pub_search_table_drks) |> 
  bind_rows(pub_search_table_ctgov) |> 
  mutate(registry_url = get_registry_url(trial_id),
         registry = get_registry_name(trial_id),
         crossreg_id = NA,
         crossreg_euctr = NA,
         crossreg_drks = NA,
         crossreg_ctgov = NA) |> 
  select(all_of(pub_table_vars)) |> 
  bind_rows(pub_search_table_crossreg) |> 
  mutate(running_id = if_else(is.na(crossreg_id), trial_id, crossreg_id),
         running_id = running_id != lag(running_id, default = ""),
         running_id = cumsum(running_id),
         is_crossreg = !is.na(crossreg_id))


pub_search_table |> 
  distinct(running_id) |> 
  nrow()

# per person
18670/67 

0.7 * 279

cases_rev_7 |> 
  distinct(crossreg_id, trial_id) 


premature_cases <- pub_search_table |> 
  filter(has_de_premature |
           has_cluster_de_protocol == FALSE & has_premature) |> 
  mutate(rev_nr = 6)

premature_cases |> nrow()
premature_cases |> distinct(crossreg_id) |> nrow()
195 - 61


crossreg_rev_7 = pub_search_table |> 
  filter(is_crossreg == TRUE) |> 
  slice_sample(n = 25)

#split simple cases
cases_rev_7 = pub_search_table |> 
  filter(is_crossreg == FALSE) |> 
  slice_sample(n = 104) |> 
  bind_rows(premature_cases) |> 
  bind_rows(crossreg_rev_7) |> 
  mutate(rev_nr = 7)

dest_folder <- here("data", "processed")

cases_rev_7 |> 
  distinct(crossreg_id) |> 
  nrow()


prep_and_print(cases_rev_7, dest_folder)

pub_search_simple <- pub_search_table |> 
  filter(is_crossreg == FALSE, !trial_id %in% cases_rev_7$trial_id) |> 
  group_by(crossreg_id) |> 
  mutate(rev_nr = 1 + (running_id %% 6)) |> 
  ungroup()

pub_search_simple |> 
  count(rev_nr)

other_crossreg_cases <- pub_search_table |> 
  filter(is_crossreg == TRUE,
         !crossreg_id %in% c(premature_cases$crossreg_id,
                             cases_rev_7$crossreg_id)) |>
  mutate(running_id = crossreg_id != lag(crossreg_id, default = ""),
         running_id = cumsum(running_id)) |> 
  group_by(crossreg_id) |> 
  mutate(rev_nr = 1 + (running_id %% 6)) |> 
  ungroup()


other_crossreg_cases |>
  count(rev_nr)

other_crossreg_cases |>
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(rev_nr)

other_cases_total <- other_crossreg_cases |> 
  bind_rows(pub_search_simple) |> 
  select(-contains("premature")) |> 
  group_by(rev_nr) |> 
  group_split()

walk(other_cases_total, \(x) prep_and_print(x, dest_folder))

other_cases_total[[1]] |> 
  semi_join(cases_rev_7, by = "crossreg_id") |> 
  filter(!is.na(crossreg_id))


prep_and_print <- function(tib_pub_s, target_folder) {
  filename <- tib_pub_s |> 
    slice_head(n = 1) |> 
    pull(rev_nr)
  
  filename <- file.path(target_folder, paste0("pub_search_table_rev_", filename, ".csv"))
  
  write_csv(tib_pub_s, filename)
  
}


# pub_search_table <- pub_search_table |> 
#   left_join(combined_inex, by = "trial_id")

pub_search_table |> count(is.na(status))

pub_search_table |> filter(is.na(status))

# falling_clusters <- pub_search_table |> 
#   filter(!is.na(crossreg_id)) |> 
#   group_by(crossreg_id) |> 
#   arrange(desc(last_updated), desc(registration_date)) |> 
#   summarise(across(everything(), first))
# 
# falling_clusters |> count(is_completed_2018_2021)
# 
# falling_clusters_ids <- falling_clusters |> 
#   filter(is.na(is_completed_2018_2021) | is_completed_2018_2021 == FALSE)
# 
# clusters_to_exclude <- 
# pub_search_table |> 
#   filter(crossreg_id %in% falling_clusters_ids$crossreg_id) |> 
#   write_excel_csv(here("data", "processed", "potential_excluded_clusters.csv"))
# clusters_to_exclude |> distinct(crossreg_id) |> nrow()
# pub_search_table |> 
#   filter(str_detect(trial_id, "NCT")) |> 
#   count(status, sort = TRUE)

# pub_search_table <- pub_search_table |> 
#   mutate(is_withdrawn = trial_id %in% c(withdrawn_trn_ctgov$nct_id, withdrawn_trns_drks$trial_id))

pub_search_table |> 
  write_excel_csv(here("data", "processed", "pub_search_table.csv"))

pub_search_table |> 
  distinct(crossreg_id) |> 
  nrow()

pub_search_table <- read_csv((here("data", "processed", "pub_search_table.csv")))

pub_search_table |> 
  count(str_count(crossreg_id, "_") > 1)

euctr_trns_updated |>
  rename(trial_id = eudract_number) |> 
  group_by(trial_id) |> 
  summarise(n_countries = n()) |>
  left_join(pub_search_table, by = "trial_id") |> 
  # left_join(ctgov_export |> select(trial_id = nct_id, results_reporting)) |> 
  filter(str_count(crossreg_id, "_") > 0, n_countries > 1) |> 
  pull(crossreg_id)
