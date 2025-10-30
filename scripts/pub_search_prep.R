library(tidyverse)
library(ctregistries)
library(progressr)
library(furrr)
library(here)
library(janitor)
library(readxl)

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

unfiltered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_unfiltered.csv"))
filtered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_filtered.csv"))
euctr_export <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv"))



fallen_clusters <- unfiltered_crossreg_ids |> 
  anti_join(filtered_crossreg_ids, by = "crossreg_id")
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
  filter(!trial_id %in% unfiltered_crossreg_ids$trial_id) |> 
  distinct(trial_id, .keep_all = TRUE)

pub_search_table_drks <- drks_export |> 
  filter(!trial_id %in% unfiltered_crossreg_ids$trial_id)

pub_search_table_ctgov <- ctgov_export |> 
  select(trial_id = nct_id, everything()) |> 
  filter(!trial_id %in% unfiltered_crossreg_ids$trial_id)

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


withdrawn_cases <- pub_search_table |> 
  filter(str_detect(status, "WITHDRAWN"))

# pub_search_table <- pub_search_table |> 
#   filter(!trial_id %in% withdrawn_cases$trial_id)

premature_cases <- pub_search_table |> 
  filter(has_de_premature |
           has_cluster_de_protocol == FALSE & has_premature) |> 
  mutate(rev_nr = 7)

premature_cases |> nrow()
premature_cases |> distinct(crossreg_id) |> nrow()
190 - 61


crossreg_rev_7 <- pub_search_table |> 
  filter(is_crossreg == TRUE) |> 
  slice_sample(n = 20)

crossreg_rev_7 <- pub_search_table |> 
  filter(crossreg_id %in% crossreg_rev_7$crossreg_id)

pub_search_table |> 
  anti_join(pub_table, by = "trial_id")

#split simple cases
cases_rev_7 <- pub_search_table |> 
  filter(is_crossreg == FALSE) |> 
  slice_sample(n = 50) |> 
  bind_rows(premature_cases) |> 
  bind_rows(crossreg_rev_7) |> 
  mutate(rev_nr = 7)

dest_folder <- here("data", "processed")

cases_rev_7 |> 
  distinct(crossreg_id) |> 
  nrow()


prep_and_print(cases_rev_7, dest_folder)

pub_search_simple <- pub_search_table |> 
  filter(is_crossreg == FALSE, !trial_id %in% cases_rev_7$trial_id,
         !trial_id %in% withdrawn_cases$trial_id) |> 
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

other_cases_total[[6]] |> 
  semi_join(cases_rev_7, by = "crossreg_id") |> 
  filter(!is.na(crossreg_id))


count_cases <- function(tib) {
  tib |> 
    # dplyr::group_by(i) |> 
    dplyr::summarise(total_simple = sum(is.na(crossreg_id)), 
                     total_crossreg = sum(is_crossreg)) |> 
    dplyr::mutate(total_cases = total_simple + total_crossreg)
}

cases_rev_7 |> count_cases()
other_cases_total |> map(count_cases)
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
# 
# pub_search_table <- pub_search_table |>
#   mutate(is_withdrawn = trial_id %in% c(withdrawn_trn_ctgov$nct_id, withdrawn_trns_drks$trial_id))

pub_search_table |> 
  filter(!trial_id %in% withdrawn_cases$trial_id) |> 
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


files <- list.files(here("data", "processed"), pattern = "_rev", full.names = TRUE)

all_extractions <- files |> 
  map(read_csv) |> 
  map(\(x) filter(x, status != "WITHDRAWN"))


xlsx_files <- list.files("C:/Users/Vladi/Downloads", pattern = "iv3-main", full.names = TRUE) 

all_xlsx_extractions <- xlsx_files |> 
  map(readxl::read_xlsx)


all_xlsx_extractions <- all_xlsx_extractions |> 
  map(\(x) mutate(x, exclude = trial_id %in% fallen_clusters$trial_id))


all_xlsx_extractions[[7]] |> 
  select(trial_id,
         exclude) |> 
  filter(exclude == TRUE)

pub_table <- all_xlsx_extractions |> 
  list_rbind()

missed_cr_cases <- pub_table |> 
  filter(is_crossreg == FALSE, trial_id %in% filtered_crossreg_ids$trial_id)

missed_cr_cases |> 
  count(rev_nr)

fallen_clusters_missed <- pub_table |> 
  filter(trial_id %in% fallen_clusters$trial_id)

fallen_clusters_missed <- fallen_clusters |> 
  inner_join(fallen_clusters_missed |> select(trial_id, rev_nr), by = "trial_id")

fallen_clusters_missed |> 
  count(rev_nr)
# 33 cases so far (5 euctr, 2 drks, 26 ctgov)

assigned_cases <- read_csv(here("data", "processed", "assigned_cases.csv")) |> 
  group_by(crossreg_id) |> 
  mutate(actually_excluded = any(trial_id %in% fallen_clusters_missed$trial_id)) |> 
  ungroup()

assigned_cases |> filter(actually_excluded == FALSE) |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(rev_nr)

cases_still_missing <- pub_search_table |> 
  group_by(crossreg_id) |> 
  mutate(actually_excluded = any(trial_id %in% fallen_clusters$trial_id),
         missed_before = any(trial_id %in% missed_cr_cases$trial_id)) |> 
  ungroup() |> 
  filter(!trial_id %in% pub_table$trial_id,
         actually_excluded == FALSE,
         missed_before == FALSE)


trns_to_drop <- pub_search_table_euctr |> 
  anti_join(pub_search_table_euctr, by = "trial_id") |> 
  bind_rows(pub_search_table_ctgov_old |> 
              anti_join(pub_search_table_ctgov, by = "trial_id")) |> 
  bind_rows(pub_search_table_drks_old |> 
              anti_join(pub_search_table_drks, by = "trial_id")) |> 
  select(trial_id, status, everything())

assigned_cases <- read_xlsx(here("data", "processed", "IV3_PUBSEARCH.xlsx"))

assigned_cases <- assigned_cases |> 
  mutate(trial_id = `Trial ID (EudraCT number or NCT ID or DRKS ID, see your trial sheet and please take care to avoid typos)`) |> 
  filter(trial_id %in% missed_cr_cases$trial_id) |> 
  left_join(pub_search_table, by  = "trial_id")

missing_pub_search_cases <- pub_search_table |> 
  group_by(crossreg_id) |> 
  mutate(has_misclassified_crossreg = any(trial_id %in% missed_cr_cases$trial_id, na.rm = TRUE)) |> 
  ungroup() |> 
  filter(has_misclassified_crossreg == TRUE)

write_csv(assigned_cases, here("data", "processed", "assigned_cases.csv"))

write_csv(missing_pub_search_cases, here("data", "processed", "misclassified_crossreg_cases.csv"))

rev_nr_missing <- missing_pub_search_cases |> 
  left_join(pub_table |> select(trial_id, rev_nr)) |> 
  group_by(crossreg_id) |> 
  fill(rev_nr, .direction = "up") |> 
  write_csv(here("data", "processed", "assigned_cases.csv"))


pub_table_drop_cases <- pub_table |> 
  filter(trial_id %in% trns_to_drop$trial_id)

pub_table_drop_cases |> count(rev_nr)

n_crossreg_per_registry <- pub_table |> 
  filter(is_crossreg == TRUE) |> 
  # count(registry, crossreg_id) |>
  count(registry)

n_per_registry <- pub_table |> 
  filter(is_crossreg == FALSE) |> 
  count(registry)


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
         registry = get_registry_name(trial_id))

library(flowchart)

fc_simple <- combined_inex |> as_fc(label = "All TRNS from all registries")


inex_per_registry <- function(tib) {
  fc <- flowchart::as_fc(tib, label = "All TRNs")
  
  fc |> 
    flowchart::fc_split(registry) |>
    flowchart::fc_filter(is_interventional, label = "Interventional", show_exc = TRUE) |> 
    flowchart::fc_filter(is_german_umc, label = "German UMC", show_exc = TRUE) |> 
    flowchart::fc_filter(is_completed_2018_2021, label = "Completed 2018-2021", show_exc = TRUE) |>
    flowchart::fc_filter(!is_crossreg, label = "Not crossregistered", show_exc = TRUE) |>
    flowchart::fc_filter(!is_withdrawn, label = "Not withdrawn", show_exc = TRUE) |>
    flowchart::fc_draw()
}


combined_inex |> 
  inex_per_registry()
pub_search_table |> 
  filter(is_crossreg == FALSE, status != "WITHDRAWN") |> 
  count(registry)

falling_clusters <- read_csv(here("data", "processed", "crossreg_unfiltered.csv"))


falling_clusters |> 
  mutate(registry = get_registry_name(trial_id)) |> 
  as_fc(label = "Crossreg TRNs (at least one TRN in cluster is included according to 'simple' criteria") |> 
  fc_split(registry) |> 
  fc_filter(has_interventional, label = "Interventional", show_exc = TRUE) |> 
  fc_filter(has_german_umc, label = "German UMC", show_exc = TRUE) |> 
  fc_filter(has_completion_2018_2021, label = "Completed 2018-2021", show_exc = TRUE) |>
  fc_filter(!has_withdrawn_status, label = "Not withdrawn", show_exc = TRUE) |>
  flowchart::fc_draw()


pub_search_table |> 
  filter(is_crossreg == TRUE, status != "WITHDRAWN") |> 
  count(registry)

gumc <- falling_clusters |> 
  filter(!has_german_umc)

testthat::expect_equal(distinct(pub_search_table, crossreg_id) |> na.omit() |>  nrow(),
             distinct(filtered_crossreg_ids, crossreg_id) |> nrow())


qa_crossreg_pub_table <- pub_table |> 
  filter(exclude == FALSE) |> 
  mutate(in_crossreg_table = trial_id %in% unfiltered_crossreg_ids$trial_id) |> 
  filter(is_crossreg != in_crossreg_table)

qa_crossreg_pub_table |> 
  count(rev_nr)
