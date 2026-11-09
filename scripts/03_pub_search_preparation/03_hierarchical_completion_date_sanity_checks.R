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

source(here("scripts", "utils.R"))


validated_mtm_resolved <- read_xlsx(here("data", "processed", "validations",
                                         "mtm_othernr_manually_validated – 20250724.xlsx"))

crossreg_title_ids <- read_csv(here("data", "processed", "crossreg_title_ids.csv"))

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

# potentially affected cases
qa_drks_actual_completion <- drks_inex |>
  filter(is_completed_2018_2021, is_german_umc, is_interventional,
    !is.na(completion_date) & !is.na(estimated_completion_date))

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


# 1 if results completion date, if available has priority,
# 2 most recently updated actual completion date between DRKS and CTgov
# 3 latest registration from any if only estimated completion dates available

validated_crossreg_ids <- read_csv(here("data", "processed", "crossreg_ids_unfiltered.csv"))
falling_clusters <- validated_crossreg_ids |> 
  left_join(combined_inex, by = "trial_id") |>
  mutate(
    filled_actual_completion_dates = case_when(
      !is.na(actual_completion_date) ~ actual_completion_date,
      .default = NA_Date_),
    filled_euctr_completion_date = if_else(has_euctr_results == TRUE, results_completion_date, NA_Date_)) |>
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
         last_updated_actual = if_else(!is.na(actual_completion_date), last_updated, NA_Date_),
         only_estimated = all(is.na(results_completion_date), na.rm = TRUE) &
           all(is.na(actual_completion_date), na.rm = TRUE),
         is_last_updated = last_updated_actual == max(last_updated_actual, na.rm = TRUE),
         n_last_updated = sum(is_last_updated, na.rm = TRUE),
         is_latest_actual = actual_completion_date == max(actual_completion_date, na.rm = TRUE),
         is_latest_estimated = estimated_completion_date == max(estimated_completion_date, na.rm = TRUE),
         n_latest_estimated = sum(is_latest_estimated, na.rm = TRUE),
         is_index_reg = case_when(
           !is.na(results_completion_date) ~ TRUE,
           all(is.na(results_completion_date)) & 
             n_last_updated == 1 &
             is_last_updated & only_estimated == FALSE ~ TRUE,
           all(is.na(results_completion_date)) & 
             n_last_updated > 1 &
             is_latest_actual & only_estimated == FALSE ~ TRUE,
           only_estimated & is_latest_estimated & n_latest_estimated == 1 ~ TRUE,
           only_estimated & is_last_updated & n_latest_estimated > 1 ~ TRUE,
           .default = FALSE
         ),
         n_index_reg = sum(is_index_reg),
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

# falling_clusters_old <- falling_clusters

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
                      "2011-004228-37_DRKS00004186", ymd("2021-03-08")
)

cluster_check <- falling_clusters |>
  inner_join(test_cases, by = "crossreg_id", relationship = "many-to-many")

cluster_check |> 
  filter(hierarchical_completion_date != verified_cluster_completion_date) |>
  nrow() |> 
  testthat::expect_equal(0)

crossreg_ids_to_check <- falling_clusters |> 
  filter(trial_id %in% qa_drks_actual_completion$trial_id) |> 
  pull(crossreg_id)

# the reason was that DRKS originally did not include estimated completion dates

falling_clusters_new <- falling_clusters

all.equal(falling_clusters_new$hierarchical_completion_date, falling_clusters_old$hierarchical_completion_date)

comp_old_clusters <- falling_clusters_old |> 
  select(crossreg_id, trial_id, hierarchical_completion_date_old =
           hierarchical_completion_date)

comp_tibble <- falling_clusters_new |>
  left_join(comp_old_clusters, by = c("crossreg_id", "trial_id")) |> 
  filter(hierarchical_completion_date != hierarchical_completion_date_old)

ctgov_only_est_crossreg <- falling_clusters |>
  filter(is_completed_2018_2021, is_german_umc, is_interventional,
         is_index_reg == TRUE, str_detect(trial_id, "NCT"),
         is.na(actual_completion_date), !is.na(estimated_completion_date))

