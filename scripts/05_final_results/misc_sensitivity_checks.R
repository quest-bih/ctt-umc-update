library(tidyverse)
library(janitor)
library(here)

source(here("scripts", "utils.R"))

umc_validations <- read_csv(here("data", "processed", "umc_validations.csv")) |> 
  mutate(raw_affil = str_squish(raw_affil))

unclear_cases <- umc_validations |> 
  filter(validation == 2)

validation_umcs_drks_complete <- read_csv(here("data", "processed", "validation_umcs_drks_complete.csv"))

validation_umcs_ctgov_complete <- read_csv(here("data", "processed", "validation_umcs_ctgov_complete.csv"))

complete_validations <- bind_rows(validation_umcs_ctgov_complete,
                                  validation_umcs_drks_complete)

euctr_withdrawn_exclusions <- read_csv(here("data", "processed", "euctr_withdrawn_exclusions.csv"))

analysis_results <- read_csv(here("data", "processed", "analysis_results.csv"))


inex_combined <- read_csv(here("data", "processed", "inex_combined.csv")) |> 
  mutate(resolves = TRUE,
         is_withdrawn_euctr = trial_id %in% euctr_withdrawn_exclusions$trial_id) |> 
  # rows_upsert(non_resolving_trns, by = "trial_id") |> 
  mutate(in_analysis = trial_id %in% analysis_results$trial_id)

withdrawn_cases <- inex_combined |> 
  filter(has_withdrawn_status == TRUE | is_withdrawn_euctr == TRUE)

##### find out what which TRNs were a hit but inconclusive for German UMC
##### and were not included in final sample

complete_validations_inconclusive <- complete_validations |> 
  filter(raw_affil %in% unclear_cases$raw_affil,
         !id %in% analysis_results$trial_id,
         !id %in% withdrawn_cases$trial_id) 

complete_validations_inconclusive |> 
  left_join(inex_combined, by = c("id" = "trial_id")) |> 
  distinct(crossreg_id) |>
  nrow()

qa_inconcl_fields <- 
complete_validations_inconclusive |> 
  left_join(inex_combined, by = c("id" = "trial_id")) |> 
  group_by(crossreg_id) |> 
  mutate(inconcl_pi = any(str_detect(field, "pci|overall"), na.rm = TRUE),
         inconcl_resp_party = any(str_detect(field, "resp"), na.rm = TRUE),
         inconcl_sponsor = any(str_detect(field, "sponsor|resp"), na.rm = TRUE)) |> 
  ungroup()

qa_inconcl_fields |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(inconcl_pi)

qa_inconcl_fields |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(inconcl_resp_party)

qa_inconcl_fields |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(inconcl_sponsor)

########### proportion of unclear sponsor where the other fields were clear
inconclusive_sponsors <- complete_validations |> 
  filter(raw_affil %in% unclear_cases$raw_affil,
         str_detect(field, "sponsor"),
         id %in% inex_combined$trial_id) |> 
  rename(trial_id = id) |> 
  left_join(inex_combined |>
              select(trial_id, 
                     crossreg_id),
            by = "trial_id")

qa_umc_inconclusive_sponsors <- analysis_results |> 
  filter(crossreg_id %in% c(inconclusive_sponsors$crossreg_id)) |> 
  select(trial_id, crossreg_id, contains("umc")) |> 
  group_by(crossreg_id) |> 
  mutate(rescued_via_crossreg = any(!is.na(umc_sponsor))) |> 
  ungroup()

qa_umc_inconclusive_sponsors |> 
  filter(rescued_via_crossreg == FALSE) |> 
  distinct(crossreg_id) |> 
  nrow()

qa_umc_inconclusive_sponsors |> 
  count(is.na(umc_sponsor), is.na(umc_resp_party))

inconclusive_pi <- complete_validations |> 
  filter(raw_affil %in% unclear_cases$raw_affil,
         str_detect(field, "pci|overall"),
         id %in% inex_combined$trial_id) |> 
  rename(trial_id = id) |> 
  left_join(inex_combined |>
              select(trial_id, 
                     crossreg_id),
            by = "trial_id")

qa_umc_inconclusive_pi <- analysis_results |> 
  filter(crossreg_id %in% c(inconclusive_pi$crossreg_id)) |> 
  select(trial_id, crossreg_id, contains("umc")) |> 
  group_by(crossreg_id) |> 
  mutate(rescued_via_crossreg = any(!is.na(umc_pi))) |> 
  ungroup()


qa_umc_inconclusive_pi |> 
  filter(rescued_via_crossreg == FALSE) |> 
  distinct(crossreg_id) |> 
  nrow()

qa_umc_inconclusive_pi |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(rescued_via_crossreg)

####### sanity checks final results
combined_data_enriched <- read_csv(here("data", "processed", "analysis_results.csv"))

# corrections_reglinked_url <- tibble(
#   trial_id = c("2013-001294-24",
#                "NCT00278408",
#                "DRKS00009323",
#                "DRKS00011876",
#                "NCT03264222",
#                "NCT03290157",
#                "NCT03720665",
#                "NCT03128528",
#                "DRKS00012329",
#                "DRKS00025281",
#                "NCT03838211"),
#   pub_reglinked_url_clean = c("10.1136/bmj.m3734",
#                               "10.1097/hs9.0000000000000904",
#                               "10.2196/15361",
#                               "10.3233/nre-201583",
#                               "10.1016/j.jacc.2020.01.012",
#                               "10.1016/j.cgh.2020.03.051",
#                               "10.1016/j.clinph.2021.01.024",
#                               "10.1007/s00392-022-02119-7",
#                               "10.3238/arztebl.2020.0833",
#                               "10.1016/j.kint.2021.10.029",
#                               "10.1038/s41467-023-38910-x")
# )
# 
# corrections_reglinked_url <- tibble(
#   trial_id = c("2013-001294-24",
#                "NCT00278408",
#                "DRKS00009323",
#                "DRKS00011876",
#                "NCT03264222",
#                "NCT03290157",
#                "NCT03720665",
#                "NCT03128528",
#                "DRKS00012329",
#                "DRKS00025281",
#                "NCT03838211"),
#   pub_reglinked_url_clean = c("10.1136/bmj.m3734",
#                               "10.1097/hs9.0000000000000904",
#                               "10.2196/15361",
#                               "10.3233/nre-201583",
#                               "10.1016/j.jacc.2020.01.012",
#                               "10.1016/j.cgh.2020.03.051",
#                               "10.1016/j.clinph.2021.01.024",
#                               "10.1007/s00392-022-02119-7",
#                               "10.3238/arztebl.2020.0833",
#                               "10.1016/j.kint.2021.10.029",
#                               "10.1038/s41467-023-38910-x")
# )
# 
corrections_earliest_pub_url <- tibble(
  trial_id = c("NCT01477606",
               "NCT04053829",
               "2014-003647-34",
               "DRKS00009323",
               "DRKS00024830",
               "DRKS00028561",
               "DRKS00011562",
               "NCT02326428"),
  earliest_pub_url_clean = c("10.1182/blood.v126.23.322.322",
                             "10.2196/37229",
                             "10.1097/hs9.0000000000000877",
                             "10.2196/15361",
                             "10.2196/56514",
                             "10.2196/33733",
                             "10.2196/15312",
                             "10.1161/strokeaha.120.031031"))


qa_corrections_earliest_pub <- combined_data_enriched |> 
  filter(trial_id %in% corrections_earliest_pub_url$trial_id) |> 
  select(trial_id, crossreg_id, contains("earliest"), everything())



expect_equal(qa_corrections_earliest_pub$earliest_pub_date,
             ymd(c("2016-12-02", "2019-07-04", "2024-01-19", "2019-07-01",
               "2021-09-23", "2024-10-01", "2020-11-27", "2015-12-03")))

qa_search_date <- combined_data_enriched |> 
  mutate(search_before_pub = earliest_pub_date > cluster_results_search_end_date) |> 
  filter(search_before_pub == TRUE,
         has_pub != FALSE) |> 
  select(crossreg_id, trial_id, has_pub, earliest_pub_date, cluster_results_search_end_date, search_before_pub, everything())

qa_jmir <- combined_data_enriched |> 
  filter(str_detect(earliest_pub_doi, "10.2196") |
           str_detect(review_earliest_doi, "10.2196") |
           str_detect(earliest_pub_url, "jmir")) |> 
  select(trial_id, crossreg_id, earliest_pub_doi, earliest_pub_date, review_earliest_doi, review_has_pub)

qa_no_doi <- combined_data_enriched |> 
  filter(is.na(earliest_pub_doi), has_pub == TRUE) |> 
  select(trial_id, crossreg_id, earliest_pub_doi, earliest_pub_date, earliest_pub_url, review_has_pub, review_earliest_pubtype, review_earliest_doi, review_has_pub)

# Was the correct "earliest publication" selected for a given trial or cross-registration cluster, taking into account the initial search and review (if applicable), and the determination of the earliest publication date (if >1 pub)? (this might need to be checked manually)
# TODO
# Were eligible publications with a pub date > search date preserved but not counted in the KM analysis? How many such cases were there?
# Yes, n = 4. Yes.
#   For cross-registration clusters with different status, has the correct status been allocated given the pre-specified hierarchy?
# DONE
#   For trials involving a publication without a DOI, was the correct publication date still entered?
# DONE
#   Were all cases where a cross-registration was missed, and both trials were independently extracted in the search, correctly combined? And in these cases, was the correct earliest eligible publication selected (based on the initial search, review, and determination of earliest publication date)?
# TODO
#   Were all trials involving an EUCTR record with euctr_is_no_enrolment == "Yes, the trial was withdrawn without enrolment" correctly labelled as withdrawn (in case of a cross-registration, withdrawn would extend to the full cluster).