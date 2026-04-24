library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(testthat)
library(openalexR)
library(easyRPubMed)

source(here("scripts", "utils.R"))

# extractions_old <- read_xlsx(here("data", "processed", "2026-03-05-iv3_pubsearch_main.xlsx"))
extractions_raw <- read_xlsx(here("data", "manual", "2026-04-09_iv3_pubsearch_main.xlsx"),
                             guess_max = 2500)

# extractions_raw <- read_csv(here("data", "manual", "2026-03-31_extractions-corrected.csv")) |> 
#   mutate(timestamp = as_datetime(timestamp))

euctr_inex <- read_csv(here("data", "processed", "inclusion_exclusion_euctr.csv"))
# setdiff(names(extractions_raw), names(extractions_old))
# setdiff(names(extractions_old), names(extractions_raw))

unfiltered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_unfiltered.csv"))
filtered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_filtered.csv"))
euctr_export <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv"))
pub_search_table <- read_csv((here("data", "processed", "pub_search_table.csv")))

# setdiff(old_names, new_names)
# setdiff(new_names, old_names)

extractions_with_dupes <- extractions_raw |>
  clean_names() |>
  rename(
    # Timestamp
    timestamp = timestamp,

    # Column 1
    extractor = column_1,

    # Trial ID (EudraCT number or NCT ID or DRKS ID, see your trial sheet and please take care to avoid typos)
    trial_id = trial_id_eudra_ct_number_or_nct_id_or_drks_id_see_your_trial_sheet_and_please_take_care_to_avoid_typos,

    # What is the registry for this trial ID?
    registry = what_is_the_registry_for_this_trial_id,

    # FOCUS ONLY ON EUCTR

    # Does the EUCTR registration indicate that the trial status is "Prematurely Ended"?
    euctr_is_prematurely_ended = does_the_euctr_registration_indicate_that_the_trial_status_is_prematurely_ended,

    # Was the trial withdrawn without enrolment?
    euctr_is_no_enrolment = was_the_trial_withdrawn_without_enrolment,

    # Explain why you are unsure whether the trial was withdrawn without enrolment
    euctr_comment_withdrawn = explain_why_you_are_unsure_whether_the_trial_was_withdrawn_without_enrolment,

    # FOCUS ONLY ON DRKS

    # Have summary results been uploaded in the registration?
    drks_is_sumres = have_summary_results_been_uploaded_in_the_registration,

    # Date of summary results in DRKS
    drks_sumres_date = date_of_summary_results_in_drks,

    # Explain why you are unsure whether summary results have been uploaded in DRKS
    drks_comment_sumres = explain_why_you_are_unsure_whether_summary_results_have_been_uploaded_in_drks,

    # FOCUS ON CROSS-REGISTRATION CLUSTERS

    # Does your extraction sheet indicate that this is a cross-registration of a trial you already extracted?
    crossreg_is_subsequent_reg = does_your_extraction_sheet_indicate_that_this_is_a_cross_registration_of_a_trial_you_already_extracted,

    # In case you have a comment about the cross-registration, enter it here:
    crossreg_comment1 = in_case_you_have_a_comment_about_the_cross_registration_enter_it_here,

    # Was an eligible results publication already identified in the previous extraction(s) for this trial?
    crossreg_pub_already_identified = was_an_eligible_results_publication_already_identified_in_the_previous_extraction_s_for_this_trial,

    # In case you have a comment about the cross-registration, enter it here:
    crossreg_comment2 = in_case_you_have_a_comment_about_the_cross_registration_enter_it_here_2,

    # Based on the current trial ID, can you identify an earlier eligible results publication than the one already found?
    crossreg_new_earlier_pub_found = based_on_the_current_trial_id_can_you_identify_an_earlier_eligible_results_publication_than_the_one_already_found,

    # NOW FOCUS ON SINGLE REGISTRATIONS AND *ONLY* FIRST REGISTRATION OF CROSS-REGISTRATION CLUSTERS

    # Was an eligible results publication linked in the registration?
    is_pub_reglinked = was_an_eligible_results_publication_linked_in_the_registration,

    # Provide the URL here:
    pub_reglinked_url = provide_the_url_here,

    # IF A PUB WAS LINKED IN THE REGISTRATION

    # Did you find an earlier eligible results publication via the Google search?
    is_earlier_pub_google = did_you_find_an_earlier_eligible_results_publication_via_the_google_search,

    # IF NO PUB WAS LINKED IN THE REGISTRATION

    # Did you find an eligible results publication via the Google search?
    is_pub_google = did_you_find_an_eligible_results_publication_via_the_google_search,

    # NOW FOCUSING ON EARLIEST ELIGIBLE PUB FOUND (WHETHER LINKED OR GOOGLE)

    # Provide the URL of this publication:
    earliest_pub_url = provide_the_url_of_this_publication,

    # What type of results publication is it?
    earliest_pub_type = what_type_of_results_publication_is_it,

    # Does the publication match the registration on the specified criteria?
    earliest_pub_matches_reg = does_the_publication_match_the_registration_on_the_specified_criteria,

    # If you had a doubt regarding matching on the eligibility criteria, you can add a comment here:
    earliest_pub_matches_reg_comment = if_you_had_a_doubt_regarding_matching_on_the_eligibility_criteria_you_can_add_a_comment_here,

    # Is there a Digital Object Identifier (DOI)?
    earliest_pub_has_doi = is_there_a_digital_object_identifier_doi,

    # Publication DOI (always starting with "10")
    earliest_pub_doi = publication_doi_always_starting_with_10,

    # Is there a PubMed ID (PMID)?
    earliest_pub_has_pmid = is_there_a_pub_med_id_pmid,

    # Enter the PMID here:
    earliest_pub_pmid = enter_the_pmid_here,

    # Publication date
    earliest_pub_date = publication_date,

    # Check this box if you feel a second review of the publication search is needed for this trial ID
    second_review_requested = check_this_box_if_you_feel_a_second_review_of_the_publication_search_is_needed_for_this_trial_id,

    # If this unreported study might be of high media interest, you can flag this here
    high_media_interest = if_this_unreported_study_might_be_of_high_media_interest_you_can_flag_this_here,

    # Any final comments on the extraction can be entered here
    final_comments = any_final_comments_on_the_extraction_can_be_entered_here_dont_miss_to_click_submit
  ) |>
  filter(!str_detect(extractor, regex("training|_second_", ignore_case = TRUE))) |> 
  # left_join(unfiltered_crossreg_ids |> select(trial_id, crossreg_id), by = "trial_id") |> 
  mutate(earliest_pub_pmid = as.character(earliest_pub_pmid),
         earliest_pub_doi = case_when(
    str_detect(earliest_pub_doi, regex("elife", ignore_case = TRUE)) ~ earliest_pub_doi |> 
      str_remove("\\.\\d($|\\..*)") |> 
      tolower(),
    .default = earliest_pub_doi |>
      str_remove_all("\\?.*") |>
      str_replace_all("%2F", "/") |>  
      tolower()
  ),
  pub_reglinked_url = case_when(
    str_detect(pub_reglinked_url, "proxy") ~
      str_remove(pub_reglinked_url, ".proxy.kib.ki.se") |>
      str_replace_all("\\-(?!(\\d|\\w$))", "\\."),
    .default = pub_reglinked_url
  ),
  earliest_pub_url = case_when(
    str_detect(earliest_pub_url, "proxy") ~  
      str_remove(earliest_pub_url, ".proxy.kib.ki.se") |>
      str_replace_all("\\-(?!(\\d|\\w$))", "\\."),
    .default = earliest_pub_url
  ),
  earliest_pub_date = map_chr(earliest_pub_date, str_date_clean_floor),
  drks_sumres_date = map_chr(drks_sumres_date, str_date_clean_floor)
  )

dedupe_instructions <- read_xlsx(here("data", "manual", "iv3_dupes.xlsx"))

dupes <- get_dupes(extractions_with_dupes,  trial_id)
# dupes |> 
#   write_csv(here("data", "processed", "dupes_pub_search.csv"))

extractions <- extractions_with_dupes |> 
  group_by(trial_id) |>
  # deduplication happens here
  mutate(is_dupe = case_when(
    n() > 1 & trial_id == "DRKS00013493" &
      timestamp == max(timestamp) ~ TRUE,
    trial_id != "DRKS00013493" &
    n() > 1 & timestamp == min(timestamp) ~ TRUE,
    .default = FALSE
  )) |> 
  ungroup() |> 
  filter(is_dupe == FALSE) |> 
  arrange(timestamp) |> 
  select(-is_dupe)

combined_data_filtered <- read_csv(here("data", "processed", "harmonized_data_filtered.csv"))
extractions_filtered_exclusions <- extractions |> 
  filter(trial_id %in% combined_data_filtered$trial_id)

excluded_extractions <- extractions |> 
  filter(!trial_id %in% combined_data_filtered$trial_id)

extractions_filtered_enriched <- extractions_filtered_exclusions |> 
  mutate(registry_url = get_registry_url(trial_id),
         .after = trial_id) |> 
  left_join(combined_data_filtered |>
              select(trial_id, crossreg_id, status), by = "trial_id") |>
  relocate(c(status, crossreg_id), .after = registry)


# Log corrections needed to the 'crossreg_is_subsequent_reg` variable
corrections_subsequent_reg <- tibble(
  trial_id = c("NCT01614132", "NCT01717677", "NCT02653313", "NCT03339973"),
  crossreg_is_subsequent_reg = c("No", "No", "No", "Yes"),
  crossreg_pub_already_identified = c(NA_character_, NA_character_, NA_character_, "No")
)

# Integrate these corrections back into the dataset
extractions_filtered_enriched <- extractions_filtered_enriched |>
  rows_upsert(corrections_subsequent_reg, by = "trial_id")

# Implement corrections ---------------------------------------------------

# Log correction requested by TB (by mistake did not acknowledge pub already found in first reg of crossreg cluster)
corrections_additional <- tibble(
  trial_id = "2007-007262-38",             
  crossreg_pub_already_identified = "Yes",
  crossreg_new_earlier_pub_found = "No, neither linked in the registration nor via the Google search",
  is_pub_reglinked = NA_character_,
  is_pub_google = NA_character_
)

# Integrate these corrections back into the dataset
extractions_filtered_enriched <- extractions_filtered_enriched |>
  rows_upsert(corrections_additional, by = "trial_id")

# Implement discrete corrections
# Correct extractor name
extractions_filtered_enriched$extractor[extractions_filtered_enriched$trial_id == "NCT02906475"] <- "Gina_MW"

# Correct DOI
extractions_filtered_enriched$earliest_pub_doi[extractions_filtered_enriched$trial_id == "NCT00894569"] <- "10.1200/JCO.2019.37.15_suppl.4120"

# Correct DOI
extractions_filtered_enriched$earliest_pub_doi[extractions_filtered_enriched$trial_id == "NCT02682693"] <- "10.1200/JCO.2020.38.15_suppl.580"

# Correct DOI
extractions_filtered_enriched$earliest_pub_doi[extractions_filtered_enriched$trial_id == "DRKS00004431"] <- "10.1024/1661-4747/a000288"

# Correct registry-linked URL
extractions_filtered_enriched$pub_reglinked_url[extractions_filtered_enriched$trial_id == "DRKS00017528"] <- "https://doi.org/10.3233/JPD-213119"
extractions_filtered_enriched$pub_reglinked_url[extractions_filtered_enriched$trial_id == "DRKS00033531"] <- 
  extractions_filtered_enriched$earliest_pub_url[extractions_filtered_enriched$trial_id == "DRKS00033531"]

# Correct earliest pub URL
extractions_filtered_enriched$earliest_pub_url[extractions_filtered_enriched$trial_id == "NCT01225575"] <- "https://doi.org/10.1177/0363546516646092"

# Correct earliest pub DOI
extractions_filtered_enriched$earliest_pub_doi[extractions_filtered_enriched$trial_id == "NCT01225575"] <- "10.1177/0363546516646092"

# exclusions <- extractions |> 
#   filter(!trial_id %in% combined_data_filtered$trial_id)

####### sanity checks
n_per_extractor <- extractions_filtered_enriched |> 
  count(extractor, is.na(crossreg_is_subsequent_reg))

## missed extractions
missed_extractions <- pub_search_table |> 
  filter(!trial_id %in% extractions$trial_id)

error_logs <- NULL

nrow(missed_extractions) |> 
  expect_equal(0)

### review of these three TRNS "NCT01289301"    "2012-005717-39" "2016-000205-36"
### showed that the NCT was linked to a withdrawn EUCTR and the EUCTR were not resolving
### so these are properly omitted

## extractions not in pub_table that should have been excluded, last check n = 2
qa_excluded <- extractions_filtered_enriched |> 
  anti_join(pub_search_table, by = "trial_id")

## review of cluster 2015-005219-34_DRKS00009451 showed it should have 
## actually been included in pub_search_table
### so no need to exclude now

## registry name validation
extractions_filtered_enriched |> 
  mutate(reg2 = get_registry_name(trial_id)) |> 
  filter(registry != reg2) |> 
  nrow() |>
  expect_equal(0)

### updating crossreg info

manual_crossreg_info <- read_xlsx(here("data", "manual", "iv3_crossreg.xlsx")) |> 
  filter(
    # action != "none", 
         str_detect(action, "link") # may revisit this later to add "add"
         ) |> 
  mutate(crossreg_id = if_else(!is.na(new_crossreg),
                               paste(trial_id, new_crossreg, sep = "_"),
                               trial_id)) |> 
  select(trial_id, new_crossreg, crossreg_id) |> 
  pivot_longer(-crossreg_id, values_to = "trial_id") |> 
  select(-name) |> 
  filter(!is.na(trial_id))

extractions_filtered_enriched <- extractions_filtered_enriched |> 
  rows_upsert(manual_crossreg_info, by = "trial_id")

# #earliest_pub_pmid = character
# extractions_filtered_enriched$earliest_pub_date |> 
#   class()

## prematurely ended
extractions_filtered_enriched |> 
  count(registry, euctr_is_prematurely_ended) |>
  filter((registry != "EUCTR" & !is.na(euctr_is_prematurely_ended)) |
           (registry == "EUCTR") & is.na(euctr_is_prematurely_ended)) |> 
  nrow() |> 
  expect_equal(0)
  
prem_end_extracted <- extractions_filtered_enriched |> 
  filter(euctr_is_prematurely_ended == "Yes")

## are there falsely marked prematurely ended TRNs 
prem_euctr <- euctr_inex |> 
  semi_join(extractions_filtered_enriched, by = "trial_id") |> 
  group_by(trial_id) |> 
  mutate(
    is_premature_extracted = trial_id %in% prem_end_extracted$trial_id,
    has_de_prematurely_ended = any(status == "Prematurely Ended" &
                                     str_detect(eudract_number_with_country, "DE"))
  ) |>
  select(trial_id, eudract_number_with_country,
         is_premature_extracted,
         has_trial_de_protocol,
         has_de_prematurely_ended, status)

prem_euctr |> 
  filter(has_de_prematurely_ended == TRUE & is_premature_extracted == FALSE) |> 
  nrow() |> 
  expect_equal(0)

qa_prem_euctr <- prem_euctr |>
  filter(has_de_prematurely_ended == FALSE,
         is_premature_extracted == TRUE,
         has_trial_de_protocol == FALSE)
qa_prem_euctr |> 
  nrow() |> 
  expect_equal(0)

unique_prem_euctr <- extractions_filtered_enriched |> 
  filter(trial_id %in% qa_prem_euctr$trial_id)

prematurely_ended_no_de_protocol <- prem_euctr |>
  filter(has_de_prematurely_ended == FALSE,
         is_premature_extracted == TRUE)
# e.g. 2014-002765-30 has no German protocol, but prematurely ended in NL

error_logs <- update_error_log(error_logs, unique_prem_euctr, "prematurely_ended_no_DE_protocol", "euctr_is_prematurely_ended")

## no enrolment
extractions_filtered_enriched |> 
  count(euctr_is_prematurely_ended, missing_no_enrolment = is.na(euctr_is_no_enrolment)) |> 
  filter((euctr_is_prematurely_ended == "Yes" & missing_no_enrolment == TRUE) | 
           (euctr_is_prematurely_ended != "Yes" & missing_no_enrolment == FALSE)) |> 
  nrow() |> 
  expect_equal(0)

### prematurely ended reclassifying
euctr_enrolment <- read_xlsx(here("data", "manual",
                                  "iv3_check_euctr_enrolment.xlsx")) |> 
  filter(registry == "EUCTR") |> 
  select(trial_id, status_cleaned = status)

extractions_filtered_enriched <- extractions_filtered_enriched |> 
  mutate(status_cleaned = case_when(
    euctr_is_no_enrolment ==
      "Yes, the trial was withdrawn without enrolment" ~ "Withdrawn",
    euctr_is_no_enrolment ==
      "No, the trial enrolled some patients" ~ "Terminated",
    status %in% c("COMPLETE_FOLLOW_UP_COMPLETE",
                  "COMPLETED",
                  "Completed") ~ "Completed",
    status %in% c("INVITE_ONLY",
                  "PENDING",
                  "COMPLETE_FOLLOW_UP_CONTINUING",
                  "RECRUITING",
                  "Ongoing") ~ "Ongoing",
    status %in% c("DISCONTINIUED",
                  "Prematurely Ended",
                  "TERMINATED") ~ "Terminated",
    status == "WITHDRAWN" ~ "Withdrawn",
    str_detect(status,
               regex("suspended",
                     ignore_case = TRUE)
    ) ~ "Suspended",
    status == "UNKNOWN" ~ "Unknown",
    # status == "\\N" |
    #    ~ "Other",
    .default = NA
  ),
  is_withdrawn = status_cleaned == "Withdrawn",
  # crossreg_id = case_when(
  #   is.na(crossreg_id) ~ trial_id,
  #   .default = crossreg_id
  # )
  ) |>
  rows_upsert(euctr_enrolment, by = "trial_id") |> 
  group_by(crossreg_id) |> 
  mutate(has_withdrawn_validated = any(is_withdrawn, na.rm = TRUE)) |> 
  ungroup()


withdrawn_exclusions <- extractions_filtered_enriched |> 
  filter(has_withdrawn_validated) |> 
  select(trial_id, has_withdrawn_validated) |> 
  # add cross-registered exclusions due to withdrawn
  rows_insert(tibble(trial_id = "NCT01289301", 
                     has_withdrawn_validated = TRUE)) |> 
  write_csv(here("data", "processed", "euctr_withdrawn_exclusions.csv"))

## sumres
extractions_filtered_enriched |> 
  count(registry, drks_is_sumres) |> 
  filter((registry != "DRKS" & !is.na(drks_is_sumres)) | 
           (registry == "DRKS") & is.na(drks_is_sumres)) |> 
  nrow() |> 
  expect_equal(0)

## crossreg
extracted_crossreg <- extractions_filtered_enriched |> 
  group_by(crossreg_id) |> 
  mutate(has_prematurely_ended = any(euctr_is_prematurely_ended == "Yes", na.rm = TRUE),
         # is_crossreg_filtered = trial_id %in% filtered_crossreg_ids$trial_id
         ) |>
  filter(has_withdrawn_validated == FALSE) |>  
  mutate(n = 1:n(),
         crossreg_n = max(n),
         n_subsequent_yes = sum(crossreg_is_subsequent_reg == "Yes", na.rm = TRUE),
         n_previous_yes = sum(crossreg_pub_already_identified == "Yes", na.rm = TRUE),
         n_dois = sum(earliest_pub_has_doi == "Yes", na.rm = TRUE)) |> 
  ungroup()

extracted_crossreg |> 
  count(crossreg_n, n_subsequent_yes)

## missed cross-registrations
qa_missed_crossreg <- extracted_crossreg |> 
  filter(crossreg_n == 1, n_subsequent_yes == 1,
         # has_prematurely_ended == FALSE
         ) 
qa_missed_crossreg |>
  nrow() |> 
  expect_equal(0)


## diads
qa_diads <- extracted_crossreg |> 
  filter(crossreg_n == 2, n_subsequent_yes > 1 | n_subsequent_yes == 0)
qa_diads |> 
  nrow()  |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_diads, "7_diads", "crossreg_is_subsequent_reg")

## triads
qa_triads <- extracted_crossreg |> 
  filter(crossreg_n == 3, n_subsequent_yes != 2, n_subsequent_yes > 0 | n_subsequent_yes == 0)
qa_triads |> 
  nrow() |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_triads, "7_triads", "crossreg_is_subsequent_reg")


## crossreg_pub_already_identified
extracted_crossreg |> 
  filter(((crossreg_is_subsequent_reg == "No") & !is.na(crossreg_pub_already_identified)) |
    ((crossreg_is_subsequent_reg == "Yes") & is.na(crossreg_pub_already_identified))
  ) |> 
  nrow() |> 
  expect_equal(0)

## diads crossreg_pub_already_identified
qa_diads_yes <- extracted_crossreg |> 
  # count(crossreg_n, n_previous_yes, crossreg_pub_already_identified)
  filter(crossreg_n == 2, n_previous_yes == 1, n == 1, is.na(earliest_pub_has_doi))
qa_diads_yes |> 
  nrow()  |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_diads_yes, "8_diads", "earliest_pub_has_doi")

qa_diads_no <- extracted_crossreg |>
  filter(crossreg_n == 2, n_previous_yes == 0, n == 1, !is.na(earliest_pub_has_doi))
qa_diads_no |> 
  nrow()  |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_diads_no, "8_diads", "earliest_pub_has_doi")

## triads crossreg_pub_already_identified
qa_triads_yes <- extracted_crossreg |> 
  filter(crossreg_n == 3, n_previous_yes > 0, n_dois == 0) 
  
qa_triads_yes |> 
  nrow() |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_triads_yes, "8_triads", "earliest_pub_has_doi")


qa_triads_no <- extracted_crossreg |> 
  filter(crossreg_n == 3, n_previous_yes == 0, n_dois > 0) 

qa_triads_no |> 
  nrow() |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_triads_no, "8_triads", "earliest_pub_has_doi")

## crossreg_new_earlier_pub_found
extracted_crossreg |> 
  filter(((crossreg_pub_already_identified == "Yes") & (is.na(crossreg_new_earlier_pub_found))) |
  ((crossreg_pub_already_identified != "Yes") & (!is.na(crossreg_new_earlier_pub_found)))) |> 
  nrow() |> 
  expect_equal(0)

# qa_dois <- 
extracted_crossreg |>
  filter((str_detect(crossreg_new_earlier_pub_found, "Yes") & n_dois == 0) |
           (str_detect(crossreg_new_earlier_pub_found, "No") & !is.na(earliest_pub_has_doi))) |> 
  nrow() |> 
  expect_equal(0)

## is_pub_reglinked
extracted_crossreg |> 
  filter(str_detect(is_pub_reglinked, "Yes"), is.na(pub_reglinked_url)) |> 
  nrow() |> 
  expect_equal(0)
  
## earliest_pub_type
extracted_crossreg |> 
  filter((!is.na(earliest_pub_url) & is.na(earliest_pub_type)) |
           ((is.na(earliest_pub_url)) & !is.na(earliest_pub_type))) |> 
  nrow() |> 
  expect_equal(0)

pub_type_clean <- extracted_crossreg |> 
  mutate(earliest_pub_type_clean = case_when(
    str_detect(earliest_pub_type, regex("abstract|conference", ignore_case = TRUE)) ~ "Abstract",
    str_detect(earliest_pub_type, regex("letter", ignore_case = TRUE)) ~ "Letter",
    str_detect(earliest_pub_type, regex("conference", ignore_case = TRUE)) ~ "Conference",
    .default = earliest_pub_type
  ))
  
# pub_type_clean |>
# count(earliest_pub_type_clean)

## doi checks (offline)
extracted_crossreg |> 
  filter((!is.na(earliest_pub_url) & is.na(earliest_pub_has_doi)) |
           ((is.na(earliest_pub_url)) & !is.na(earliest_pub_has_doi))) |> 
  nrow() |> 
  expect_equal(0)

extracted_crossreg |> 
  filter((!is.na(earliest_pub_doi) & earliest_pub_has_doi != "Yes") |
           ((is.na(earliest_pub_doi)) & earliest_pub_has_doi == "Yes")) |> 
  nrow() |> 
  expect_equal(0)

## pmid checks
extracted_crossreg |>
  filter((earliest_pub_has_doi == "No"  & is.na(earliest_pub_has_pmid)) |
           (earliest_pub_has_doi != "No"   & !is.na(earliest_pub_has_pmid))) |> 
  nrow() |> 
  expect_equal(0)

qa_extracted_crossreg <- extracted_crossreg |> 
  filter((!is.na(earliest_pub_pmid) & earliest_pub_has_pmid != "Yes") |
           ((is.na(earliest_pub_pmid)) & earliest_pub_has_pmid == "Yes"))
nrow(qa_extracted_crossreg) |> 
  expect_equal(0)

error_logs <- update_error_log(error_logs, qa_extracted_crossreg,
                               "pmid format", "earliest_pub_has_pmid")


## identifier cleaning/format check
qa_format <- extracted_crossreg |> 
  mutate(is_valid_doi = str_detect(earliest_pub_doi , "^10\\."),
         is_valid_pmid = str_detect(earliest_pub_pmid, "[1-9]\\d{7}")) |> 
  filter(!is.na(earliest_pub_has_doi) | !is.na(earliest_pub_has_pmid),
         is_valid_doi == FALSE | is_valid_pmid == FALSE)

nrow(qa_format) |> 
  expect_equal(0)

error_logs <- update_error_log(error_logs, qa_format, "pid format", "earliest_pub_pmid or earliest_pub_doi")


## date format check
qa_date_format <- extracted_crossreg |> 
  # count(is.na(earliest_pub_date), is.na(ymd_date))
  mutate(ymd_date = dmy(earliest_pub_date)) |>
  filter(!is.na(earliest_pub_date),
         is.na(ymd_date))
nrow(qa_date_format) |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_date_format, "date format", "earliest_pub_date")

qa_drks_date_format <- extracted_crossreg |> 
  # count(is.na(earliest_pub_date), is.na(ymd_date))
  mutate(ymd_date = dmy(drks_sumres_date)) |>
  filter(!is.na(drks_sumres_date),
         is.na(ymd_date))
nrow(qa_drks_date_format) |> 
  expect_equal(0)
error_logs <- update_error_log(error_logs, qa_drks_date_format, "drks date format", "drks_sumres_date")

# "https://www-sciencedirect-com.proxy.kib.ki.se/science/article/pii/S0006497119811151" |> 
# "https://doi-org.proxy.kib.ki.se/10.1007/s00134-022-06949-x" |> 
# "https://iovs.arvojournals.org/article.aspx?articleid=2768731eij-d-19" |> 
#   str_remove(".proxy.kib.ki.se") |> 
#   str_remove_all("\\?(?!articleid).*") |> 
#   str_replace_all("%2F", "/") |> 
#   str_replace_all("(?<!eij)\\-(?!(\\d|\\w$))", "\\.")
# str_view("10.1016/S0140-6736(20)30167-7",
#          regex("10.1016/s0140-6736(20)30167-7", ignore_case = TRUE))

# str_view(c("10.e930439403/full", "ddidfjk.full.pdf",
#            "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2023.1280397/full#h3"),
#          "\\?(?!(article|reference)).*|(\\.|\\/)?full(\\.pdf)?$|(\\/full)?#.*")

## online tests
urls_to_check <- extractions_filtered_enriched |>
  filter(!is.na(pub_reglinked_url) | !is.na(earliest_pub_url)) |> 
  select(trial_id, contains("url"), earliest_pub_doi, contains("pmid"), extractor, crossreg_id) |> 
  mutate(pub_reglinked_url_clean = case_when(
    str_detect(pub_reglinked_url, "10\\.\\d{4,}") ~
      str_extract(pub_reglinked_url, "10\\.\\d{4,}[\\w\\W]+$"),
    str_detect(pub_reglinked_url, "proxy") ~
      str_remove(pub_reglinked_url, ".proxy.kib.ki.se") |>
      str_replace_all("\\-(?!(\\d|\\w$))", "\\."),
    .default = pub_reglinked_url
  ),
  earliest_pub_url_clean = case_when(
    str_detect(earliest_pub_url, "10\\.\\d{4,}") ~
      str_extract(earliest_pub_url, "10\\.\\d{4,}[\\w\\W]+$"),
    str_detect(earliest_pub_url, "proxy") ~  
      str_remove(earliest_pub_url, ".proxy.kib.ki.se") |>
      str_replace_all("\\-(?!(\\d|\\w$))", "\\."),
    .default = earliest_pub_url
  )) |> 
  mutate(pub_reglinked_url_clean = str_replace_all(pub_reglinked_url_clean, "%2F", "/") |>
           # tolower() |> 
           str_remove_all("\\?(?!(article|reference)).*|(\\.|\\/)?full(\\.pdf)?$|(\\/full)?#.*"),
         pub_reglinked_url_distinct = case_when(
           is.na(earliest_pub_doi) ~ pub_reglinked_url_clean,
           earliest_pub_doi == tolower(pub_reglinked_url_clean) ~ NA_character_,
           !str_detect(pub_reglinked_url_clean, regex(coalesce(earliest_pub_doi, ".*"),
                                                      ignore_case = TRUE)) ~ pub_reglinked_url_clean,
           .default = NA_character_
         ),
         # doi_clean = str_extract(pub_reglinked_url, "10\\.\\d{1,}.*"),
         # doi_clean2 = str_extract(earliest_pub_url, "10\\.\\d{1,}.*"), 
         earliest_pub_url_clean = str_replace_all(earliest_pub_url_clean, "%2F", "/") |>
           # tolower() |> 
           str_remove_all("\\s|Digital.*") |> 
           str_remove_all("\\s") |> 
           str_remove_all("\\?(?!(article|reference)).*|(\\.|\\/)?full(\\.pdf)?$|(\\/full)?#.*"),
         earliest_pub_distinct = case_when(
           is.na(earliest_pub_doi) ~ earliest_pub_url_clean,
           earliest_pub_doi == tolower(earliest_pub_url_clean) ~ NA_character_,
           !str_detect(earliest_pub_url_clean, regex(coalesce(earliest_pub_doi, ".*"),
                                                      ignore_case = TRUE)) ~ earliest_pub_url_clean,
           
           # !is.na(earliest_pub_url_clean) ~ earliest_pub_url_clean,
           .default = NA_character_
         ))
qa_url_checks <- urls_to_check |> 
  select(trial_id, earliest_pub_doi, contains("distinct"))

dupe_dois <- get_dupes(urls_to_check, earliest_pub_doi)

dois_to_check <- urls_to_check |> 
  filter(!is.na(earliest_pub_doi)) |>
  distinct(earliest_pub_doi) |> 
  pull(earliest_pub_doi)

dois_reglinked_url <- urls_to_check |> 
  filter(str_detect(pub_reglinked_url_distinct, "^10\\.")) |> 
  pull(pub_reglinked_url_distinct)

dois_earliest_url <- urls_to_check |> 
  filter(str_detect(earliest_pub_distinct, "^10\\.")) |> 
  pull(earliest_pub_distinct)

# dois_to_check <- c(dois_to_check, dois_earliest_url, dois_reglinked_url) |> 
#   unique()

doi_status_tib <- dois_to_check |> 
  ping_dois()

# error_logs <- error_logs |> 
#   mutate(rule = case_when(
#     str_detect(rule, "premature") ~ "prematurely ended no DE protocol",
#     .default = rule
#   ))
# 
# url_check <- urls_to_check |>
#   filter(!is.na(earliest_pub_url_clean)) |>
#   slice(1:10) |>
#   pull(earliest_pub_url_clean) |>
#   ping_dois()

non_resolving_dois <- doi_status_tib |> 
  filter(status > 200) |>
  mutate(doi = str_remove_all(doi, "(\\?|#).*")) |> 
  pull(doi)

non_resolving_dois |> 
  ping_dois()

### check 
#### earliest url but no earliest doi
### cleaning of the proxy ad leaving urls as are in the export

qa_doi <- urls_to_check |> 
  filter(earliest_pub_doi %in% non_resolving_dois)

error_logs <- update_error_log(error_logs, qa_doi, "doi does not resolve", "earliest_pub_doi")

qa_doi <- urls_to_check |> 
  filter(pub_reglinked_url_distinct %in% non_resolving_dois)

error_logs <- update_error_log(error_logs, qa_doi, "doi does not resolve", "pub_reglinked_url")

pub_reglinked_url_distinct <- urls_to_check |> 
  filter(!is.na(pub_reglinked_url_distinct),
         !str_detect(pub_reglinked_url_distinct, "^10"))

responses <- pub_reglinked_url_distinct |>
  mutate(pub_reglinked_url_response = map_dbl(pub_reglinked_url_distinct, url_extract_response))

### one was 0 manually checked and it resolved

earliest_pub_distinct <- urls_to_check |> 
  filter(!is.na(earliest_pub_distinct),
         !str_detect(earliest_pub_distinct, "^10"),
         earliest_pub_distinct != pub_reglinked_url_distinct)

responses <- earliest_pub_distinct |>
  mutate(pub_reglinked_url_response = map_dbl(earliest_pub_distinct, url_extract_response))

### clean DOI and URLS

clean_doi_urls <- urls_to_check |> 
  select(trial_id, earliest_pub_url_clean,
         pub_reglinked_url_clean)

extractions_filtered_enriched_cleaned_dois <- extractions_filtered_enriched |> 
  mutate(earliest_pub_url_clean = NA_character_,
         pub_reglinked_url_clean = NA_character_) |> 
  rows_upsert(clean_doi_urls, by = "trial_id")

## is_earlier_pub_google

qa_google_no <- extractions_filtered_enriched_cleaned_dois |> 
  filter(is_earlier_pub_google == "No",
         # regex below verified to contain same link
         # but with alternative URLS for pub_reglinked_url and
         # earliest_pub_url, therefore should be excluded here
         !str_detect(pub_reglinked_url_clean, "aerzt|bmj|nre|lww|kidney|32240835|33762129"),
         tolower(pub_reglinked_url_clean) != tolower(earliest_pub_url_clean)) |> 
  select(trial_id, pub_reglinked_url_clean, earliest_pub_url_clean, everything())

qa_google_yes <- extractions_filtered_enriched_cleaned_dois |> 
  filter(is_earlier_pub_google == "Yes",
         tolower(pub_reglinked_url_clean) == tolower(earliest_pub_url_clean)) |> 
  select(trial_id, pub_reglinked_url_clean, earliest_pub_url_clean, everything())

## Here qa_google_no results are changed to "Yes" and
## qa_google_yes results are changed to "No":
google_corrected <- extractions_filtered_enriched |> 
  mutate(is_earlier_pub_google = case_when(
    trial_id %in% qa_google_yes$trial_id ~ "No",
    trial_id %in% qa_google_no$trial_id ~ "Yes",
    .default = is_earlier_pub_google
  )) |> 
  select(trial_id, is_earlier_pub_google)
## update table with corrected results
extractions_filtered_enriched_cleaned_google <- extractions_filtered_enriched |> 
  rows_upsert(google_corrected, by = "trial_id")

## publication_type

# tt <- tibble(doi = dois_to_check)

oa_resp <- oa_fetch(entity = "works", doi = dois_to_check,
                    mailto = Sys.getenv("EMAIL"),
                    verbose = TRUE)

oa_pubtype <- oa_resp |> 
  mutate(earliest_pub_doi = str_extract(doi, "10\\..*") |> 
           tolower()) |> 
  distinct(earliest_pub_doi, .keep_all = TRUE) |> 
  select(earliest_pub_doi, pub_type_openalex = type)

# missing_oa <- setdiff(dois_to_check, oa_pubtype$earliest_pub_doi) |> 
#   str_replace("%2F", "/") |> 
#   tolower()

# retrying here just to make sure records were not missed due to 
# temporary issues
# oa_missing <- oa_fetch(entity = "works", doi = missing_oa, mailto = Sys.getenv("EMAIL"))

oa_resp |> 
  count(type)

oa_resp_pmids <- oa_resp |> 
  # rowwise() |> 
  unnest_longer(ids) |> 
  filter(str_detect(ids, "pubmed")) |> 
  mutate(pmid = str_extract(ids, "\\d+"),
         doi = str_extract(doi, "10\\..*") |> 
           tolower()) |> 
  select(doi, pmid)

#### the following code may break due to unresolvable DOIs

pubtype_meta <- tibble(doi = dois_to_check) |>
# pubtype_meta <- tibble(doi = missing_oa) |> 
  distinct(doi) |> 
  inner_join(oa_resp_pmids, by = "doi") |> 
  easyRPubMed::get_metadata(pmid, chunksize = 30, api_key = Sys.getenv("NCBI_KEY"))

pmid_pubtype <- pubtype_meta |> 
  list_rbind() |> 
  select(earliest_pub_doi = doi, pub_type_pubmed = pubtype)

## publication table for result integration
# clean_pub <- pub_search_table |> 
#   select(trial_id, registry_url:crossreg_ctgov) |> 
#   left_join(extractions_filtered_enriched |> select(-registry, -crossreg_id),
#             by = "trial_id") |> 
#   mutate(earliest_pub_doi = tolower(earliest_pub_doi)) |> 
#   left_join(oa_pubtype, by = "earliest_pub_doi") |> 
#   left_join(pmid_pubtype, by = "earliest_pub_doi") |> 
#   write_excel_csv(here("data", "processed", paste0("reordered_snapshot_", today(), "_pubtypes_new.csv")))

results_clean <- extractions_filtered_enriched_cleaned_google |> 
  filter(has_withdrawn_validated == FALSE,
         trial_id != "2012-003604-13") |> 
  select(-contains("s_withdrawn")) |> 
  mutate(earliest_pub_doi = tolower(earliest_pub_doi)) |> 
  left_join(oa_pubtype, by = "earliest_pub_doi") |> 
  left_join(pmid_pubtype, by = "earliest_pub_doi") |> 
  arrange(crossreg_id, trial_id) |>
  write_csv(here("data", "processed", paste0("results_clean_", today(), ".csv")))

error_logs |> 
  write_csv(here("data", "processed", paste0("error_logs_", today(), ".csv")))

# clean_pub_old <- read_csv(here("data", "processed", "reordered_snapshot_20251105_pubtypes.csv")) |> 
#   distinct(trial_id, .keep_all = TRUE)
# clean_pub_n <- clean_pub |> 
#   distinct(trial_id, .keep_all = TRUE)
# all.equal(clean_pub_n, clean_pub_old)
# count(clean_pub_n, is.na(oa_pubtype))
# count(clean_pub, is.na(oa_pubtype))
# count(clean_pub_n, is.na(pubtype_pmid))
# count(clean_pub, is.na(pubtype_pmid))

clean_pub_n |> 
  left_join(clean_pub_old, by = "trial_id") |> 
  filter(drks_sumres_date.x != drks_sumres_date.y | is.na(drks_sumres_date.x) | is.na(drks_sumres_date.y)) |> 
  filter(is.na(drks_sumres_date.y) & !is.na(drks_sumres_date.x)) |> 
  pull(trial_id)




extractions_filtered_enriched |> 
  filter(!trial_id %in% pub_search_table$trial_id)

dupes_extr <- extractions_filtered_enriched |> 
  get_dupes(trial_id) |> 
  select(extractor, trial_id, dupe_count)


### add further manual corrections if any, after validations complete
results_clean <- read_csv(here("data", "processed", "results_clean_2026-04-09.csv"))

