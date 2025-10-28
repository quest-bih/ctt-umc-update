library(tidyverse)
library(ctregistries)
library(progressr)
library(furrr)
library(here)
library(janitor)
library(readxl)
library(testthat)

source(here("scripts", "utils.R"))

plan(multisession)
handlers(global = TRUE)


extractions_raw <- read_xlsx(here("data", "processed", "IV3_PUBSEARCH (Main extraction)-3.xlsx"))
euctr_inex <- read_csv(here("data", "processed", "inclusion_exclusion_euctr.csv"))

unfiltered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_unfiltered.csv"))
filtered_crossreg_ids <- read_csv(here("data", "processed", "crossreg_filtered.csv"))
euctr_export <- read_csv(here("data", "processed", "EUCTR_sample.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_export <- read_csv(here("data", "processed", "CTgov_sample.csv"))


extractions <- extractions_raw |>
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
    crossreg_comment1 = in_case_you_have_a_comment_about_the_cross_registration_enter_it_here_12,
    
    # Was an eligible results publication already identified in the previous extraction(s) for this trial?
    crossreg_pub_already_identified = was_an_eligible_results_publication_already_identified_in_the_previous_extraction_s_for_this_trial,
    
    # In case you have a comment about the cross-registration, enter it here:
    crossreg_comment2 = in_case_you_have_a_comment_about_the_cross_registration_enter_it_here_14,
    
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
  )

####### sanity checks
n_per_extractor <- extractions |> 
  count(extractor, is.na(crossreg_is_subsequent_reg))

## registry validation
extractions |> 
  mutate(reg2 = get_registry_name(trial_id)) |> 
  filter(registry != reg2) |> 
  nrow() |>
  expect_equal(0)
## prematurely ended
extractions |> 
  count(registry, euctr_is_prematurely_ended) |>
  filter((registry != "EUCTR" & !is.na(euctr_is_prematurely_ended)) |
           (registry == "EUCTR") & is.na(euctr_is_prematurely_ended)) |> 
  nrow() |> 
  expect_equal(0)
  
prem_end_extracted <- extractions |> 
  filter(euctr_is_prematurely_ended == "Yes")

## are there falsely marked prematurely ended TRNs 
prem_euctr <- euctr_inex |> 
  semi_join(prem_end_extracted, by = "trial_id") |> 
  group_by(trial_id) |> 
  mutate(has_prematurely_ended = any(status == "Prematurely Ended")) |> 
  select(trial_id, eudract_number_with_country,
         has_prematurely_ended, status) |> 
  arrange(trial_id) |> 
  filter(!has_prematurely_ended) |> 
  nrow() |> 
  expect_equal(0)

## no enrolment
extractions |> 
  count(euctr_is_prematurely_ended, missing_no_enrolment = is.na(euctr_is_no_enrolment)) |> 
  filter((euctr_is_prematurely_ended == "Yes" & missing_no_enrolment == TRUE) | 
           (euctr_is_prematurely_ended != "Yes" & missing_no_enrolment == FALSE)) |> 
  nrow() |> 
  expect_equal(0)

## sumres
extractions |> 
  count(registry, drks_is_sumres) |> 
  filter((registry != "DRKS" & !is.na(drks_is_sumres)) | 
           (registry == "DRKS") & is.na(drks_is_sumres)) |> 
  nrow() |> 
  expect_equal(0)

## crossreg
extracted_crossreg <- extractions |> 
  mutate(is_crossreg_filtered = trial_id %in% filtered_crossreg_ids$trial_id) |> 
  filter(euctr_is_prematurely_ended != "Yes" | is.na(euctr_is_prematurely_ended)) |> 
  left_join(filtered_crossreg_ids |> select(trial_id, crossreg_id), by = "trial_id") |> 
  group_by(extractor, crossreg_id) |> 
  mutate(n = 1:n(),
         crossreg_n = if_else(is_crossreg_filtered == FALSE, 1, max(n)),
         n_yes = sum(crossreg_pub_already_identified == "Yes")) |> 
  ungroup()

extracted_crossreg |> 
  count(crossreg_n, n_yes)

## missed cross-registrations
qa_missed_crossreg <- extracted_crossreg |> 
  filter(crossreg_n == 1, n_yes == 1) 
qa_missed_crossreg |> 
  nrow() |> 
  expect_equal(0)

## diads
qa_diads <- extracted_crossreg |> 
  filter(crossreg_n == 2, n_yes != 1)
qa_diads |> 
  nrow()  |> 
  expect_equal(0)
  
## triads
qa_triads <- extracted_crossreg |> 
  filter(crossreg_n == 3, n_yes != 2)
qa_triads |> 
  nrow() |> 
  expect_equal(0)

## crossreg_pub_already_identified
extracted_crossreg |> 
  filter((crossreg_is_subsequent_reg == "No") & ) | ())
  count(crossreg_is_subsequent_reg, crossreg_pub_already_identified)
