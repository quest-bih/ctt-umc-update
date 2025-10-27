# TODO @Vladi READ THIS BEFORE RUNNING!
# There are two sections:
# 1) your suggestions + my comments for changes;
# 2) my revisions already implemented (if you agree, this is good to go)


library(dplyr)
library(readr)
library(readxl)
library(janitor)


# Load in (interim) data from the pub search
data_raw <- read_excel("pub-search.xlsx")

# Harmonize very long column names
data_clean_colnames <- data_raw %>% clean_names()


# Vladi's suggestions + comments from DF ----------------------------------


# Rename all columns (the colnames from the extraction form are listed as comments)
data_new_colnames <- data_clean_colnames %>%
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
    # TODO: @Vladi, I would change this to euctr_is_prematurely_ended
    is_prematurely_ended = does_the_euctr_registration_indicate_that_the_trial_status_is_prematurely_ended,
    
    # Was the trial withdrawn without enrolment?
    # TODO: @Vladi, I would change this to euctr_is_no_enrolment
    is_no_enrolment = was_the_trial_withdrawn_without_enrolment,
    
    # Explain why you are unsure whether the trial was withdrawn without enrolment
    # TODO: @Vladi, I would change this to euctr_comment_withdrawn
    comment_withdrawn = explain_why_you_are_unsure_whether_the_trial_was_withdrawn_without_enrolment,
    
    # FOCUS ONLY ON DRKS
    
    # Have summary results been uploaded in the registration?
    # TODO: @Vladi, I would change this to drks_is_sumres
    is_sumres = have_summary_results_been_uploaded_in_the_registration,
    
    # Date of summary results in DRKS
    # TODO: @Vladi, I would change this to drks_sumres_date
    sumres_drks_date = date_of_summary_results_in_drks,
    
    # Explain why you are unsure whether summary results have been uploaded in DRKS
    # TODO: @Vladi, I would change this to drks_comment_sumres
    comment_sumres_drks = explain_why_you_are_unsure_whether_summary_results_have_been_uploaded_in_drks,
    
    # FOCUS ON CROSS-REGISTRATION CLUSTERS
    
    # Does your extraction sheet indicate that this is a cross-registration of a trial you already extracted?
    # TODO: @Vladi, I would change this to crossreg_is_subsequent_reg (this should only be TRUE for all non-first registrations in a crossreg cluster!)
    is_crossreg = does_your_extraction_sheet_indicate_that_this_is_a_cross_registration_of_a_trial_you_already_extracted,
    
    # In case you have a comment about the cross-registration, enter it here:
    # TODO: @Vladi, I would change this to crossreg_comment1
    comment_crossreg1 = in_case_you_have_a_comment_about_the_cross_registration_enter_it_here_12,
    
    # Was an eligible results publication already identified in the previous extraction(s) for this trial?
    # TODO: @Vladi, I would change this to crossreg_pub_already_identified
    is_previously_extracted = was_an_eligible_results_publication_already_identified_in_the_previous_extraction_s_for_this_trial,
    
    # In case you have a comment about the cross-registration, enter it here:
    # TODO: @Vladi, I would change this to crossreg_comment2
    comment_crossreg2 = in_case_you_have_a_comment_about_the_cross_registration_enter_it_here_14,
    
    # Based on the current trial ID, can you identify an earlier eligible results publication than the one already found?
    # TODO: @Vladi, I would change this to crossreg_new_earlier_pub_found
    is_earlier_than_previous = based_on_the_current_trial_id_can_you_identify_an_earlier_eligible_results_publication_than_the_one_already_found,
    
    # NOW FOCUS ON SINGLE REGISTRATIONS AND *ONLY* FIRST REGISTRATION OF CROSS-REGISTRATION CLUSTERS
    
    # Was an eligible results publication linked in the registration?
    is_pub_reglinked = was_an_eligible_results_publication_linked_in_the_registration,
    
    # Provide the URL here:
    # TODO: @Vladi, I would change this to pub_reglinked_url
    url_reglinked = provide_the_url_here,
    
    # IF A PUB WAS LINKED IN THE REGISTRATION
    
    # Did you find an earlier eligible results publication via the Google search?
    # TODO: @Vladi, I would change this to is_earlier_pub_google
    is_se_earlier = did_you_find_an_earlier_eligible_results_publication_via_the_google_search,
    
    # IF NO PUB WAS LINKED IN THE REGISTRATION
    
    # Did you find an eligible results publication via the Google search?
    # TODO: @Vladi, I would change this to is_pub_google
    is_se_pub = did_you_find_an_eligible_results_publication_via_the_google_search,
    
    # NOW FOCUSING ON EARLIEST ELIGIBLE PUB FOUND (WHETHER LINKED OR GOOGLE)
    
    # Provide the URL of this publication:
    # TODO: @Vladi, I would change this to earliest_pub_url
    url_se = provide_the_url_of_this_publication,
    
    # What type of results publication is it?
    # TODO: @Vladi, I would change this to earliest_pub_type
    se_pub_type = what_type_of_results_publication_is_it,
    
    # Does the publication match the registration on the specified criteria?
    # TODO: @Vladi, I would change this to earliest_pub_matches_reg
    pub_matches_reg = does_the_publication_match_the_registration_on_the_specified_criteria,
    
    # If you had a doubt regarding matching on the eligibility criteria, you can add a comment here:
    # TODO: @Vladi, I would change this to earliest_pub_matches_reg_comment
    comment_pub_matches_reg = if_you_had_a_doubt_regarding_matching_on_the_eligibility_criteria_you_can_add_a_comment_here,
    
    # Is there a Digital Object Identifier (DOI)?
    # TODO: @Vladi, I would change this to earliest_pub_has_doi
    has_doi = is_there_a_digital_object_identifier_doi,
    
    # Publication DOI (always starting with "10")
    # TODO: @Vladi, I would change this to earliest_pub_doi
    doi = publication_doi_always_starting_with_10,
    
    # Is there a PubMed ID (PMID)?
    # TODO: @Vladi, I would change this to earliest_pub_has_pmid
    has_pmid = is_there_a_pub_med_id_pmid,
    
    # Enter the PMID here:
    # TODO: @Vladi, I would change this to earliest_pub_pmid
    pmid = enter_the_pmid_here,
   
     # Publication date
    # TODO: @Vladi, I would change this to earliest_pub_date
    pub_date = publication_date,
    
    # Check this box if you feel a second review of the publication search is needed for this trial ID
    # TODO: @Vladi, I would change this to second_review_requested
    check_second_review = check_this_box_if_you_feel_a_second_review_of_the_publication_search_is_needed_for_this_trial_id,
    
    # If this unreported study might be of high media interest, you can flag this here
    high_media_interest = if_this_unreported_study_might_be_of_high_media_interest_you_can_flag_this_here,
    
    # Any final comments on the extraction can be entered here
    final_comments = any_final_comments_on_the_extraction_can_be_entered_here_dont_miss_to_click_submit
  )


# Revisions implemented ---------------------------------------------------

# TODO @Vladi now with all suggestions implemented, so you can just use this if you agree with changes 

data_new_colnames <- data_clean_colnames |>
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

