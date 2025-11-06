# Prepare snapshot of current extractions for manual resolution

library(readr)
library(dplyr)
library(dplyr)
library(janitor)
library(readxl)
library(rlang)
library(stringr)


# Load and prepare manual extractions -------------------------------------

# Load in (interim) data from the pub search from 3 November 2025
data_extracted_raw <- read_csv("reordered_snapshot_20251103.csv")

data_extracted_raw <- data_extracted_raw |>
  rename(
    pub_type_openalex = oa_pubtype,
    pub_type_pmid = pubtype_pmid
  )

# Filter for already reviewed cases
data_extracted <- data_extracted_raw |>
  mutate(
    extracted = !is.na(extractor)
    ) |>
  group_by(
    crossreg_id
    ) |>
  mutate(
    any_extracted_in_cluster = any(extracted, na.rm = TRUE)
    ) |>
  ungroup() |> 
  filter(
    # keep extracted single registrations
    (is.na(crossreg_id) & extracted) |
      # or any in a cluster with an extracted record
      (!is.na(crossreg_id) & (extracted | any_extracted_in_cluster))
    ) |>
  select(-extracted, -any_extracted_in_cluster)

# Display missing extractions
missing_extractions <- data_extracted |>
  filter(
    is.na(extractor)
  )

data_extracted <- data_extracted |>
  mutate(
    skipped_reg_in_crossreg = if_else(crossreg_id %in% missing_extractions$crossreg_id, TRUE, FALSE)
  )

# NCT01289301 (DF): skipped as withdrawn without enrolment
# 2012-005717-39 (GB): skipped as no match in EUCTR
# 2014-001427-79 (GB): skipped as not a cross-registration

# Explore duplicates
dupes <- janitor::get_dupes(data_extracted, trial_id)  

# Check if dupes are identical or different
dupes_check_differences <- dupes %>%
  group_by(trial_id) %>%
  summarise(
    all_same = all(across(-timestamp, ~ length(unique(.x)) == 1))
  )

# Identical dupes
dupes_no_differences <- dupes_check_differences |>
  filter(all_same)

# For non-identical dupes, select which can be removed based on max timestamp
dupes_take_maxdate <- c(
  "DRKS00011653", # MMP clarified to keep max timestamp
  "2014-005344-17", # SY clarified to keep max timestamp
  "DRKS00009418") # no differences

# For non-identical dupes, select the one with the max timestamp
data_extracted_deduped <- data_extracted |>
  group_by(trial_id) |>
  filter(
    case_when(
      trial_id %in% dupes_take_maxdate ~ timestamp == max(timestamp),  # keep latest for target trials
      TRUE ~ TRUE                                                 # keep all others
    )
  ) |>
  ungroup()

# Add counts and arrange data by crossreg cluster
data_extracted_sorted <- data_extracted_deduped |>
  group_by(crossreg_id) |> 
  mutate(n = 1:n(),
         # Add number of crossreg per cluster
         crossreg_n = if_else(is.na(crossreg_id), 1, max(n)),
         # Add number of subsequent reg in a cluster
         n_subsequent_reg = sum(crossreg_is_subsequent_reg == "Yes", na.rm = TRUE)
  ) |> 
  ungroup() |>
  arrange(
    crossreg_id, 
    crossreg_is_subsequent_reg
  )

# List cases where `crossreg_is_subsequent_reg` wrongly encoded and needs to be corrected
check_subsequent_crossreg <-
  data_extracted_sorted |>
  filter(
    crossreg_n == 2 & n_subsequent_reg != 1 | crossreg_n == 3 & n_subsequent_reg != 2
  ) |>
  anti_join(missing_extractions, by = "crossreg_id")


# Implement corrections ---------------------------------------------------


# Log correction requested by TB (by mistake did not acknowledge pub already found in first reg of crossreg cluster)
corrections_1 <- tibble(
  trial_id = "2007-007262-38",             
  crossreg_pub_already_identified = "Yes",
  crossreg_new_earlier_pub_found = "No, neither linked in the registration nor via the Google search",
  is_pub_reglinked = NA_character_,
  is_pub_google = NA_character_
)

# Log corrections needed to the 'crossreg_is_subsequent_reg` variable
corrections_2 <- tibble(
  trial_id = c("NCT01614132", "NCT01717677"),
  crossreg_is_subsequent_reg = c("No", "No"),
  crossreg_pub_already_identified = c(NA_character_, NA_character_)
  )

# Integrate corrections_1 back into the dataset
data_extracted_sorted <- data_extracted_sorted |>
  rows_upsert(corrections_1, by = "trial_id")

# Integrate corrections_2 back into the dataset
data_extracted_sorted <- data_extracted_sorted |>
  rows_upsert(corrections_2, by = "trial_id")

# Add comments from Slack, emails, trial sheets, etc.
# TODO see if I need to make a change for MW's comment in Slack
comment_updates <- c(
  "NCT03626831"      = "please check possible earlier publication https://doi.org/10.1161/circ.146.suppl_1.13958", #MW slack
  "2012-005717-39"   = "no match in EUCTR", #GB trial sheet
  "2014-001427-79"   = "not a cross-registration", #GB trial sheet
  "NCT02174848"      = "not a cross-registration", #GB trial sheet
  "NCT01401582" = "Please double check if I got the earliest eligible publication", #TB trial sheet
  "NCT04695964" = "WARNING Observational study, so I did not search for results", #TB trial sheet
  "NCT03236454" = "Letter to the editor - not sure if peer reviewed", #TB trial sheet
  "NCT02854319" = "REPEAT SEARCH. Many previous trials of this device so I may have missed the publication for this trial", #TB trial sheet
  "NCT01183767" = "CROSS REGISTERED. Results on EUCRT but these do NOT contain results: https://www.clinicaltrialsregister.eu/ctr-search/trial/2009-016482-28/DE", #TB trial sheet
  "NCT04698993" = "Does not seem to be an UMC study", #TB trial sheet
  "NCT02419378" = "pp from the registration are only one cohort within the CTRP. This seems to be the only CTRP reporting on the results from this cohort. Would you judge this as an eligible results publication?" # MMP slack
)

# Add comments into final_comments provided this is empty

data_extracted_sorted <- data_extracted_sorted |>
  mutate(
    new_comment = comment_updates[trial_id],
    
    # Concatenate if existing comment is non-empty, otherwise just replace
    final_comments = case_when(
      !is.na(new_comment) & !is.na(final_comments) & final_comments != "" ~ 
        str_c(final_comments, " | ", new_comment),  # append with separator
      
      !is.na(new_comment) ~ new_comment,  # new comment only
      
      TRUE ~ final_comments  # unchanged
    )
  ) |>
  select(-new_comment)

# TODO Add any other corrections

# Add variables for manual resolution ------------------------------------------

# Note for cross-registrations: the check box responses for second review requested
# and high media interest will be considered as soon as it is checked for any
# trial ID in a cross-registration cluster

resolution_pubs <- data_extracted_sorted |>
  group_by(crossreg_id) |>
  mutate(
    # Only for crossreg clusters, extend the second_review_requested answer to all reg
    second_review_requested = case_when(
      !is.na(crossreg_id) & any(second_review_requested == "Yes, second review needed", na.rm = TRUE) ~ 
        "Yes, second review needed",
      TRUE ~ second_review_requested
    ),
    # Only for crossreg clusters, extend the high_media_interest answer to all reg
    high_media_interest = case_when(
      !is.na(crossreg_id) & any(high_media_interest == "Yes, this study might be of high media interest", na.rm = TRUE) ~ 
        "Yes, this study might be of high media interest",
      TRUE ~ high_media_interest
    )
  ) |>
  ungroup()

# Flag cases where no pub found
resolution_pubs <- resolution_pubs |>
  mutate(
    no_pub = is.na(earliest_pub_type)
  ) |>
  group_by(
    crossreg_id
  ) |>
  mutate(
    no_pub_in_cluster = if_else(is.na(crossreg_id), NA, all(no_pub))
  ) |>
  ungroup() |>
  mutate(
    no_pub_found = if_else((is.na(crossreg_id) & no_pub) | (!is.na(crossreg_id) & no_pub_in_cluster), TRUE, FALSE)
  ) |>
  select(-no_pub, -no_pub_in_cluster)

# Update as needed
pubtypes_to_check <- c("abstract", "communication", "conference", "letter", "news", "poster", "presentation", "review", "talk")

resolution_pubs <- resolution_pubs |>
  mutate(
    # Create flag for publication types to check
    pub_type_check_needed = if_else(
      if_any(
      c(earliest_pub_type, pub_type_openalex, pub_type_pmid),
      ~str_detect(., regex(paste0("\\b(", paste(pubtypes_to_check, collapse = "|"), ")\\b"), ignore_case = TRUE))
    ), TRUE, NA),
    # Triage whether review is needed based on existing data and comments
    review_action = case_when(
      second_review_requested == "Yes, second review needed" ~ "Review Requested (+ pubtype if TRUE)",
      no_pub_found ~ "No publication Found",
      earliest_pub_matches_reg == "I have a doubt" | !is.na(earliest_pub_matches_reg_comment) ~ "Review Needed (+ pubtype if TRUE)",
      !is.na(crossreg_comment1) | !is.na(crossreg_comment2) | !is.na(final_comments) ~ "Check Comments (+ pubtype if TRUE)",
      pub_type_check_needed ~ "Check pub type",
      TRUE ~ "No Review Needed"
    ),
    review_status = NA_character_,
    reviewer = NA_character_,
    review_has_pub = NA,
    review_doi = NA,
    review_pub_type = NA,
    review_pmid = NA,
    review_comments = NA_character_
  ) |>
  arrange(
    factor(review_action, levels = c(
      "Review Requested (+ pubtype if TRUE)", 
      "Review Needed (+ pubtype if TRUE)", 
      "Check Comments (+ pubtype if TRUE)", 
      "Check pub type",
      "No Review Needed", 
      "No publication Found"))
    )

resolution_pubs <- resolution_pubs |>
  relocate(
    euctr_comment_withdrawn,
    crossreg_comment1,
    crossreg_comment2,
    earliest_pub_matches_reg_comment, 
    final_comments,
    .before = pub_type_check_needed
    ) |>
  relocate(
    pub_type_check_needed, .before = review_action
  )

# Resolution of EUCTR recruitment status ---------------------------------------

# Flag cases that came up in general comments that are not prematurely ended or are but not with "I am unsure" comment
# There is a comment for 2013-003714-40 but this trial was marked as 'exclude'

additional_euctr_recruitment_all_comments <- c(
  "2012-001725-26",
  "2014-002765-30",
  "NCT01183767",
  "2015-001309-14",
  "2012-000411-91"
)

resolution_euctr_recruitment <- data_extracted_sorted |>
  select(
    trial_id,
    extractor,
    registry,
    registry_url,
    crossreg_id,
    euctr_is_prematurely_ended,
    euctr_is_no_enrolment,
    euctr_comment_withdrawn,
    final_comments
  ) |>
  filter(
    # Filter for cases where doubt or general doubt about enrollment
    euctr_is_no_enrolment == "I am unsure: there is a \"Results\" tab in the EUCTR registration but I still have a doubt" | euctr_is_no_enrolment == "I am unsure: there is no \"Results\" tab in the EUCTR registration" | trial_id %in% additional_euctr_recruitment_all_comments
  )


# Resolution of DRKS summary results -------------------------------------------


# Add specific cases that TB flagged needing review in the trial sheet
extraction_tb <- read_excel("extraction-tb.xlsx")

extraction_tb_drks <- extraction_tb |>
  filter(
    registry == "DRKS",
    Extractor == "TB"
  ) |>
  rename(
    comments = ...16
  ) |>
  # join in results of the extraction
  left_join(
    data_extracted_sorted |> select(trial_id, drks_is_sumres), by = "trial_id"
  ) |>
  # Filter for those where a review was requested
  filter(
    !is.na(comments)
  )

drks_trials_review_tb <- extraction_tb_drks$trial_id

# Additional trial to check for DRKS summary results based on all remaining comments (all extractors)
supp_drks_to_check <- "DRKS00010783"


# Limit to DRKS trials where response for DRKS sumres was something other than "No" or there was a doubt (all extractors) or a review was requested by TB
resolution_drks_sumres <- data_extracted_sorted |>
  filter(
    registry == "DRKS",
    drks_is_sumres != "No, summary results are not uploaded in the DRKS registration" | trial_id %in% drks_trials_review_tb | trial_id == supp_drks_to_check
    ) |> 
  select(
    trial_id,
    extractor,
    registry,
    registry_url,
    crossreg_id,
    drks_is_sumres,
    drks_sumres_date,
    drks_comment_sumres,
    final_comments
  )

# TODO Next, for all cases with summary results = YES, check or determine summary results date


# Check of publication type (MMP) -----------------------------------------

resolution_pubtype_mmp <- resolution_pubs |>
  filter(
    extractor == "Merle MP",
    !no_pub_found,
    !is.na(earliest_pub_url)
    ) |>
  select(
    trial_id,
    registry_url,
    registry,
    crossreg_id,
    timestamp,
    extractor,
    earliest_pub_url,
    earliest_pub_type,
    earliest_pub_doi,
    earliest_pub_matches_reg_comment,
    final_comments
  ) |>
  arrange(timestamp)

resolution_pubtype_mmp <- resolution_pubtype_mmp |>
  relocate(
    timestamp,
    extractor,
    .before = trial_id
  ) |>
  relocate(
    earliest_pub_doi, .before = earliest_pub_type
  ) |>
  mutate(
    earliest_pub_type_corrected = NA_character_
  ) |>
  relocate(earliest_pub_type_corrected, .after = earliest_pub_type)

# Notifications -----------------------------------------------------------

print(paste0("There are ", length(unique(dupes$trial_id)), " unique duplicates in the dataset!"))
print(paste0("A total of ", nrow(data_extracted) - nrow(data_extracted_deduped), " duplicated trial IDs have been removed!"))

# write_csv(your_data, "your_file.csv", na = "")
  