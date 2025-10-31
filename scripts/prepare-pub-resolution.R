# Prepare snapshot of current extractions for manual resolution

library(readr)
library(dplyr)
library(dplyr)
library(janitor)
library(readxl)


# Load and prepare manual extractions -------------------------------------


# Load in (interim) data from the pub search from 29 October 2025
data_extracted_raw <- read_csv("snapshot_1_29102025.csv")

# Filter for already reviewed cases
data_extracted <- data_extracted_raw |>
  filter(
    !is.na(extractor)
  )

# Explore duplicates
dupes <- janitor::get_dupes(data_extracted, trial_id)  

dupes_differences <- dupes %>%
  group_by(trial_id) %>%
  summarise(
    all_same = all(across(-timestamp, ~ length(unique(.x)) == 1))
  )

# Select dupes that can be removed based on max timestamp

# DRKS00011653: confirmed with the coder
# DRKS00009418: no differences
# TODO: 2014-005344-17 clarify which to keep!

dupes_to_remove <- c(
  "DRKS00011653",
  "DRKS00009418"
  )

# Remove dupes
data_extracted_no_dupes <- data_extracted |>
  group_by(trial_id) |>
  filter(
    case_when(
      trial_id %in% dupes_to_remove ~ timestamp == max(timestamp),  # keep latest for target trials
      TRUE ~ TRUE                                                 # keep all others
    )
  ) |>
  ungroup()

# Add counts and arrange data by crossreg cluster
data_extracted_sorted <- data_extracted_no_dupes |>
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

print(paste0("There are ", length(unique(dupes$trial_id)), " duplicates in the dataset!"))
print(paste0("A total of ", length(dupes_to_remove), " duplicates have been removed!"))



# Implement corrections ---------------------------------------------------


# Log correction requested by Till (by mistake did not acknowledge pub already found in first reg of crossreg cluster)
corrections <- tibble(
  trial_id = "2007-007262-38",             
  crossreg_pub_already_identified = "Yes",
  crossreg_new_earlier_pub_found = "No, neither linked in the registration nor via the Google search",
  is_pub_reglinked = NA_character_,
  is_pub_google = NA_character_
)

# Integrate corrections back into the dataset
data_extracted_sorted <- data_extracted_sorted |>
  rows_upsert(corrections, by = "trial_id")

# Add comment from Slack (MW) for resolution
data_extracted_sorted <- data_extracted_sorted |>
  mutate(
    final_comments = if_else(trial_id == "NCT03626831", "please check possible earlier publication https://doi.org/10.1161/circ.146.suppl_1.13958", final_comments)
    )

# TODO Add corrections needed to the 'crossreg_is_subsequent_reg` variable


# Add variables for manual resolution ------------------------------------------
resolution_pubs <- data_extracted_sorted |>
  mutate(
    # Add flag for whether review is needed and pre-populate with second review field
    review_flag = if_else(second_review_requested == "Yes, second review needed", "Review Requested", NA_character_),
    reviewed_by = NA_character_,
    comments = NA_character_
  )



# Resolution of EUCTR recruitment status ---------------------------------------

# Flag cases that came up in general comments that are not prematurely ended or are but not with "I am unsure" comment
# There is a comment for 2013-003714-40 but this trial was eventually excluded

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
    crossreg_id,
    extractor,
    registry,
    registry_url,
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


# Add specific cases that TB flagged needing review
extraction_tb <- read_excel("extraction-tb.xlsx")

extraction_tb_drks <- extraction_tb |>
  filter(
    registry == "DRKS",
    Extractor == "TB"
  ) |>
  rename(
    comments = ...16
  ) |>
  # join in comments from the extractions
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
    drks_is_sumres,
    drks_sumres_date,
    drks_comment_sumres
  )

# TODO Next, for all cases with summary results = YES, check or determine summary results date

  