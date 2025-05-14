##### Extracting and cleaning TRNs, cross-registration prep

library(tidyverse)
library(here)
library(furrr)
library(progressr)
library(jsonlite)
library(yaml)

drks_tib <- fromJSON(here("data", "raw", "DRKS_search_20250513.json"))

source(here("scripts", "utils.R"))

# AACT_folder <- "C:/Datenablage/AACT/AACT_dataset_240927"
AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_250513")

ctgov_id_info <- file.path(AACT_folder, "id_information.txt") |> 
  read_delim(delim = "|")
ctgov_aliases <- read_csv(here("data", "processed", "ctgov_aliases.csv"))

### TODO: import euctr secondary TRN tables for existence check

drks_secondary_ids <- drks_tib |> 
  select(drksId, secondaryIds) |> 
  unnest(secondaryIds) |> 
  mutate(across(where(is.character), \(x) na_if(x, "") |> 
                  str_remove_all("\\s") |> 
                  na_if("-"))) |> 
  mutate(otherPrimaryRegisterName = case_when(
    str_detect(otherPrimaryRegisterId, "(?<!S)NCT") ~ "ClinicalTrials.gov",
    str_detect(otherPrimaryRegisterId, regexes$EudraCT) ~ "EUCTR",
    str_detect(otherPrimaryRegisterId, regexes$DRKS) ~ "DRKS",
    .default = otherPrimaryRegisterName
  ),
  euctr_clean = case_when(
    !is.na(eudraCtNumber) ~ eudraCtNumber,
    .default = otherPrimaryRegisterId |>
      str_extract(regexes$EudraCT) 
  ),
  ##### tihs part commented out because no obsolete ctgov TRNs were in the dataset
  # ctgov_clean_preupdate = otherPrimaryRegisterId |>
  #   str_extract(regexes$ClinicalTrials.gov),
  # ctgov_clean = map_chr(ctgov_clean_preupdate, \(x) update_ctgov_alias(x, ctgov_aliases)),
  # ctgov_updated = ctgov_clean != ctgov_clean_preupdate,
  ctgov_clean = otherPrimaryRegisterId |>
      str_extract(regexes$ClinicalTrials.gov),
  drks_clean = case_when(
    # if the "secondary ID" just repeats the trial number return NA
    otherPrimaryRegisterId |>
      str_extract(regexes$DRKS) == drksId ~ NA_character_,
    .default = otherPrimaryRegisterId |>
      str_extract(regexes$DRKS)
  ),
  has_alias = !is.na(drks_clean))

drks_other_secondary_ids <- drks_tib |>
  select(drksId, otherSecondaryIds) |> 
  unnest(otherSecondaryIds) |> 
  rename(secondaryId = value, secondaryRegister = description) |> 
  filter(!is.na(secondaryId),
         str_length(secondaryId) > 1) |> 
  mutate(euctr_clean2 = secondaryId |>
           str_extract(regexes$EudraCT),
         ctgov_clean2 = secondaryId |>
           str_extract(regexes$ClinicalTrials.gov),
         drks_clean2 = case_when(
           # if the "secondary ID" just repeats the trial number return NA
           secondaryId |>
             str_extract(regexes$DRKS) == drksId ~ NA_character_,
           .default = secondaryId |>
             str_extract(regexes$DRKS)
         ),
         has_alias2 = !is.na(drks_clean2),
         #drks2_exists = drks_clean2 %in% drks_tib$drksId, # all of the drks_clean 2 exist
         secondaryRegisterName = case_when(
           str_detect(secondaryId, regexes$ClinicalTrials.gov) ~ "ClinicalTrials.gov",
           str_detect(secondaryId, regexes$EudraCT) ~ "EUCTR",
           str_detect(secondaryId, regexes$DRKS) ~ "DRKS",
           .default = secondaryRegister
         ))


drks_secondary_ids <- drks_secondary_ids |> 
  left_join(drks_other_secondary_ids |> 
              filter(!is.na(ctgov_clean2) | !is.na(drks_clean2) | !is.na(euctr_clean2)), by = "drksId") |> 
  mutate(euctr_clean2 = case_when(
    euctr_clean2 == euctr_clean ~ NA_character_,
    str_extract(otherPrimaryRegisterId, regexes$EudraCT) != eudraCtNumber ~ str_extract(otherPrimaryRegisterId, regexes$EudraCT),
    .default = euctr_clean2),
    ctgov_clean2 = case_when(
      ctgov_clean2 == ctgov_clean ~ NA_character_,
      .default = ctgov_clean2),
    drks_alias_exists = drks_clean %in% drksId,
    ctgov_exists = ctgov_clean %in% ctgov_id_info$nct_id, # all of the ctgov_clean exist
    ctgov2_exists = ctgov_clean2 %in% ctgov_id_info$nct_id # all of the ctgov2_clean exist
  )

# explore multiple euctr trial numbers
drks_secondary_ids |> 
  filter(euctr_clean != euctr_clean2) |> 
  select(drksId, contains("euctr"))

drks_secondary_ids <- drks_secondary_ids |> 
  mutate(has_any_euctr = !is.na(euctr_clean)  | !is.na(euctr_clean2),
         has_any_ctgov = !is.na(ctgov_clean) | !is.na(ctgov_clean2),
         has_any_alias = !is.na(drks_clean) | !is.na(drks_clean2),
         has_multiple_euctr = !is.na(euctr_clean) & !is.na(euctr_clean2),
         has_multiple_ctgov = !is.na(ctgov_clean) & !is.na(ctgov_clean2)) 

# proportion of aliases
drks_secondary_ids |> 
  count(has_any_alias) |> 
  mutate(prop = n / sum(n))

# proportion of potential cross-registrations
drks_secondary_ids |> 
  count(has_any_ctgov, has_any_euctr) |> 
  mutate(prop = n / sum(n))

### now filter secondary id tibble and later join on the main table to generate DRKS data set

drks_secondary_ids <- drks_secondary_ids |> 
  filter(!is.na(ctgov_clean) | !is.na(euctr_clean) | !is.na(ctgov_clean))



