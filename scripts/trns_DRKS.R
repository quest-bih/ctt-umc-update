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

euctr_ids  <- readRDS(here("data", "raw", "euctr_combined.rds")) |> 
  distinct(eudract_number)

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
    ctgov2_exists = ctgov_clean2 %in% ctgov_id_info$nct_id, # all of the ctgov_clean2 exist
    euctr_exists = euctr_clean %in% euctr_ids$eudract_number, # > 100 of the euctr_clean do not exist!!!
    euctr2_exists = euctr_clean2 %in% euctr_ids$eudract_number # all of the euctr_clean2 exist
  )

# explore multiple euctr trial numbers
drks_secondary_ids |> 
  filter(euctr_clean != euctr_clean2) |> 
  select(drksId, contains("euctr"))

drks_secondary_ids <- drks_secondary_ids |> 
  mutate(has_any_euctr = (!is.na(euctr_clean) & euctr_exists)  |
           (!is.na(euctr_clean2) & euctr2_exists) ,
         has_any_ctgov = (!is.na(ctgov_clean) & ctgov_exists) |
           (!is.na(ctgov_clean2) & ctgov2_exists),
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
drks_unknown_euctr <- drks_secondary_ids |> 
  filter(!is.na(euctr_clean) & !euctr_exists) |> 
  select(drksId, eudraCtNumber)

drks_unknown_euctr |> 
  write_excel_csv(here("data", "processed", "drks_unknown_euctr.csv"))

drks_secondary_ids <- drks_secondary_ids |> 
  filter(has_any_ctgov | has_any_euctr | has_any_alias)


crossreg_drks <- drks_secondary_ids |> 
  unite("ctgov_all", c(ctgov_clean, ctgov_clean2), na.rm = TRUE) |> 
  unite("euctr_all", c(euctr_clean, euctr_clean2), na.rm = TRUE) |> 
  mutate(drks_all = coalesce(drks_clean, drks_clean2),
    triad = has_any_euctr & has_any_ctgov,
         ctgov_all = map_chr(ctgov_all, \(x) update_ctgov_alias(x, ctgov_aliases)),
         many_to_many = has_any_alias) |> 
  pivot_longer(cols = contains("_all"), values_to = "linked_id") |> 
  filter(linked_id != "") |> 
  select(trial_id = drksId, linked_id, triad, many_to_many) |> 
  mutate(linked_id = na_if(linked_id, ""), via_id = TRUE, bidirectional = NA)

crossreg_drks |> 
  write_excel_csv(here("data", "processed", "crossreg_drks.csv"))

crossreg_drks <- read_csv(here("data", "processed", "crossreg_drks.csv")) |>
  rowwise() |> 
  mutate(binary_id = case_when(
    str_detect(linked_id, "-") ~ paste(c(linked_id, trial_id), collapse = "_"),
    .default = paste(c(trial_id, linked_id), collapse = "_")
  ), .before = 1) |> 
  ungroup()

crossreg_euctr_drks <- read_csv(here("data", "processed", "crossreg_euctr.csv")) 

bidirectional_euctr_drks <- crossreg_euctr_drks |> 
  semi_join(crossreg_drks, by = "binary_id") |> 
  pull(binary_id)

new_crossregs_drks <- crossreg_drks |> 
  filter(!binary_id %in% bidirectional_euctr_drks) |> 
  mutate(bidirectional = ifelse(str_detect(linked_id, "^20"), FALSE, bidirectional))

crossreg_euctr_drks <- crossreg_euctr_drks |> 
  bind_rows(new_crossregs_drks) |> 
  mutate(bidirectional = case_when(
    binary_id %in% bidirectional_euctr_drks ~ TRUE,
    !str_detect(linked_id, "NCT") & is.na(bidirectional) ~ FALSE,
    .default = bidirectional
  )) 

qa_euctr_drks <- crossreg_euctr_drks |> 
  filter(is.na(bidirectional), !str_detect(linked_id, "NCT"))

crossreg_euctr_drks |> 
  write_excel_csv(here("data", "processed", "crossreg_euctr_drks.csv"))
