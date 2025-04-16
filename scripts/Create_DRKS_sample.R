#----------------------------------------------------------------------------------------------------------------------
#
# The following script creates the sample of trials with contributing German university medical centers (UMC)
# from the DRKS database. Due to size, the raw registry data is stored in Zenodo and downloaded into the local project via a separate script.
#
# The script searches the DRKS dataset for affiliations of the sponsor/PI/responsible party/recruitment locations
# associated with the different UMCs (keywords are loaded from city_search_terms.csv).
#
# The script saves a filtered version of the dataset, containing only relevant trials. Please be
# aware that the filtered dataset still contains false positives (i.e. trials that were found with the
# keywords but that were not associated with the UMCs - e.g. when a communal hospital in Berlin was found
# by the keyword "Berlin"). All trial affiliations were checked during the manual publication search to
# remove false positives.
#
#----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(furrr)
library(jsonlite)
library(yaml)

#----------------------------------------------------------------------------------------------------------------------
# Data loading and transformation
#----------------------------------------------------------------------------------------------------------------------

# Get registry data if not already downloaded/unzipped
# source(here::here("code", "0_get_registry_data.R"))

# DRKS_sample <- read_csv2(here::here("data", "raw", "2020-06-03_drks.csv"))
# DRKS_full <- read_json(here("data", "raw", "DRKS_search_20250303.json"))

drks_tib <- fromJSON(here("data", "raw", "DRKS_search_20250303.json"))

source(here("scripts", "utils.R"))
# regexes <- yaml::read_yaml(here("inst", "extdata", "keywords_patterns.yaml"))
regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

drks_material_support <- drks_tib |> 
  select(drksId, materialSupports) |> 
  unnest(materialSupports) |> 
  unnest(contact) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))

drks_sponsors <- drks_tib |> 
  select(drksId, trialContacts) |> 
  unnest(trialContacts) |> 
  unnest(c(idContactIdType, contact))

drks_trial_contacts <- drks_tib |> 
  select(drksId, trialContacts) |> 
  unnest(trialContacts) |> 
  unnest(c(contact, idContactIdType)) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))

drks_recruitment_umcs <- drks_tib |> 
  select(drksId, recruitment) |> 
  unnest(recruitment) |> 
  unnest(countries) |> 
  mutate(country_code = unlist(idCountry)) |> 
  unnest(institutes) |> 
  filter(type == "UNI_MEDICAL_CENTER") |> 
  mutate(across(where(is.character), \(x) na_if(x, ""))) |> 
  select(drksId, country_code, name, status, actualCompletionDate)

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
    # ctgov_exists = ctgov_clean %in% id_info$nct_id, # all of the ctgov_clean exist (one was from 2024-11-12 so not in earlier data set, but exists on CT.gov)
    # ctgov2_exists = ctgov_clean2 %in% id_info$nct_id # all of the ctgov2_clean exist
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

################ UMC fields to search according to protocol

umc_search_terms <- get_umc_terms()

plan(multisession)

### 1. Trial contacts

umc_drks_sponsors <- drks_trial_contacts |> 
  filter(type == "PRIMARY_SPONSOR",
    str_detect(city, umc_search_terms) |
      str_detect(affiliation, umc_search_terms)) |> 
  unite("raw_affil", c(affiliation, city), sep = ", ") |> 
  mutate(field = "primary_sponsor_affil_city")

umc_drks_pcis <- drks_trial_contacts |> 
  filter(type == "PRINCIPAL_COORDINATING_INVESTIGATOR" |
           otherType == "OTHER_PRINCIPAL_COORDINATING_INVESTIGATOR",
         str_detect(city, umc_search_terms) |
           str_detect(affiliation, umc_search_terms)) |> 
  unite("raw_affil", c(affiliation, city), sep = ", ")  |> 
  mutate(field = "pci_affil_city")

drks_study_characteristic <- drks_tib |> 
  select(drksId, studyCharacteristic) |> 
  unnest(studyCharacteristic)

drks_interventional_trns <- drks_study_characteristic |> 
  filter(type == "INTERVENTIONAL") |> 
  pull(drksId)

drks_2018_2020 <- drks_tib |> 
  select(drksId, recruitment) |> 
  unnest(recruitment) |>
  filter(between(as_date(actualCompletionDate), as_date("2018-01-01"), as_date("2020-12-31"))) |> 
  pull(drksId)

validation_umcs_drks <- umc_drks_sponsors |> 
  bind_rows(umc_drks_pcis) |>
  filter(drksId %in% drks_interventional_trns, # apply interventional and time filter here
         drksId %in% drks_2018_2020) |>  
  rowwise() |> 
  mutate(umc = which_umcs(raw_affil),
         validation = NA) |> 
  ungroup() |> 
  select(id = drksId, umc, raw_affil, field, validation)

validation_umcs_drks_deduplicated <- validation_umcs_drks |> 
  group_by(raw_affil) |> 
  summarise(across(everything(), first),
            n = n()) |>
  ungroup() |> 
  arrange(umc, desc(n)) |> 
  relocate(id, .before = everything())

validation_umcs_drks_deduplicated |>
  write_excel_csv(here("data", "processed", "validation_umcs_drks.csv"))

qa_umc_terms <- drks_sponsors |> 
  filter(str_detect(affiliation, umc_search_terms),
         type %in% c("PRIMARY_SPONSOR", "PRINCIPAL_COORDINATING_INVESTIGATOR",
                     "SECONDARY_SPONSOR"),
         str_detect(affiliation, " GmbH"),
         str_detect(affiliation, " Uni")) |> # space to exclude gGmbH
  distinct(affiliation, .keep_all = TRUE)


drks_recruitment_vs_sponsor_affil <- drks_recruitment_umcs |> 
  filter(str_detect(name, umc_search_terms)) |> 
  left_join(drks_trial_contacts |> 
              filter(type %in% c("PRIMARY_SPONSOR", "PRINCIPAL_COORDINATING_INVESTIGATOR") |
                       otherType == "OTHER_SECONDARY_SPONSOR",
                     str_detect(affiliation, umc_search_terms) |
                       str_detect(city, umc_search_terms))) |> 
  filter(country_code == "DE", is.na(affiliation))

drks_recruitment_vs_sponsor_affil |> 
  distinct(drksId) |> 
  nrow()


qa_city <- drks_trial_contacts |> 
  filter(type %in% c("PRIMARY_SPONSOR", "PRINCIPAL_COORDINATING_INVESTIGATOR") |
           otherType == "OTHER_SECONDARY_SPONSOR",
         !str_detect(affiliation, umc_search_terms),
         str_detect(city, umc_search_terms))
