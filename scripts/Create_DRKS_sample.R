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

drks_tib <- fromJSON(here("data", "raw", "DRKS_search_20250513.json"))

# save the existent DRKS TRNs for easier existence and cross-reference checks later
drks_tib |> 
  select(drksId) |> 
  write_excel_csv(here("data", "raw", "drks_ids.csv"))

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

drks_2018_2021 <- drks_tib |> 
  select(drksId, recruitment) |> 
  unnest(recruitment) |>
  filter(between(as_date(actualCompletionDate), as_date("2018-01-01"), as_date("2021-12-31"))) |> 
  pull(drksId)

validation_umcs_drks <- umc_drks_sponsors |> 
  bind_rows(umc_drks_pcis) |>
  filter(drksId %in% drks_interventional_trns, # apply interventional and time filter here
         drksId %in% drks_2018_2021) |> 
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
