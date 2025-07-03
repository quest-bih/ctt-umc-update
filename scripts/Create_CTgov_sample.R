#----------------------------------------------------------------------------------------------------------------------
#
# The following script creates the sample of trials with contributing German university medical centers (UMC)
# from the clinicaltrials.gov registry. Due to size, the raw registry data 
# will be stored in Zenodo and downloaded into the local project via a separate script.
# 
# The script searches the AACT dataset for affiliations of the sponsor/PI/responsible party
# associated with the different UMCs (keywords are loaded from city_search_terms.csv). It also filters
# the relevant completion years and study status (Completed, Terminated, Suspended, Unknown status).
#
# The script saves a filtered version of the dataset, containing only relevant trials. 
# Please be
# aware that the filtered dataset still contains false positives (i.e. trials that were found with the
# keywords but that were not associated with the UMCs - e.g. when a communal hospital in Berlin was found
# by the keyword "Berlin"). 
# This sentence above does not hold true with the current regular expressions, we should rephrase
#All trial affiliations were checked during the manual publication search to
# remove false positives.
#
#----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(yaml)
library(janitor)
library(furrr)
library(progressr)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

source(here("scripts", "utils.R"))
# the complete AACT was downloaded with the timestamp given and saved in the data/raw folder

# AACT_folder <- "C:/Datenablage/AACT/AACT_dataset_240927"
AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_250513")
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", 
                        "facility_investigators",
                        "interventions", "calculated_values", "id_information")

AACT_datasets <- load_AACT_datasets(AACT_folder, AACT_dataset_names)
studies <- AACT_datasets$studies |> 
  select(nct_id, source_class, everything())
sponsors_cleaned <- AACT_datasets$sponsors |>
  filter(lead_or_collaborator == "collaborator") |>  # Keep only lead sponsors
  select(nct_id, agency_class, name)

#----------------------------------------------------------------------------------------------------------------------
# Load search terms for the affiliations/cities
#----------------------------------------------------------------------------------------------------------------------

#different seach terms for each university medical center are stored loaded from this csv
# city_search_terms <- readLines(here("data", "umc_search_terms", "city_search_terms.csv"), encoding = "UTF-8") |>
#   str_split(";")
# cities <- city_search_terms |> map_chr(1)
# city_search_terms <- city_search_terms  |>
#   map(\(x) paste0("\\b", x, "\\b", collapse = "|"))
# names(city_search_terms) <- cities

city_search_terms <- get_umc_terms(collapse = FALSE)
### if the facilities are to be used later, but we decided not to:
facilities <- AACT_datasets$facilities
facility_inv <- AACT_datasets$facility_investigators
#----------------------------------------------------------------------------------------------------------------------
#  search for studies affiliated with a german medical faculty and get NCTs of those studies
#----------------------------------------------------------------------------------------------------------------------

#we want to find the names of the different universities in the
#PI/sponsor/responsible_party/facilities columns
grep_fast <- function(pattern, x)
{
  return(which(str_detect(x, pattern)))
}

get_nct <- function(affil_indices, dataset)
{
  ncts <- dataset |> 
    slice(affil_indices) |> 
    select(nct_id)
  return(ncts[[1]])
}

city_grep <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  city_ncts <- map(indices, get_nct, dataset=dataset)
  return(city_ncts)
}

city_grep_indices <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  return(indices)
}
sp <- AACT_datasets$sponsors
#search the different affiliation datasets for the city search terms
grep_PI <- city_grep(AACT_datasets$overall_officials, "affiliation", city_search_terms)
grep_sponsor <- city_grep(AACT_datasets$sponsors |>
                            filter(lead_or_collaborator == "lead"), "name", city_search_terms)
grep_resp_party_org <- city_grep(AACT_datasets$responsible_parties, "organization", city_search_terms)
grep_resp_party_affil <- city_grep(AACT_datasets$responsible_parties, "affiliation", city_search_terms)

# long_format umc hits from the different fields, for easier join later

enframe_umc_list <- function(grep_ls) {
  grep_ls |> 
    tibble::enframe(name = "umc", value = "nct_id") |> 
    tidyr::unnest(nct_id) |> 
    dplyr::group_by(nct_id) |> 
    dplyr::summarise(umc = paste(umc, collapse = ";"))
}
pi_umcs <- enframe_umc_list(grep_PI)

sponsor_umcs <- enframe_umc_list(grep_sponsor)
resp_party_org_umcs <- enframe_umc_list(grep_resp_party_org)
resp_party_affil_umcs <- enframe_umc_list(grep_resp_party_affil)

# responsible parties either give organization or affiliation, never both!!!

AACT_datasets$responsible_parties |> 
  filter(!is.na(organization) & !is.na(affiliation)) |> 
  nrow()

# therefore coalesce to org_affil and proceed with single column
# prepare umc_validation templates, at this stage unfiltered for inclusion/exclusion
# criteria and with blank validation column
umc_resp_party <- AACT_datasets$responsible_parties |> 
  mutate(raw_affil = coalesce(organization, affiliation)) |> 
  filter(!is.na(raw_affil),
         raw_affil != "[Redacted]") |>
  inner_join(bind_rows(resp_party_org_umcs, resp_party_affil_umcs), by = "nct_id") |>
  select(id = "nct_id", umc, raw_affil) |> 
  mutate(field = "responsible_parties_org_affil",
         validation = NA)

umc_ctgov_sponsors <- AACT_datasets$sponsors |> 
  filter(!is.na(name),
         name != "[Redacted]",
         lead_or_collaborator == "lead") |>
  inner_join(sponsor_umcs, by = "nct_id") |>
  select(id = "nct_id", umc, raw_affil = name) |> 
  mutate(field = "sponsor_name",
         validation = NA)

umc_ctgov_pi_host <- AACT_datasets$overall_officials |> 
  filter(!is.na(affiliation),
         affiliation != "[Redacted]") |>
  inner_join(pi_umcs, by = "nct_id") |>
  select(id = "nct_id", umc, raw_affil = affiliation) |> 
  mutate(field = "overall_officials_affiliation",
         validation = NA)

# extract TRNs that correspond to the inclusion criteria only
inclusion_trns <- AACT_datasets$studies |>
  filter(study_type == "INTERVENTIONAL",
         between(as_date(completion_date), as_date("2018-01-01"), as_date("2021-12-31"))) |> 
  pull(nct_id)

plan(multisession) # parallelize processing here, but still takes a long time!
handlers(global = TRUE)

validation_umcs_ctgov <- bind_rows(list(umc_ctgov_sponsors,
                                        umc_resp_party,
                                        umc_ctgov_pi_host)) |> 
  filter(id %in% inclusion_trns) |>  # apply inclusion filter here
  mutate(umc = which_umcs(raw_affil),
         search_needed = NA,
         correction = NA,
         comments = "")

validation_umcs_ctgov_deduplicated <- validation_umcs_ctgov |>
  filter(!is.na(umc), umc != "") |> 
  group_by(raw_affil, umc) |>
  summarise(across(everything(), first),
            n = n()) |>
  ungroup() |>
  arrange(umc, desc(n)) |>
  relocate(id, .before = everything()) |> 
  relocate(n, .before = validation)

validation_umcs_ctgov_deduplicated |>
  write_excel_csv(here("data", "processed", "validation_umcs_ctgov.csv"))

# If we want to merge in later processing for e.g. harmonization with
# other registries, combine fields into the different filter categories
# we want here:
# 
# umcs_ctgov <- validation_umcs_ctgov |>
#   filter(!is.na(umc), umc != "") |> 
#   pivot_wider(names_from = field, values_from = c(umc, raw_affil),
#               values_fn = deduplicate_collapsed,
#               values_fill = NA_character_)

### some exploration of the umc output here for quality assurance (qa)
qa_pi <- AACT_datasets$overall_officials |> 
  inner_join(pi_umcs, by = "nct_id") |> 
  select(umc, affiliation, nct_id, everything())
qa_sponsor <- AACT_datasets$sponsors |>
  filter(lead_or_collaborator == "lead") |> 
  inner_join(sponsor_umcs, by = "nct_id") |> 
  select(umc, name, nct_id, everything())
gmbh_sponsor <- qa_sponsor |> 
  filter(str_detect(name, "GmbH"))
qa_resp_org <- AACT_datasets$responsible_parties |> 
  inner_join(resp_party_org_umcs, by = "nct_id") |> 
  select(umc, organization, nct_id, everything())
gmbh_resp_org <- qa_resp_org |> 
  filter(str_detect(organization, "GmbH"))

# #joining of the different grep results
# affil_join <- function(affil_nct_list)
# {
#   affil_indices_joined <- affil_nct_list  |> 
#     pmap(c) |> 
#     map(unique) |> 
#     map(sort)
# }

#----------------------------------------------------------------------------------------------------------------------
# reduce the CTgov dataset to those studies that are indeed affiliated
# and filter for lead completion years & study status
#----------------------------------------------------------------------------------------------------------------------
# add validated umc info

umc_validations_ctgov <- read_csv(here("data", "processed", "umc_validations.csv")) |> 
  filter(str_detect(id, "NCT")) |>
  mutate(raw_affil = str_squish(raw_affil)) |> 
  select(raw_affil, umc)

validated_umc_ctgov <- bind_rows(list(umc_ctgov_sponsors,
                                        umc_resp_party,
                                        umc_ctgov_pi_host)) |> 
  rename(umc_estimated = umc) |>
  mutate(raw_affil = str_squish(raw_affil)) |> 
  left_join(umc_validations_ctgov, by = "raw_affil") |> 
  mutate(type = case_when(
    field == "overall_officials_affiliation" ~ "umc_pi",
    field == "sponsor_name" ~ "umc_sponsor",
    field == "responsible_parties_org_affil" ~ "umc_resp_party",
    .default = NA
  )) |> 
  select(id, umc, type)

validated_umc_ctgov_deduplicated <- validated_umc_ctgov |>  
  filter(!is.na(umc), umc != "false positive") |> 
  group_by(id, type) |> 
  summarise(umc = deduplicate_collapsed(umc)) |> 
  pivot_wider(id_cols = id, names_from = type, values_from = umc) |>
  mutate(umc = deduplicate_collapsed(c(umc_pi, umc_sponsor, umc_resp_party)))

qa_validated_umc_ctgov <- validated_umc_ctgov |>  
  filter(!is.na(umc)) |> 
  group_by(id) |> 
  summarise(validated_affils = case_when(
    all(umc == "false positive", na.rm = TRUE) ~ "falsely included",
    # "falsesly assigned" not as useful, as sometimes one affil is false but another is correct
    any(umc == "false positive", na.rm = TRUE) ~ "falsely assigned", 
    .default = deduplicate_collapsed(umc)
  )) |> 
  ungroup()

qa_validated_umc_ctgov |> 
  filter(id %in% inclusion_trns) |> 
  count(validated_affils, sort = TRUE) |>
  mutate(total = sum(n),
         prop = n / total) |> 
  filter(str_detect(validated_affils, "false"))

# total for both registries:
(374 + 307) / (1294 + 1161)
#----------------------------------------------------------------------------------------------------------------------
# new: apply inclusion filter and add affiliation columns directly
# deprecated: create for each study a list of affiliated cities and add to main table
#----------------------------------------------------------------------------------------------------------------------

#filter cases for affiliation, years, study status, and study type, etc.
CTgov_sample <- AACT_datasets$studies |> 
  filter(nct_id %in% inclusion_trns,
         nct_id %in% validated_umc_ctgov_deduplicated$id) |> # apply inclusion filter here, incl. umc
  left_join(validated_umc_ctgov_deduplicated, by = c("nct_id" = "id")) |> 
  select(nct_id, contains("umc"), everything())

CTgov_sample |> 
  count(is.na(umc_sponsor), is.na(umc_pi), is.na(umc_resp_party))


CTgov_sample_save <- CTgov_sample
write_excel_csv(CTgov_sample_save, here("data", "processed", "CTgov_sample.csv"), na = "")

qa_CTgov <- CTgov_sample |> 
  select(nct_id, umc, everything()) 

count(qa_CTgov, umc, sort = TRUE)

qa_excluded <- AACT_datasets$studies |>
  filter(nct_id %in% inclusion_trns,
         !nct_id %in% validated_umc_ctgov_deduplicated$id) |>
  left_join(validated_umc_ctgov, by = c("nct_id" = "id")) |>
  left_join(validation_umcs_ctgov |> 
              select(id, umc_estimated = umc, raw_affil, field), by = c("nct_id" = "id")) |> 
  select(nct_id, contains("umc"), raw_affil, field, everything())

umc_search_terms <- get_umc_terms()

qa_na_excluded <- qa_excluded |>
  filter(is.na(umc),
         str_detect(raw_affil, umc_search_terms))



######### In previous code now commented out (see below) only the first PI was taken
# from each study, regardless if others may have been UMC-related or not?

# get_city_per_NCT <- function(cities_nct_list, unique_ncts)
# {
#   cities_col <- vector("list", length(unique_ncts))
#   names(cities_col) <- unique_ncts
#   for (city in names(cities_nct_list)) {
#     cities_col[cities_nct_list[[city]]] <-
#       paste(cities_col[cities_nct_list[[city]]], city, sep = " ")
#   }
#   cities_col <- substring(cities_col, first = 6)
#   names(cities_col) <- unique_ncts
#   return(cities_col)
# }
# 
# #create columns that list which cities are affiliated with the studies
# nct_cities_lead <- get_city_per_NCT(affil_ncts_lead, unique_ncts_lead)
# 
# #prepare for joining with main table
# nct_cities_lead_tbl <- tibble(nct_id = unique_ncts_lead, cities_lead = nct_cities_lead)
# 
# #add columns to main table
# CTgov_sample <- CTgov_sample |>
#   left_join(nct_cities_lead_tbl, by = "nct_id")


#add PI affil info to main table
#first get only affils of relevant PIs from full table
# grep_PI_indices <- city_grep_indices(AACT_datasets$overall_officials, "affiliation", city_search_terms) |>
#   unlist() |> unique() |> sort()
# PI_affils_table_filtered <- AACT_datasets$overall_officials[grep_PI_indices,] |>
#   distinct(nct_id, .keep_all = TRUE) #only take first relevant PI for each study to allow a clean join
# CTgov_sample <- CTgov_sample |>
#   left_join(PI_affils_table_filtered, by = "nct_id")

#add intervention name
interventions_combined <- AACT_datasets$interventions |>
  group_by(nct_id) |>
  summarise(intervention_names_comb = paste(name, collapse=" | "))

CTgov_sample <- CTgov_sample |>
  left_join(interventions_combined, by = "nct_id")

#add calculated values
CTgov_sample <- CTgov_sample |>
  left_join(AACT_datasets$calculated_values, by = "nct_id")

# 
# CTgov_sample_save <- CTgov_sample |>
#   # TODO: This code below is now broken.
#   # decide which affil columns to keep/standardize for comparison with
#   # data from other registries, in place of old PI_name and PI_affiliation 
#   # variables here 
#   rename(PI_name = name,
#          PI_affiliation = affiliation,
#          interventions = intervention_names_comb) |> 
#   select(nct_id, cities_lead, brief_title, official_title,
#          study_first_submitted_date, start_date, start_date_type,
#          completion_date, completion_date_type, PI_name,
#          PI_affiliation, interventions, overall_status,
#          phase, enrollment, enrollment_type, were_results_reported)



#save CT.gov trial sample
#please be aware that not all associations of the trials to the cites are correct (there are still false positives)
#such that the city associations had to be checked manually during publication search
write_excel_csv(CTgov_sample_save, here("data", "processed", "CTgov_sample.csv"), na = "")

