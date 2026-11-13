library(tidyverse)
library(here)
library(furrr)
library(progressr)
library(janitor)
library(openalexR)
library(readxl)
### rerun everything to pubsearch to test refactoring!!!!
plan(multisession)
handlers(global = TRUE)

source(here("scripts", "utils.R"))
### Missing many trials, expecting 2135
combined_data_filtered <- read_csv(here("data", "processed", "harmonized_data_filtered_after_pub_search.csv"))

crossreg_ids <- combined_data_filtered |> 
  select(trial_id, crossreg_id)
# resolution |> select(contains("crossreg")) |> names()

missed_pubs <- read_csv(here("data", "manual", "2026-10-07_iv3-second-review-enriched.csv")) |>
  filter(review == TRUE) |> 
  select(trial_id, crossreg_id, earliest_pub_matches_reg, earliest_pub_has_doi, earliest_pub_doi)

resolution <- read_csv(here("data", "processed", "results_corrected_2026-10-27.csv")) |>
  filter(trial_id %in% combined_data_filtered$trial_id) |>
  rows_upsert(missed_pubs, by = c("trial_id", "crossreg_id")) |> 
  mutate(results_search_end_date = as.Date(timestamp)) |> 
  select(-crossreg_id) |> 
  left_join(crossreg_ids, by = "trial_id") |> 
  group_by(crossreg_id) |> 
  mutate(cluster_results_search_end_date = max(results_search_end_date, na.rm = TRUE)) |> 
  ungroup() |> 
  select(trial_id, crossreg_id, everything())

drks_processed <- read_csv(here("data", "processed", "DRKS_sample.csv"))
ctgov_processed <- readRDS(here("data", "processed", "ctgov_processed.rds"))
euctr_processed <- readRDS(here("data", "processed", "euctr_processed.rds"))
# euctr_sponsors_raw <- read_csv(here("data", "raw", ""))
updated_sumres_2026 <- read_csv(here("data", "processed", "updated_sumres_2026.csv"))
drks_sumres <- read_csv(here("data", "manual", "2026-10-14_iv3-drks-sumres.csv")) |> 
  mutate(trial_id,
         drks_is_sumres = review_drks_is_sumres,
         drks_sumres_date = dmy(review_drks_sumres_date),
         .keep = "none")

# These would need to be updated with every registry update
# dates for sumres update:
aact_query_date <- "2026-06-06" 
# drks_query_date <- "2025-05-13"
euctr_query_date <- "2026-06-06"
drks_manual_sumres_date <- "2026-10-11"

query_dates <- tribble(
  ~registry, ~registry_query_date,
  "ClinicalTrials.gov", aact_query_date,
  "DRKS", drks_manual_sumres_date,
  "EUCTR", euctr_query_date
) |> 
  mutate(registry_query_date = ymd(registry_query_date))

pub_info_clean <- resolution |> 
  select(trial_id, crossreg_id, review_has_pub, 
         review_earliest_doi,
         review_comments,
         review_next_best_doi,
         review_next_best_pubtype,
         sgs_review,
         contains("earliest_pub"),
         results_search_end_date,
         cluster_results_search_end_date,
         is_pub_reglinked,
         earliest_pub_matches_reg)
# qa_earliest <- pub_info_clean |> select(trial_id, crossreg_id, review_has_pub,
#                                         review_earliest_doi,
#                                         review_comments,
#                                         sgs_review,
#                                         contains("earliest")) |> 
#   filter(review_earliest_doi != earliest_pub_doi |
#            (is.na(earliest_pub_doi) & !is.na(earliest_pub_url)))
# pub_dois <- pub_info_clean |> 
#   pull(earliest_pub_doi) |> 
#   na.omit() |> 
#   unique()

# doi_dates <- oa_fetch(doi = pub_dois, entity = "works",
#                       mailto = Sys.getenv("EMAIL"),
#                       verbose = TRUE) |>
#   saveRDS(here("data", "raw", "openalex_pubdates.rds"))

doi_dates <- readRDS(here("data", "raw", "openalex_pubdates.rds"))

doi_dates_clean <- doi_dates |> 
  mutate(doi = str_remove(doi, ".*(?=10\\.\\d+\\/)"))

### strangely, the frequency of (false) duplicates for doi in
### openAlex has increased, here examine and decide/implement deduplication
### strategy

doi_dates_dupes <- doi_dates_clean |>
  get_dupes(doi) |> 
  left_join(pub_info_clean |> 
              select(earliest_pub_doi, earliest_pub_type) |> 
              distinct(earliest_pub_doi, earliest_pub_type, .keep_all = TRUE),
            by = c("doi" = "earliest_pub_doi")) |> 
  group_by(doi) |> 
  mutate(earliest_year = min(publication_year),
         year_diff = max(publication_year) - min(publication_year)) |> 
  select(doi, earliest_pub_type, version, host_organization_name,
         year_diff, earliest_year, publication_date, publication_year,
         everything()) |> 
  ungroup()

doi_dates_no_dupes <- doi_dates_dupes |> 
  select(-dupe_count) |> 
  filter(version == "publishedVersion",
         host_organization_name != "National Institutes of Health",
         # when there are dupes, the latest date was always the correct one
         # and duplicates were several years earlier
         year_diff == 0 | earliest_year != publication_year) |> 
  select(doi, publication_date)

doi_dates_deduped <- doi_dates_clean |> 
  # take only non-duplicate dois first
  filter(!doi %in% c(doi_dates_no_dupes$doi)) |>
  select(doi, publication_date) |>
  # add deduplicated dois
  bind_rows(doi_dates_no_dupes) |> 
  mutate(earliest_pub_doi = str_remove(doi, ".*(?=10\\.\\d+\\/)")) |> 
  select(earliest_pub_doi, earliest_pub_date = publication_date)

# check that deduplication worked 
qa_dupes <- doi_dates_deduped |> 
  get_dupes(earliest_pub_doi)

missing_dates <- pub_info_clean |>
  filter(!is.na(earliest_pub_doi)) |>
  rows_upsert(doi_dates_deduped,
            by = "earliest_pub_doi") |>
  filter(is.na(earliest_pub_date),
         is.na(review_has_pub) | review_has_pub == TRUE)

nrow(missing_dates)
# 
# 
missing_dates |>
  pull(earliest_pub_doi)
# 
# oa_dates <- oa_fetch(doi = missing_dates$earliest_pub_doi,
#                      entity = "works",
#                      mailto = Sys.getenv("EMAIL"),
#                      verbose = TRUE)
 
pub_pmids <- pub_info_clean |>
  pull(earliest_pub_pmid) |>
  na.omit() |>
  unique()

# 
# pmid_query <- oa_query(pmid = pub_pmids, verbose = TRUE)
# pmid_dates <- oa_request(pmid_query, api_key = oa_apikey())
# pmid_dates <- works2df(pmid_dates)
# 
# pmid_dates_clean <- pmid_dates |>
#   select(ids, publication_date) |>
#   unnest(ids) |>
#   filter(str_detect(ids, "https://pubmed.ncbi.nlm.nih.gov/")) |>
#   mutate(earliest_pub_pmid = str_remove(ids, "https://pubmed.ncbi.nlm.nih.gov/") |>
#            as.numeric()) |>
#   select(-ids, earliest_pub_date = publication_date)

pmid_dates_clean <- tibble(
  earliest_pub_date = ymd("2022-11-15"), 
  earliest_pub_pmid = pub_pmids
)

### what is added and what is overwritten here?
# need the full table with all pub_info for all pubs!

pub_info_enriched <- pub_info_clean |>
  # filter(!is.na(earliest_pub_doi) | !is.na(earliest_pub_pmid) |
  #          !is.na(earliest_pub_date) | review_has_pub == FALSE) |> 
  rows_upsert(doi_dates_deduped |> 
                filter(earliest_pub_doi %in% pub_info_clean$earliest_pub_doi), by = "earliest_pub_doi") |> 
  rows_upsert(pmid_dates_clean, by = "earliest_pub_pmid")


# qa_earliest_pub_date <- 
# pub_info_enriched |> 
#   filter(is.na(earliest_pub_date))

dd <- 
pub_info_clean |> 
  get_dupes(earliest_pub_doi)

drks_pubsearch_data <- resolution |> 
  filter(str_detect(trial_id, "DRKS")) |> 
  rows_upsert(drks_sumres, by = "trial_id") |> 
  mutate(is_sumres_manual = str_detect(drks_is_sumres, "^Yes|Unsure")) |>
  select(trial_id, is_sumres_manual, summary_results_date = drks_sumres_date,
         drks_is_sumres, drks_comment_sumres)

#### check that not reviewed cases all do not have summary results in initial extraction
qa_drks_not_reviewed <- resolution |> 
  filter(registry == "DRKS",
         !trial_id %in% drks_sumres$trial_id)

qa_drks_not_reviewed |> 
  count(drks_is_sumres)


drks_enrichment_data <- drks_processed |> 
  semi_join(combined_data_filtered, by = "trial_id") |> 
  select(trial_id, start_date,
         start_date_type,
         contains("umc"),
         primary_sponsor) |> 
  left_join(drks_pubsearch_data, by = "trial_id")
setdiff(drks_pubsearch_data$trial_id, drks_enrichment_data$trial_id)
# registration DRKS00014040 was excluded

ctgov_enrichment_data <- ctgov_processed |> 
  rename(summary_results_date = results_first_submitted_date) |> 
  semi_join(combined_data_filtered, by = "trial_id") |>
  select(trial_id, start_date,
         start_date_type,
         is_sumres = results_reporting,
         summary_results_date,
         contains("umc"),
         primary_sponsor
         ) |> 
  # add sumres info from 2026 download
  rows_upsert(updated_sumres_2026 |>
                filter(str_detect(trial_id, "NCT")) |>
                semi_join(combined_data_filtered, by = "trial_id"),
              by = "trial_id") 

qa_umc_resp_party <- 
ctgov_enrichment_data |> 
  filter(umc_sponsor != umc_resp_party |
           is.na(umc_sponsor) & !is.na(umc_resp_party))

euctr_enrichment_data <- euctr_processed |>
  rename(trial_id = eudract_number) |> 
  semi_join(combined_data_filtered, by = "trial_id") |>
  group_by(trial_id) |> 
  mutate(is_trial_de_protocol = str_detect(eudract_number_with_country, "DE")) |>
  arrange(desc(is_trial_de_protocol)) |>
  summarise(across(everything(), first)) |> # this takes first also for is_trial_de_protocol == FALSE
  select(-eudract_number_with_country, -is_trial_de_protocol) |>
  ungroup() |>
  mutate(trial_id, 
         is_sumres = results_reporting,
         summary_results_date = results_first_version_date,
         umc,
         umc_sponsor = umc,
         primary_sponsor = future_map_chr(sponsors, extract_sponsor_classifications,
                                          .progress = TRUE),
         .keep = "none") |> 
  # add sumres info from 2026 scrape
  rows_upsert(updated_sumres_2026 |>
                filter(str_detect(trial_id, "-")) |>
                semi_join(combined_data_filtered, by = "trial_id"),
              by = "trial_id")

euctr_enrichment_data |> 
  count(is_sumres)


#######
# for manually added crossregistrations,
# recalculate the hierarchical completion date
#######
manual_crossreg_info <- read_xlsx(here("data", "manual", "iv3_crossreg.xlsx"))
crossreg_linked <- manual_crossreg_info |> 
  filter(action == "link")


new_crossregistrations <- combined_data_filtered |>
  filter(trial_id %in% c(crossreg_linked$new_crossreg,
                         crossreg_linked$trial_id)) |> 
  update_crossreg_manual(crossreg_linked) |> 
  mutate(actual_completion_date = case_when(
    completion_date_type == "ACTUAL" ~ completion_date,
    registry == "DRKS" & (is.na(estimated_completion_date) |
                            estimated_completion_date != completion_date)  ~ completion_date,
    .default = NA_Date_
  )) |> 
  get_hierarchical_completion_date() |> 
  select(-has_cluster_de_protocol, -registries, -recent_status)


#    Completion date: In case of month-year format, convert to last of the month.  
# does not happen for DRKS, CT.gov
# (EUCTR: a single example with a wrong date format, but not German UMC)
# Publication date: In case of month-year format, convert to first day of the month. 
# All dates on OpenAlex are full (no month-year format)
combined_data_enriched <- combined_data_filtered |> 
  mutate(start_date = NA_Date_,
         start_date_type = NA_character_,
         is_sumres = NA,
         is_sumres_manual = NA,
         summary_results_date = NA_Date_,
         umc = NA_character_,
         umc_sponsor = NA_character_,
         umc_pi = NA_character_,
         umc_resp_party = NA_character_,
         primary_sponsor = NA_character_) |> 
  left_join(pub_info_enriched, by = c("trial_id", "crossreg_id")) |> 
  rows_upsert(drks_enrichment_data |> 
                select(-drks_is_sumres,
                       -drks_comment_sumres), by = "trial_id") |> 
  rows_upsert(ctgov_enrichment_data, by = "trial_id") |>
  rows_upsert(euctr_enrichment_data, by = "trial_id") |> 
  rows_upsert(new_crossregistrations, by = c("trial_id", "crossreg_id")) |> 
  left_join(query_dates, by = "registry") |> 
  group_by(crossreg_id) |>
  mutate(summary_results_followup_date = if_else(
    # DRKS without second review
    registry == "DRKS" & !trial_id %in% drks_sumres$trial_id,
    results_search_end_date, # date of the original publication search
    registry_query_date # date of the second drks sumres review
  ), 
         cluster_summary_results_followup_date = min(summary_results_followup_date, na.rm = TRUE),
         cluster_summary_results_date = min(summary_results_date, na.rm = TRUE),
         earliest_pub_date = min(earliest_pub_date, na.rm = TRUE),
         nonindex_completion_earlier_than_sumres = any(
           completion_date <= cluster_summary_results_date &
             is_index_reg == FALSE, na.rm = TRUE
         )) |> 
  ungroup() |> 
  mutate(
    status_raw = status,
    status = recode_status(status_raw),
    is_sumres_combined = coalesce(is_sumres_manual, is_sumres),
    is_prospective = case_when(
    registry == "EUCTR" ~ TRUE,
    .default = floor_date(start_date, "month") >=
      floor_date(registration_date, "month")
    ),
    cluster_completion_date =
      if_else(!is.na(hierarchical_completion_date),
              hierarchical_completion_date,
              completion_date),
    days_cd_to_publication = duration_days(cluster_completion_date, earliest_pub_date),
    days_reg_to_publication = duration_days(registration_date, earliest_pub_date),
    # makes sense to calculate sumres separately for each registry but followup across registries
    days_cd_to_cluster_summary = duration_days(cluster_completion_date, cluster_summary_results_date),
    days_cd_to_registry_summary = duration_days(cluster_completion_date, summary_results_date),
    # Using registry download date for reporting as summary results
    results_followup_sumres = duration_days(cluster_completion_date,
                                            cluster_summary_results_followup_date),
      # case_when(
      #### if DRKS sumres
      # is_sumres_manual != "No" & registry == "DRKS" ~ duration_days(cluster_completion_date, drks_manual_sumres_date),
      ### if no DRKS sumres
      # is_sumres_manual == "No" ~ duration_days(cluster_completion_date,
      #               cluster_results_search_end_date),
      # .default = duration_days(cluster_completion_date, cluster_summary_results_followup_date)
    
    is_followup_2y_sumres = results_followup_sumres >= 365*2,
    is_followup_5y_sumres = results_followup_sumres >= 365*5,
    
    is_summary_results_1y = days_cd_to_registry_summary < 365*1,
    is_summary_results_2y = days_cd_to_registry_summary < 365*2,
    is_summary_results_5y = days_cd_to_registry_summary < 365*5,
    
    # Using end of search period for reporting as publication
    results_followup_pub = duration_days(cluster_completion_date,
                                         cluster_results_search_end_date),
    
    is_followup_2y_pub = results_followup_pub >= 365*2,
    is_followup_5y_pub = results_followup_pub >= 365*5,
    
    is_publication_2y = days_cd_to_publication < 365*2,
    is_publication_5y = days_cd_to_publication < 365*5,
    across(contains("is_summary_results_"),
           \(x) if_else(is.na(x), FALSE, x)) # consider this for survival analysis
    ) |> 
  group_by(crossreg_id) |> 
  mutate(umc_pi = deduplicate_collapsed(umc_pi),
         umc_sponsor = deduplicate_collapsed(umc_sponsor),
         umc = deduplicate_collapsed(umc),
         has_sumres = any(is_sumres_combined, na.rm = TRUE),
         has_sumres_all = all(is_sumres_combined),
         has_summary_results_1y = any(is_summary_results_1y,
                                      na.rm = TRUE),
         has_summary_results_2y = any(is_summary_results_2y,
                                      na.rm = TRUE),
         has_summary_results_5y = any(is_summary_results_5y,
                                      na.rm = TRUE),
         has_followup_sumres_5y = any(is_followup_5y_sumres,
                                      na.rm = TRUE),
         has_sumres_all_1y = all(is_summary_results_1y, na.rm = TRUE),
         has_sumres_all_2y = all(is_summary_results_2y, na.rm = TRUE),
         has_sumres_all_5y = all(is_summary_results_5y, na.rm = TRUE),
         has_followup_sumres_all_5y = all(is_followup_5y_sumres, na.rm = TRUE),
         pre_completion_summary_flag = has_sumres == TRUE & days_cd_to_cluster_summary < 0,
         negative_censor_time_sumres_flag = has_sumres == FALSE & results_followup_sumres < 0,
         # sumres_pre_completion_class = case_when(
         #   pre_completion_summary_flag == TRUE & ~ "before index completion date but on/after another registry completion date",
         #   pre_completion_summary_flag == TRUE ~ "before index completion date but on/after another registry completion date",
         #   
         #   .default = NA_character_
         # ),
         has_pub = any(!is.na(earliest_pub_date) & !is.infinite(earliest_pub_date)),
         has_publication_2y = any(is_publication_2y,
                                      na.rm = TRUE),
         has_publication_5y = any(is_publication_5y,
                                      na.rm = TRUE),
         has_followup_pub_5y = any(is_followup_5y_pub,
                                      na.rm = TRUE),
         pre_completion_publication_flag = has_pub == TRUE & days_cd_to_publication < 0,
         negative_censor_time_publication_flag = has_pub == FALSE & results_followup_pub < 0,
         has_prospective = any(is_prospective, na.rm = TRUE),
         has_pub_reglinked = any(str_detect(is_pub_reglinked, "Yes"),
                                 na.rm = TRUE),
         has_earliest_match = 
           any(str_detect(earliest_pub_matches_reg, "Yes"), na.rm = TRUE),
         has_commercial = any(str_detect(primary_sponsor, "^Comm"), na.rm = TRUE)
         ) |> 
  ungroup()

qa_km <- combined_data_enriched |> 
  select(crossreg_id, trial_id, earliest_pub_date, has_pub,
         is_sumres_manual, is_sumres_combined,
         registry_query_date,
         results_followup_sumres,
         review_has_pub, contains("pre_completion"), contains("negative_censor_time"),
         has_sumres, contains("days_cd"),
         contains("cluster_"), completion_date, summary_results_date)

euctr_status_recoded <- 
  read_csv(here("data", "processed", "euctr_status_recoded.csv")) |> 
  rename(status = status_cleaned)

combined_data_enriched <- combined_data_enriched |> 
  rows_upsert(euctr_status_recoded, by = "trial_id") |> 
  group_by(crossreg_id) |> 
  mutate(cluster_status = case_when(
    any(status == "Terminated") ~ "Terminated",
    any(status == "Completed") & any(status != "Suspended") ~ "Completed",
    any(status == "Ongoing") & any(status == "Unknown") ~ "Ongoing",
    .default = first(status)
  )) |> 
  ungroup()

analysis_results <- combined_data_enriched |>
  arrange(crossreg_id) |> 
  group_by(crossreg_id) |> 
  mutate(index_status = status [is_index_reg == TRUE],
         cluster_status_old = deduplicate_collapsed(status),
         has_discrepant_status = str_detect(cluster_status_old, ";"),
         has_completion_date_estimated = case_when(
           is_crossreg ~ only_estimated,
           str_detect(completion_date_type, "ESTIMATED") ~ TRUE,
           str_detect(trial_id, "-") & is.na(results_completion_date) &
             !is.na(estimated_completion_date) ~ TRUE,
           str_detect(trial_id, "DRKS") & is.na(completion_date) &
             !is.na(estimated_completion_date) ~ TRUE,
           .default = FALSE
         ),
         is_completion_date_equals_estimated = case_when(
           is_crossreg == TRUE ~ coalesce(estimated_completion_date == hierarchical_completion_date, FALSE),
           is_crossreg == FALSE ~ coalesce(estimated_completion_date == completion_date, FALSE),
         )
  ) |> 
  ungroup() |> 
  rename(cluster_only_estimated = only_estimated)

analysis_results |> 
  write_csv(here("data", "processed", "analysis_results.csv"))

## check that the status was harmonized!
combined_data_enriched |>
  count(is_sumres_manual,
        is_sumres,
        registry)

#### missing summary_results_date (could not be determined)
combined_data_enriched |>
  filter(is_sumres_manual == TRUE,
         is.na(summary_results_date)) |>
  pull(trial_id)

combined_data_enriched |> count(registry)
combined_data_enriched |> count(primary_sponsor)

### calculate cross-reg level parameters has_


### ensure that there are no missing is_sumres_combined
qa_na_is_sumres <- combined_data_enriched |>
  filter(is.na(is_sumres_combined))

### trial level results

combined_data_trial_level <- combined_data_enriched |> 
  filter(is_index_reg == TRUE) |> 
  mutate(across(contains("days_cd_to_"),
                \(x) if_else(is.infinite(x),
                             NA, x)))

combined_data_trial_level  |> 
  count(registry, is_prospective)

combined_data_trial_level  |> 
  count(registry, has_sumres) ### check if this makes sense

combined_data_trial_level  |> 
  count(registry, is_crossreg, has_sumres)

combined_data_enriched |> 
  count(registry, has_sumres, has_summary_results_1y)


#######
# Time to reported results: We will calculate median time (and interquartile range) from trial completion to the reporting of summary results or results publication, whichever occurred first, taking into account differential follow-up. This will also be illustrated with Kaplan-Meier curves.
# We will also repeat this illustration separating publications and summary results. For unpublished trials, we will censor the timeline at the latest date that searches were performed for that trial. 

visualize_iv_km <- function(trials) {
  tidycmprsk::cuminc(survival::Surv(days_cd_to_result_censored, as.factor(has_results)) ~ 1, trials) |>
    ggsurvfit::ggcuminc() +
    ggsurvfit::add_censor_mark() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_quantile(linetype = 5) +
    ggplot2::annotate("label", x = 10, y = 0.5, label = "50%", fill = "white") +
    ggsurvfit::add_quantile(0.2, linetype = 5) +
    ggsurvfit::add_quantile(x = 365, linetype = 3) +
    ggplot2::annotate("label", x = 10, y = 0.2, label = "20%", fill = "white") +
    ggplot2::annotate("label", x = 365, y = 0, label = "1 year", fill = "white") +
    
    ggsurvfit::add_quantile(x = 2*365, linetype = 3) +
    ggplot2::annotate("label", x = 2*365, y = 0, label = "2 years", fill = "white") +
    
    ggsurvfit::add_quantile(x = 5*365, linetype = 3) +
    ggplot2::annotate("label", x = 5*365, y = 0, label = "5 years", fill = "white") +
    
    ggsurvfit::add_risktable(risktable_stats = c("cum.event", "n.risk", "cum.censor")) +
    ggsurvfit::scale_ggsurvfit(
      x_scales=list(
        breaks = scales::breaks_width(200),
        expand = c(0.023, 0) # Create space for x axis text
      ),
      y_scales = list(limits = c(0, 1))
    ) +
    labs(x = "Time in days")
}

# combined_data_trial_level |> select(contains("results_search_end_date"))
km_pub <- combined_data_trial_level |> 
  mutate(days_cd_to_result_censored = case_when(
    has_pub & earliest_pub_date <= cluster_results_search_end_date ~ days_cd_to_publication,
    .default = results_followup_pub
  ),
  days_cd_to_result_censored = if_else(days_cd_to_result_censored < 0,
                                       0, days_cd_to_result_censored),
         has_results = has_pub) |> 
  select(trial_id, crossreg_id, 
         has_pub,
         contains("days_cd"), contains("followup"),
         contains("has_sum"), contains("has_pub"),
         earliest_pub_date,
         cluster_completion_date, summary_results_date, cluster_results_search_end_date,
         everything())

km_pub |> count(is.na(days_cd_to_result_censored),
                has_pub)
# km_pub |> 
#   filter(is.na(days_cd_to_result_censored),
#          has_pub == FALSE) |> 
#   pull(trial_id)

qa_km |> 
  count(has_pub, review_has_pub)

km_pub |> 
  # filter(status %in% c("Completed", "Ongoing")) |> 
  visualize_iv_km()

# prop no results
km_pub |> 
  count(has_results) |> 
  mutate(prop = n / sum(n))


km_sumres <- combined_data_trial_level |> 
  mutate(
    days_cd_to_result_censored = if_else(
    has_sumres, days_cd_to_cluster_summary,
    results_followup_sumres
  ),
  days_cd_to_result_censored = if_else(days_cd_to_result_censored < 0,
                                       0, days_cd_to_result_censored),
         has_results = has_sumres
  ) |> 
  select(trial_id, crossreg_id,
         contains("followup"),
         is_sumres_manual,
         contains("has_sum"), contains("has_pub"),
         contains("days_cd"),
         cluster_completion_date, summary_results_date, cluster_results_search_end_date,
         everything())

km_sumres |> 
  visualize_iv_km()

qa_km_pub <- km_pub |> 
  filter(days_cd_to_publication < 0) |> 
  count(earliest_pub_type, sort = TRUE)

km_sumres |> 
  count(is.na(days_cd_to_cluster_summary), has_sumres)

qa_km_sumres <- km_sumres |> 
  filter(is.na(days_cd_to_cluster_summary), has_sumres)

qa_followup <- combined_data_enriched |> 
  filter(is.infinite(days_cd_to_cluster_summary)) |> 
  select(trial_id, crossreg_id, contains("followup"),
         contains("has_sum"), contains("has_pub"),
         contains("days_cd"),
         cluster_completion_date, 
         cluster_summary_results_followup_date,
         summary_results_date, cluster_results_search_end_date)
qa_followup |> 
  filter(str_detect(crossreg_id, "DRKS")) |> 
  group_by(has_sumres) |> 
  slice_sample(n = 5)

## any result
### some trials with results are missing sumres date because
### it could not be determined 

km_any <- combined_data_trial_level |> 
  rowwise() |> 
  mutate(days_cd_to_result_censored = 
           case_when(
             has_sumres | has_pub ~ min(days_cd_to_cluster_summary,
                                       days_cd_to_publication,
                                       na.rm = TRUE),
             !is.na(results_followup_sumres) |
               !is.na(results_followup_pub) ~
               min(results_followup_sumres, results_followup_pub,
                                                  na.rm = TRUE),
             .default = NA
             
           ),
         days_cd_to_result_censored = case_when(
           is.infinite(days_cd_to_result_censored) ~ NA,
           days_cd_to_result_censored < 0 ~ 0,
           .default = days_cd_to_result_censored),
         pre_completion_any_flag = (has_sumres == TRUE | has_pub == TRUE) &
           days_cd_to_result_censored < 0,
         # the following was always FALSE, so drop in future
         negative_censor_time_any_flag = has_sumres == FALSE & has_pub == FALSE &
           days_cd_to_result_censored < 0,
         # days_cd_to_result_censored = case_when(
         #   days_cd_to_result_censored < 0 ~ 0,
         #   is.infinite(days_cd_to_result_censored) ~ NA,
         #   .default = days_cd_to_result_censored),
  has_results = has_sumres | has_pub) |> 
  ungroup() |> 
  select(trial_id, crossreg_id, 
         pre_completion_any_flag,
         negative_censor_time_any_flag,
         days_cd_to_result_censored, contains("days_cd"),
         has_results, has_sumres, has_pub, contains("followup"),
         everything())
km_any |> 
  visualize_iv_km()

qa_any <- km_any |> 
  filter(is.na(days_cd_to_result_censored))

count(combined_data_enriched, has_sumres)
count(combined_data_enriched, has_pub)


# prop no results
km_any |> 
  count(has_results) |> 
  mutate(prop = n / sum(n))

### prospective registration
combined_data_trial_level |> 
  count(has_prospective) |> 
  summarise(total = sum(n),
            perc = round(n [has_prospective == TRUE] / total, 2) * 100
            )

combined_data_trial_level |> 
  separate_longer_delim(umc, ";") |> 
  count(umc, has_prospective) |> 
  group_by(umc) |> 
  summarise(total = sum(n),
            perc = round(n [has_prospective == TRUE] / total, 2) * 100
  )

combined_data_trial_level |> 
  separate_longer_delim(umc_sponsor, ";") |> 
  filter(!is.na(umc_sponsor)) |> 
  count(umc_sponsor, has_prospective) |> 
  group_by(umc_sponsor) |> 
  summarise(total = sum(n),
            perc = round(n [has_prospective == TRUE] / total, 2) * 100
  )

### Comparison of 2-year reporting rate with published data from the previous IntoValue studies (trials associated with German UMC completed between 2009 and 2017, (1,2)). We will compare 2-year reporting rates (summary results only, publication only, either route) using chi-squared tests. This analysis will be limited to trials affiliated with a German UMC as primary sponsor, responsible party, and/or host institution of the principal investigator (see “Role B” below), which was also the focus of the previous IntoValue studies. 


# Linking of eligible results publications in the registry: 
#   
#   Linking of any results publication in ClinicalTrials.gov and DRKS: Among trials/trial clusters with at least one eligible results publication identified during the manual search – and for which publication information did not require correction during data review – we will report the number and proportion where any of these publications (not necessarily the earliest identified) was linked in at least one ClinicalTrials.gov or DRKS record. EUCTR-only registrations will be excluded for this outcome because EUCTR does not provide a dedicated section for linked results publications. 
# 

# Denominator = DRKS + CT.gov where any results publication found
# Numerator = one of the publications was linked on DRKS or CT.gov
#### eligible result publications
pubs_ctgov_or_drks_only <- combined_data_trial_level |> 
  filter(is_crossreg | !str_detect(trial_id, "-"),
         !is.na(earliest_pub_url) | earliest_pub_has_doi == TRUE |
           earliest_pub_has_pmid == TRUE)

pubs_ctgov_or_drks_only |> 
  count(has_pub_reglinked) |> 
  mutate(total = sum(n),
         perc = n / total * 100)

# corrections_reglinked_url <- tibble(
#   trial_id = c("2013-001294-24",
#                "NCT00278408",
#                "DRKS00009323",
#                "DRKS00011876",
#                "NCT03264222",
#                "NCT03290157",
#                "NCT03720665",
#                "NCT03128528",
#                "DRKS00012329",
#                "DRKS00025281",
#                "NCT03838211"),
#   pub_reglinked_url_clean = c("10.1136/bmj.m3734",
#                               "10.1097/hs9.0000000000000904",
#                               "10.2196/15361",
#                               "10.3233/nre-201583",
#                               "10.1016/j.jacc.2020.01.012",
#                               "10.1016/j.cgh.2020.03.051",
#                               "10.1016/j.clinph.2021.01.024",
#                               "10.1007/s00392-022-02119-7",
#                               "10.3238/arztebl.2020.0833",
#                               "10.1016/j.kint.2021.10.029",
#                               "10.1038/s41467-023-38910-x")
# )
# 
# corrections_earliest_pub_url <- tibble(
#   trial_id = c("NCT01477606",
#                "NCT04053829",
#                "2014-003647-34",
#                "DRKS00009323",
#                "DRKS00024830",
#                "DRKS00028561",
#                "DRKS00011562",
#                "NCT02326428"),
#   earliest_pub_url_clean = c("10.1182/blood.v126.23.322.322", 
#                              "10.2196/37229",
#                              "10.1097/hs9.0000000000000877",
#                              "10.2196/15361",
#                              "10.2196/56514",
#                              "10.2196/33733",
#                              "10.2196/15312",
#                              "10.1161/strokeaha.120.031031")
# )

# Linking of the earliest results publication in ClinicalTrials.gov and DRKS: Among trials/trial clusters with at least one eligible results publication identified during the manual search – and for which publication information did not require correction during data review – we will report the number and proportion where the earliest eligible results publication was linked in at least one ClinicalTrials.gov or DRKS record. EUCTR-only registrations will be excluded for this outcome because EUCTR does not provide a dedicated section for linked results publications. Note: For cross-registered trials, all relevant registrations were screened manually. However, after identifying the first registration containing an eligible results publication, subsequent registrations were assessed only to determine whether they contained an earlier eligible publication, and not whether the already identified earliest publication was linked. Therefore, we may underestimate the proportion of trials for which the earliest results publication was linked in at least one registry record. 
# qa_reglinked <- read_csv(here("data", "processed", "qa_reglinked.csv")) |> 
#   rows_upsert(corrections_reglinked_url, by = "trial_id") |> 
#   rows_upsert(corrections_earliest_pub_url, by = "trial_id") |> 
#   rowwise() |> 
#   mutate(earliest_pub_doi = str_remove(earliest_pub_doi, "preprints\\."),
#          earliest_matching_reglinked = any(c(pub_reglinked_url_clean == earliest_pub_url_clean,
#                                             pub_reglinked_url_clean == earliest_pub_doi),
#                                           na.rm = TRUE)
#            ) |> 
#   ungroup()
# 
# qa_reglinked |> 
#   group_by(crossreg_id) |>
#   mutate(has_earliest_matching_reglinked = any(earliest_matching_reglinked,
#                                                na.rm = TRUE),
#          is_crossreg = str_detect(crossreg_id, "_")) |> 
#   ungroup() |> 
#   filter(is_crossreg | !str_detect(trial_id, "-"),
#          !is.na(earliest_pub_url_clean) | !is.na(earliest_pub_doi)) |> 
#   distinct(crossreg_id, .keep_all = TRUE) |> 
#   count(has_earliest_matching_reglinked) |> 
#   mutate(total = sum(n),
#          perc = n / total * 100)


## Linking of the earliest results publication 
# is_pub_reglinked  vs earliest_pub_doi but in registry (not search engine)
combined_data_trial_level |> count(status)


combined_data_trial_level |> select(contains("date")) |> names()


combined_data_trial_level |> count(completion_date_type)


# role <- "umc_resp_party"
# 
# qa_any_filter <- combined_data_trial_level |> 
#   separate_longer_delim(all_of(role), delim = ";") |>
#   mutate(has_any = has_sumres | has_pub,
#          # has_any_pub_1y = has_summary_results_1y | has_publication_1y,
#          # has_any_pub_2y = has_summary_results_2y | has_publication_2y,
#          has_any_pub_5y = has_summary_results_5y | has_publication_5y,
#          has_followup_any_5y = has_followup_pub_5y & has_followup_sumres_5y,
#          result = .data[[route_col]] == TRUE,
#          across(all_of(c(role, "result")), factor)
#   ) |> 
#   select(trial_id, crossreg_id, has_any_pub_5y, has_followup_any_5y,
#          has_summary_results_5y, has_publication_5y, days_cd_to_publication,
#          days_cd_to_summary, results_followup_pub,
#          results_followup_sumres, has_followup_pub_5y, has_followup_sumres_5y)



qa_umc_cluster <- combined_data_enriched |> 
  filter(umc_sponsor != umc | is.na(umc_sponsor)) |> 
  select(trial_id, crossreg_id, contains("umc"))

combined_data_enriched |> 
  select(contains("date"))

qa_status <- combined_data_enriched |> 
  # filter(str_detect(crossreg_id, "_")) |>
  select(trial_id, crossreg_id, status_raw, status, cluster_status, is_index_reg,
         estimated_completion_date,
         results_completion_date,
         is_crossreg,
         only_estimated,
         completion_date, completion_date_type, hierarchical_completion_date) |> 
  arrange(crossreg_id) |> 
  group_by(crossreg_id) |> 
  mutate(index_status = status [is_index_reg == TRUE],
         cluster_status_old = deduplicate_collapsed(status),
         has_discrepant_status = str_detect(cluster_status_old, ";"),
         has_completion_date_estimated = case_when(
           is_crossreg ~ only_estimated,
           str_detect(completion_date_type, "ESTIMATED") ~ TRUE,
           str_detect(trial_id, "-") & is.na(results_completion_date) &
             !is.na(estimated_completion_date) ~ TRUE,
           str_detect(trial_id, "DRKS") & is.na(completion_date) &
             !is.na(estimated_completion_date) ~ TRUE,
           .default = FALSE
         ),
         is_completion_date_equals_estimated = case_when(
           # results_completion_date == estimated_completion_date |
           #   results_completion_date == hierarchical_completion_date ~ FALSE,
           is_crossreg == TRUE ~ coalesce(estimated_completion_date == hierarchical_completion_date, FALSE),
           is_crossreg == FALSE ~ coalesce(estimated_completion_date == completion_date, FALSE),
           # .default = FALSE
         )
         # ,
         # has_completion_date_estimated = case_when(
         #   any(results_completion_date == hierarchical_completion_date, na.rm = TRUE)  |
         #     any(results_completion_date == estimated_completion_date, na.rm = TRUE) ~ FALSE,
         #   any(str_detect(completion_date_type, "ACTUAL"), na.rm = TRUE) ~ FALSE,
         #   any(str_detect(trial_id, "DRKS") & 
         #         (estimated_completion_date != completion_date) |
         #         is.na(estimated_completion_date), na.rm = TRUE) ~ FALSE,
         #   .default = first(only_estimated2)
         # )
         ) |> 
  ungroup() |> 
  rename(cluster_only_estimated = only_estimated)

qa_status |> 
  write_csv(here("data", "processed", "qa_status.csv"))

qa_status |>
  filter(has_discrepant_status == TRUE) |> 
  count(cluster_status, cluster_status_old, sort = TRUE)


qa_status |> 
  # filter(has_discrepant_status == TRUE) |> 
  count(cluster_status, cluster_status_old, sort = TRUE)

qa_status |> 
  filter(is_index_reg == TRUE, has_completion_date_estimated) |> 
  count(is_crossreg)

qa_status |> get_dupes(trial_id)

crossreg_only_est <- qa_status |> 
  filter(is_crossreg, has_completion_date_estimated) 



qa_sumres <- combined_data_enriched |> 
  select(trial_id, crossreg_id, is_sumres_combined,
         has_sumres, has_sumres_all,
         is_summary_results_1y, 
         has_sumres_all_1y)

combined_data_enriched |> 
  count(is_summary_results_1y, has_sumres_all_1y)

###########
# comparison with IV2 dataset
###########

combined_data_enriched_iv2_comparator <- combined_data_filtered |> 
  mutate(start_date = NA_Date_,
         start_date_type = NA_character_,
         is_sumres = NA,
         is_sumres_manual = NA,
         drks_is_sumres = NA_character_,
         summary_results_date = NA_Date_,
         umc = NA_character_,
         umc_sponsor = NA_character_,
         umc_pi = NA_character_,
         umc_resp_party = NA_character_,
         primary_sponsor = NA_character_) |> 
  left_join(pub_info_enriched, by = c("trial_id", "crossreg_id")) |> 
  rows_upsert(drks_enrichment_data |> 
                select(-drks_comment_sumres), by = "trial_id") |> 
  rows_upsert(ctgov_enrichment_data, by = "trial_id") |>
  rows_upsert(new_crossregistrations, by = c("trial_id", "crossreg_id")) |> 
  left_join(query_dates, by = "registry") |> 
  filter(registry != "EUCTR") |> 
  group_by(crossreg_id) |>
  mutate(is_sumres_manual = str_detect(drks_is_sumres, "^Yes"),
         summary_results_followup_date = if_else(
    # DRKS without second review
    registry == "DRKS" & !trial_id %in% drks_sumres$trial_id,
    results_search_end_date, # date of the original publication search
    registry_query_date # date of the second drks sumres review
  ), 
  cluster_summary_results_followup_date = min(summary_results_followup_date, na.rm = TRUE),
  cluster_summary_results_date = min(summary_results_date, na.rm = TRUE),
  earliest_pub_date = min(earliest_pub_date, na.rm = TRUE),
  nonindex_completion_earlier_than_sumres = any(
    completion_date <= cluster_summary_results_date &
      is_index_reg == FALSE, na.rm = TRUE
  )) |> 
  ungroup() |> 
  mutate(
    status_raw = status,
    status = recode_status(status_raw),
    is_sumres_combined = coalesce(is_sumres_manual, is_sumres),
    is_prospective = case_when(
      registry == "EUCTR" ~ TRUE,
      .default = floor_date(start_date, "month") >=
        floor_date(registration_date, "month")
    ),
    cluster_completion_date =
      if_else(!is.na(hierarchical_completion_date),
              hierarchical_completion_date,
              completion_date),
    days_cd_to_publication = duration_days(cluster_completion_date, earliest_pub_date),
    days_reg_to_publication = duration_days(registration_date, earliest_pub_date),
    # makes sense to calculate sumres separately for each registry but followup across registries
    days_cd_to_cluster_summary = duration_days(cluster_completion_date, cluster_summary_results_date),
    days_cd_to_registry_summary = duration_days(cluster_completion_date, summary_results_date),
    # Using registry download date for reporting as summary results
    results_followup_sumres = duration_days(cluster_completion_date,
                                            cluster_summary_results_followup_date),
    # case_when(
    #### if DRKS sumres
    # is_sumres_manual != "No" & registry == "DRKS" ~ duration_days(cluster_completion_date, drks_manual_sumres_date),
    ### if no DRKS sumres
    # is_sumres_manual == "No" ~ duration_days(cluster_completion_date,
    #               cluster_results_search_end_date),
    # .default = duration_days(cluster_completion_date, cluster_summary_results_followup_date)
    
    is_followup_2y_sumres = results_followup_sumres >= 365*2,

    is_summary_results_2y = days_cd_to_registry_summary < 365*2,

    
    # Using end of search period for reporting as publication
    results_followup_pub = duration_days(cluster_completion_date,
                                         cluster_results_search_end_date),
    
    is_followup_2y_pub = results_followup_pub >= 365*2,
    
    is_publication_2y = days_cd_to_publication < 365*2,
    across(contains("is_summary_results_"),
           \(x) if_else(is.na(x), FALSE, x)) # consider this for survival analysis
  ) |> 
  group_by(crossreg_id) |> 
  mutate(umc_pi = deduplicate_collapsed(umc_pi),
         umc_sponsor = deduplicate_collapsed(umc_sponsor),
         umc = deduplicate_collapsed(umc),
         has_sumres = any(is_sumres_combined, na.rm = TRUE),
         has_sumres_all = all(is_sumres_combined),
         has_summary_results_2y = any(is_summary_results_2y,
                                      na.rm = TRUE),
         has_sumres_all_2y = all(is_summary_results_2y, na.rm = TRUE),
         pre_completion_summary_flag = has_sumres == TRUE & days_cd_to_cluster_summary < 0,
         negative_censor_time_sumres_flag = has_sumres == FALSE & results_followup_sumres < 0,
         # sumres_pre_completion_class = case_when(
         #   pre_completion_summary_flag == TRUE & ~ "before index completion date but on/after another registry completion date",
         #   pre_completion_summary_flag == TRUE ~ "before index completion date but on/after another registry completion date",
         #   
         #   .default = NA_character_
         # ),
         has_pub = any(!is.na(earliest_pub_date) & !is.infinite(earliest_pub_date)),
         has_publication_2y = any(is_publication_2y,
                                  na.rm = TRUE),
         pre_completion_publication_flag = has_pub == TRUE & days_cd_to_publication < 0,
         negative_censor_time_publication_flag = has_pub == FALSE & results_followup_pub < 0,
         has_prospective = any(is_prospective, na.rm = TRUE),
         has_pub_reglinked = any(str_detect(is_pub_reglinked, "Yes"),
                                 na.rm = TRUE),
         has_earliest_match = 
           any(str_detect(earliest_pub_matches_reg, "Yes"), na.rm = TRUE),
         has_commercial = any(str_detect(primary_sponsor, "^Comm"), na.rm = TRUE)
  ) |> 
  ungroup()


combined_data_enriched_iv2_comparator |> 
  count(has_summary_results_2y) |> 
  mutate(perc = n / sum(n) * 100)

# combined_data_enriched_iv2_comparator |> 
#   count(has_sumres_all_2y) |> 
#   mutate(perc = n / sum(n) * 100)

combined_data_enriched_iv2_comparator |> 
  count(has_publication_2y) |> 
  mutate(perc = n / sum(n) * 100)

combined_data_enriched_iv2_comparator |>
  mutate(has_any_results_2y = has_publication_2y | has_summary_results_2y) |> 
  count(has_any_results_2y) |> 
  mutate(perc = n / sum(n) * 100)

qa_is_sumres_iv2_comparator <- combined_data_enriched_iv2_comparator |> 
  select(trial_id, crossreg_id, contains("is_s"), everything())
