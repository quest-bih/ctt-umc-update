library(tidyverse)
library(here)
library(flowchart)
library(ggVennDiagram)

source(here("scripts", "utils.R"))

euctr_withdrawn_exclusions <- read_csv(here("data", "processed", "euctr_withdrawn_exclusions.csv"))

results_clean <- read_csv(here("data", "processed", "results_clean_2026-04-09.csv"))

inex_combined <- read_csv(here("data", "processed", "inex_combined.csv")) |> 
  mutate(resolves = TRUE,
         is_withdrawn_euctr = trial_id %in% euctr_withdrawn_exclusions$trial_id) |> 
  # rows_upsert(non_resolving_trns, by = "trial_id") |> 
  mutate(in_results_clean = trial_id %in% results_clean$trial_id)

count(inex_combined, is_interventional)

sample_data_filtered <- inex_combined |> 
  filter(has_german_umc, has_completion_2018_2021, has_interventional,
         !has_withdrawn_status,
         in_results_clean) |> 
  mutate(is_index_reg = if_else(is_crossreg == FALSE, TRUE, is_index_reg)) |> 
  select(trial_id, crossreg_id, everything())



sample_data_filtered |> 
  write_csv(here("data", "processed", "harmonized_data_filtered_after_pub_search.csv"))

n_crossreg_trials <- sample_data_filtered |> 
  distinct(crossreg_id, .keep_all = TRUE) |> 
  count(is_crossreg) |>
  mutate(is_crossreg = if_else(is_crossreg == TRUE, "Crossregistered", "Not crossregistered")) |> 
  pivot_wider(names_from = is_crossreg, values_from = n) |> 
  mutate(total = Crossregistered + `Not crossregistered`)

fc_inex <- inex_combined |> 
  as_fc(label = "All clinical trial registrations", text_padding = 0.3) |> 
  # fc_split(registry) |>
  fc_filter(has_interventional, label = "Interventional",
            show_exc = TRUE, offset_exc = 0.2, round_digits = 1,
            text_padding = 0.3, text_padding_exc = 0.3) |>
  fc_filter(has_german_umc, label = "German UMC",
            show_exc = TRUE, offset_exc = 0.2,
            round_digits = 1, text_padding = 0.3, text_padding_exc = 0.3) |> 
  fc_filter(has_completion_2018_2021, label = "Completed 2018-2021", show_exc = TRUE, offset_exc = 0.2,
            round_digits = 1, text_padding = 0.3, text_padding_exc = 0.3) |>
  # fc_filter(!is_crossreg, label = "Not crossregistered", show_exc = TRUE) |>
  fc_filter(!has_withdrawn_status, label = "Not withdrawn from DRKS or\nClinicalTrials.gov",
            show_exc = TRUE, offset_exc = 0.2,
            round_digits = 1, text_padding = 0.3, text_padding_exc = 0.3) |>
  fc_filter(!is_withdrawn_euctr, label = "Not withdrawn from EUCTR",
            show_exc = TRUE, offset_exc = 0.2,
            round_digits = 1, text_padding = 0.3, text_padding_exc = 0.3) |> 
  fc_filter(in_results_clean,
            text_pattern = "{label}\n N = {n} registrations ({perc}%)",
            label = paste0("Resolved online\n(N = ",
                           n_crossreg_trials$total,
                           " trials)"
                           ),
            show_exc = TRUE, offset_exc = 0.2,
            round_digits = 1, text_padding = 0.3, text_padding_exc = 0.3) |>
  fc_split(is_crossreg,
           text_pattern = "{label}\n N = {n} registrations ({perc}%)",
           label = c(paste0("Not crossregistered\n(N = ",
                            n_crossreg_trials$`Not crossregistered`,
                            " trials)"),
                     paste0("Crossregistered\n(N = ",
                            n_crossreg_trials$Crossregistered,
                            " trials)")),
           text_padding = 0.5) |> # add n trials for crossreg
  fc_draw()

# n = 1794 trials
# 
# fc_simple <- sample_data_filtered |>
#   as_fc(label = "Included Registrations")
# 
# list(fc_inex, fc_simple) |> 
#   fc_stack(unite = TRUE) |> 
#   fc_draw()


inex_per_registry <- function(tib) {
  fc <- flowchart::as_fc(tib, label = "All TRNs")
  
  fc |> 
    flowchart::fc_split(registry) |>
    flowchart::fc_filter(is_german_umc, label = "German UMC", show_exc = TRUE) |> 
    flowchart::fc_filter(is_completed_2018_2021, label = "Completed 2018-2021", show_exc = TRUE) |>
    flowchart::fc_filter(is_interventional, label = "Interventional", show_exc = TRUE) |> 
    flowchart::fc_filter(!is_crossreg, label = "Not crossregistered", show_exc = TRUE) |>
    flowchart::fc_filter(!is_withdrawn, label = "Not withdrawn", show_exc = TRUE) |>
    flowchart::fc_draw()
}


inex_combined |> 
  inex_per_registry()


falling_clusters <- read_csv(here("data", "processed", "crossreg_unfiltered.csv")) |> 
  mutate(registry = get_registry_name(trial_id),
         is_crossreg = TRUE) |> 
  select(-contains("premature"), -contains("filled"), -registries,
         -has_cluster_de_protocol, -recent_status, -actual_completion_date)


falling_clusters |> 
  as_fc(label = "Crossreg TRNs (at least one TRN in cluster is included according to 'simple' criteria") |> 
  fc_split(registry) |> 
  fc_filter(has_completion_2018_2021, label = "Completed 2018-2021", show_exc = TRUE) |>
  fc_filter(has_interventional, label = "Interventional", show_exc = TRUE) |> 
  fc_filter(has_german_umc, label = "German UMC", show_exc = TRUE) |> 
  fc_filter(!has_withdrawn_status, label = "Not withdrawn", show_exc = TRUE) |>
  flowchart::fc_draw()

gumc <- falling_clusters |> 
  filter(!has_german_umc)

setdiff(names(combined_inex), names(falling_clusters))

setdiff(names(falling_clusters), names(combined_inex))

