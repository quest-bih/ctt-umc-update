library(tidyverse)
library(ctregistries)

# extract list of regexes for given registry names
get_registry_regex <- function(registry_names_vec) {
  registry_tib <- ctregistries::registries |> 
    filter(registry %in% registry_names_vec) |> 
    select(registry, trn_regex)
  registry_tib |> 
    pull(trn_regex) |> 
    set_names(registry_tib$registry) |> 
    as.list()
}
# regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

# combine separator-listed vectors into a single collapsed, deduplicated string
deduplicate_collapsed <- function(str_vec, collapse = ";") {
  str_vec |>
    strsplit(split = collapse) |> 
    unlist() |> 
    unique() |> 
    dplyr::na_if("") |>
    dplyr::na_if("-") |>
    dplyr::na_if("NA") |> 
    stats::na.omit() |> 
    paste(collapse = collapse) |> 
    dplyr::na_if("")
}
