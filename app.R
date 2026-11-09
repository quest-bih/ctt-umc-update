# launch_app <- function() {
#   quarto::quarto_render("index.qmd")
# }
# 
# 
# launch_app()
# 
# 

library(flexdashboard)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(here)
library(shiny)
library(bslib)


source("scripts/utils.R")

analysis_results <- read_csv(here("data", "processed", "analysis_results.csv")) |> 
  group_by(crossreg_id) |> 
  mutate(has_publication_1y = any(days_cd_to_publication < 365,
                                  na.rm = TRUE),
         .before = has_publication_2y) |> 
  mutate(umc_pi_resp_party_no_sponsor = deduplicate_collapsed(c(umc_resp_party, umc_pi),
                                                              exclude = umc_sponsor),
         .after = umc_resp_party) |>
  ungroup() |> 
  filter(is_index_reg == TRUE)
# qa_umc <- analysis_results |> select(trial_id, crossreg_id, contains("umc")) |> 
#   mutate(has_charite_sponsor = str_detect(umc_sponsor, "Charite"),
#          has_charite_pi_or_resp_party = str_detect(umc_pi, "Charite") |
#            str_detect(umc_resp_party, "Charite"))

source(here("app", "modules", "selectUMC.R"))
source(here("app", "modules", "dataset_panel.R"))
source(here("app", "modules", "primary_outcomes.R"))
source(here("scripts", "utils.R"))


institutions_raw <- get_umc_terms(collapse = FALSE) |> names()
institutions <- c(institutions_raw[!str_detect(institutions_raw, "Munich")], "München_LMU", "München_TU") |> 
  sort()


show_dashboard <- function(...) {
  #----------------------------------------------------------------------------------------------------------------------
  # ui
  #----------------------------------------------------------------------------------------------------------------------
  
  ui <-
    tagList(
      page_sidebar(
        title = "German University Medical Centers (UMC) Clinical Trials Update 2018-2021",
        theme = bs_theme(version = 5, bootswatch = "pulse"),
        sidebar = sidebar(
                          width = 350,
                          resizable = FALSE,
                          
          selectUMCInput("mainUMC",
                         strong("Select a University Medical Center from the drop-down menu"),
                         choices = c(
                           "All (cumulative, side-by-side)",
                           institutions
                         )
          ),
          
          radioButtons( 
            inputId = "route", 
            label = h4("Reporting route"), 
            choices = list(
              "Summary results on any registry" = "has_sumres",
              "Summary results on every registry" = "has_sumres_all",
              "Journal publications" = "has_pub",
              "Any" = "has_any"
            )),
          
          radioButtons( 
            inputId = "role", 
            label = h4("Role of University Medical Center"), 
            choices = list(
              "Sponsor, responsible party, or host of the Principal Investigator" = "umc",
              "Sponsor" = "umc_sponsor",
              "UMC affiliation without lead sponsorship" = "umc_pi_resp_party_no_sponsor"
               
            )),
          
          radioButtons( 
            inputId = "timing", 
            label = h4("Timing"), 
            choices = list(
              "At followup" = 6,
              "Within 5 years of completion" = 5,
              "Within 2 years of completion" = 2,
              "Within 1 year of completion" = 1 
            )),
          
          checkboxGroupInput( 
            inputId = "status", 
            label = h4("Status of the trial"), 
            choices = c(
              "Completed",
              "Ongoing",
              "Terminated",
              "Suspended",
              "Unknown"
            ),
            selected = c(
              "Completed",
              "Ongoing",
              "Terminated",
              "Suspended",
              "Unknown"
            )),
          
          checkboxGroupInput(
            inputId = "year", 
            label = h4("Completion year (of the index registration)"), 
            choices = 2018:2021,
            selected = 2018:2021,
            inline = TRUE
          ),
          
          checkboxInput(
            inputId = "estimated_cd",
            label = "Include index registrations with only estimated completion dates?",
            value = TRUE,
            width = "100%"
          ),
          
          radioButtons( 
            inputId = "euctr_involvement", 
            label = h4("EUCTR involvement"), 
            choices = list(
              "All trials" = "all_trials",
              "Only trials with EUCTR registrations" = "only_euctr_trials",
              "Only trials without EUCTR registrations" = "no_euctr_trials"
            )),
          
          
          radioButtons(
            inputId = "sponsor_status",
            label = h4("Sponsor status"),
            choices = list(
              "Any" = "all_trials",
              # "Only commercial" = "only_comm",
              "Only non-commercial" = "non_comm"
            )
          )
        ),
        
          # layout_columns(
          #     h5("Sponsor, responsible party, or host of the Principal Investigator",
          #        style = "font-weight: bold;"),
          #     h5("Sponsor",
          #        style = "margin-bottom: 8.5mm; font-weight: bold;")
          #   ),
        page_fillable(
          
          # textOutput("test"),
          # DT::dataTableOutput("dt"),
          overallOutput("umcValueBox"),
          primaryOutput("umcPlot")
        )
        )
    )
  #----------------------------------------------------------------------------------------------------------------------
  # server
  #----------------------------------------------------------------------------------------------------------------------
  server <- function(input, output, session) 
  {
    # data_summaries_sponsors <- selectUMCServer("mainUMC", analysis_results, "umc_sponsor")
    
    
    RVs <- reactiveValues(route = NA, timing = NA, status = NA,
                          estimated_cd = NA, euctr_involvement = NA,
                          sponsor_status = NA,
                          role = NA)
    
    
    
    observe({
      RVs$route <- input$route
    }) |> 
      bindEvent(input$route)
    
    # 
    observe({
      RVs$role <- input$role
    }) |>
      bindEvent(input$role)
    
    observe({
      RVs$timing <- input$timing
    }) |> 
      bindEvent(input$timing)
    
    observe({
      RVs$status <- input$status
    }) |> 
      bindEvent(input$status)
    
    observe({
      RVs$selected_years <- input$year
    }) |> 
      bindEvent(input$year)
    
    observe({
      RVs$estimated_cd <- input$estimated_cd
    }) |> 
      bindEvent(input$estimated_cd)
    
    observe({
      RVs$euctr_involvement <- input$euctr_involvement
    }) |> 
      bindEvent(input$euctr_involvement)
    
    observe({
      RVs$sponsor_status <- input$sponsor_status
    }) |> 
      bindEvent(input$sponsor_status)
    
    data_summaries <- selectUMCServer("mainUMC", analysis_results, reactive(RVs$role))
    
    primaryServer("umcPlot", data_summaries,
                  route = reactive(RVs$route),
                  role = reactive(RVs$role),
                  status_filter = reactive(RVs$status),
                  timing = reactive(RVs$timing),
                  selected_years = reactive(RVs$selected_years),
                  estimated_cd = reactive(RVs$estimated_cd),
                  euctr_involvement = reactive(RVs$euctr_involvement),
                  sponsor_status = reactive(RVs$sponsor_status),
                  color_fill = "darkgreen",
                  title = "")
    
    # output$test <- renderText({paste(RVs$role)})
    # output$dt <- renderDataTable({ data_summaries()$data})

    
    primaryServer("umcValueBox", analysis_results,
                  route = reactive(RVs$route),
                  role = reactive(RVs$role),
                  status_filter = reactive(RVs$status),
                  timing = reactive(RVs$timing),
                  selected_years = reactive(RVs$selected_years),
                  estimated_cd = reactive(RVs$estimated_cd),
                  euctr_involvement = reactive(RVs$euctr_involvement),
                  sponsor_status = reactive(RVs$sponsor_status),
                  color_fill = "bg-gradient-purple-green",
                  title = ""
    )
    
    # primaryServer("umcsponsorValueBox", analysis_results,
    #               route = reactive(RVs$route),
    #               role = "umc_sponsor",
    #               status_filter = reactive(RVs$status),
    #               timing = reactive(RVs$timing),
    #               selected_years = reactive(RVs$selected_years),
    #               estimated_cd = reactive(RVs$estimated_cd),
    #               euctr_involvement = reactive(RVs$euctr_involvement),
    #               sponsor_status = reactive(RVs$sponsor_status),
    #               color_fill = "bg-gradient-purple-blue",
    #               title = ""
    # )
  }

shinyApp(ui, server)

}

show_dashboard()