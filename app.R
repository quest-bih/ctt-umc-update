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

analysis_results <- read_csv(here("data", "processed", "analysis_results.csv")) |> 
  group_by(crossreg_id) |> 
  mutate(has_publication_1y = any(days_cd_to_publication < 365,
                                  na.rm = TRUE)) |>
  ungroup() |> 
  filter(is_index_reg == TRUE)

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
      page_sidebar(theme = bs_theme(version = 5, bootswatch = "minty"),
        title = "German UMC Clinical Trials Update 2018-2021",
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
            inputId = "timing", 
            label = h4("Timing"), 
            choices = list(
              "At followup" = 6,
              "Within 5 years of completion (excluding trials with less than 5-year follow-up)" = 5,
              "Within 2 years of completion" = 2,
              "Within 1 year of completion" = 1 
            )),
          
          checkboxGroupInput( 
            inputId = "status", 
            label = h4("Status (of index registration)"), 
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
              "Only commercial" = "only_comm",
              "Only non-commercial" = "non_comm"
            )
          )
        ),
        
          layout_columns(
              h5("Sponsor, responsible party, or host of the Principal Investigator",
                 style = "font-weight: bold;"),
              h5("Sponsor or responsible party",
                 style = "margin-bottom: 8.5mm; font-weight: bold;")
            ),
        layout_columns(
          overallOutput("umcValueBox"),
          overallOutput("umcsponsorValueBox")
        ),
        layout_columns(
          primaryOutput("umcPlot"),
          primaryOutput("umcsponsorPlot")
        )
            
        
             # wellPanel(
           #         br(),
           #         fluidRow(
           #           column(8,
           #                  h1(style = "margin-left:0cm", strong("Charité Dashboard on Responsible Research"), align = "left"),
           #                  h4(style = "margin-left:0cm",
           #                     HTML('Charité has committed itself to establish, promote and maintain a
           #                  research environment which enhances the robustness of research and
           #                  the reproducibility of results
           #                       (<a href="https://www.charite.de/en/charite/about_us/strategic_direction_2030/">Rethinking Health – Charité 2030</a>).')),
           #                  h4(style = "margin-left:0cm",
           #                     HTML('This dashboard gives an overview of several metrics of open and responsible
           #                  research at the Charité (including the Berlin Institute of Health).
           #                  For a detailed discussion about monitoring core Open Science practices see
           #                  (<a href = "https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001949">Cobey et al. 2023</a>).
           #                  For more detailed information on the methods used to calculate those metrics, the dataset
           #                  underlying the metrics, or resources to improve your own research practices, click one of
           #                       the following buttons on the right.')),
           #                  # h4(style = "margin-left:0cm",
           #                  #"This dashboard is a pilot that is still under development. More metrics will be added in the future."),
           #                  h4(style = "margin-left:0cm",
           #                     HTML('For more detailed open access metrics you can visit the
           #                  <a href="https://medbib-charite.github.io/oa-dashboard/">Charité Open Access Dashboard</a>
           #                       developed by the Charité Medical Library.')),
           #                  br()
           #           ),
           #           column(4,
           #                  hr(),
           #                  br(),
           #                  br(),
           #                  br(),
           #                  actionButton(style = "color: white; background-color: #aa1c7d;",
           #                               'buttonMethods',
           #                               'See methods'),
           #                  actionButton(style = "color: white; background-color: #aa1c7d;",
           #                               'buttonResources',
           #                               'See resources'),
           #                  actionButton(style = "color: white; background-color: #aa1c7d;",
           #                               'buttonDatasets',
           #                               'See dataset'),
           #                  br(),
           #                  br(),
           #                  h4(style = "margin-left:18mm", strong("Latest Update: April 2025")))
           #         ),
           #         fluidRow(column(1,
           #                         selectInput("citationStyle",
           #                                     h5(HTML("<b>Cite us:</b>")),
           #                                     c("APA",
           #                                       "MLA",
           #                                       "Chicago"),
           #                                     width = "100px")),
           #                  column(11,
           #                         hr(),
           #                         # br(),
           #                         htmlOutput("citation_text"))
           #         )
           #       ),
                 
                 # generate Open Science & Clinical trial metrics UI dynamically to determine column width during start of the app
                 # uiOutput("OpenScience_metrics") |>
                 #   shinycssloaders::withSpinner(color = "#007265"),
                 # 
                 # uiOutput("CT_metrics") |>
                 #   shinycssloaders::withSpinner(color = "#007265"),
                 # 
                 # uiOutput("Broader_transparency_metrics") |>
                 #   shinycssloaders::withSpinner(color = "#007265"),
                 # 
                 # uiOutput("Visualizations_metrics") |>
                 #   shinycssloaders::withSpinner(color = "#007265"),
                 
        )
    )
  #----------------------------------------------------------------------------------------------------------------------
  # server
  #----------------------------------------------------------------------------------------------------------------------
  server <- function(input, output, session) 
  {
    data_summaries_sponsors <- selectUMCServer("mainUMC", analysis_results, "umc_sponsor")
    
    data_summaries <- selectUMCServer("mainUMC", analysis_results, "umc")
    
    RVs <- reactiveValues(route = NA, timing = NA, status = NA, estimated_cd = NA, euctr_involvement = NA, sponsor_status = NA)
    
    observe({
      RVs$route <- req(input$route)
    }) |> 
      bindEvent(input$route)
    
    observe({
      RVs$timing <- req(input$timing)
    }) |> 
      bindEvent(input$timing)
    
    observe({
      RVs$status <- req(input$status)
    }) |> 
      bindEvent(input$status)
    
    observe({
      RVs$selected_years <- req(input$year)
    }) |> 
      bindEvent(input$year)
    
    observe({
      RVs$estimated_cd <- req(input$estimated_cd)
    }) |> 
      bindEvent(input$estimated_cd)
    
    observe({
      RVs$euctr_involvement <- req(input$euctr_involvement)
    }) |> 
      bindEvent(input$euctr_involvement)
    
    observe({
      RVs$sponsor_status <- req(input$sponsor_status)
    }) |> 
      bindEvent(input$sponsor_status)
    
    primaryServer("umcPlot", data_summaries,
                  route = reactive(RVs$route),
                  role = "umc",
                  status_filter = reactive(RVs$status),
                  timing = reactive(RVs$timing),
                  selected_years = reactive(RVs$selected_years),
                  estimated_cd = reactive(RVs$estimated_cd),
                  euctr_involvement = reactive(RVs$euctr_involvement),
                  sponsor_status = reactive(RVs$sponsor_status),
                  color_fill = "darkgreen",
                  title = "")
    
    primaryServer("umcsponsorPlot", data_summaries_sponsors,
                  route = reactive(RVs$route),
                  role = "umc_sponsor",
                  status_filter = reactive(RVs$status),
                  timing = reactive(RVs$timing),
                  selected_years = reactive(RVs$selected_years),
                  estimated_cd = reactive(RVs$estimated_cd),
                  euctr_involvement = reactive(RVs$euctr_involvement),
                  sponsor_status = reactive(RVs$sponsor_status),
                  color_fill = "navy",
                  title = "")
    
    primaryServer("umcValueBox", analysis_results,
                  route = reactive(RVs$route),
                  role = "umc",
                  status_filter = reactive(RVs$status),
                  timing = reactive(RVs$timing),
                  selected_years = reactive(RVs$selected_years),
                  estimated_cd = reactive(RVs$estimated_cd),
                  euctr_involvement = reactive(RVs$euctr_involvement),
                  sponsor_status = reactive(RVs$sponsor_status),
                  color_fill = "bg-gradient-purple-green",
                  title = ""
    )
    
    primaryServer("umcsponsorValueBox", analysis_results,
                  route = reactive(RVs$route),
                  role = "umc_sponsor",
                  status_filter = reactive(RVs$status),
                  timing = reactive(RVs$timing),
                  selected_years = reactive(RVs$selected_years),
                  estimated_cd = reactive(RVs$estimated_cd),
                  euctr_involvement = reactive(RVs$euctr_involvement),
                  sponsor_status = reactive(RVs$sponsor_status),
                  color_fill = "bg-gradient-purple-blue",
                  title = ""
    )
  }

shinyApp(ui, server)

}

show_dashboard()