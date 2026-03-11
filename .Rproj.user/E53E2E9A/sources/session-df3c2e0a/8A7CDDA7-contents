library(shiny)
library(ggiraph)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reactable)

addResourcePath("my_images", "images/")

# 1. Source plotting functions
files <- list.files("R", full.names = TRUE)
for (f in files) {
  source(f, local = TRUE, encoding = "UTF-8")
}

# 2. Load clean data
df <- readRDS("data/Rain_DW.rds") %>%
  mutate(
    Date = as.Date(Date),
    Rain = as.numeric(Rain),
    FY_Year = if_else(Month >= 7, Year, Year - 1)
  )

# --- UI Section ---
ui <- fluidPage(
  titlePanel("Daly Waters Rainfall Dashboard"),

  sidebarLayout(
    sidebarPanel(
      # This slider will now control plots across different tabs
      sliderInput("yearRange", "Financial Year Range:",
                  min = min(df$FY_Year, na.rm = TRUE),
                  max = max(df$FY_Year, na.rm = TRUE),
                  value = c(1889, 2026),
                  sep = ""),
      hr(),
      p("The slider filters both the annual totals and the anomalies simultaneously.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("About & README",
                 fluidRow(
                   column(8, offset = 2,
                          br(),
                          h2("Daly Waters Rainfall Analysis (1889–2026)"),
                          p("This dashboard provides a visual history of rainfall recorded at Daly Waters, Northern Territory."),

                          hr(),
                          p("Financial year rainfall (July to June) has been used in this dashboard because
                            almost all rain falls during the 'wet season' from late October to April. Aggregating by
                            financial year therefore captures seasonal totals."),
                          hr(),
                          h4("How to use this Dashboard:"),
                          tags$ul(
                            tags$li(strong("Slider:"), " Use the sidebar to filter the date range for all charts and tables."),
                            tags$li(strong("Interactive Plots:"), " Hover over bars and lines to see specific Financial Year (July-June) totals."),
                            tags$li(strong("Anomalies:"), " View departures from the long-term mean (Blue = Wet, Red = Dry)."),
                            tags$li(strong("Rainy Days:"), " Rank years by the frequency of rainy days in the final tab.")
                          ),

                          hr(),

                          h4("Data Sources"),
                          p("Data has been sourced from patched data from the ",
                            tags$a(href = "https://www.longpaddock.qld.gov.au/silo/",
                                   "SILO climate database", target = "_blank"),
                            "which is derived from BOM weather stations:
                            'The Patched Point Data system is constructed from
                            observational data obtained from the Bureau of
                            Meteorology and other suppliers'."),
                          br(),
                          p("Rainfall has been recorded for Daly Waters at two weather stations:
                            the township (ID: 014618) and airstrip (ID: 014626).
                            Data availability for each station is shown below."),
                          tags$figure(
                            style = "text-align: center;", # Centers both image and caption
                            img(src = "my_images/DW_town_dataavail.png",
                                width = "100%",
                                style = "border-radius: 10px; border: 1px solid #ddd;"),
                            tags$figcaption(
                              style = "margin-top: 10px; font-style: italic; color: #555; font-size: 0.9em;",
                              "Figure 1: Rainfall data availability at Daly Waters township station (014618)."
                            )
                          ),
                          tags$figure(
                            style = "text-align: center;", # Centers both image and caption
                            img(src = "my_images/DW_air_dataavail.png",
                                width = "100%",
                                style = "border-radius: 10px; border: 1px solid #ddd;"),
                            tags$figcaption(
                              style = "margin-top: 10px; font-style: italic; color: #555; font-size: 0.9em;",
                              "Figure 2: Rainfall data availability at Daly Waters airstrip station (014626)."
                            )
                          ),
                          p("Based on the availablility above, four periods were combined:"),
                          tags$ul(
                            tags$li(strong("1889 - 1960:"), "Township"),
                            tags$li(strong("1960 - 1970:"), "Airstrip"),
                            tags$li(strong("1970 - 2000:"), "Township"),
                            tags$li(strong("2000 - 2026:"), "Airstrip")
                          ),
                          br(), br(),
                          br()
                   )
                 )
        ), # End of README tab
        tabPanel("Seasonal Accumulation", girafeOutput("cumPlot")),

        # Stacking the Bar and Anomaly plots
        tabPanel("Annual Totals",
                 fluidRow(
                   column(12,
                          br(),
                          h4("Financial year rainfall totals"),
                          p("Adjusting the slidebar updates the standardised anomaly and median calculations, showing
                            that the median annual rainfall (mm) value generally increases over time."),
                          girafeOutput("totalBarPlot", width = "100%", height = "auto"),
                          hr(), # Visual line between plots
                          girafeOutput("anomalyPlot", width = "100%", height = "auto")
                   )
                 )
        ),
        tabPanel("Rainy Days",
                 br(),
                 fluidRow(
                   column(10, offset = 1,
                          h4("Rainy Days Ranking"),
                          p("Years ranked by the frequency of rainy days (>0mm). Click the 'Rainy Days'
                          column to change between sorting by ascending/descending order."),
                          br(),
                          p("Any column can be used to rank data in ascending/descending order."),
                          br(),
                          reactableOutput("daysTable") # Output for the table
                   )
                 )
        )
      )
    )
  )
)

# --- Server Section ---
server <- function(input, output) {

  # Reactive data subset based on the slider
  filtered_df <- reactive({
    df %>%
      filter(FY_Year >= input$yearRange[1],
             FY_Year <= input$yearRange[2])
  })

  # 1. Total Bar Plot (Top)
  output$totalBarPlot <- renderGirafe({
    create_bar_plot(filtered_df())
  })

  # 2. Anomaly Plot (Bottom)
  output$anomalyPlot <- renderGirafe({
    create_anomaly_plot(filtered_df())
  })

  # 3. Cumulative Plot (From previous steps)
  output$cumPlot <- renderGirafe({
    create_cum_plot(filtered_df())
  })

  # 4. Reactable
  output$daysTable <- renderReactable({
    create_rainy_days_table(filtered_df())
  })
}

# Run the App
shinyApp(ui = ui, server = server)
