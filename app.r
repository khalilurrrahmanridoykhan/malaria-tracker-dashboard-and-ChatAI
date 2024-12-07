library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(shinyWidgets)
library(dplyr)

df <- read.csv("./data/updated_dataset.csv")

ui <- dashboardPage(
  dashboardHeader(
    title = "Community Malaria Tracker: An Integrated Solution for Malaria Elimination",
    titleWidth = 1000
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      # Division Dropdown
      pickerInput(
        inputId = "division",
        label = "DIVISION:",
        choices = NULL, # Initially set to NULL, will be updated in server
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select Division"
        ),
        multiple = TRUE
      ),

      # District Dropdown
      pickerInput(
        inputId = "district",
        label = "DISTRICT:",
        choices = NULL, # Initially set to NULL, will be updated in server
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select District"
        ),
        multiple = TRUE
      ),

      # Upazila Multi-Select Dropdown
      pickerInput(
        inputId = "upazila",
        label = "UPAZILA:",
        choices = NULL, # Initially set to NULL, will be updated in server
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select Upazila"
        ),
        multiple = TRUE
      ),

      # Union Multi-Select Dropdown
      pickerInput(
        inputId = "union",
        label = "UNION:",
        choices = NULL, # Initially set to NULL, will be updated in server
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select Union"
        ),
        multiple = TRUE
      ),

      # Date Range Input
      dateRangeInput(
        inputId = "date_range",
        label = "DATE:",
        start = NULL, # Initially set to NULL, will be updated in server
        end = NULL,   # Initially set to NULL, will be updated in server
        format = "yyyy-mm-dd"
      ),

      # Reset Button
      actionButton("reset_date", "Reset date"),

      # Last Updated Date Info
      tags$div(
        style = "margin-top: 20px;",
        tags$strong("Last Updated Date"),
        tags$br(),
        "KOBO: 2024-09-30",
        tags$br(),
        "SMS: 2023-10-04"
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # Include the custom CSS file
      tags$script(src = "script.js")
    ),
    fluidRow(
      uiOutput("valueBoxes")
    ),
    fluidRow(
      class = "equal-height",
      column(
        width = 3, class = "custom-column",
        box(
          title = "Type of Test", width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("typeOfTestPlot")
        )
      ),
      column(
        width = 3, class = "custom-column",
        box(
          title = "Month Wise Case", width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("monthWiseCasePlot")
        )
      ),
      column(
        width = 3, class = "custom-column",
        box(
          title = "Delay Between Diagnosis & Treatment", width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("delayDiagnosisTreatmentPlot")
        )
      ),
      column(
        width = 3, class = "custom-column",
        box(
          title = "Delay Between Diagnosis & Submission", width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("delayDiagnosisSubmissionPlot")
        )
      )
    ),
    fluidRow(
      column(
        width = 6, class = "no-gap mapclumn", # Map column remains fixed
        box(
          title = "Map", status = "primary", solidHeader = TRUE, width = 12,
          leafletOutput("map", height = 500)
        )
      ),
      column(
        width = 6, class = "scrollable-column", # Apply custom class for scrollable behavior
        div(
          style = "height: 535px; overflow-y: auto;", # Set height and enable vertical scrolling
          column(
            width = 12, class = "custom-column no-gap",
            box(
              title = "Race", status = "primary", solidHeader = TRUE, width = 4,
              plotOutput("racePlot")
            ),
            box(
              title = "Age Group", status = "primary", solidHeader = TRUE, width = 4,
              plotOutput("ageGroupPlot")
            ),
            box(
              title = "Occupation", status = "primary", solidHeader = TRUE, width = 4,
              plotOutput("occupationPlot")
            )
          ),
          column(
            width = 12, class = "custom-column no-gap",
            box(
              title = "Occupation", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("unionWiseCasePlot")
            )
          ),
          column(
            width = 12, class = "custom-column no-gap",
            box(
              title = "Case Identification", status = "primary", solidHeader = TRUE, width = 6,
              plotOutput("caseIdentificationPlot")
            ),
            box(
              title = "LLIN", status = "primary", solidHeader = TRUE, width = 6,
              plotOutput("llinPlot")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Read the dataset
  df <- read.csv("./data/updated_dataset.csv")
  df$submission_time <- as.Date(df$submission_time, format = "%Y-%m-%d")

  # Extract distinct values for each column
  divisions <- unique(df$DIVISION)
  districts <- unique(df$DISTRICT)
  upazilas <- unique(df$UPAZILA)
  unions <- unique(df$UNION)

  # Update pickerInput choices
  updatePickerInput(session, "division", choices = divisions, selected = divisions)
  updatePickerInput(session, "district", choices = districts, selected = districts)
  updatePickerInput(session, "upazila", choices = upazilas, selected = upazilas)
  updatePickerInput(session, "union", choices = unions, selected = unions)

  # Extract the first and last dates from the submission_time column
  min_date <- min(df$submission_time, na.rm = TRUE)
  max_date <- max(df$submission_time, na.rm = TRUE)

  # Update dateRangeInput with extracted dates
  updateDateRangeInput(session, "date_range", start = min_date, end = max_date)

  observe({
    # Filter dataset based on user input
    filtered_df <- df %>%
      filter(
        DIVISION %in% input$division,
        DISTRICT %in% input$district,
        UPAZILA %in% input$upazila,
        UNION %in% input$union,
        submission_time >= input$date_range[1] & submission_time <= input$date_range[2]
      )

    # Calculate counts for each category
    count_kobo <- nrow(filter(filtered_df, Type == "kobo"))
    count_sms <- nrow(filter(filtered_df, Type == "sms"))
    count_pv <- nrow(filter(filtered_df, Type_of_disease == "PV"))
    count_pf <- nrow(filter(filtered_df, Type_of_disease == "PF"))
    count_mixed <- nrow(filter(filtered_df, Type_of_disease == "Mixed"))
    count_infected_villages <- nrow(distinct(filter(filtered_df, !is.na(Ward)), Ward))
    count_male <- nrow(filter(filtered_df, Patient_Gender == "Male"))
    count_female <- nrow(filter(filtered_df, Patient_Gender == "Female"))

    # Create value boxes
    output$valueBoxes <- renderUI({
      fluidRow(
        div(
          class = "value-box-custom",
          icon("mobile-alt", class = "icon"),
          div(count_kobo, class = "value"),
          div("KOBO", class = "label")
        ),
        div(
          class = "value-box-custom",
          icon("sms", class = "icon"),
          div(count_sms, class = "value"),
          div("SMS", class = "label")
        ),
        div(
          class = "value-box-custom",
          icon("stethoscope", class = "icon"),
          div(count_pv, class = "value"),
          div("PV", class = "label")
        ),
        div(
          class = "value-box-custom",
          icon("stethoscope", class = "icon"),
          div(count_pf, class = "value"),
          div("PF", class = "label")
        ),
        div(
          class = "value-box-custom",
          icon("cogs", class = "icon"),
          div(count_mixed, class = "value"),
          div("MIXED", class = "label")
        ),
        div(
          class = "value-box-custom",
          icon("map-marker-alt", class = "icon"),
          div(count_infected_villages, class = "value"),
          div("Infected Villages", class = "label")
        ),
        div(
          class = "value-box-custom",
          icon("male", class = "icon"),
          div(count_male, class = "value"),
          div("Male", class = "label")
        ),
        div(
          class = "value-box-custom",
          icon("female", class = "icon"),
          div(count_female, class = "value"),
          div("Female", class = "label")
        )
      )
    })
  })

  # Reset date button functionality
  observeEvent(input$reset_date, {
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
  })

  # Add your plots and map rendering code here
  # Example for one plot:
  output$typeOfTestPlot <- renderPlot({
    # Add your plotting code here
  })
  # Similarly, add the rest of the plots and map
}

shinyApp(ui, server)

