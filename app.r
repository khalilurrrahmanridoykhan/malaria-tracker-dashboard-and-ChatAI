library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = "Community Malaria Tracker: An Integrated Solution for Malaria Elimination",
    titleWidth = 1000 # Adjust title width to show full title
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      dateRangeInput("dateRange",
        label = "Date range input: yyyy-mm-dd",
        start = Sys.Date() - 365, end = Sys.Date()
      ),
      tags$div(
        "Last Updated Date",
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
      div(
        class = "value-box-custom",
        icon("mobile-alt", class = "icon"),
        div("2563", class = "value"),
        div("KOBO", class = "label")
      ),
      div(
        class = "value-box-custom",
        icon("sms", class = "icon"),
        div("34", class = "value"),
        div("SMS", class = "label")
      ),
      div(
        class = "value-box-custom",
        icon("stethoscope", class = "icon"),
        div("1451", class = "value"),
        div("PV", class = "label")
      ),
      div(
        class = "value-box-custom",
        icon("stethoscope", class = "icon"),
        div("1108", class = "value"),
        div("PF", class = "label")
      ),
      div(
        class = "value-box-custom",
        icon("cogs", class = "icon"),
        div("38", class = "value"),
        div("MIXED", class = "label")
      ),
      div(
        class = "value-box-custom",
        icon("map-marker-alt", class = "icon"),
        div("436", class = "value"),
        div("Infected Villages", class = "label")
      ),
      div(
        class = "value-box-custom",
        icon("male", class = "icon"),
        div("1614", class = "value"),
        div("Male", class = "label")
      ),
      div(
        class = "value-box-custom",
        icon("female", class = "icon"),
        div("983", class = "value"),
        div("Female", class = "label")
      )
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

server <- function(input, output) {
  output$koboBox <- renderValueBox({
    valueBox(2563, "KOBO", icon = icon("mobile-alt"), color = "green")
  })
  output$smsBox <- renderValueBox({
    valueBox(34, "SMS", icon = icon("sms"), color = "green")
  })
  output$pvBox <- renderValueBox({
    valueBox(1451, "PV", icon = icon("stethoscope"), color = "green")
  })
  output$pfBox <- renderValueBox({
    valueBox(1108, "PF", icon = icon("stethoscope"), color = "green")
  })
  output$mixedBox <- renderValueBox({
    valueBox(38, "MIXED", icon = icon("cogs"), color = "green")
  })
  output$infectedVillagesBox <- renderValueBox({
    valueBox(436, "Infected Villages", icon = icon("map-marker-alt"), color = "green")
  })
  output$maleBox <- renderValueBox({
    valueBox(1614, "Male", icon = icon("male"), color = "green")
  })
  output$femaleBox <- renderValueBox({
    valueBox(983, "Female", icon = icon("female"), color = "green")
  })
  # Add your plots and map rendering code here
  # Example for one plot:
  output$typeOfTestPlot <- renderPlot({
    # Add your plotting code here
  })
  # Similarly, add the rest of the plots and map
}

shinyApp(ui, server)
