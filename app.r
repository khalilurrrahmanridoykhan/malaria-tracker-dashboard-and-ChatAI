library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(shinyWidgets)
library(dplyr)
library(sf)
library(leaflet.extras)


utils::globalVariables(c("Type_of_test"))

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

  # Define filtered_df as a reactive expression
  filtered_df <- reactive({
    df %>%
      filter(
        DIVISION %in% input$division,
        DISTRICT %in% input$district,
        UPAZILA %in% input$upazila,
        UNION %in% input$union,
        submission_time >= input$date_range[1] & submission_time <= input$date_range[2]
      )
  })

  observe({
    # Calculate counts for each category
    count_kobo <- nrow(filter(filtered_df(), Type == "kobo"))
    count_sms <- nrow(filter(filtered_df(), Type == "sms"))
    count_pv <- nrow(filter(filtered_df(), Type_of_disease == "PV"))
    count_pf <- nrow(filter(filtered_df(), Type_of_disease == "PF"))
    count_mixed <- nrow(filter(filtered_df(), Type_of_disease == "Mixed"))
    count_infected_villages <- nrow(distinct(filter(filtered_df(), !is.na(Ward)), Ward))
    count_male <- nrow(filter(filtered_df(), Patient_Gender == "Male"))
    count_female <- nrow(filter(filtered_df(), Patient_Gender == "Female"))

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
  # Type of Test Plot
  output$typeOfTestPlot <- renderPlot({
    type_of_test_counts <- filtered_df() %>%
      count(Type_of_test) %>%
      arrange(n)

    # Adjust margins to fit the plot within the width
    par(mar = c(3, 0, 0, 2)) # Reduce bottom margin

    barplot(
      type_of_test_counts$n,
      names.arg = type_of_test_counts$Type_of_test,
      horiz = TRUE,
      col = "#c9e8e2",
      cex.names = 0.7, # Adjust the size of the labels to fit
      cex.axis = 0.8 # Adjust the size of the axis labels to fit
    )
  }, width = 325, height = 145) # Adjust width and height as needed

 # Month Wise Case Plot

output$monthWiseCasePlot <- renderPlot({
  month_wise_counts <- filtered_df() %>%
    mutate(month = format(submission_time, "%m")) %>%
    count(month) %>%
    arrange(month)

  # Define month labels
  month_labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  # Adjust margins to fit the plot within the width
  par(mar = c(5, 0, 0, 2) * 0.4) # Adjust margins for better fit, top margin set to 0

  barplot(
    month_wise_counts$n,
    names.arg = month_labels[as.numeric(month_wise_counts$month)],
    col = "#c9e8e2",
    cex.names = 0.7, # Adjust the size of the labels to fit
    cex.axis = 0.8, # Adjust the size of the axis labels to fit
    las = 2, # Make the labels perpendicular to the axis
    ylim = c(0, max(month_wise_counts$n) * 1.2),
    axes = FALSE # Remove the bar scale
  ) -> bp

  # Add text labels to the bars
  text(bp, month_wise_counts$n, labels = month_wise_counts$n, pos = 3, cex = 0.8)
}, width = 325, height = 145)



output$delayDiagnosisTreatmentPlot <- renderPlot({
  delay_data <- filtered_df() %>%
    mutate(
      Diagnosis_Date = as.Date(Date_of_doing_test, format = "%Y-%m-%d"),
      Treatment_Date = as.Date(Date_of_Initiation_Treatment, format = "%Y-%m-%d"),
      Delay = as.numeric(difftime(Treatment_Date, Diagnosis_Date, units = "days"))
    ) %>%
    filter(!is.na(Delay))

  delay_counts <- delay_data %>%
    mutate(
      Delay_Group = case_when(
        Delay == 0 ~ "0 days",
        Delay == 1 ~ "1 day",
        Delay == 2 ~ "2 days",
        Delay %in% 3:7 ~ "3-7 days",
        TRUE ~ "< 7 days"
      )
    ) %>%
    count(Delay_Group) %>%
    arrange(factor(Delay_Group, levels = c("0 days", "1 day", "2 days", "3-7 days", "< 7 days")))

  # Adjust margins to fit the plot within the width
  par(mar = c(5, 0, 0, 2) * 0.4) # Reduce bottom margin

  barplot(
    delay_counts$n,
    names.arg = delay_counts$Delay_Group,
    col = "#c9e8e2",
    cex.names = 0.7, # Adjust the size of the labels to fit
    cex.axis = 0.8, # Adjust the size of the axis labels to fit
    axes = FALSE
  ) -> bp

  # Add text labels to the bars
  text(bp, delay_counts$n, labels = delay_counts$n, pos = 3, cex = 0.8)
}, width = 325, height = 145)
# in my dataset has a column name = Date_of_doing_test for Diagnosis date and column name = Date_of_Initiation_Treatment for Treatment date. calclate Delay Between Diagnosis & Treatment here. And show as the typeOfTestPlot in the delayDiagnosisSubmissionPlot


output$delayDiagnosisSubmissionPlot <- renderPlot({
  delay_data <- filtered_df() %>%
    mutate(
      Treatment_Date = as.Date(Date_of_Initiation_Treatment, format = "%Y-%m-%d"),
      Submission_Date = as.Date(submission_time, format = "%Y-%m-%d"),
      Delay = as.numeric(difftime(Submission_Date, Treatment_Date, units = "days"))
    ) %>%
    filter(!is.na(Delay))

  delay_counts <- delay_data %>%
    mutate(
      Delay_Group = case_when(
        Delay == 0 ~ "0 days",
        Delay == 1 ~ "1 day",
        Delay == 2 ~ "2 days",
        Delay %in% 3:7 ~ "3-7 days",
        TRUE ~ "< 7 days"
      )
    ) %>%
    count(Delay_Group) %>%
    arrange(factor(Delay_Group, levels = c("0 days", "1 day", "2 days", "3-7 days", "< 7 days")))

  # Adjust margins to fit the plot within the width
  par(mar = c(5, 0, 0, 2) * 0.4) # Reduce bottom margin

  barplot(
    delay_counts$n,
    names.arg = delay_counts$Delay_Group,
    col = "#c9e8e2",
    cex.names = 0.7, # Adjust the size of the labels to fit
    cex.axis = 0.8, # Adjust the size of the axis labels to fit
    axes = FALSE
  ) -> bp

  # Add text labels to the bars
  text(bp, delay_counts$n, labels = delay_counts$n, pos = 3, cex = 0.8)
}, width = 325, height = 145)

# in my dataset colum name Date_of_Initiation_Treatment and submission_time. my plat is =  delayDiagnosisSubmissionPlot. make it same as delayDiagnosisTreatmentPlot design and show the data as the delayDiagnosisSubmissionPlot
# d
# Load required libraries for spatial data

# Read the shapefile
shapefile_path <- "./shap/lamadidar.shp" # Update with the correct path to your shapefile
shapefile <- st_read(shapefile_path)

# Render the map
output$map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = shapefile, color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addCircleMarkers(
      data = filtered_df(),
      lat = ~latitude,
      lng = ~longitude,
      radius = 5,
      color = ~ifelse(Type_of_disease == "PV", "blue", ifelse(Type_of_disease == "PF", "red", "green")),
      stroke = FALSE,
      fillOpacity = 0.8,
      popup = ~paste("Type of Test:", Type_of_test, "<br>",
                     "Type of Disease:", Type_of_disease, "<br>",
                     "Date of Test:", Date_of_doing_test, "<br>",
                     "Date of Treatment:", Date_of_Initiation_Treatment)
    ) %>%
    addFullscreenControl()
})
# Create Layer Groups
output$map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = shapefile, color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE), group = "Shapefile") %>%
    addCircleMarkers(
      data = filtered_df(),
      lat = ~latitude,
      lng = ~longitude,
      radius = 5,
      color = ~ifelse(Type_of_disease == "PV", "blue", ifelse(Type_of_disease == "PF", "red", "green")),
      stroke = FALSE,
      fillOpacity = 0.8,
      popup = ~paste("Type of Test:", Type_of_test, "<br>",
                     "Type of Disease:", Type_of_disease, "<br>",
                     "Date of Test:", Date_of_doing_test, "<br>",
                     "Date of Treatment:", Date_of_Initiation_Treatment),
      group = "Cases"
    ) %>%
    addFullscreenControl() %>%
    addLayersControl(
      overlayGroups = c("Shapefile", "Cases"),
      options = layersControlOptions(collapsed = TRUE)
    )
})
}

shinyApp(ui, server)
