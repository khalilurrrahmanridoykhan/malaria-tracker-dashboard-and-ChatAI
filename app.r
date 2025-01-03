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
      pickerInput(
        inputId = "division",
        label = "DIVISION:",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select Division"
        ),
        multiple = TRUE
      ),

      pickerInput(
        inputId = "district",
        label = "DISTRICT:",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select District"
        ),
        multiple = TRUE
      ),

      pickerInput(
        inputId = "upazila",
        label = "UPAZILA:",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select Upazila"
        ),
        multiple = TRUE
      ),

      pickerInput(
        inputId = "union",
        label = "UNION:",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} selected",
          `none-selected-text` = "Select Union"
        ),
        multiple = TRUE
      ),

      dateRangeInput(
        inputId = "date_range",
        label = "DATE:",
        start = NULL,
        end = NULL,
        format = "yyyy-mm-dd"
      ),

      actionButton("reset_date", "Reset date"),
      tags$script(HTML('
        $(document).on("click", "#reset_date", function() {
          location.reload();
        });
      ')),

      tags$div(
        style = "margin-top: 20px;",
        tags$strong("Last Updated Date"),
        tags$br(),
        "KOBO: 2024-02-28",
        tags$br(),
        "SMS: 2024-02-28"
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
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
        width = 6, class = "no-gap mapclumn",
        box(
          title = "Map", status = "primary", solidHeader = TRUE, width = 12,
          leafletOutput("map", height = 420)
        )
      ),
      column(
        width = 6, class = "scrollable-column",
        div(
          style = "height: 490px; overflow-y: auto;",
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
              title = "Union Wise Case", status = "primary", solidHeader = TRUE, width = 12,
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
  df <- read.csv("./data/updated_dataset.csv")
  df$submission_time <- as.Date(df$submission_time, format = "%Y-%m-%d")

  divisions <- unique(df$DIVISION)
  districts <- unique(df$DISTRICT)
  upazilas <- unique(df$UPAZILA)
  unions <- unique(df$UNION)

  updatePickerInput(session, "division", choices = divisions, selected = divisions)
  updatePickerInput(session, "district", choices = districts, selected = districts)
  updatePickerInput(session, "upazila", choices = upazilas, selected = upazilas)
  updatePickerInput(session, "union", choices = unions, selected = unions)

  min_date <- min(df$submission_time, na.rm = TRUE)
  max_date <- max(df$submission_time, na.rm = TRUE)

  updateDateRangeInput(session, "date_range", start = min_date, end = max_date)

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
    count_kobo <- nrow(filter(filtered_df(), Type == "kobo"))
    count_sms <- nrow(filter(filtered_df(), Type == "sms"))
    count_pv <- nrow(filter(filtered_df(), Type_of_disease == "PV"))
    count_pf <- nrow(filter(filtered_df(), Type_of_disease == "PF"))
    count_mixed <- nrow(filter(filtered_df(), Type_of_disease == "Mixed"))
    count_infected_villages <- nrow(distinct(filter(filtered_df(), !is.na(Ward)), Ward))
    count_male <- nrow(filter(filtered_df(), Patient_Gender == "Male"))
    count_female <- nrow(filter(filtered_df(), Patient_Gender == "Female"))

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

  observeEvent(input$reset_date, {
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
  })

  output$typeOfTestPlot <- renderPlot({
    type_of_test_counts <- filtered_df() %>%
      count(Type_of_test) %>%
      arrange(n)

    par(mar = c(0, 2, 0, 2))

    barplot(
      type_of_test_counts$n,
      names.arg = type_of_test_counts$Type_of_test,
      horiz = TRUE,
      col = "#c9e8e2",
      cex.names = 0.7,
      cex.axis = 0.8,
      axes = FALSE
    ) -> bp

    text(type_of_test_counts$n / 2, bp, labels = type_of_test_counts$n, pos = 3, cex = 0.8, col = "black")
  }, width = 325, height = 145)


output$monthWiseCasePlot <- renderPlot({
  month_wise_counts <- filtered_df() %>%
    mutate(month = format(submission_time, "%m")) %>%
    count(month) %>%
    arrange(month)

  month_labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  par(mar = c(5, 0, 0, 2) * 0.4)

  barplot(
    month_wise_counts$n,
    names.arg = month_labels[as.numeric(month_wise_counts$month)],
    col = "#c9e8e2",
    cex.names = 0.7,
    cex.axis = 0.8,
    las = 2,
    ylim = c(0, max(month_wise_counts$n) * 1.2),
    axes = FALSE
  ) -> bp

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

  par(mar = c(5, 0, 0, 2) * 0.4)

  barplot(
    delay_counts$n,
    names.arg = delay_counts$Delay_Group,
    col = "#c9e8e2",
    cex.names = 0.7,
    cex.axis = 0.8,
    axes = FALSE
  ) -> bp

  text(bp, delay_counts$n, labels = delay_counts$n, pos = 3, cex = 0.8)
}, width = 325, height = 145)


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

  par(mar = c(5, 0, 0, 2) * 0.4)

  barplot(
    delay_counts$n,
    names.arg = delay_counts$Delay_Group,
    col = "#c9e8e2",
    cex.names = 0.7,
    cex.axis = 0.8,
    axes = FALSE
  ) -> bp

  text(bp, delay_counts$n, labels = delay_counts$n, pos = 3, cex = 0.8)
}, width = 325, height = 145)

#
output$racePlot <- renderPlot({
  race_counts <- filtered_df() %>%
    count(Race_Group) %>%
    arrange(n)

  par(mar = c(4.5, 0, 0, 2) * 0.4)

  barplot(
    race_counts$n,
    col = "#c9e8e2",
    cex.axis = 0.8,
    las = 2,
    axes = FALSE
  ) -> bp

  text(bp, race_counts$n, labels = race_counts$n, pos = 3, cex = 0.8)

  text(bp, par("usr")[3] - 0.5, labels = race_counts$Race_Group, srt = 55, adj = 1, xpd = TRUE, cex = 0.7)
}, width = 210, height = 145)

output$ageGroupPlot <- renderPlot({
  agegroup_counts <- filtered_df() %>%
    count(agegroup) %>%
    arrange(n)

  par(mar = c(4.5, 0, 0, 2) * 0.4)

  barplot(
    agegroup_counts$n,
    col = "#c9e8e2",
    cex.axis = 0.8,
    las = 2,
    axes = FALSE
  ) -> bp

  text(bp, agegroup_counts$n, labels = agegroup_counts$n, pos = 3, cex = 0.8)

  text(bp, par("usr")[3] - 0.5, labels = agegroup_counts$agegroup, srt = 55, adj = 1, xpd = TRUE, cex = 0.7)
}, width = 210, height = 145)

output$occupationPlot <- renderPlot({
  occupation_counts <- filtered_df() %>%
    count(Occupation) %>%
    arrange(n)

  par(mar = c(4.5, 0, 0, 2) * 0.4)

  barplot(
    occupation_counts$n,
    col = "#c9e8e2",
    cex.axis = 0.8,
    las = 2,
    axes = FALSE
  ) -> bp

  text(bp, occupation_counts$n, labels = occupation_counts$n, pos = 3, cex = 0.8)

  text(bp, par("usr")[3] - 0.5, labels = occupation_counts$Occupation, srt = 55, adj = 1, xpd = TRUE, cex = 0.7)
}, width = 210, height = 145)

output$unionWiseCasePlot <- renderPlot({
  union_counts <- filtered_df() %>%
    count(UNION) %>%
    arrange(n)

  par(mar = c(4.5, 0, 0, 2) * 0.4)

  barplot(
    union_counts$n,
    col = "#c9e8e2",
    cex.axis = 0.8,
    las = 2,
    axes = FALSE
  ) -> bp

  text(bp, union_counts$n, labels = union_counts$n, pos = 3, cex = 0.8)

  text(bp, par("usr")[3] - 0.5, labels = union_counts$UNION, srt = 20, adj = 1, xpd = TRUE, cex = 0.7)
}, width = 630, height = 145)

output$caseIdentificationPlot <- renderPlot({
  case_identification_counts <- filtered_df() %>%
    count(Case_identification) %>%
    arrange(n)

  par(mar = c(4.5, 0, 0, 2) * 0.4)

  barplot(
    case_identification_counts$n,
    col = "#c9e8e2",
    cex.axis = 0.8,
    las = 2,
    axes = FALSE
  ) -> bp

  text(bp, case_identification_counts$n, labels = case_identification_counts$n, pos = 3, cex = 0.8)

  text(bp, par("usr")[3] - 0.5, labels = case_identification_counts$Case_identification, srt = 55, adj = 1, xpd = TRUE, cex = 0.7)
}, width = 320, height = 145)

output$llinPlot <- renderPlot({
  llin_counts <- filtered_df() %>%
    count(Number_of_LLIN_House) %>%
    arrange(n)

  par(mar = c(4.5, 0, 0, 2) * 0.4)

  barplot(
    llin_counts$n,
    col = "#c9e8e2",
    cex.axis = 0.8,
    las = 2,
    axes = FALSE
  ) -> bp

  text(bp, llin_counts$n, labels = llin_counts$n, pos = 3, cex = 0.8)

  text(bp, par("usr")[3] - 0.5, labels = llin_counts$Number_of_LLIN_House, srt = 0, adj = 1, xpd = TRUE, cex = 0.8)
}, width = 320, height = 145)

shapefile_path <- "./shap/lamadidar.shp"
shapefile <- st_read(shapefile_path)

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
