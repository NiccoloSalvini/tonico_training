## app.R ##



library(shinydashboard)
library(fontawesome)
library(glue)
library(here)
library(reactable)
library(ggplot2)
source(here::here("simple_dash", "theme_TT.R"))
fake_db = readRDS(here::here("simple_dash","data", "fake_db.Rds"))


### header ----
header <- dashboardHeader(
  title = tags$a(
    href = "https://www.treccani.it/vocabolario/tonico/",
    tags$img(src = "logo.jpg")
  ),

  ### lettera
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Sales Dept",
      message = "Sales are steady this month."
    ),
    messageItem(
      from = "New User",
      message = "How do I register?",
      icon = icon("question"),
      time = "13:45"
    ),
    messageItem(
      from = "Support",
      message = "The new server is ready.",
      icon = icon("life-ring"),
      time = "2014-12-01"
    )
  ),

  ### Triangolo
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "5 new users today",
      icon("users")
    ),
    notificationItem(
      text = "12 items delivered",
      icon("truck"),
      status = "success"
    ),
    notificationItem(
      text = "Server load at 86%",
      icon = icon("exclamation-triangle"),
      status = "warning"
    )
  ),

  ### checkpoints
  dropdownMenu(
    type = "tasks", badgeStatus = "success",
    taskItem(
      value = 90, color = "green",
      "Documentation"
    ),
    taskItem(
      value = 17, color = "aqua",
      "Project X"
    ),
    taskItem(
      value = 75, color = "yellow",
      "Server deployment"
    ),
    taskItem(
      value = 80, color = "red",
      "Overall project"
    )
  )
)

### sidebar ----
sidebar <- dashboardSidebar(
  sidebarSearchForm(
    textId = "searchText", buttonId = "searchButton",
    label = "Search..."
  ),
  sidebarMenu(
    menuItem("Athlete Dashboard", tabName = "dash", icon = icon("dashboard")),
    menuItem("Exercises", tabName = "Exercises", icon = icon("running")),
    menuItem("Diet", tabName = "diet", icon = icon("pizza-slice"))
  ),
  menuItem("  Source Code",
    icon = icon("file-code-o"),
    href = "https://github.com/NiccoloSalvini"
  )
)

### body ----
body <- dashboardBody(

  ## refer to CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  ## DIET ----
  tabItems(
    tabItem(
      tabName = "diet",
      fluidRow(
        tabBox(
          title = "Remeber Text Box",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", side = "right", height = "250px",
          textAreaInput("remember", label = tags$p(fa("pencil", fill = "red", height = "20px"), "Type in..."), height = "100px")
        ),
        tabBox(
          title = "Goal Text Box",
          id = "tabset1", side = "left", height = "250px",
          textAreaInput("goal", label = tags$p(fa("pencil", fill = "red", height = "20px"), "Type in..."), height = "100px")
        ),
        fluidRow(
          tabBox(
            title = "See User Inputs", id = "tabset1", side = "right", height = "250px",
            DT::dataTableOutput("datatable")
          ),
          tabBox(
            title = "Choose Donwload option", id = "tabset3", side = "right", height = "250px",

            ## radio buttons
            column(2, radioButtons(
              inputId = "report_format",
              label = "Format of report",
              choices = c("HTML" = "html", "PDF" = "pdf", "DOCX" = "docx")
            )),
            column(2, textInput(
              inputId = "reportAuthor",
              label = "Author"
            )),
            column(2, textInput(
              inputId = "reportDataName",
              label = "Dataset"
            )),
            column(
              2,
              h4("Content of report... "), 
              downloadButton('report_gen')
              
            ),
            # downloadButton(
            #   outputId = 'report_gen',
            #   label = "Create my report"
            # ),
            tags$hr(),
            br(),
            br(),
            # * DOWNLOAD ####
            p(
              strong("Recommendation: "), "Report generation can be faster and more reliable when you first check
             sections of intended contents. For example, if you wish to include a ", strong("3PL IRT"),
              " model, you can first visit the ", strong("Dichotomous models"), " subsection of the ",
              strong("IRT models"), "section and fit the ", strong("3PL IRT"), " model."
            ),
            br(),
            div(
              style = "display:inline-block",
              actionButton(
                inputId = "generate",
                label = "Generate report",
                class = "btn btn-primary",
                icon = icon("bar-chart-o")
              )
            ),
            tags$style(HTML("#download_report_button { margin-left: 25px }")),
            div(style = "display:inline-block", uiOutput("download_report_button"))
          )
        )
      ),
    ),

    ## DASH ----
    tabItem(
      tabName = "dash",
      fluidRow(
  
        selectInput("athlete", "Athelete Name",
                    fake_db$name),
        selectInput("athlete", "Athelete Surname",
                    fake_db$surname)
      ),

      ## INFOBOXES
      fluidRow(
        ## STatic infoBoxes
        infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
        # Dynamic infoBoxes
        infoBoxOutput("progressBox"),
        infoBoxOutput("approvalBox")
      ),
      # infoBoxes with fill=TRUE
      fluidRow(
        infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
        infoBoxOutput("progressBox2"),
        infoBoxOutput("approvalBox2")
      ),

      fluidRow(
        ## db display
        tabBox(
          title = "Athletes table", side  = "left",
          reactableOutput("table")
          )
        )
      )
    )
)


ui <- dashboardPage(
  skin = "red",
  header,
  sidebar,
  body
)



  server <- function(input, output) {

  
  # gen a REPORT
  ###  PROGRESS bar ----
  observeEvent(input$generate, {
    withProgress(message = "Creating content", value = 0, style = "notification", {
      list( # header
        author = input$reportAuthor,
        dataset = input$reportDataName
      )
    })
  })
  
  output$download_report_button <- renderUI({
    if (is.null(input$generate)) {
      return(NULL)
    }
    downloadButton(
      outputId = "report",
      label = "Download report",
      class = "btn btn-primary"
    )
  })


  ###  download Report ---
  output$report <- downloadHandler(
    filename = reactive({
      paste0("report.", input$report_format)
    }),
    content = function(file) {
      reportPath <- here::here("simple_dash", glue::glue("report{input$report_format}.Rmd"))
      parameters <- list(
        author = input$reportAuthor,
        dataset = input$reportDataName
      )
      rmarkdown::render(reportPath,
        output_file = file,
        params = parameters,
        envir = new.env(parent = globalenv())
      )
    }
  )

  ### info BOXES ----

  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"),
      icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })

  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"),
      icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })

  output$datatable <- DT::renderDataTable({
    data.frame(
      Remember = input$goal,
      Goal = input$remember,
      stringsAsFactors = FALSE
    )
  })
  
  output$table <- renderReactable({
    reactable(fake_db, filterable = TRUE, 
              showPageSizeOptions = TRUE,
              highlight = TRUE,
              columns = list(
                bmi_index = colDef(filterable = FALSE, format = colFormat(digits = 2)),
                height = colDef(filterable = FALSE, format = colFormat(digits = 2)),
                weight = colDef(filterable = FALSE,  format = colFormat(digits = 2))
                ),
              theme = reactableTheme(
                borderColor = "#8a2f2c",
                stripedColor = "#8a2f2c"
                )
              )
  })

  
  
  
  # old download buttons
# output$report_gen <- downloadHandler(
# 
#   filename = "",
#   content = function(file) {
#     # copy markdown report file to a temporary directory before knitting it with the
#     # selected dataset. This is useful if we don't have write permissions for the current
#     # working directory
#     temp_report <- file.path(tempdir(), "report_temp.docx")
#     message("\n... report_temp path: ", temp_report, "\n")
# 
#     # copy the report template into the temp directory
#     file.copy(here::here("shiny_report_gen", "report_temp.docx"),
#               temp_report, overwrite = TRUE)
# 
#     # create a named list of parameters to pass to to Rmd template.
#     # can also pass reactiveValues or reactive objects
#     pass_params <- list(
#       imported = my_vals
#     )
# 
#     # knit the document, passing in the `pass_params` list, and evaluate it in a
#     # child of the global environment (this isolates the code in the document
#     # from the code in the app).
#     rmarkdown::render(
#       temp_report,
#       output_file = file,
#       params = pass_params,
#       envir = new.env(parent = globalenv())
#     )
# 
#   }
# 
# )

}

shinyApp(ui, server)
