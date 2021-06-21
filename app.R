## app.R ##


## libraries ----
library(shinydashboard)
library(fontawesome)
library(glue)
library(here)
library(reactable)
library(ggplot2)

## theme -----
source(here::here("theme_TT.R"))


## load data -----
fake_db = readRDS(here::here("data", "fake_db.Rds"))


### * Dash Header ----
header <- dashboardHeader(
  title = tags$a(
    href = "https://www.treccani.it/vocabolario/tonico/",
    tags$img(src = "logo.jpg")
  ),

  ### letter
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

  ### triangle
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

  ### checkpoint
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

## * Dash Sidebar ----
sidebar <- dashboardSidebar(
  sidebarSearchForm(
    textId = "searchText", buttonId = "searchButton",
    label = "Search..."
  ),
  sidebarMenu(
    menuItem("Athlete Dashboard", tabName = "dash", icon = icon("dashboard")),
    menuItem("Diet", tabName = "diet", icon = icon("pizza-slice")),
    menuItem("Exercises", tabName = "exercises", icon = icon("running")), 
    menuItem("Database", tabName = "db", icon = icon("database")),
    menuItem("Form", tabName = "form", icon = icon("list-ul"))
    
  ),
  ## source
  sidebarMenu(
  menuItem("  Source Code",
    icon = icon("file-code-o"),
    href = "https://github.com/NiccoloSalvini"
  )
  )
)

## * Dash Body ----
body <- dashboardBody(

  ## CSS binding
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

 
  tabItems(
    
    ### diet ----
    tabItem(
      tabName = "diet",
      
      fluidRow(
          tabBox(
            title = "Free Text",
            id = "freetext", side = "left", height = "250px",
            textAreaInput("freetextArea", label = tags$p(fa("fas fa-pencil-alt", fill = "red", height = "20px"), "Type in..."), height = "100px")
          ),
          tabBox(
            title = "Remeber Text Box",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "remeberbox", side = "right", height = "250px",
            textAreaInput("rememberArea", label = tags$p(fa("fas fa-pencil-alt", fill = "red", height = "20px"), "Type in..."), height = "100px")
          )
        ),
      
      fluidRow(
        
        tabBox(
          title = "Every Day tips",
          id = "everydaytips", side = "left", height = "250px",
          textAreaInput("tipsArea", label = tags$p(fa("fas fa-pencil-alt", fill = "red", height = "20px"), "Type in..."), height = "100px")
        ),
        tabBox(
          title = "Goal Text Box",
          id = "goalbox", side = "right", height = "250px",
          textAreaInput("goalArea", label = tags$p(fa("fas fa-pencil-alt", fill = "red", height = "20px"), "Type in..."), height = "100px")
        ),
        
      fluidRow(
        
        tabBox(
            title = "Daily targets",
            id = "dailytargets", side = "left", height = "250px",
            textAreaInput("targetsArea", label = tags$p(fa("fas fa-pencil-alt", fill = "red", height = "20px"), "Type in..."), height = "100px")
          )),
      
      fluidRow(
        
        tabBox(
            title = "See User Inputs", id = "tabset1", side = "right", height = "250px",
            DT::dataTableOutput("datatable")
          ),
          tabBox(
            title = "Choose Donwload option", id = "tabset3", side = "right", height = "250px",
            
            column(2, radioButtons(
              inputId = "report_format",
              label = "Format of report",
              choices = c("PDF" = "pdf", "DOCX" = "docx")
            )),
            column(2, textInput(
              inputId = "reportAuthor",
              label = "Author"
            )),
            column(2, dateInput(
              inputId = "reportDate",
              label = "Date",
              value = lubridate::today(),
              format = "dd-mm-yyyy"
            )),
            column(2, textInput(
              inputId = "reportClient",
              label = "Client"
            )),
            tags$hr(),
            br(),
            br(),
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

    ###  athlete ----
    tabItem(
      tabName = "dash",
      fluidRow(
        tabBox(
        selectInput(inputId = "athlete",
                    label = "Athelete Surname",
                    choices = fake_db$surname)
        )),
      ## 1st row infoboxes
      fluidRow(
        infoBox("BMI index",25, icon = icon("heartbeat"), fill = TRUE),
        infoBoxOutput("progressBox"),
        infoBoxOutput("approvalBox")
      ),
      ## 2nd row infoboxes
      fluidRow(
        infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
        infoBoxOutput("progressBox2"),
        infoBoxOutput("approvalBox2")
      ),

      fluidRow(
        tabPanel(
          title = "Athletes table", side  = "left",
          reactableOutput("table")
          )
        )
      ),
    ### form completion ----
    tabItem(
      tabName = "form",
      fluidRow(
        infoBox("bih", 25 ,  icon = icon("heartbeat"))
        )
      ),
    
    ### database ----
    tabItem(
      tabName = "db",
      fluidPage(
        shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(),
        # Application Title
        h1("Explore Yssup records", align = 'center'),
        yssup_table_module_ui("yssup_table")
      )
     
      )
    )
)

## * UI ----
ui <- dashboardPage(

  skin = "red",
  header,
  sidebar,
  body
)


## * Server ----
server <- function(input, output, session) {
  
  ## module call
  session$userData$email <- 'niccolo.salvini27@gmail.com'
  print(getwd())
  
  
  ###  progress bar ----
  observeEvent(input$generate, {
    withProgress(message = "Creating content", value = 0, style = "notification", {
      list( # header
        author = input$reportAuthor,
        date = input$reportDate,
        remember = input$rememberArea,
        goal = input$goalArea,
        freetext = input$freetextArea,
        targets = input$targetsArea,
        tips = input$tipsArea
        
      )
    })
  })
  
  ###  Donwload Button ----
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

  
  ###  Download Report ----
  output$report <- downloadHandler(
    filename = reactive({
      paste0("report.", input$report_format)
    }),
    content = function(file) {
      
      reportPath <- here::here(glue::glue("report{input$report_format}.Rmd"))
      parameters <- list(
        author = input$reportAuthor,
        date = input$reportDate,
        remember = input$rememberArea,
        client = input$reportClient,
        goal = input$goalArea,
        freetext = input$freetextArea,
        targets = input$targetsArea,
        tips = input$tipsArea
        
      )
      rmarkdown::render(reportPath,
        output_file = file,
        params = parameters,
        envir = new.env(parent = globalenv())
      )
    }
  )


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
      Remember = input$goalArea,
      Goal = input$rememberArea,
      author = input$reportAuthor,
      date = input$reportDate,
      client = input$reportClient,
      freetext = input$freetextArea,
      tips = input$tipsArea,
      targets = input$targetsArea,
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
  
  

  # Call the server function portion of the `yssup_table_module.R` module file
  callModule(
    yssup_table_module,
     "yssup_table"
  )

}

shinyApp(ui,server)
