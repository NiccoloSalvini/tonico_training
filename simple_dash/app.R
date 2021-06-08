## app.R ##



library(shinydashboard)
library(fontawesome)

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
  tabItems(
    tabItem(
      tabName = "diet",
      fluidRow(
        tabBox(
          title = "Remeber Text Box",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", side = "right", height = "250px",
          textAreaInput("remember", label = tags$p(fa("pencil", fill = "red",height = "20px"), "Type in..."), height = "100px")
        ),
        tabBox(
          title = "Goal Text Box",
          id = "tabset1", side = "left", height = "250px",
          textAreaInput("goal", label = tags$p(fa("pencil", fill = "red",height = "20px"), "Type in..."), height = "100px")
        ),
      fluidRow(
        tabBox(
          title = "See User Inputs", id = "tabset1", side = "right", height = "250px",
          DT::dataTableOutput("datatable")
          )
        )
      ),
    ),
    tabItem(
      tabName = "dash",
      
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
        tabBox(
          # Clicking this will increment the progress amount
          box(width = 4, actionButton("count", "Increment progress"))
  
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
}

shinyApp(ui, server)