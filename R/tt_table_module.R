#' tt Table Module UI
#'
#' The UI portion of the module for displaying the tt datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
tt_table_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 2,
        actionButton(
          ns("add_tt"),
          "Add New Client",
          class = "btn-success",
          style = "color: #fff;",
          icon = icon('plus'),
          width = '100%'
        ),
        tags$br(),
        tags$br()
      )
    ),
    fluidRow(
      column(
        width = 12,
        title = "Motor Trend Car Road Tests",
        DTOutput(ns('tt_table')) %>%
          withSpinner(),
        tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "tt_table_module.js"),
    tags$script(paste0("tt_table_module_js('", ns(''), "')"))
  )
}

#' tt Table Module Server
#'
#' The Server portion of the module for displaying the tt datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

tt_table_module <- function(input, output, session) {
  
  # trigger to reload data from the "tt" table
  session$userData$tt_trigger <- reactiveVal(0)
  
  # Read in "tt" table from the database
  tt <- reactive({
    session$userData$tt_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('clients_info') %>%
        collect() %>%
        mutate(
          created_at = as.POSIXct(created_at, tz = "Europe/London"),
          modified_at = as.POSIXct(modified_at, tz = "Europe/London")
        ) %>%
        arrange(desc(modified_at))
    }, error = function(err) {
      
      
      msg <- "Tonico Training DB Connection Error"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    out
  })
  
  
  tt_table_prep <- reactiveVal(NULL)
  
  observeEvent(tt(), {
    out <- tt()
    
    ids <- out$uid
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })
    
    # Remove the `uid` column. We don't want to show this column to the user
    out <- out %>%
      select(-uid)
    
    # Set the Action Buttons row to the first column of the `tt` table
    out <- cbind(
      tibble(" " = actions),
      out
    )
    
    if (is.null(tt_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      tt_table_prep(out)
      
    } else {
      
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(tt_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
      
    }
  })
  
  output$tt_table <- renderDT({
    req(tt_table_prep())
    out <- tt_table_prep()
    
    datatable(
      out,
      rownames = FALSE,
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      extensions = c("Buttons"),
      options = list(
        "pageLength" = 20,
        scrollX = TRUE,
        dom = 'Bftip',
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("tt-", Sys.Date()),
            exportOptions = list(
              columns = 1:(length(out) - 1)
            )
          )
        ),
        columnDefs = list(
          list(targets = 0, orderable = FALSE)
        ),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }")
      )
    ) %>%
      formatDate(
        columns = c("created_at", "modified_at"),
        method = 'toLocaleString',
        params = list("se", list(timeZone = "Europe/London")) 
      )
    
  })
  
  tt_table_proxy <- DT::dataTableProxy('tt_table')
  
  callModule(
    tt_edit_module,
    "add_tt",
    modal_title = "Add Client to Tonico Training DB",
    tt_to_edit = function() NULL,
    modal_trigger = reactive({input$add_tt})
  )
  
  tt_to_edit <- eventReactive(input$tt_id_to_edit, {
    
    tt() %>%
      filter(uid == input$tt_id_to_edit)
  })
  
  callModule(
    tt_edit_module,
    "edit_tt",
    modal_title = "Edit Client to Tonico Training DB",
    tt_to_edit = tt_to_edit,
    modal_trigger = reactive({input$tt_id_to_edit})
  )
  
  tt_to_delete <- eventReactive(input$tt_id_to_delete, {
    
    out <- tt() %>%
      filter(uid == input$tt_id_to_delete) %>%
      as.list()
  })
  
  callModule(
    tt_delete_module,
    "delete_tt",
    modal_title = "Delete Client from Tonico Training DB",
    tt_to_delete = tt_to_delete,
    modal_trigger = reactive({input$tt_id_to_delete})
  )
  
}