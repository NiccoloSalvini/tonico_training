#' tt DTedit Module UI
#'
#' UI module element of DTedit output (DT extension)
#'
#' @importFrom shiny NS tagList fluidRow column tags
#' @importFrom DTedit 
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 
tt_dtedit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    fluidRow(
      column(width = 12,
             title = "prova INSERT nutrition plan",
        dteditmodUI(ns('client_eng_calc')) %>% 
          withSpinner(),
        tags$br(),
        tags$br()
      )
    )
    # tags$script(src = "tt_table_module.js"),
    # tags$script(paste0("tt_table_module_js('", ns(''), "')"))
  )
}

my.insert.callback <- function(data, row) {
  # 'data' contains the dataframe *after* the row has been inserted/added
  # 'row' is the row number where data has been inserted
  mydata <<- rbind(mydata, data[row,])
  # in this case, 'mydata' should just be the same as 'data'
  return(mydata)
}

my.update.callback <- function(data, olddata, row) {
  # 'data' contains the dataframe *after* the row has been updated
  # 'row' is the row number where data has been updated
  # 'olddata' is the previous version of the data
  mydata[row,] <<- data[row,]
  # in this case, 'mydata' should just be the same as 'data'
  return(mydata)
}

my.delete.callback <- function(data, row) {
  # 'data' contains the dataframe *before* the row has been deleted
  # 'row' is the row number where data is to be deleted
  mydata <<- mydata[-row,]
  # in this case, 'mydata' should just be the same as data[-c(row),]
  return(mydata)
}


#' tt DTedit Module
#'
#' A modularized version of a DTedit package (DT extension)
#'
#' @importFrom shiny NS tagList fluidRow column tags icon
#' @importFrom DTedit 
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 
tt_dtedit_module  <- function(input, output, session) {
  
  
  data <- reactiveVal() # # 'data' will be a 'reactive' dataframe
  data(data.frame(day = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
                  steps =rep(2000, 7),
                  eng_intake= rep(2400, 7),
                  stringsAsFactors = FALSE))
  
  
  client_eng_calc <- shiny::callModule(
    dteditmod,
    id = 'client_eng_calc',
    thedata = data,
    edit.cols = c('day','steps', 'eng_intake'),
    edit.label.cols = c('Week Day', '# of Steps', 'Daily Energy Intake'),
    view.cols = c('day', 'steps', "eng_intake"),
    show.delete = T,
    show.insert = T,
    show.copy = FALSE,
    icon.delete = shiny::icon("trash"), 
    icon.edit = shiny::icon("edit"), 
    icon.add = shiny::icon("plus"),
    input.types = c(steps      = 'numericInput',
                    day        = 'selectInput',
                    eng_intake = 'numericInput'),
    datatable.rownames = FALSE, # needed for the format*() functions to work
    datatable.call = function(...) {
      DT::datatable(...) %>%
        DT::formatStyle(
          'day',
          color = 'red', backgroundColor = 'orange', fontWeight = 'bold'
        )
      # note, none of this is proper formatting for the mtcars data!
      # but serves to demonstrate the formatting
    },
    # callback.update = my.update.callback,
    # callback.insert = my.insert.callback,
    # callback.delete = my.delete.callback,
    input.choices = list(day = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
  )
  
}
