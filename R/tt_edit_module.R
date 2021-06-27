#' tt Add & Edit Module
#'
#' Module to add & edit TT in the Tonico Training database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param tt_to_edit reactive returning a 1 row data frame of the tt client to edit
#' from the "tt" table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
tt_edit_module <- function(input, output, session, modal_title, tt_to_edit, modal_trigger) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- tt_to_edit()
    
    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("client_name"),
              'Client Name',
              value = hold$client_name
            ),
            textInput(
              ns("client_surname"),
              'Client Surname',
              value = hold$client_surname
            ),
            textInput(
              ns("email"),
              'Client email',
              value = hold$email
              )
            ),
          column(
            width = 3,
            numericInput(
              ns('phone_number_prefix'),
              'Phone num prefix',
              value = 800,
              min = 1,
              max = 10000
            ),
            numericInput(
              ns('phone_number'),
              'Phone number',
              value = 3341683142
            ),
            dateInput(
              ns('birthday'),
              "Date of Birth",
              value = lubridate::today(),
              format = "mm/dd/yy"
              )
            ),
          column(
            width = 3,
            textInput(
              ns("city"),
              'Home Town',
              value = hold$city
              ),
            numericInput(
              ns('age'),
              'Current Age',
              value = interval(start = hold$birthday, end = lubridate::today()) / duration(n = 1, unit = "years") %>% floor()
              ),
            selectInput(
              ns('sex'),
              'Client Gender',
              choices = c('F', 'M', 'non-binary'),
              selected = ifelse(is.null(hold), "", hold$sex)
              )
            )
          ),
        
        title = modal_title,
        size = 'm',
        footer = list(
          modalButton('Cancel'),
          actionButton(
            ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )
    
    # Observe event for "Model" text input in Add/Edit tt
    # `shinyFeedback`
    observeEvent(input$model, {
      if (input$model == "") {
        shinyFeedback::showFeedbackDanger(
          "model",
          text = "Must enter what you have donee!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("model")
        shinyjs::enable('submit')
      }
    })
    
  })
  
  
  
  
  
  edit_tt_dat <- reactive({
    hold <- tt_to_edit()
    
    out <- list(
      uid = if (is.null(hold)) NA else hold$uid,
      data = list(
        "client_name" = input$client_name,
        "client_surname" = input$client_surname,
        "email" = input$email,
        "phone_number_prefix" = input$phone_number_prefix,
        "phone_number" = input$phone_number,
        "birthday" = input$birthday,
        "city" = input$city,
        "age" = input$age,
        "sex" = input$sex
      )
    )
    
    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "Europe/London"))
    
    if (is.null(hold)) {
      # adding a new tt
      
      out$data$created_at <- time_now
      out$data$created_by <- session$userData$email
    } else {
      # Editing existing tt
      
      out$data$created_at <- as.character(hold$created_at)
      out$data$created_by <- hold$created_by
    }
    
    out$data$modified_at <- time_now
    out$data$modified_by <- session$userData$email
    
    out
  })
  
  validate_edit <- eventReactive(input$submit, {
    dat <- edit_tt_dat()
    
    # Logic to validate inputs...
    
    dat
  })
  
  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()
    
    tryCatch({
      
      if (is.na(dat$uid)) {
        # creating a new tt
        uid <- uuid::UUIDgenerate()
        
        dbExecute(
          conn,
          "INSERT INTO clients_info (
          uid, 
          client_name, 
          client_surname, 
          email,
          phone_number_prefix, 
          phone_number, 
          birthday, 
          city, 
          age, 
          sex,
          created_at, 
          created_by,
          modified_at,
          modified_by) VALUES
          ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)",
          params = c(
            list(uid),
            unname(dat$data)
          )
        )
      } else {
        # editing an existing tt
        dbExecute(
          conn,
          "UPDATE clients_info SET client_name = $1, client_surname=$2, email=$3, phone_number_prefix=$4, phone_number=$5, birthday=$6, city=$7,
          age=$8, sex=$9, created_at=$10, created_by=$11, modified_at=$12, modified_by=$13 WHERE uid=$14",
          params = c(
            unname(dat$data),
            list(dat$uid)
          )
        )
      }
      
      session$userData$tt_trigger(session$userData$tt_trigger() + 1)
      showToast("success", paste0(modal_title, " Successs"))
    }, error = function(error) {
        
      msg <- paste0(modal_title, " Error")
      
      
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
  })
  
}
