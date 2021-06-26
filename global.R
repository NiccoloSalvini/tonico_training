# Library in packages used in this application
library(shiny, quietly = T, warn.conflicts = F)
library(DT, quietly = T, warn.conflicts = F)
library(DBI, quietly = T, warn.conflicts = F)
library(RSQLite, quietly = T, warn.conflicts = F)
library(shinyjs, quietly = T, warn.conflicts = F)
library(shinycssloaders, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(shinyFeedback, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(dbplyr, quietly = T, warn.conflicts = F)
library(config, quietly = T, warn.conflicts = F)


## new shit
# library(processx)
# library(RPostgres)
# library(httr)
# library(dbplyr)
# 
# db_config <- config::get()$db
# 
# download heroku CLI CHECK
# try to move path to heroku cli 
# chiaramente non sente il comando 
# 
# config <- run("heroku", c("config:get", "PG_HRK_HOST", "-a", "tonico-training-db"))

db_config <- config::get()$tonicodb


conn <- dbConnect(RPostgres::Postgres(),
                  port = 5432,
                  dbname   = db_config$dbname, 
                  host     = db_config$host, 
                  user     = db_config$username, 
                  password = db_config$password) 

# # Create database connection
# conn <- dbConnect(
#   RSQLite::SQLite(),
#   dbname = db_config$dbname
# )

# Stop database connection when application stops
shiny::onStop(function() {
  dbDisconnect(conn)
})

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)  
options(spinner.type = 8)
