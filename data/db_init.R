library(RSQLite, warn.conflicts = F, quietly = T)
library(tibble, warn.conflicts = F, quietly = T)
library(RPostgres, warn.conflicts = F, quietly = T)
library(DBI, warn.conflicts = F, quietly = T)


db_config <- config::get()$tonicodb


conn <- dbConnect(RPostgres::Postgres(),
                  port = 5432,
                  dbname   = db_config$dbname, 
                  host     = db_config$host, 
                  user     = db_config$username, 
                  password = db_config$password) 

# Create a query to prepare the tonico_training db table with additional 'uid', 'id',
# & the 4 created/modified columns
create_clients_info_query = "CREATE TABLE clients_info (
  uid                             TEXT UNIQUE,
  client_name                     VARCHAR(100),
  client_surname                  VARCHAR(100),
  email                           VARCHAR(100),
  city                            VARCHAR(100),
  phone_number_prefix             NUMERIC,
  phone_number                    NUMERIC,
  sex                             TEXT,
  birthday                        TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  age                             NUMERIC,
  created_at                      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by                      TEXT,
  modified_at                     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_by                     TEXT,
  PRIMARY KEY(uid)
)"

create_clients_measurements_query = "CREATE TABLE clients_measurements (
  measurement_id                  TEXT UNIQUE,
  uid_client                      TEXT,
  form_name                       VARCHAR(100),
  form_surname                    VARCHAR(100),
  measurement_day                 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  height                          NUMERIC,
  weight                          NUMERIC,
  body_fat                        NUMERIC,
  fat                             NUMERIC,
  body_muscle                     NUMERIC,
  muscle                          NUMERIC,
  created_at                      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by                      TEXT,
  modified_at                     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_by                     TEXT,
  PRIMARY KEY(measurement_id),
  CONSTRAINT fk_measurement
    FOREIGN KEY(uid_client) 
      REFERENCES clients_info(uid)
)"



# dbExecute() executes a SQL statement with a connection object
# Drop the table if it already exists
dbExecute(conn, "DROP TABLE IF EXISTS clients_info CASCADE")
dbExecute(conn, "DROP TABLE IF EXISTS clients_measurements CASCADE")
# Execute the query created above
dbExecute(conn, create_clients_info_query)
dbExecute(conn, create_clients_measurements_query)

# Read in the RDS file created in 'data_prep.R'
dat <- readRDS(here::here("data", "prepped", "clients_info.RDS"))

# add uid column to the `dat` data frame
dat$uid <- uuid::UUIDgenerate(n = nrow(dat))

# reorder the columns
# dat <- dat %>%
#   select(uid, everything())


# Fill in the postgresql tables with the values from the RDS file
DBI::dbWriteTable(
  conn,
  name = "clients_info",
  value = dat,
  overwrite = FALSE,
  append = TRUE
)

# List tables to confirm 'clients_info' and 'clients_measuraments' tables exist
dbListTables(conn)

# disconnect from SQLite before continuing
dbDisconnect(conn)

