# global.R
library(shiny)
library(DBI)
library(RPostgres)
library(shinyjs)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dplyr)
library(bslib)
library(tidyr)

# -- KONFIGURASI DATABASE --
db_config <- list(
  dbname = "produksi_qc_v1",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "admin123" 
)

# Fungsi untuk koneksi ke database
get_db_conn <- function() {
  tryCatch({
    dbConnect(RPostgres::Postgres(),
              dbname = db_config$dbname,
              host = db_config$host,
              port = db_config$port,
              user = db_config$user,
              password = db_config$password
    )
  }, error = function(e) {
    stop("Gagal terhubung ke database: ", e$message)
  })
}

# Memuat semua file dari folder R
#source("validation.R")
source("auth_module.R")
# source("R/modules/operator_qc_module.R")