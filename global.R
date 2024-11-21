library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(digest)
library(pool)
library(DT)

# Configuración de la conexión a PostgreSQL
pool <- dbPool(
  drv = Postgres(),
  host = "localhost",
  dbname = "DB_Consultas_CNG",
  user = "postgres",
  password = "inegi",
  port = 5432,
  idleTimeout = 3600,
  minSize = 1,
  maxSize = 5
)

