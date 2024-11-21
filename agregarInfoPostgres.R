library(DBI)
library(RPostgres)
library(readr)

# Paso 1: Conectar a la base de datos PostgreSQL
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "DB_Consultas_CNG", # Cambiar por el nombre de tu base de datos
  host = "localhost",          # Cambiar si el host es diferente
  port = 5432,                 # Puerto de PostgreSQL (por defecto 5432)
  user = "postgres",         # Cambiar por tu usuario
  password = "inegi"   # Cambiar por tu contraseña
)

#-----------------------------------------------------------------------------------------------------------------------
# ENTIDAD
archivo_csv <- "documentacion/tc_entidad.csv" # Cambiar por la ruta de tu archivo
datos <- read_csv(archivo_csv)

# Paso 3: Insertar los datos en la tabla PostgreSQL
dbWriteTable(
  con,
  name = "entidades",
  value = datos,
  append = TRUE,  # Agregar datos a la tabla existente
  row.names = FALSE
)

#-----------------------------------------------------------------------------------------------------------------------
# PROGRAMA
archivo_csv <- "documentacion/tc_programas.csv" # Cambiar por la ruta de tu archivo
datos <- read_csv(archivo_csv)

# Paso 3: Insertar los datos en la tabla PostgreSQL
dbWriteTable(
  con,
  name = "programas",
  value = datos,
  append = TRUE,  # Agregar datos a la tabla existente
  row.names = FALSE
)
#-----------------------------------------------------------------------------------------------------------------------
# MODULOS
archivo_csv <- "documentacion/tc_modulos.csv" # Cambiar por la ruta de tu archivo
datos <- read_csv(archivo_csv)

# Paso 3: Insertar los datos en la tabla PostgreSQL
dbWriteTable(
  con,
  name = "modulos",
  value = datos,
  append = TRUE,  # Agregar datos a la tabla existente
  row.names = FALSE
)
#-----------------------------------------------------------------------------------------------------------------------
# SECCIONES
archivo_csv <- "documentacion/tc_secciones.csv" # Cambiar por la ruta de tu archivo
datos <- read_csv(archivo_csv)

# Paso 3: Insertar los datos en la tabla PostgreSQL
dbWriteTable(
  con,
  name = "secciones",
  value = datos,
  append = TRUE,  # Agregar datos a la tabla existente
  row.names = FALSE
)
#-----------------------------------------------------------------------------------------------------------------------
# PREGUNTAS
archivo_csv <- "documentacion/tc_preguntas.csv" # Cambiar por la ruta de tu archivo
datos <- read_csv(archivo_csv)

# Paso 3: Insertar los datos en la tabla PostgreSQL
dbWriteTable(
  con,
  name = "preguntas",
  value = datos,
  append = TRUE,  # Agregar datos a la tabla existente
  row.names = FALSE
)
#-----------------------------------------------------------------------------------------------------------------------
# Paso 4: Verificar que los datos se hayan insertado
# consulta <- dbGetQuery(con, "SELECT * FROM entidades LIMIT 10;")
# print(consulta)

# Paso 5: Cerrar la conexión
dbDisconnect(con)