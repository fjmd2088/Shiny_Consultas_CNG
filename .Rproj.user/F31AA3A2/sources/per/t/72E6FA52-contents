library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(blastula)
library(fs)

# [Código de initialize_db() se mantiene igual hasta la configuración del correo]

# Configuración del correo electrónico para Outlook
# email_creds <- creds(
#   user = "francisco.deferia@inegi.org.com.mx", 
#   password = "Cim@t206320612",  # Cambia aquí por la contraseña, o usa creds_env_key() para mayor seguridad
#   host = "smtp.office365.com",
#   port = 587,
#   use_ssl = FALSE,  # Cambia SSL a FALSE
#   use_tls = TRUE    # Activa TLS
# )

# Función para inicializar la base de datos
# initialize_db <- function() {
#   con <- dbConnect(RSQLite::SQLite(), "registros.db")
#   
#   # Crear tablas con relaciones
#   dbExecute(con, "CREATE TABLE IF NOT EXISTS entidades (id INTEGER PRIMARY KEY, nombre TEXT)")
#   dbExecute(con, "CREATE TABLE IF NOT EXISTS programas (
#     id INTEGER PRIMARY KEY, 
#     nombre TEXT,
#     entidad_id INTEGER,
#     FOREIGN KEY(entidad_id) REFERENCES entidades(id)
#   )")
#   dbExecute(con, "CREATE TABLE IF NOT EXISTS modulos (
#     id INTEGER PRIMARY KEY, 
#     nombre TEXT,
#     programa_id INTEGER,
#     FOREIGN KEY(programa_id) REFERENCES programas(id)
#   )")
#   dbExecute(con, "CREATE TABLE IF NOT EXISTS secciones (
#     id INTEGER PRIMARY KEY, 
#     nombre TEXT,
#     modulo_id INTEGER,
#     FOREIGN KEY(modulo_id) REFERENCES modulos(id)
#   )")
#   dbExecute(con, "CREATE TABLE IF NOT EXISTS preguntas (
#     id INTEGER PRIMARY KEY, 
#     nombre TEXT,
#     seccion_id INTEGER,
#     FOREIGN KEY(seccion_id) REFERENCES secciones(id)
#   )")
#   dbExecute(con, "CREATE TABLE IF NOT EXISTS registros (
#     id INTEGER PRIMARY KEY,
#     entidad TEXT,
#     programa TEXT,
#     modulo TEXT,
#     seccion TEXT,
#     pregunta TEXT,
#     descripcion TEXT,
#     archivo TEXT,
#     fecha TIMESTAMP DEFAULT CURRENT_TIMESTAMP
#   )")
#   
#   # Insertar datos de ejemplo si las tablas están vacías
#   if (dbGetQuery(con, "SELECT COUNT(*) FROM entidades")[1,1] == 0) {
#     dbExecute(con, "INSERT INTO entidades (id, nombre) VALUES 
#       (1, 'Ciudad de México'),
#       (2, 'Estado de México'),
#       (3, 'Jalisco')")
#     
#     dbExecute(con, "INSERT INTO programas (id, nombre, entidad_id) VALUES 
#       (1, 'Programa 1 CDMX', 1),
#       (2, 'Programa 2 CDMX', 1),
#       (3, 'Programa 1 EdoMex', 2),
#       (4, 'Programa 2 EdoMex', 2),
#       (5, 'Programa 1 Jalisco', 3)")
#     
#     dbExecute(con, "INSERT INTO modulos (id, nombre, programa_id) VALUES 
#       (1, 'Módulo A P1-CDMX', 1),
#       (2, 'Módulo B P1-CDMX', 1),
#       (3, 'Módulo A P2-CDMX', 2),
#       (4, 'Módulo A P1-EdoMex', 3)")
#     
#     dbExecute(con, "INSERT INTO secciones (id, nombre, modulo_id) VALUES 
#       (1, 'Sección 1 MA-P1-CDMX', 1),
#       (2, 'Sección 2 MA-P1-CDMX', 1),
#       (3, 'Sección 1 MB-P1-CDMX', 2)")
#     
#     dbExecute(con, "INSERT INTO preguntas (id, nombre, seccion_id) VALUES 
#       (1, 'Pregunta 1 S1-MA-P1-CDMX', 1),
#       (2, 'Pregunta 2 S1-MA-P1-CDMX', 1),
#       (3, 'Pregunta 1 S2-MA-P1-CDMX', 2)")
#   }
#   
#   dbDisconnect(con)
# }

# Inicializar base de datos
# initialize_db()

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Panel superior con imagen y texto
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      height = 260,
      
      # Contenedor principal que alinea en horizontal usando flex
      div(
        style = css(display = "flex", align_items = "center", padding = "0", width = "100%"),
        
        # Columna de la imagen (1/3 del ancho)
        div(
          style = css(display = "flex", align_items = "center", justify_content = "center", width = "15%"),
          img(src = "img.jpg", height = "90px", style = "object-fit: contain;")
        ),
        
        # Columna del texto (2/3 del ancho)
        div(
          style = css(display = "flex", align_items = "center", font_size = "22px", font_weight = "bold", width = "85%"),
          "Sistema de registro, seguimiento y control de consultas conceptuales de los Censos Nacional de Gobierno"
        )
      )
    )
  ),
  
  # Panel con texto de prueba
  card(
    height = 170,
    "Bienvenidas y bienvenidos al sistema de registro, seguimiento y control de consultas conceptuales de los Censos Nacional de Gobierno. 
Para una mejor atención de su consulta, le solicitamos por favor leer detenidamente cada uno de los campos solicitados. 
Es importante tener presente que solo se puede hacer un registro por cada duda."
  ),

# Barra de navegación
navset_card_tab(
  nav_panel(
    title = "Registro consultas",
    card(
      card_header("Formulario de Registro de consultas"),
      
      # Contenedor con padding para los elementos del formulario
      div(
        style = "padding: 20px;",
        
        # Primera fila de selectInputs con margen inferior
        div(
          style = "margin-bottom: 20px;",
          layout_column_wrap(
            width = 1/2,
            style = css(gap = "20px"),  # Espacio entre columnas
            div(
              style = "width: 100%;",
              selectInput("entidad", "Entidad Federativa *", choices = NULL)
            ),
            div(
              style = "width: 100%;",
              selectInput("programa", "Programa *", choices = NULL)
            )
          )
        ),
        
        # Segunda fila de selectInputs con margen inferior
        div(
          style = "margin-bottom: 20px;",
          layout_column_wrap(
            width = 1/2,
            style = css(gap = "20px"),  # Espacio entre columnas
            div(
              style = "width: 100%;",
              selectInput("modulo", "Módulo *", choices = NULL)
            ),
            div(
              style = "width: 100%;",
              selectInput("seccion", "Sección *", choices = NULL)
            )
          )
        ),
        
        # Pregunta con margen inferior
        div(
          style = "margin-bottom: 20px;",
          selectInput("pregunta", "Pregunta *", choices = NULL)
        ),
        
        # Área de descripción con margen inferior
        div(
          style = "margin-bottom: 20px; position: relative;",
          textAreaInput("descripcion", "Descripción de la duda *", 
                        height = "120px", resize = "vertical"),
          div(
            id = "char_count",
            style = "position: absolute; bottom: 5px; right: 10px; 
                      background: rgba(255,255,255,0.8); padding: 2px 5px;
                      border-radius: 3px; font-size: 0.8em; color: #666;",
            textOutput("char_counter")
          )
        ),
        
        # File input con margen inferior
        div(
          style = "margin-bottom: 20px;",
          fileInput("archivo", "Adjuntar archivo Excel (opcional)",
                    accept = c(".xlsx", ".xls"))
        ),
        
        # Email con margen inferior
        div(
          style = "margin-bottom: 20px;",
          textInput("email_destino", "Correo electrónico destino *", "")
        ),
        
        # Botones
        layout_column_wrap(
          width = 1/2,
          style = css(gap = "20px"),  # Espacio entre botones
          actionButton("enviar", "Enviar", class = "btn-primary w-100"),
          actionButton("limpiar", "Limpiar", class = "btn-secondary w-100")
        ),
        
        div(
          style = "margin-top: 20px;",
          helpText("* Campos obligatorios")
        )
      )
    )
  ),
  
  nav_panel(
    title = "Histórico",
    card(
      card_header("Consulta de Registros"),
      tableOutput("tabla_registros")
    )
  ),
  
  nav_panel(
    title = "Dashboard",
    card(
      card_header("Dashboard"),
      "Contenido del dashboard"
    )
  )
)
)

server <- function(input, output, session) {
  # [El código del servidor se mantiene igual]
  
  # Conexión a la base de datos
  con <- dbConnect(RSQLite::SQLite(), "registros.db")
  
  # Cargar entidades inicialmente
  observe({
    updateSelectInput(session, "entidad", 
                      choices = c("", dbGetQuery(con, "SELECT nombre FROM entidades")$nombre))
  })
  
  # Actualizar programas basado en entidad
  observe({
    req(input$entidad)
    query <- "SELECT p.nombre 
              FROM programas p 
              JOIN entidades e ON p.entidad_id = e.id 
              WHERE e.nombre = ?"
    programas <- dbGetQuery(con, query, params = list(input$entidad))$nombre
    updateSelectInput(session, "programa", choices = c("", programas))
  })
  
  # Actualizar módulos basado en programa
  observe({
    req(input$programa)
    query <- "SELECT m.nombre 
              FROM modulos m 
              JOIN programas p ON m.programa_id = p.id 
              WHERE p.nombre = ?"
    modulos <- dbGetQuery(con, query, params = list(input$programa))$nombre
    updateSelectInput(session, "modulo", choices = c("", modulos))
  })
  
  # Actualizar secciones basado en módulo
  observe({
    req(input$modulo)
    query <- "SELECT s.nombre 
              FROM secciones s 
              JOIN modulos m ON s.modulo_id = m.id 
              WHERE m.nombre = ?"
    secciones <- dbGetQuery(con, query, params = list(input$modulo))$nombre
    updateSelectInput(session, "seccion", choices = c("", secciones))
  })
  
  # Actualizar preguntas basado en sección
  observe({
    req(input$seccion)
    query <- "SELECT p.nombre 
              FROM preguntas p 
              JOIN secciones s ON p.seccion_id = s.id 
              WHERE s.nombre = ?"
    preguntas <- dbGetQuery(con, query, params = list(input$seccion))$nombre
    updateSelectInput(session, "pregunta", choices = c("", preguntas))
  })
  
  # Función para enviar registro y correo
  observeEvent(input$enviar, {
    # Validar campos obligatorios
    if (!all(
      nzchar(input$entidad),
      nzchar(input$programa),
      nzchar(input$modulo),
      nzchar(input$seccion),
      nzchar(input$pregunta),
      nzchar(input$descripcion),
      nzchar(input$email_destino)
    )) {
      showNotification("Por favor complete todos los campos obligatorios", type = "error")
      return()
    }
  })
}

shinyApp(ui, server)
