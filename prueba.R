library(shiny)
library(bslib)
library(DBI)
library(RSQLite)

# Función para verificar credenciales (simulada)
check_credentials <- function(username, password) {
  # Simulación de usuarios y sus roles
  users <- list(
    "usuario_ce" = list(password = "ce123", role = "CE"),
    "usuario_oc" = list(password = "oc123", role = "OC"),
    "admin" = list(password = "admin123", role = "Administrador")
  )
  
  if (!is.null(users[[username]]) && users[[username]]$password == password) {
    return(users[[username]]$role)
  }
  return(NULL)
}

ui <- function(request) {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    
    # UI condicional basada en el estado de login
    uiOutput("mainUI")
  )
}

server <- function(input, output, session) {
  # Estado de la sesión
  credentials <- reactiveValues(
    logged_in = FALSE,
    user_role = NULL
  )
  
  # UI principal
  output$mainUI <- renderUI({
    if (!credentials$logged_in) {
      # Pantalla de login
      page_fillable(
        div(
          style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
          card(
            width = "400px",
            card_header(
              "Inicio de Sesión",
              class = "text-center"
            ),
            padding = 4,
            textInput("username", "Usuario"),
            passwordInput("password", "Contraseña"),
            div(
              style = "text-align: center; margin-top: 20px;",
              actionButton("login", "Iniciar Sesión", class = "btn-primary w-100")
            )
          )
        )
      )
    } else {
      # UI después del login
      page_fillable(
        # Barra superior con botón de logout
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            height = 260,
            div(
              style = css(
                display = "flex",
                justify_content = "space-between",
                align_items = "center",
                padding = "10px",
                width = "100%"
              ),
              div(
                style = css(display = "flex", align_items = "center", width = "90%"),
                div(
                  style = css(width = "15%"),
                  img(src = "img.jpg", height = "90px", style = "object-fit: contain;")
                ),
                div(
                  style = css(
                    font_size = "22px",
                    font_weight = "bold",
                    width = "85%"
                  ),
                  "Sistema de registro, seguimiento y control de consultas conceptuales de los Censos Nacional de Gobierno"
                )
              ),
              div(
                style = css(width = "10%", text_align = "right"),
                actionButton("logout", "Cerrar Sesión", class = "btn-danger")
              )
            )
          )
        ),
        
        card(
          height = 170,
          "Bienvenidas y bienvenidos al sistema de registro, seguimiento y control de consultas conceptuales de los Censos Nacional de Gobierno."
        ),
        
        # Pestañas según el rol
        navset_card_tab(
          id = "main_tabs",
          
          if (credentials$user_role %in% c("CE", "Administrador")) {
            nav_panel(
              title = "Registro consultas",
              card(
                card_header("Formulario de Registro de consultas"),
                div(
                  style = "padding: 20px;",
                  layout_column_wrap(
                    width = 1/2,
                    style = css(gap = "20px"),
                    div(
                      style = "width: 100%;",
                      selectInput("entidad", "Entidad Federativa *", choices = NULL)
                    ),
                    div(
                      style = "width: 100%;",
                      selectInput("programa", "Programa *", choices = NULL)
                    )
                  ),
                  div(
                    style = "margin-bottom: 20px;",
                    layout_column_wrap(
                      width = 1/2,
                      style = css(gap = "20px"),
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
                  selectInput("pregunta", "Pregunta *", choices = NULL),
                  textAreaInput("descripcion", "Descripción de la duda *", height = "120px"),
                  fileInput("archivo", "Adjuntar archivo Excel (opcional)", 
                            accept = c(".xlsx", ".xls")),
                  textInput("email_destino", "Correo electrónico destino *", ""),
                  layout_column_wrap(
                    width = 1/2,
                    style = css(gap = "20px"),
                    actionButton("enviar", "Enviar", class = "btn-primary w-100"),
                    actionButton("limpiar", "Limpiar", class = "btn-secondary w-100")
                  )
                )
              )
            )
          },
          
          if (credentials$user_role %in% c("CE", "OC", "Administrador")) {
            nav_panel(
              title = "Histórico",
              card(
                card_header("Consulta de Registros"),
                tableOutput("tabla_registros")
              )
            )
          },
          
          if (credentials$user_role %in% c("OC", "Administrador")) {
            nav_panel(
              title = "Seguimiento",
              card(
                card_header("Seguimiento de Consultas"),
                "Contenido de seguimiento"
              )
            )
          },
          
          if (credentials$user_role %in% c("OC", "Administrador")) {
            nav_panel(
              title = "Dashboard",
              card(
                card_header("Dashboard"),
                "Contenido del dashboard"
              )
            )
          }
        )
      )
    }
  })
  
  # Manejo del login
  observeEvent(input$login, {
    role <- check_credentials(input$username, input$password)
    if (!is.null(role)) {
      credentials$logged_in <- TRUE
      credentials$user_role <- role
      showNotification(paste("Bienvenido,", input$username), type = "message")
    } else {
      showNotification("Credenciales incorrectas", type = "error")
    }
  })
  
  # Manejo del logout
  observeEvent(input$logout, {
    credentials$logged_in <- FALSE
    credentials$user_role <- NULL
    showNotification("Sesión cerrada", type = "message")
  })
  
  # Conexión a la base de datos
  con <- dbConnect(RSQLite::SQLite(), "registros.db")
  
  # Observadores para los inputs
  observe({
    req(credentials$logged_in)
    updateSelectInput(session, "entidad", 
                      choices = c("", dbGetQuery(con, "SELECT nombre FROM entidades")$nombre))
  })
  
  observe({
    req(credentials$logged_in, input$entidad)
    query <- "SELECT p.nombre 
              FROM programas p 
              JOIN entidades e ON p.entidad_id = e.id 
              WHERE e.nombre = ?"
    programas <- dbGetQuery(con, query, params = list(input$entidad))$nombre
    updateSelectInput(session, "programa", choices = c("", programas))
  })
  
  # Resto de los observadores...
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
  
  # Cerrar conexión al finalizar
  onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui = ui, server = server)