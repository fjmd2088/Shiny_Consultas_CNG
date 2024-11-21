
create_login_ui <- function(){
  card(
    full_screen = TRUE,
    card_header("Iniciar Sesión"),
    textInput("username", "Usuario"),
    passwordInput("password", "Contraseña"),
    actionButton("login", "Ingresar", class = "btn-primary"),
    actionButton("show_register", "Registrarse", class = "btn-link")
  )
}

#-----------------------------------------------------------------------------------------------------------------------
create_register_ui <- function(){
  card(
    full_screen = TRUE,
    card_header("Registro de Usuario"),
    textInput("reg_name","Nombre(s)"),
    textInput("reg_lastname","Apellidos"),
    textInput("reg_username", "Usuario"),
    passwordInput("reg_password", "Contraseña"),
    passwordInput("reg_password_confirm", "Confirmar Contraseña"),
    textInput("reg_email", "Correo Electrónico"),
    selectInput("reg_role", "Rol", 
                choices = c("usuario" = "user" 
                            # "administrador" = "admin"
                            )),
    actionButton("register", "Registrarse", class = "btn-primary"),
    actionButton("show_login", "Volver al Login", class = "btn-link")
  )
}

#-----------------------------------------------------------------------------------------------------------------------
# create_main_ui <- function(username) {
#   page_navbar(
#     title = paste("Bienvenido,", username),
#     # Banner superior con imagen y texto
#     header = card(
#       full_screen = FALSE,
#       height = 170,
#       div(
#         style = css(
#           display = "flex",
#           align_items = "center",
#           padding = "1rem",
#           width = "100%"
#         ),
#         # Contenedor de imagen
#         div(
#           style = css(
#             display = "flex",
#             align_items = "center",
#             justify_content = "center",
#             width = "15%"
#           ),
#           img(src = "img.jpg", 
#               height = "90px", 
#               style = "object-fit: contain;")
#         ),
#         # Contenedor de texto
#         div(
#           style = css(
#             display = "flex",
#             align_items = "center",
#             width = "85%",
#             padding_left = "1rem"
#           ),
#           h3("Sistema de registro, seguimiento y control de consultas conceptuales de los Censos Nacional de Gobierno",
#              style = css(
#                margin = 0,
#                font_weight = "bold"
#              ))
#         )
#         
#       )
#     ),
#     # Panel con texto de prueba
#     card(
#       height = 100,
#       "Bienvenidas y bienvenidos al sistema de registro, seguimiento y control de consultas conceptuales de los Censos Nacional de Gobierno. 
# Para una mejor atención de su consulta, le solicitamos por favor leer detenidamente cada uno de los campos solicitados. 
# Es importante tener presente que solo se puede hacer un registro por cada duda."
#     ),
#     # Contenido principal
#     nav_panel(
#       title = "Consultas",
#       page_sidebar(
#         sidebar = sidebar(
#           width = 300,
#           selectInput("entidad", "Entidad", choices = NULL),
#           selectInput("programa", "Programa", choices = NULL),
#           selectInput("modulo", "Módulo", choices = NULL),
#           selectInput("seccion", "Sección", choices = NULL),
#           selectInput("pregunta", "Pregunta", choices = NULL),
#           hr(),
#           actionButton("logout", "Cerrar Sesión", 
#                        class = "btn-danger w-100")
#         ),
#         card(
#           card_header("Nueva Consulta"),
#           textAreaInput("descripcion", "Descripción de la consulta", 
#                         rows = 5, 
#                         width = "100%",
#                         resize = "vertical"),
#           card_footer(
#             actionButton("enviar", "Enviar Consulta", 
#                          class = "btn-primary w-100")
#           )
#         )
#       )
#     ),
#     nav_panel(
#       title = "Histórico",
#       card(
#         card_header("Consulta de Registros"),
#         tableOutput("tabla_historico")
#       )
#     ),
#     nav_panel(
#       title = "Seguimiento",
#       card(
#         card_header("Consulta de Registros"),
#         tableOutput("tabla_seguimiento")
#       )
#     ),
#     nav_panel(
#       title = "Dashboard",
#       card(
#         card_header("Dashboard"),
#         "Contenido del dashboard"
#       )
#     )
#   )
# }
create_main_ui <- function(username, user_role) {
  
  # Definir los paneles según el rol
  nav_panels <- if (user_role == "user") {
    # Paneles para usuarios normales
    list(
      nav_panel(
        title = "Registro Consulta",
        icon = icon("plus"),
        page_sidebar(
          sidebar = sidebar(
            width = 300,
            selectInput("entidad", "Entidad", choices = NULL),
            selectInput("programa", "Programa", choices = NULL),
            selectInput("modulo", "Módulo", choices = NULL),
            selectInput("seccion", "Sección", choices = NULL),
            selectInput("pregunta", "Pregunta", choices = NULL),
            hr(),
            actionButton("logout", "Cerrar Sesión", 
                         class = "btn-danger w-100")
          ),
          # card(
          #   card_header("Registro de Consulta"),
          #   textAreaInput("descripcion", "Descripción de la consulta", 
          #                 rows = 5, 
          #                 width = "100%",
          #                 resize = "vertical"),
          #   # Agregar div para mostrar el contador
          #   div(
          #     id = "char_counter",
          #     style = "text-align: right; color: #666; font-size: 0.9em;",
          #     textOutput("char_count")
          #   ),
          #   card_footer(
          #     actionButton("enviar", "Enviar Consulta", 
          #                  class = "btn-primary w-100")
          #   )
          # )
          card(
            card_header("Registro de Consulta"),
            textAreaInput("descripcion", "Descripción de la consulta",
                          rows = 5,
                          width = "100%",
                          resize = "vertical"),
            # Agregar div para mostrar el contador
            div(
              id = "char_counter",
              style = "text-align: right; color: #666; font-size: 0.9em;",
              textOutput("char_count")
            ),
            card_footer(
              actionButton("enviar", "Enviar Consulta",
                           class = "btn-primary w-100")
            )
          )
        )
      ),
      nav_panel(
        title = "Histórico",
        icon = icon("history"),
        card(
          card_header("Historial de Consultas"),
          # Aquí va el contenido del histórico para usuarios
          # DTOutput("historico_table")
        )
      )
    )
  } else {
    # Paneles para administradores
    list(
      nav_panel(
        title = "Seguimiento",
        icon = icon("tasks"),
        card(
          card_header("Seguimiento de Consultas"),
          # Aquí va el contenido del seguimiento
          DTOutput("seguimiento_table")
        )
      ),
      nav_panel(
        title = "Dashboard",
        icon = icon("chart-line"),
        card(
          card_header("Dashboard"),
          # Aquí va el contenido del dashboard
          # plotOutput("dashboard_plot")
        )
      ),
      nav_panel(
        title = "Histórico",
        icon = icon("history"),
        card(
          card_header("Historial de Consultas"),
          # Aquí va el contenido del histórico para admin
          # DTOutput("admin_historico_table")
        )
      )
    )
  }
  
  page_navbar(
    title = paste("Bienvenido,", username),
    # Banner superior con imagen y texto
    header = card(
      full_screen = FALSE,
      height = 150,
      div(
        style = css(
          display = "flex",
          align_items = "center",
          padding = "1rem",
          width = "100%"
        ),
        div(
          style = css(
            display = "flex",
            align_items = "center",
            justify_content = "center",
            width = "15%"
          ),
          img(src = "img.jpg", 
              height = "90px", 
              style = "object-fit: contain;")
        ),
        div(
          style = css(
            display = "flex",
            align_items = "center",
            width = "85%",
            padding_left = "1rem"
          ),
          h3("Sistema de registro, seguimiento y control de consultas conceptuales de los Censos Nacional de Gobierno",
             style = css(
               margin = 0,
               font_weight = "bold"
             ))
        )
      )
    ),
    !!!nav_panels,  # Desempaqueta la lista de paneles
    nav_spacer(),
    nav_item(
      actionButton("logout", "Cerrar Sesión", 
                   class = "btn-danger")
    )
  )
}