
server <- function(input, output, session) {
  # Crear un reactiveValues para almacenar el estado
  credentials <- reactiveValues(
    logged_in = FALSE,
    user_role = NULL,
    current_view = "login",
    username = NULL
  )
  
  # Observador para debugging
  observe({
    message("Estado actual: ", 
            "logged_in=", credentials$logged_in, 
            ", role=", credentials$user_role,
            ", user=", credentials$username)
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # UI principal
  output$mainUI <- renderUI({
    # Forzar reactividad
    input$login
    
    if (!credentials$logged_in) {
      if (credentials$current_view == "login") {
        # Pantalla de login
        create_login_ui()
      } else {
        # Pantalla de registro
        create_register_ui()
      }
    } else {
      # Pantalla principal
      create_main_ui(credentials$username, credentials$user_role)
    }
  })
  
  
  #---------------------------------------------------------------------------------------------------------------------
  # MANEJO DEL LOGIN
  observeEvent(input$login, {
    req(input$username, input$password)
    
    message("Intento de login para usuario: ", input$username)
    
    withProgress(message = 'Verificando credenciales...', {
      role <- check_credentials(input$username, input$password)
      
      if (!is.null(role)) {
        message("Login exitoso")
        credentials$logged_in <- TRUE
        credentials$user_role <- role
        credentials$username <- input$username
        showNotification(paste("Bienvenido,", input$username), type = "message")
      } else {
        message("Login fallido")
        showNotification("Credenciales incorrectas", type = "warning")
      }
    })
  })
  
  # Agregar manejo de logout
  observeEvent(input$logout, {
    credentials$logged_in <- FALSE
    credentials$user_role <- NULL
    credentials$username <- NULL
    credentials$current_view <- "login"
    showNotification("Sesión cerrada exitosamente", type = "message")
  })
  
  # Cambiar a vista de registro
  observeEvent(input$show_register, {
    credentials$current_view <- "register"
  })
  
  # Cambiar a vista de login
  observeEvent(input$show_login, {
    credentials$current_view <- "login"
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # MANEJO DEL REGISTRO
  observeEvent(input$register, {
    req(input$reg_name,input$reg_lastname,input$reg_username, input$reg_password, input$reg_password_confirm,
        input$reg_email, input$reg_role)
    
    # Verificar si tiene el dominio 'inegi.org.mx'
    if (grepl("@inegi\\.org\\.mx$",input$reg_email) != TRUE ) {
      showNotification("El correo no pertenece a un dominio INEGI", type = "warning")
      return()
    }
    
    if (input$reg_password != input$reg_password_confirm) {
      showNotification("Las contraseñas no coinciden", type = "warning")
      return()
    }
    
    if (nchar(input$reg_password) < 6) {
      showNotification("La contraseña debe tener al menos 6 caracteres", type = "warning")
      return()
    }
    
    success <- register_user(
      input$reg_name,
      input$reg_lastname,
      input$reg_username,
      input$reg_password,
      input$reg_email,
      input$reg_role
    )
    
    if (success) {
      showNotification("Usuario registrado exitosamente", type = "message")
      credentials$current_view <- "login"
    } else {
      showNotification("Error al registrar usuario. El usuario o email ya existe.", type = "warning")
    }
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # COMBOBOX ENTIDAD
  observe({
    req(credentials$logged_in)
    entidades <- load_data("SELECT nom_ent FROM entidades ORDER BY nom_ent")
    updateSelectInput(session, "entidad", 
                      choices = c("Seleccione una entidad" = "", entidades$nom_ent))
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # COMBOBOX PROGRAMA DEPENDE DE ENTIDAD
  observe({
    req(credentials$logged_in, input$entidad)
    query <- "
      SELECT p.nombre 
      FROM programas p 
      JOIN entidades e ON p.entidad_id = e.id 
      WHERE e.nom_ent = $1
      ORDER BY p.nombre
    "
    programas <- load_data(query, params = list(input$entidad))
    updateSelectInput(session, "programa", 
                      choices = c("Seleccione un programa" = "", programas$nombre))
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # COMBOBOX MODULO DEPENDE DE PROGRAMA
  observe({
    req(input$programa)
    query <- "
      SELECT m.nombre 
      FROM modulos m 
      JOIN programas p ON m.programa_id = p.id 
      WHERE p.nombre = $1
      ORDER BY m.nombre
    "
    modulos <- load_data(query, params = list(input$programa))
    updateSelectInput(session, "modulo", 
                      choices = c("Seleccione un módulo" = "", modulos$nombre))
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # COMBOBOX SECCION DEPENDE DE MODULO
  observe({
    req(input$modulo)
    query <- "
      SELECT s.nombre 
      FROM secciones s 
      JOIN modulos m ON s.modulo_id = m.id 
      WHERE m.nombre = $1
      ORDER BY s.nombre
    "
    secciones <- load_data(query, params = list(input$modulo))
    updateSelectInput(session, "seccion", 
                      choices = c("Seleccione una sección" = "", secciones$nombre))
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # COMBOBOX PREGUNTA DEPENDE DE SECCIONS
  observe({
    req(input$seccion)
    query <- "
      SELECT p.nombre 
      FROM preguntas p 
      JOIN secciones s ON p.seccion_id = s.id 
      WHERE s.nombre = $1
      ORDER BY p.nombre
    "
    preguntas <- load_data(query, params = list(input$seccion))
    updateSelectInput(session, "pregunta", 
                      choices = c("Seleccione una pregunta" = "", preguntas$nombre))
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # CONTADOR DE CARACTERES Y LIMITADOR
  observe({
    # Obtener el texto actual
    desc <- input$descripcion
    
    # Si el texto excede 500 caracteres, cortarlo
    if (!is.null(desc) && nchar(desc) > 500) {
      updateTextAreaInput(session, "descripcion",
                          value = substr(desc, 1, 500))
    }
    
    # Actualizar el contador
    output$char_count <- renderText({
      if (is.null(input$descripcion)) {
        "0/500 caracteres"
      } else {
        paste(nchar(input$descripcion), "/500 caracteres")
      }
    })
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # Observador para debugging
  observe({
    # Manejo seguro de valores NULL
    logged_in <- if (is.null(credentials$logged_in)) FALSE else credentials$logged_in
    role <- if (is.null(credentials$user_role)) "none" else credentials$user_role
    username <- if (is.null(credentials$username)) "none" else credentials$username
    
    message(sprintf("Estado actual: logged_in=%s, role=%s, user=%s",
                    logged_in, role, username))
  })
  
  #---------------------------------------------------------------------------------------------------------------------
  # ENVIO DE INFORMACION DE REGISTRO DE CONSULTA
  observeEvent(input$enviar, {
    req(credentials$logged_in)
    
    # Validar campos obligatorios
    if (!all(
      nzchar(input$entidad),
      nzchar(input$programa),
      nzchar(input$modulo),
      nzchar(input$seccion),
      nzchar(input$pregunta),
      nzchar(input$descripcion)
    )) {
      showNotification(
        "Por favor complete todos los campos obligatorios",
        type = "warning"  # Cambiado de "error" a "warning"
      )
      return()
    }
    
    success <- try({
      # Obtener el ID de la entidad
      query_ent <- "SELECT id FROM entidades WHERE nom_ent = $1"
      result_ent <- load_data(query_ent, params = list(input$entidad))
      if (nrow(result_ent) == 0) {
        showNotification("Entidad no encontrada", type = "warning")
        return(FALSE)
      }
      id_ent <- result_ent$id[1]
      
      # Obtener el ID del programa
      query_prog <- "SELECT id FROM programas WHERE nombre = $1 AND entidad_id = $2"
      result_prog <- load_data(query_prog, params = list(input$programa, id_ent))
      if (nrow(result_prog) == 0) {
        showNotification("Programa no encontrado", type = "warning")
        return(FALSE)
      }
      id_prog <- result_prog$id[1]
      
      # Obtener el ID del módulo
      query_mod <- "SELECT id FROM modulos WHERE nombre = $1 AND programa_id = $2"
      result_mod <- load_data(query_mod, params = list(input$modulo, id_prog))
      if (nrow(result_mod) == 0) {
        showNotification("Módulo no encontrado", type = "warning")
        return(FALSE)
      }
      id_mod <- result_mod$id[1]
      
      # Obtener el ID de la sección
      query_sec <- "SELECT id FROM secciones WHERE nombre = $1 AND modulo_id = $2"
      result_sec <- load_data(query_sec, params = list(input$seccion, id_mod))
      if (nrow(result_sec) == 0) {
        showNotification("Sección no encontrada", type = "warning")
        return(FALSE)
      }
      id_sec <- result_sec$id[1]
      
      # Obtener el ID de la pregunta
      query_preg <- "SELECT id FROM preguntas WHERE nombre = $1 AND seccion_id = $2"
      result_preg <- load_data(query_preg, params = list(input$pregunta, id_sec))
      if (nrow(result_preg) == 0) {
        showNotification("Pregunta no encontrada", type = "warning")
        return(FALSE)
      }
      id_preg <- result_preg$id[1]
      
      # Insertar en la tabla registros_consulta
      query_insert <- "
      INSERT INTO registros_consulta (
        id_ent,
        id_prog,
        id_mod,
        id_sec,
        id_preg,
        descripcion,
        fecha_registro,
        estado,
        usuario_registro
      ) VALUES ($1, $2, $3, $4, $5, $6, CURRENT_TIMESTAMP, 'Pendiente', $7)
    "
      
      # Ejecutar la inserción
      dbExecute(
        pool,
        query_insert,
        params = list(
          id_ent,
          id_prog,
          id_mod,
          id_sec,
          id_preg,
          input$descripcion,
          credentials$username
        )
      )
      
      TRUE
    }, silent = TRUE)
    
    if (isTRUE(success)) {
      showNotification("Consulta registrada exitosamente", type = "message")
      
      # Limpiar los campos del formulario
      updateSelectInput(session, "entidad", selected = "")
      updateSelectInput(session, "programa", selected = "")
      updateSelectInput(session, "modulo", selected = "")
      updateSelectInput(session, "seccion", selected = "")
      updateSelectInput(session, "pregunta", selected = "")
      updateTextAreaInput(session, "descripcion", value = "")
    } else if (!is.logical(success)) {
      # Si es un error de try()
      showNotification("Error al procesar la consulta", type = "warning")
    }
  }, ignoreInit = TRUE)
}