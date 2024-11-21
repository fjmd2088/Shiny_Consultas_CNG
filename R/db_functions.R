
#------------------------------------------------------------------------------
# check_credentials
check_credentials <- function(username, password) {
  tryCatch({
    # Primero, verifica si el usuario existe
    query_check <- "SELECT COUNT(*) as count FROM usuarios WHERE username = $1"
    result_check <- dbGetQuery(pool, query_check, params = list(username))
    
    if (result_check$count == 0) {
      message("Usuario no encontrado: ", username)
      return(NULL)
    }
    
    # Si el usuario existe, verifica las credenciales
    query <- "SELECT role FROM usuarios WHERE username = $1 AND password = $2"
    result <- dbGetQuery(pool, query, params = list(username, password))
    
    message("Resultado de consulta: ", nrow(result), " filas")
    
    if (nrow(result) > 0) {
      message("Login exitoso para usuario: ", username)
      return(result$role[1])
    }
    message("Contraseña incorrecta para usuario: ", username)
    return(NULL)
  }, error = function(e) {
    message("Error en check_credentials: ", e$message)
    return(NULL)
  })
}

#------------------------------------------------------------------------------
register_user <- function(name, lastname, username, password, email, role) {
  tryCatch({
    query <- "SELECT COUNT(*) as count FROM usuarios WHERE username = $1 OR email = $2"
    result <- dbGetQuery(pool, query, params = list(username, email))
    
    if (result$count > 0) return(FALSE)
    
    hashed_password <- password
    # hashed_password <- digest(password, algo = "sha256")
    query <- "INSERT INTO usuarios (name, lastname, username, password, email, role) VALUES ($1, $2, $3, $4, $5, $6)"
    dbExecute(pool, query, params = list(name, lastname, username, hashed_password, email, role))
    
    return(TRUE)
  }, error = function(e) {
    message("Error en register_user: ", e$message)
    return(FALSE)
  })
}

#------------------------------------------------------------------------------
load_data <- function(query, params = NULL) {
  tryCatch({
    if (is.null(params)) {
      dbGetQuery(pool, query)
    } else {
      dbGetQuery(pool, query, params = params)
    }
  }, error = function(e) {
    message("Error en load_data: ", e$message)
    return(data.frame())
  })
}