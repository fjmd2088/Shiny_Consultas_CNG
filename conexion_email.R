library(blastula)

# readRenviron("~/.Renviron")  # Esto carga el archivo si está en el directorio principal


# Sys.getenv("EMAIL_USER")         # Esto debe devolver "francisco.deferia@inegi.org.com.mx"
# Sys.getenv("EMAIL_PASSWORD")      # Esto debe devolver la contraseña "Cim@t206320612" (evita imprimirla si es sensible)


# Credenciales de correo
email_creds <- creds_envvar(
  user = Sys.getenv("EMAIL_USER"),       # Usar variable de entorno para el usuario
  pass_envvar = "EMAIL_PASSWORD",        # Nombre de la variable de entorno con la contraseña
  host = "smtp.office365.com",
  port = 587                             # Especifica el puerto para conexión TLS
)

# Crear el cuerpo del correo
email_body <- compose_email(
  body = md("Este es un mensaje de prueba para verificar la conexión SMTP.")
)

tryCatch({
  smtp_send(
    email = email_body,                      
    from = "francisco.deferia@inegi.org.com.mx",
    to = "francisco.deferia@inegi.org.com.mx",
    subject = "Prueba de conexión",
    credentials = email_creds,
    verbose = TRUE  # Esto proporcionará más detalles sobre la conexión
  )
}, error = function(e) {
  print("Error en la conexión SMTP:")
  print(e$message)
})

# Intentar una conexión de prueba
# tryCatch({
#   smtp_send(
#     email = email_body,                      # Agrega el cuerpo del correo
#     from = "francisco.deferia@inegi.org.com.mx",
#     to = "francisco.deferia@inegi.org.com.mx",
#     subject = "Prueba de conexión",
#     credentials = email_creds
#   )
# }, error = function(e) {
#   print("Error en la conexión SMTP:")
#   print(e$message)
# })
