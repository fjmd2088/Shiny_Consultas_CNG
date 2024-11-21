# Cargar archivos fuente
source("global.R")
source("R/db_functions.R")
source("R/ui_functions.R")
source("ui.R")
source("server.R")

# Iniciar la aplicación
shinyApp(ui = ui, server = server)