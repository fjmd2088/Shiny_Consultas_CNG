
ui <- function(request) {
  page_fluid(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    title = "Sistema de Consultas CNG",
    
    uiOutput("mainUI")
  )
}