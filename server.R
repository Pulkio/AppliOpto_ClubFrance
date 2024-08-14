source("global.R")

server <- function(input, output, session) {
  # Appeler un module avec callModule
  callModule(Visualisation_Server, "Visualisation_module")
  callModule(CMJ_Server, "CMJ_module")
}
