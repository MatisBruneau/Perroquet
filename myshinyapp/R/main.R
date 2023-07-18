runApp <- function(){
  appDir <- system.file("app", package = "Perroquet")
  shiny::runApp(appDir, display.mode = "normal")
}
