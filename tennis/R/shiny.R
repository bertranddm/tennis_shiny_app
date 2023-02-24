#' Shiny launch
#'
#' Launches Shiny app
#'
#' Based on central matches computes the players match history
#' @export


shiny_tennis <- function() {
  appDir <- system.file("app.R" , package = "tennis")  ;
  shiny::runApp(appDir, display.mode = "normal")
}


