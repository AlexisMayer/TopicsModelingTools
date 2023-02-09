#' @title Launch Shiny App
#' 
#' @description Runs the Shiny app located in the "shiny" directory of the package "TopicsModelingTools".
#' 
#' @return Runs the Shiny app in the web browser.
#' 
#' @import shiny
#' 
#' @examples
#' launch_app()
#' 
#' @export
launch_app = function() {
  shiny::runApp(system.file("shiny/", package = "TopicsModelingTools"))
}