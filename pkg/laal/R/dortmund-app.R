
#' @export
run_dortmund <- function() {
  shiny::runApp(system.file("dortmund", package = "laal"), port = 8100)
}



