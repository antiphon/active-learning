#' @export
run_dortmund <- function(experiment_dir) {
  options(DORTMUND_EXPERIMENT_DIR = experiment_dir)
  
  shiny::runApp(system.file("dortmund", package = "laal"), 
                port = 8100, launch.browser = FALSE)
}

TUOMAS <-  "~/Dropbox/work/hiit/project3/active_learning_user/experiment"
