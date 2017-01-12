
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

run <- function() {
  appDir <- system.file("shiny", "elisa_app", package = "elisa")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `elisa`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
