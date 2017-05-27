#' @export
memapp <- function(...) {
  appDir <- system.file("shinyapp", package = "memapp")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `memapp`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, ...)
}