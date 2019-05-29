#' The Moving Epidemics Method Shiny Web Application
#'
#' Function \code{runmemapp} is used start the memapp Shiny Web Application.\cr
#' memapp is a web application created to serve as a graphical user interface for the R mem library.
#' It was created using Shiny, a web application framework for R. This application uses the development
#' version of the mem R library.\cr
#'
#' Input data is a data frame containing rates that represent historical influenza surveillance
#' data. It can start and end at any given week (tipically at week 40th), and rates can be
#' expressed as per 100,000 inhabitants (or per consultations, if population is not
#' available) or any other scale.\cr
#' Parameters sent to the mem R library are set in the application itself.\cr
#'
#' @name runmemapp
#'
#' @param launch.browser whether if you want to launch the app in an external browser.
#' @param ... other parameters passed to shiny::runApp.
#'
#' @examples
#' \donttest{
#' library("memapp")
#' runmemapp(launch.browser = TRUE)
#' }
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega Alonso, Tomas, Jose E Lozano Alonso, Raul Ortiz de Lejarazu, and Marisol Gutierrez Perez. 2004.
#' Modelling Influenza Epidemic: Can We Detect the Beginning and Predict the Intensity and Duration?
#' International Congress Series, Options for the Control of Influenza V. Proceedings of the International
#' Conference on Options for the Control of Influenza V, 1263 (June): 281-83. doi:10.1016/j.ics.2004.02.121.\cr
#' Vega, Tomas, Jose Eugenio Lozano, Tamara Meerhoff, Rene Snacken, Joshua Mott, Raul Ortiz de Lejarazu, and
#' Baltazar Nunes. 2013. Influenza Surveillance in Europe: Establishing Epidemic Thresholds by the Moving
#' Epidemic Method. Influenza and Other Respiratory Viruses 7 (4): 546-58. doi:10.1111/j.1750-2659.2012.00422.x.\cr
#' Vega, Tomas, Jose E. Lozano, Tamara Meerhoff, Rene Snacken, Julien Beaute, Pernille Jorgensen, Raul Ortiz
#' de Lejarazu, et al. 2015. Influenza Surveillance in Europe: Comparing Intensity Levels Calculated Using
#' the Moving Epidemic Method. Influenza and Other Respiratory Viruses 9 (5): 234-46. doi:10.1111/irv.12330.\cr
#' Lozano, Jose E. mem R package: First version of the MEM R library [Internet]. Valladolid, Spain: Foundation
#' Institute of Health Sciences Studies of Castilla y Leon; 2014. Available from: https://cran.r-project.org/web/packages/mem/index.html\cr
#' Lozano, Jose E. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1];
#' Available from: https://zenodo.org/record/165983
#'
#' @keywords influenza
#'
#' @export
#' @importFrom shiny runApp
runmemapp <- function(launch.browser = TRUE, ...) {
  appDir <- system.file("shinyapp", package = "memapp")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `memapp`.", call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = launch.browser, ...)
}
