rm(list = ls(all=TRUE))

jscode <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

testinstall.packages <- function(i.packages) {
  lapply(i.packages, function (x) if(sum(installed.packages()[, 1]%in%x)==0) install.packages(x))
  lapply(i.packages, require, character.only=TRUE)
  lapply(i.packages,  function(x) paste(x, packageVersion(x)))
}

testinstall.packages(c("shiny", "shinythemes", "shinydashboard", "shinyjs", "RColorBrewer", "shinyBS",
              "plotly", "ggplot2", "ggthemes", "reshape2", "R.utils", "openxlsx", "XLConnect",
              "stringr", "readr", "magick", "DT", "stringr", "gplots", "RODBC"))

# Install mem development version

if (packageVersion("mem")!="2.3"){
  testinstall.packages("devtools")
  devtools::install_github("lozalojo/mem")
}
library("mem")

# The palette with black:
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#-----------------------------------------------------------------------------------
#### Paths ####
#-----------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

col.mine <- c("black","blue","seagreen","red","darkgoldenrod4", "darkmagenta",
              "firebrick", "gold3", "darkslategrey", "lemonchiffon4")
palette(col.mine)

## Applikation Shiny
runApp()
