rm(list = ls(all=TRUE))
jscode <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"
ladda.paket <- function(vector.paket) {
  lapply(vector.paket, 
         function (x) if(sum(installed.packages()[, 1]%in%x)==0) install.packages(x))
  lapply(vector.paket, 
         require, 
         character.only=TRUE)
  lapply(vector.paket,  function(x) paste(x,packageVersion(x)))
}
ladda.paket(c("shiny",
              "mem",
              "shinythemes", 
              "shinydashboard",
              "shinyjs",
              "RColorBrewer",
              "shinyBS",
              "plotly",
              "ggplot2",
              "ggthemes",
              "reshape2",
              "R.utils",
              "openxlsx",
              "XLConnect",
              "stringr",
              "readr",
              "magick",
              "DT",
              "stringr",
              "gplots",
              "RODBC"))

# The palette with black:
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#-----------------------------------------------------------------------------------
#### Paths ####
#-----------------------------------------------------------------------------------
script.path <- "."

col.mine <- c("black","blue","seagreen","red","darkgoldenrod4", "darkmagenta",
              "firebrick", "gold3", "darkslategrey", "lemonchiffon4")
palette(col.mine)

## Applikation Shiny
runApp(file.path(script.path))


