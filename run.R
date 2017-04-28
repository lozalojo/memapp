rm(list = ls(all=TRUE))

jscode <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

####################################################################
##### DEPENDENCIES
##### Libraries required to run the application
####################################################################

# This function check if a set of libraries are present, install them in case they are missing
# and loads them.

check.install.load <- function(i.packages) {
  lapply(i.packages, function (x) if(sum(installed.packages()[, 1]%in%x)==0) install.packages(x))
  lapply(i.packages, require, character.only=TRUE)
  lapply(i.packages,  function(x) paste(x, packageVersion(x)))
}

check.install.load(c("shiny", "shinythemes", "shinydashboard", "shinyjs", "RColorBrewer", "shinyBS",
              "plotly", "ggplot2", "ggthemes", "reshape2", "R.utils", "openxlsx", "XLConnect",
              "stringr", "readr", "magick", "DT", "gplots", "RODBC", "mixtools"))

# Two libraries require to be compiled from sources at github since there are no working binaries at the 
# official repositories. To install them, devtools package is required -> mem and shinysky.

if ("mem" %in% installed.packages()[,"Package"]){
  if (as.numeric(as.character(packageVersion("mem")))<2){
    check.install.load("devtools")
    devtools::install_github("lozalojo/mem")
  }  
}else{
  check.install.load("devtools")
  devtools::install_github("lozalojo/mem")
}
require("mem")
if (!("shinysky" %in% installed.packages()[,"Package"])){
  check.install.load("devtools")
  devtools::install_github("AnalytixWare/ShinySky")
} 
require("shinysky")

# Note: Rstudio is a little bit tricky handling proxy server. If you are connected to the internet
# using a proxy server, maybe you have to force Rstudio to use the proxy when downloading from
# github the source code. To set the proxy manually, you can use the httr package, set it up
# and run the commands that require the proxy with the function with_proxy.
# Example:
# 
# library(httr)
# proxy.config <- use_proxy("proxy.server", port = 80, username = "user", password = "pass", auth = "basic")
# with_config(proxy.config, install_github("lozalojo/mem"))
# with_config(proxy.config, install_github("AnalytixWare/ShinySky"))

####################################################################
##### RUNNING THE APP
##### Running a Shiny app from R
####################################################################

# First set up the working directory to where the files ui.R and server.R are stored in your hard drive
# In this case, if the program is run from Rstudio, you can avoid to set the path at hand by using
# the getActiveDocumentContext function. If this program is run from R, you have to set the path to
# where the files are located.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

col.mine <- c("black","blue","seagreen","red","darkgoldenrod4", "darkmagenta",
              "firebrick", "gold3", "darkslategrey", "lemonchiffon4")
# The palette with black:
# col.mine <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(col.mine)

# To start the application, it search the files at the working directory that we've just set. If you
# want to use the internat R browser, set the option to F.
runApp(launch.browser = T)
