####################################################################
##### INSTALL PACKAGES REQUIRED TO RUN THE APPLICATION
####################################################################

# Packages from the official R repositories

list.packages<-c("shiny", "shinythemes", "shinydashboard", "shinyBS", "shinyjs", "RColorBrewer",
              "plotly", "ggthemes", "reshape2", "R.utils", "openxlsx", "XLConnect",
              "stringr", "readr", "magick", "DT", "gplots", "RODBC", "mixtools", "devtools")

lapply(list.packages, function (x) if(sum(installed.packages()[, 1]%in%x)==0) install.packages(x))

# Packages to be compiled from sources at github.

devtools::install_github("lozalojo/mem")
devtools::install_github("AnalytixWare/ShinySky")
devtools::install_github("hadley/ggplot2")

