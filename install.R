####################################################################
##### INSTALL PACKAGES REQUIRED TO RUN THE APPLICATION
####################################################################

# Packages from the official R repositories

list.packages<-c("rstudioapi","shiny", "shinythemes", "shinydashboard", "shinyBS", "shinyjs", "RColorBrewer",
              "plotly", "ggthemes", "reshape2", "R.utils", "openxlsx", "XLConnect",
              "stringr", "readr", "magick", "DT", "gplots", "RODBC", "mixtools", "devtools")

lapply(list.packages, function (x) if(sum(installed.packages()[, 1]%in%x)==0) install.packages(x))

# Packages to be compiled from sources at github.

devtools::install_github("lozalojo/mem")
devtools::install_github("AnalytixWare/ShinySky")
devtools::install_github("hadley/ggplot2")

# if you are behind a proxy
# library(httr)
# proxy.config <- use_proxy("proxy.server", port = 80, username = "user", password = "pass", auth = "basic")
# with_config(proxy.config, install_github("lozalojo/mem"))
# with_config(proxy.config, install_github("AnalytixWare/ShinySky"))
# with_config(proxy.config, install_github("hadley/ggplot2"))
# Install from local disk (download from github and unzip)
# devtools::install_local("C:/Downloads/mem-master")
# devtools::install_local("C:/Downloads/ShinySky-master")
# devtools::install_local("C:/Downloads/ggplot2-master")