# memapp
## The Moving Epidemics Method Shiny Web Application

*memapp* is a web application created to serve as a graphical user interface for the R mem library. It was created using Shiny, a web application framework for R. This application uses the development version of the mem R library.

*memapp* requires a set of packages to be installed from the official CRAN repositories:

>list.packages<-c("shiny", "shinythemes", "shinydashboard", "shinyBS", "shinyjs", "RColorBrewer",
>              "plotly", "ggthemes", "reshape2", "R.utils", "openxlsx", "XLConnect",
>              "stringr", "readr", "magick", "DT", "gplots", "RODBC", "mixtools", "devtools")
>
>lapply(list.packages, function (x) if(sum(installed.packages()[, 1]%in%x)==0) install.packages(x))

Also, some packages must be compiled from sources at github:

>devtools::install_github("lozalojo/mem")
>devtools::install_github("AnalytixWare/ShinySky")
>devtools::install_github("hadley/ggplot2")

To run it:

> shiny::runGitHub("lozalojo/memapp", launch.browser = T)

