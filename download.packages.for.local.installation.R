####################################################################
##### INSTALL PACKAGES REQUIRED TO RUN THE APPLICATION
####################################################################

myversion<-R.Version()
mydate<-Sys.Date()
mylocaldirectory<-"G:/GRIPE/Umbral/Programa MEM"
setwd(mylocaldirectory)
output.dir<-paste("files/R ",version$major,".",version$minor," ",version$arch," (",mydate,")",sep="")
if (!dir.exists("files")) dir.create("files")
if (!dir.exists(output.dir)) dir.create(output.dir)
if (!dir.exists(paste(output.dir,"cran",sep="/"))) dir.create(paste(output.dir,"cran",sep="/"))
if (!dir.exists(paste(output.dir,"github",sep="/"))) dir.create(paste(output.dir,"github",sep="/"))
if (!dir.exists(paste(output.dir,"programs",sep="/"))) dir.create(paste(output.dir,"programs",sep="/"))

# Download R and Rstudio

d.file<-paste(output.dir,"/programs/","R-",version$major,".",version$minor,"-win.exe",sep="")
if (!file.exists(d.file)) download.file(paste("https://cloud.r-project.org/bin/windows/base/R-",version$major,".",version$minor,"-win.exe",sep=""),
              d.file)
d.file<-paste(output.dir,"/programs/RStudio-1.0.143.exe",sep="")
if (!file.exists(d.file)) download.file(paste("https://download1.rstudio.org/RStudio-1.0.143.exe",sep=""),
                                        d.file)

# Packages from the official R repositories

list.packages<-c("shiny", "shinythemes", "shinydashboard", "shinyBS", "shinyjs", "RColorBrewer",
              "plotly", "ggthemes", "reshape2", "R.utils", "openxlsx", "XLConnect",
              "stringr", "readr", "magick", "DT", "gplots", "RODBC", "mixtools", "devtools", "sm", "boot",
              "ggplot2", "RJSONIO", "plyr")

getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}

packages <- getPackages(list.packages)
packages.downloaded<-download.packages(packages, destdir=paste(output.dir,"cran",sep="/"), type="source")

# Packages to be compiled from sources at github.

download.file("https://github.com/AnalytixWare/ShinySky/archive/master.zip", paste(output.dir,"/github/ShinySky-master.zip",sep=""))
download.file("https://github.com/lozalojo/mem/archive/master.zip", paste(output.dir,"/github/mem-master.zip",sep=""))

# memapp

download.file("https://github.com/lozalojo/memapp/archive/master.zip", paste(output.dir,"/programs/memapp-master.zip",sep=""))
