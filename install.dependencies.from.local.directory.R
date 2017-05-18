####################################################################
##### INSTALL PACKAGES REQUIRED TO RUN THE APPLICATION
####################################################################

mylocaldirectory<-"G:/GRIPE/Umbral/Programa MEM/files/R 3.4.0 x86_64 (2017-05-18)"
setwd(mylocaldirectory)

# Packages from the official R repositories

packages.cran<-list.files("cran", full.names = T)
lapply(packages.cran, function (x) install.packages(x, repos = NULL, type="source", dependencies=F))

# Packages to be compiled from sources at github.

packages.github<-list.files("github", full.names = T)
lapply(packages.github, function(x) unzip(x, exdir="github"))
packages.github.dir<-list.dirs("github", recursive=F)
lapply(packages.github.dir, function(x) devtools::install_local(x, dependencies=F))

# memapp

unzip(paste(mylocaldirectory,"/memapp-master.zip",sep=""), exdir = mylocaldirectory)
file.rename(paste(mylocaldirectory,"/memapp-master",sep=""),paste(mylocaldirectory,"/memapp",sep=""))

