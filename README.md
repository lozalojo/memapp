# The Moving Epidemic Method Shiny Web Application

## Overview

*memapp* is a web application created to serve as a graphical user interface for the R mem package. It was created using Shiny, a web application framework for R.

## Installation

The stable package can be installed from the official R repositories (*CRAN*) using the built-in install function (or from the package manager in some GUIs for R):

```
# install the memapp CRAN version
install.packages("memapp")
```

Or from the official repository:

```
if(!require("devtools")) install.packages("devtools")
library("devtools")
# install the memapp stable version from GitHub
install_github("lozalojo/memapp", ref = "master")
```

To install the development version of *memapp* use the *devtools* package.

```
if(!require("devtools")) install.packages("devtools")
library("devtools")
# install the memapp development version from GitHub
install_github("lozalojo/memapp", ref = "development")
```

When installing this version also you are intalling development versions of some sensitive packages that are used by *memapp* (including the *mem* package).

See installation videos for Windows 10, Ubuntu 16.04.4 and MacOS 10.13.1 High Sierra here:

[Youtube's installation videos playlist](https://www.youtube.com/watch?v=rTIlQGM0qbE&list=PLhUpdbxODE7DizLt2TA-Hgw0cQ5ycM8BB "installation videos")

## Usage

To run the memapp application, just use the function:

```
# run the app:
memapp::runmemapp()
```

You can specify other parameters passed to `shiny::runApp`, such as `display = normal`, or  
`launch.browser = TRUE`.

```
# run the app:
memapp::runmemapp(launch.browser = TRUE)
```

## Notes

In order to use the Surveillance/Animation graph, *magick* package must be installed:

```
install.packages("magick")
```

Along with *ImageMagick*:

[ImageMagick Website](https://imagemagick.org/ "ImageMagick")

To import other formats like SAS, Stata, SPSS, excel, you'll need additional packages:

```
# MS Excel(xls, xlsx)
install.packages("readxl")
# dBase (dbf), SPSS (sav) and Stata (dta)
install.packages("foreign")
# SAS (sas7bdat)
install.packages("haven")
# OpenDocument Spreadsheet (ods)
install.packages("readODS")
# MS Access under Windows (mdb, accdb)
install.packages("RODBC")
```

The installers can be downloaded from their webpages or can be installed directly from R:

```
# check if installr is installed, and install it otherwise
if(!require("installr")) install.packages('installr')
library("installr")
# install ImageMagick
install.ImageMagick()
```

Most data can be exported to csv or excel files, but for the later to work you have to install Rtools toolset and the openxlsx package.

You can download and install Rtools it here:

[Rtools download website](https://cran.r-project.org/bin/windows/Rtools/ "Rtools")

```
# check if installr is installed, and install it otherwise
if(!require("installr")) install.packages('installr')
library("installr")
# install Rtools
install.Rtools()
```

And install openxlsx package:

```
# Install openxlsx package
install.packages("openxlsx")
```

## Technical manual

The technical manual of the application can be downloaded from here:

[Technical manual (stable version)](https://github.com/lozalojo/memapp/blob/assets/technicalmanual.pdf?raw=true "manual stable")

[Technical manual (development version)](https://github.com/lozalojo/memapp/blob/assets/technicalmanualdev.pdf?raw=true "manual development")

## Localization

Starting with version 2.6, memapp enabled an option to localize the app. If your language is not listed in the Languages section and you want to see the app translated please, open the semicolon separated values file at github:

[English language definition](https://github.com/lozalojo/memapp/blob/master/inst/shinyapp/lang/en_GB.txt "en file")

With a text editor, translate the second column to your language and send it to the maintainer.

### Localization credits:

@Spanish, José E. Lozano

@French, Isabel Bergeri

## Useful links

[memapp R package *official webpage*](https://github.com/lozalojo/memapp "official webpage")

[memapp R package *CRAN webpage*](https://cran.r-project.org/package=memapp "CRAN webpage")

[memapp *official server*](http://memapp.iecscyl.com:8080/ "official server")

## References

Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic—can we detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281–3. 

Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir Viruses. 2013 Jul;7(4):546–58. DOI:10.1111/j.1750-2659.2012.00422.x.

Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other Respir Viruses. 2015 Sep;9(5):234–46. DOI:10.1111/irv.12330.

Lozano JE. lozalojo/mem: Second release of the MEM R package. Zenodo [Internet]. [cited 2017 Feb 1]; Available from: [https://zenodo.org/record/165983](https://zenodo.org/record/165983 "https://zenodo.org/record/165983"). [![DOI](https://zenodo.org/badge/47120918.svg)](https://zenodo.org/badge/latestdoi/47120918)

Lozano JE. lozalojo/memapp: Second release of the MEM Shiny Web Application R package. Zenodo [Internet]. [cited 2018 Feb 15]; Available from: [https://zenodo.org/record/1173518](https://zenodo.org/record/1173518 "https://zenodo.org/record/1173518"). [![DOI](https://zenodo.org/badge/90709196.svg)](https://zenodo.org/badge/latestdoi/90709196)



