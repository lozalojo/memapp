# The Moving Epidemic Method Shiny Web Application

[![Travis Build Status](https://travis-ci.org/lozalojo/memapp.svg?branch=master)](https://travis-ci.org/lozalojo/memapp)
[![Coverage Status](https://img.shields.io/codecov/c/github/lozalojo/memapp/master.svg)](https://codecov.io/github/lozalojo/memapp?branch=master)
[![DOI](https://zenodo.org/badge/90709196.svg)](https://zenodo.org/badge/latestdoi/90709196)
[![monthly](http://cranlogs.r-pkg.org/badges/memapp)](https://www.rpackages.io/package/memapp) 
[![total](http://cranlogs.r-pkg.org/badges/grand-total/memapp)](https://www.rpackages.io/package/memapp)
[![CRAN](http://www.r-pkg.org/badges/version/memapp?color=009999)](https://cran.r-project.org/package=memapp)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

## Overview

*memapp* is a web application created to serve as a graphical user interface for the R mem package. It was created using Shiny, a web application framework for R.

## Installation

The stable package can be installed from the official R repositories (*CRAN*) using the built-in install function (or from the package manager in some GUIs for R):

```
# install the memapp CRAN version
install.packages("memapp")
```

To install the development version of *memapp* use the *devtools* package.

```
if(!require("devtools")) install.packages("devtools")
library("devtools")
# install the memapp development version from GitHub
install_github("lozalojo/memapp")
```
When installing this version also you are intalling development versions of some sensitive packages that are used by *memapp* (including the *mem* package).

See installation videos for Windows 10, Ubuntu 16.04.4 and MacOS 10.13.1 High Sierra here:

```
https://www.youtube.com/watch?v=rTIlQGM0qbE&list=PLhUpdbxODE7DizLt2TA-Hgw0cQ5ycM8BB
```

## Usage

To run the memapp application, just use the function:

```
# run the app:
memapp::runmemapp()
```

You can specify other parameters passed to `shiny::runApp`, such as `display = normal`, or  
`launch.browser = TRUE`.

## Notes

In order to use the Surveillance/Animation graph, *magick* package must be installed:

```
install.packages("magick")
```

Or alternatively, for low specs machines (recommended for most users), the *animation* package:

```
install.packages("animation")
```

Along with one of the following programs: *GraphicsMagick* or *ImageMagick*:

www.imagemagick.org
www.graphicsmagick.org

The installers can be downloaded from their webpages or can be installed directly from R:

```
# check if installr is installed, and install it otherwise
if(!require("installr")) install.packages('installr')
library("installr")
# install GraphicsMagic
install.GraphicsMagick()
# install ImageMagick
install.ImageMagick()
```

Most data can be exported to csv or excel files, but for the later to work you have to install Rtools. You can download and install it here:

https://cran.r-project.org/bin/windows/Rtools/

```
# check if installr is installed, and install it otherwise
if(!require("installr")) install.packages('installr')
library("installr")
# install Rtools
install.Rtools()

```

## Localization

Starting with version 2.6, memapp enabled an option to localize the app. If your language is not listed in the Languages section and you want to see the app translated please, open the semicolon separated values file at github:

```
https://github.com/lozalojo/memapp/blob/master/inst/shinyapp/lang/en-GB.txt
```

With a text editor, translate the second column to your language and send it to the maintainer.

### Localization credits:

@Spanish, José E. Lozano

@French, Isabel Bergeri

## Useful links

memapp R package *official webpage*

```
https://github.com/lozalojo/memapp
```

memapp R package *CRAN webpage*

```
https://cran.r-project.org/package=memapp
```

memapp *official server*

```
www.memapp.me
```

## Technical manual

At the following google drive:

```
https://drive.google.com/file/d/0B0IUo_0NhTOoX29zc2p5RmlBUWc/view?usp=sharing
```

## References

Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic—can we detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281–3. 

Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir Viruses. 2013 Jul;7(4):546–58. DOI:10.1111/j.1750-2659.2012.00422.x.

Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other Respir Viruses. 2015 Sep;9(5):234–46. DOI:10.1111/irv.12330.

Lozano JE. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1]; Available from: https://zenodo.org/record/165983. [![DOI](https://zenodo.org/badge/47120918.svg)](https://zenodo.org/badge/latestdoi/47120918)

Lozano JE. lozalojo/memapp: Second release of the MEM Shiny Web Application R package. Zenodo [Internet]. [cited 2018 Feb 15]; Available from: https://zenodo.org/record/1173518. [![DOI](https://zenodo.org/badge/90709196.svg)](https://zenodo.org/badge/latestdoi/90709196)
