# The Moving Epidemic Method Shiny Web Application

[![Travis Build Status](https://travis-ci.org/lozalojo/memapp.svg?branch=master)](https://travis-ci.org/lozalojo/memapp)
[![Coverage Status](https://img.shields.io/codecov/c/github/lozalojo/memapp/master.svg)](https://codecov.io/github/lozalojo/memapp?branch=master)
[![monthly](http://cranlogs.r-pkg.org/badges/memapp)](https://www.rpackages.io/package/memapp) 
[![total](http://cranlogs.r-pkg.org/badges/grand-total/memapp)](https://www.rpackages.io/package/memapp)
[![CRAN](http://www.r-pkg.org/badges/version/memapp?color=009999)](https://cran.r-project.org/package=memapp)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

## Overview

*memapp* is a web application created to serve as a graphical user interface for the R mem package. It was created using Shiny, a web application framework for R. This application uses the development version of the mem R library.

## Installation

The package can be installed from the official R repositories (*CRAN*) using the built-in install function:

```
# install the memapp CRAN version
install.packages("memapp")
```

Nevertheless, *memapp* uses some sensitive packages that are updated frequently in their respective development versions hosted at github. To install the development version of *memapp* along with development versions of their dependencies, it is recommended to install it from the sources at github.

```
# install the devtools package
install.packages("devtools")
# install the memapp development version from GitHub
devtools::install_github("lozalojo/memapp")
```

## Usage

To run the memapp application, just use the function:

```
# run the app:
memapp::runmemapp()
```

You can specify other parameters passed to `shiny::runApp`, such as `display = normal`, or  
`launch.browser = TRUE`.

## References

Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic—can we detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281–3. 

Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir Viruses. 2013 Jul;7(4):546–58. DOI:10.1111/j.1750-2659.2012.00422.x.

Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other Respir Viruses. 2015 Sep;9(5):234–46. DOI:10.1111/irv.12330.

Lozano JE. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1]; Available from: https://zenodo.org/record/165983. DOI:10.5281/zenodo.165983

