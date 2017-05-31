# The Moving Epidemics Method Shiny Web Application

## Overview

*memapp* is a web application created to serve as a graphical user interface for the R mem library. It was created using Shiny, a web application framework for R. This application uses the development version of the mem R library.

## Installation

The package can be installed from the official R repositories (*CRAN*) using the built-in install function:

```
# install the memapp CRAN version
install.packages("memapp")
```

Nevertheless, *memapp* uses some sensible packages that are updated frequently in their respective development versions hosted at github. To install the development version of *memapp* along with development versions of their dependencies, it is recommended to install it from the sources at github.

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

## More on the memapp

There are two documents that you will find useful when running the app:

1.  The *surveillance Guidelines*.
    
1.  The *Technical Manual*.

Both are included in the help menu of the application.

## References

Vega T., Lozano J.E. (2004) Modelling influenza epidemic - can we detect the beginning and predict the intensity and duration? International Congress Series 1263 (2004) 281-283.

Vega T., Lozano J.E. (2012) Influenza surveillance in Europe: establishing epidemic thresholds by the Moving Epidemic Method. Influenza and Other Respiratory Viruses, DOI:10.1111/j.1750-2659.2012.00422.x.

Lozano, Jose E. mem R package: First version of the MEM R library [Internet]. Valladolid, Spain: Foundation Institute of Health Sciences Studies of Castilla y Leon; 2014. Available from: https://cran.r-project.org/web/packages/mem/index.html

Lozano, Jose E. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1]; Available from: https://zenodo.org/record/165983
