# The Moving Epidemics Method Shiny Web Application

[![Travis Build Status](https://travis-ci.org/lozalojo/memapp.svg?branch=master)](https://travis-ci.org/lozalojo/memapp)
[![Coverage Status](https://img.shields.io/codecov/c/github/lozalojo/memapp/master.svg)](https://codecov.io/github/lozalojo/memapp?branch=master)

## Overview

*memapp* is a web application created to serve as a graphical user interface for the R mem library. It was created using Shiny, a web application framework for R. This application uses the development version of the mem R library.

## Installation

Currently there is no CRAN version of the *memapp* library, so it has to be installed from its source.

```
# install the devtools package:
install.packages("devtools")
# install the memapp development version from GitHub:
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