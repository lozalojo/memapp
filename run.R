####################################################################
##### RUN THE APPLICATION
####################################################################

# First option: run from sources

# shiny::runGitHub("lozalojo/memapp", launch.browser = T)

# Second option: run from local directory

# Set up the path where the app files are, if you use Rstudio and put this file in the same directory
# the following command will work

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shiny::runApp(launch.browser = T)
