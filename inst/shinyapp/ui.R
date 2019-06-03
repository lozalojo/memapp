library("shiny")
library("shinythemes")
library("shinydashboard")
library("shinyWidgets")
library("shinyBS")
library("shinyjs")
library("RColorBrewer")
library("tidyr")
library("dplyr")
library("openxlsx")
library("foreign")
library("haven")
library("readxl")
library("stringr")
library("stringi")
library("DT")
library("RODBC")
library("formattable")
library("ggplot2")
library("plotly")
library("mem")

source("helpers.R")

cat("preparation> begin\n")
cat("preparation> creating translation file\n")
build.languages()
languages <- get.languages()
languages.list <- as.list(languages$filename)
names(languages.list) <- languages$lang_name
running.versions <- get.r.versions()

shinyUI(
  dashboardPage(
    title = "The Moving Epidemic Method Web Application",
    skin = "black",
    ###################################
    ### HEADER SECTION              ###
    ###################################
    # Tricky way of placing elements in dashboardHeader, expects a tag element of type li and class dropdown,
    # so we can pass such elements instead of dropdownMenus
    header = dashboardHeader(
      title = "MEM dashboard",
      tags$li(paste(running.versions$r, "/", running.versions$platform, ", memapp ", running.versions$memapp, ", mem ", running.versions$mem, " - code under GPLv2 at", sep = ""),
        class = "dropdown"
      ),
      tags$li(a(
        href = "https://github.com/lozalojo/",
        target = "_blank",
        img(
          src = "GitHub_Logo.png",
          title = "José E. Lozano", height = "30px"
        ),
        style = "padding-top:10px; padding-bottom:0px;"
      ),
      class = "dropdown"
      ),
      tags$li(a(
        href = "http://www.icscyl.com",
        target = "_blank",
        img(
          src = "logoiecscyl.gif",
          title = "IECSCyL", height = "40px"
        ),
        style = "padding-top:5px; padding-bottom:0px;"
      ),
      class = "dropdown"
      ),
      tags$li(a(
        onclick = "setTimeout(function(){window.close();}, 100); ",
        icon("power-off", "fa-2x"),
        title = "Power off"
      ),
      class = "dropdown"
      )
    ),
    ###################################
    ### LEFT PANEL SECTION          ###
    ###################################
    sidebar = dashboardSidebar(
      width = "250px",
      ################################
      ###    Load data          ######
      ################################
      uiOutput("uifile"),
      uiOutput("uiDataset"),
      ################################
      ###    Model                ####
      ################################
      uiOutput("uiModel"),
      ################################
      ###    Surveillance         ####
      ################################
      uiOutput("uiSurveillance"),
      ################################
      ###    Visualize            ####
      ################################
      uiOutput("uiVisualize"),
      ################################
      ###  Thresholds             ####
      ################################
      uiOutput("uiThresholds")
    ),
    ###################################
    ### BODY/MAIN SECTION           ###
    ###################################
    body = dashboardBody(
      # All credits of the busy-indicator goes to AnalytixWare/ShinySky, I was forced to include the code
      # here because the package does not have a CRAN release.
      tags$body(inlineCSS(list(".shinysky-busy-indicator" = "position: absolute !important; z-index:800; "))),
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      uiOutput("uiTitle"),
      ###################################
      ### BODY/MAIN SECTION           ###
      ###   FIRST COLUMN DEFINITION   ###
      ###################################
      #######################################
      ### BODY/MAIN SECTION               ###
      ###   FIRST COLUMN DEFINITION       ###
      ###      FIRST PART: OUTPUTS        ###
      #######################################
      fluidRow(
        column(11, uiOutput("uiProcedures")),
        column(
          1,
          # Text options
          uiOutput("uiTextoptions"),
          # Graph options
          uiOutput("uiGraphoptions"),
          # MEM options
          uiOutput("uiMEMoptions"),
          # Support
          uiOutput("uiSupport"),
          # Language
          dropdown(shinydashboard::box(
            title = "", solidHeader = TRUE, status = "warning", width = 12,
            uiOutput("uiLanguage"),
            selectInput("language", label = "", choices = languages.list, size = 1, selectize = FALSE, selected = "en_GB")
          ),
          circle = TRUE,
          tooltip = tooltipOptions(placement = "left", title = "Language", html = TRUE),
          margin = "0px",
          style = "minimal",
          icon = icon("sign-language"),
          status = "warning",
          width = "300px",
          right = TRUE,
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInRight,
            exit = animations$fading_exits$fadeOutRight
          )
          )
        )
      )
    )
  )
)
