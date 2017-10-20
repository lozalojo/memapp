library("shiny")
library("shinythemes")
library("shinydashboard")
library("shinyBS")
library("shinyjs")
library("RColorBrewer")
library("ggthemes")
library("reshape2")
library("openxlsx")
library("readxl")
library("stringr")
library("stringi")
library("DT")
library("RODBC")
library("formattable")
library("ggplot2")
library("plotly")
library("mem")

languages<-memapp:::get.languages()
languages.list<-as.list(languages$lcidstring)
names(languages.list)<-languages$locale

shinyUI(
  dashboardPage(skin = "black",
                ###################################
                ### HEADER SECTION              ###
                ###################################
                # Tricky way of placing elements in dashboardHeader, expects a tag element of type li and class dropdown, 
                # so we can pass such elements instead of dropdownMenus
                dashboardHeader(title = "MEM dashboard",
                                tags$li(paste("memapp v",packageVersion("memapp")," and mem v",packageVersion("mem"),", code under GPLv2 at",sep=""),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://github.com/lozalojo/',
                                          target="_blank",
                                          img(src = 'GitHub_Logo.png',
                                              title = "José E. Lozano", height = "30px"),
                                          style = "padding-top:10px; padding-bottom:0px;"),
                                        class = "dropdown"),
                                tags$li(a(href = 'http://www.icscyl.com',
                                          target="_blank",
                                          img(src = 'logoiecscyl.gif',
                                              title = "IECSCyL", height = "40px"),
                                          style = "padding-top:5px; padding-bottom:0px;"),
                                        class = "dropdown"),
                                tags$li(a(onclick = "setTimeout(function(){window.close();}, 100); ",
                                          icon("power-off", "fa-2x"),
                                          title = "Power off"),
                                        class = "dropdown")),
                ###################################
                ### LEFT PANEL SECTION          ###
                ###################################
                dashboardSidebar(width='250px', 
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
                
                dashboardBody(
                  # All credits of the busy-indicator goes to AnalytixWare/ShinySky, I was forced to include the code
                  # here because the package does not have a CRAN release.
                  tags$body(inlineCSS(list(".shinysky-busy-indicator" = "position: absolute !important; z-index:800; "))),
                  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  uiOutput("uiTitle"),
                  fluidPage(
                    fluidRow(
                      ###################################
                      ### BODY/MAIN SECTION           ###
                      ###   FIRST COLUMN DEFINITION   ###
                      ###################################
                      column(9,
                             #######################################
                             ### BODY/MAIN SECTION               ###
                             ###   FIRST COLUMN DEFINITION       ###
                             ###      FIRST PART: OUTPUTS        ###
                             #######################################
                             uiOutput("uiProcedures")
                      ),
                      ###################################
                      ### BODY/MAIN SECTION           ###
                      ###   SECOND COLUMN DEFINITION  ###
                      ###################################
                      column(3,
                             uiOutput("uiTextoptions"),
                             uiOutput("uiGraphoptions"),
                             uiOutput("uiMEMoptions"),
                             uiOutput("uiSupport"),
                             box(
                               title="", solidHeader = TRUE, status = "warning", width = 12,
                               uiOutput("uiLanguage"),
                               selectInput("lang", label = "", choices = languages.list, size=1, selectize = FALSE, selected = "en-gb")
                             )
                      )
                    )
                  )
                )
  )
)

