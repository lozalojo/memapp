library(shiny)
shinyUI(dashboardPage(skin = "blue",
                      
  ###################################
  ### HEADER SECTION              ###
  ###################################
                      
  dashboardHeader(title = "Dashboard"),
  
  ###################################
  ### LEFT PANEL SECTION          ###
  ###################################
  
  dashboardSidebar(width='250px', 
                   fileInput("file", label = h5("Load data (.CSV)")),
                   # selectInput("K", h5("    Apply MEM on season"), size=1,
                   #             selectize = FALSE, choices = ""),
                   
                   selectInput("K", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                       bsButton("q1", label = "", 
                                                icon = icon("question"), 
                                                style = "info", size = "extra-small"), "Apply MEM on season"), size=1,
                                             selectize = FALSE, choices = ""),
                   bsPopover(id = "q1", title = "Current season",
                             content = "Choose a season to apply MEM thresholds on. Your choosen seasen will be excluded when calculating the MEM thresholds for pre- and post season.",
                             placement = "top", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   
                   selectInput("K2", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                        bsButton("q2", label = "", 
                                                 icon = icon("question"), 
                                                 style = "info", size = "extra-small"), "Calculate MEM from season"), 
                               choices = "", size=1, selectize = FALSE),
                   bsPopover(id = "q2", title = "Calculate MEM",
                             content = "MEM is estimated using historical seasons. MEM will be calculated from the historical season you choose here up to one year before the season you choosen to apply MEM on.",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   selectInput('K3', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                        bsButton("q3", label = "", 
                                                 icon = icon("question"), 
                                                 style = "info", size = "extra-small"), "Add seasons to graph"),
                               choices = NULL, multiple = TRUE),
                   bsPopover(id = "q3", title = "Multiple seasons",
                             content = "By clicking at the seasons you can display multiple seasons in the graph. To delete a season click on it and press delete on your keyboard. You cannot apply MEM on this graph. To get MEM back delete the choices you have made.",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   
                   selectInput('K4', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                        bsButton("q4", label = "", 
                                                 icon = icon("question"), 
                                                 style = "info", size = "extra-small"), "Check epi-timing for seasons"), 
                               choices = "", size=1,
                               selectize = FALSE),
                   bsPopover(id = "q4", title = "Evaluating MEM",
                             content = "MEM is applied ob historical seasons. Here you can evaluate the fit on MEM by studying the fit of pre- and post season on previous seasons.",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   checkboxInput("mem_knapp", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                         bsButton("q5", label = "", 
                                                                  icon = icon("question"), 
                                                                  style = "info", size = "extra-small"),
                                                         "Include MEM-thresholds in graph"),
                                 value = FALSE),
                   bsPopover(id = "q5", title = "Include MEM thresholds",
                             content = "Check the box if you want to include the MEM-thresholds in the plot output.",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   checkboxInput("mem_intensitet", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                              bsButton("q7", label = "", 
                                                                       icon = icon("question"), 
                                                                       style = "info", size = "extra-small"), "Include intensity"), 
                                 value = FALSE),
                   bsPopover(id = "q7", title = "MEM-intensity",
                             content = "Click if you would like to include the intensity of the desease. The intensity is a geometrical mean of the seasons you have choosen for calculating MEM. You can change graphical parameters for the displayed intensity unde Graph options",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body"))),
  
  ###################################
  ### BODY/MAIN SECTION           ###
  ###################################
  
  dashboardBody(
    fluidPage(

  # Application title
  titlePanel(h1("Moving Epidemic Method")),
  
  fluidRow(
    
    ###################################
    ### BODY/MAIN SECTION           ###
    ###   FIRST COLUMN DEFINITION   ###
    ###################################
    
    # Show a plot of the generated distribution

  column(9,
         
         #######################################
         ### BODY/MAIN SECTION               ###
         ###   FIRST COLUMN DEFINITION       ###
         ###      FIRST PART: GRAPH OPTIONS  ###
         #######################################
         
         box(title="Graph options", status = "primary", solidHeader = TRUE, width = 12,  background = "black",
                      collapsible = TRUE, collapsed=TRUE,
             fluidRow(column(2, selectInput("colOBS",
                                            h5("Observed",
                                               tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                               bsButton("q9", label = "", 
                                                        icon = icon("question"), 
                                                        style = "info", size = "extra-small")), width = '130px',
                                            choices = sapply(colors(), function(x) list(x)),
                                            size=1,
                                            selectize = FALSE,
                                            selected = "black")),
                      column(2, selectInput("colMEMstart",
                         h5("MEM season start",
                            tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                            bsButton("q10", label = "", 
                                     icon = icon("question"), 
                                     style = "info", size = "extra-small")), width = '130px',
                         choices = sapply(colors(), function(x) list(x)),
                         size=1,
                         selectize = FALSE,
                         selected = "red")),
                      column(2,selectInput("colMEMstop",
                         h5("MEM season end",
                            tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                            bsButton("q11", label = "", 
                                     icon = icon("question"), 
                                     style = "info", size = "extra-small")), width = '130px',
                         choices = sapply(colors(), function(x) list(x)),
                         size=1,
                         selectize = FALSE,
                         selected = "green")),
                      column(2,selectInput("colpal",
                                           h5("Color palette",
                                              tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                              bsButton("q111", label = "", 
                                                       icon = icon("question"), 
                                                       style = "info", size = "extra-small")), width = '130px',
                                           choices = sapply(rownames(brewer.pal.info), function(x) list(x)),
                                           size=1,
                                           selectize = FALSE,
                                           selected = "Blues")),
                      column(2,selectInput("colpalTran",
                                           h5("Transparency",
                                              tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                              bsButton("q112", label = "", 
                                                       icon = icon("question"), 
                                                       style = "info", size = "extra-small")), width = '130px',
                                           choices = sapply(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), function(x) list(x)),
                                           size=1,
                                           selectize = FALSE,
                                           selected = 1)))),
         
         #######################################
         ### BODY/MAIN SECTION               ###
         ###   FIRST COLUMN DEFINITION       ###
         ###      SECOND PART: OUTPUTS       ###
         #######################################
         
         box(title="Outputs", status = "primary", solidHeader = TRUE, width = 12,  background = "black",
             collapsible = FALSE, uiOutput("tb")),
         
         #######################################
         ### BODY/MAIN SECTION               ###
         ###   FIRST COLUMN DEFINITION       ###
         ###      THIRD PART: MEM OPTIONS    ###
         #######################################
         
         box(title="MEM options", status = "primary", solidHeader = TRUE, width = 12,  background = "black",
             collapsible = TRUE, collapsed=TRUE,
             fluidRow(column(3, selectInput("i.method",
                                            h5("Method for timing", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                               bsButton("q12", label = "", 
                                                        icon = icon("question"), 
                                                        style = "info", size = "extra-small")), width = '230px',
                                            choices = list("Original method"=1, 
                                                           "Fixed criterium method"=2, 
                                                           "Slope method"=3, 
                                                           "Second derivative method"=4),
                                            size=1,
                                            selectize = FALSE,
                                            selected = 2),
                             bsPopover(id = "q12", title = "Method for epidemic timing",
                                       content = "The original method uses the process shown in the original paper. The fixed criterium method uses the slope of the MAP curve fo find the optimum, which is the point where the slope is lower than a predefined value. The slope method calculates the slope of the MAP curve, but the optimum is the one that matches the global mean slope. The second derivative method calculates the second derivative and equals to zero to search an inflexion point in the original curve.",
                                       placement = "right", 
                                       trigger = "hover", 
                                       options = list(container = "body"))),
                      column(3,numericInput("memparameter", "Slope parameter:", 2.8, step=0.1)),
                      column(6, selectInput("i.type.threshold",
                                            h5("C.I. epidemic thr.", 
                                               tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                               bsButton("q13", label = "", 
                                                        icon = icon("question"), 
                                                        style = "info", size = "extra-small")), width = '420px',
                                            choices = list("Arithmetic mean and mean confidence interval"=1,
                                                           "Geometric mean and mean confidence interval"=2, 
                                                           "Median and the KC Method to calculate its confidence interval"=3,
                                                           "Median and bootstrap confidence interval"=4, 
                                                           "Arithmetic mean and point confidence interval"=5, 
                                                           "Geometric mean and point confidence interval"=6),
                                            size=1,
                                            selectize = FALSE,
                                            selected = 5),
                             bsPopover(id = "q13", title = "95% upper confidence limit",
                                       content = "There are six different methods for calculating the 95% upper confidence limit for the MEM thresholds.",
                                       placement = "right", 
                                       trigger = "hover", 
                                       options = list(container = "body"))),
                      column(6, selectInput("i.type.intensity",
                                            h5("C.I. intensity thr.", 
                                               tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                               bsButton("q13b", label = "", 
                                                        icon = icon("question"), 
                                                        style = "info", size = "extra-small")), width = '420px',
                                            choices = list("Arithmetic mean and mean confidence interval"=1,
                                                           "Geometric mean and mean confidence interval"=2, 
                                                           "Median and the KC Method to calculate its confidence interval"=3,
                                                           "Median and bootstrap confidence interval"=4, 
                                                           "Arithmetic mean and point confidence interval"=5, 
                                                           "Geometric mean and point confidence interval"=6),
                                            size=1,
                                            selectize = FALSE,
                                            selected = 6),
                             bsPopover(id = "q13b", title = "95% upper confidence limit",
                                       content = "There are six different methods for calculating the 95% upper confidence limit for the MEM thresholds.",
                                       placement = "right", 
                                       trigger = "hover", 
                                       options = list(container = "body")))
                      ))),
  
  ###################################
  ### BODY/MAIN SECTION           ###
  ###   SECOND COLUMN DEFINITION  ###
  ###################################
  
  column(3,
        box(title="Graph text", status = "primary", solidHeader = TRUE, width = 7,  background = "black",
            collapsible = FALSE,
            textInput("textMain", label = h5("Main title"), 
                      value = "Main title"),
            textInput("textY", label = h5("Y-axis"), 
           value = "Y-axis"),
           textInput("textX", label = h5("X-axis"), 
           value = "X-axis")))
  ))))
  )










  
