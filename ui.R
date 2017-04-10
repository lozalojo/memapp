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
                   
                   ################################
                   ###    Load data          ######
                   ################################
                   
                   fileInput("file", label = h5("Load data")),
                   # selectInput("K", h5("    Apply MEM on season"), size=1,
                   #             selectize = FALSE, choices = ""),

                   ################################
                   ###  Apply MEM on season    ####
                   ################################
                   
                   # selectInput("K", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                   #                     bsButton("q1", label = "", 
                   #                              icon = icon("question"), 
                   #                              style = "info", size = "extra-small"), "Apply MEM on season"), size=1,
                   #                           selectize = FALSE, choices = ""),
                   # bsPopover(id = "q1", title = "Current season",
                   #           content = "Choose a season to apply MEM thresholds on. Your choosen seasen will be excluded when calculating the MEM thresholds for pre- and post season.",
                   #           placement = "top", 
                   #           trigger = "hover", 
                   #           options = list(container = "body")),

                   ######################################
                   ###  Calculate MEM from season    ####
                   ######################################
                   
                   # selectInput("K2", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                   #                      bsButton("q2", label = "", 
                   #                               icon = icon("question"), 
                   #                               style = "info", size = "extra-small"), "Calculate MEM from season"), 
                   #             choices = "", size=1, selectize = FALSE),
                   # bsPopover(id = "q2", title = "Calculate MEM",
                   #           content = "MEM is estimated using historical seasons. MEM will be calculated from the historical season you choose here up to one year before the season you choosen to apply MEM on.",
                   #           placement = "right", 
                   #           trigger = "hover", 
                   #           options = list(container = "body")),
                   
                   ######################################
                   ###  Add seasons to graph         ####
                   ######################################

                   # selectInput('K3', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                   #                      bsButton("q3", label = "", 
                   #                               icon = icon("question"), 
                   #                               style = "info", size = "extra-small"), "Add seasons to graph"),
                   #             choices = NULL, multiple = TRUE),
                   # bsPopover(id = "q3", title = "Multiple seasons",
                   #           content = "By clicking at the seasons you can display multiple seasons in the graph. To delete a season click on it and press delete on your keyboard. You cannot apply MEM on this graph. To get MEM back delete the choices you have made.",
                   #           placement = "right", 
                   #           trigger = "hover", 
                   #           options = list(container = "body")),

                   ######################################
                   ###  Check epi-timing for seasons ####
                   ######################################
                   
                   # selectInput('K4', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                   #                      bsButton("q4", label = "", 
                   #                               icon = icon("question"), 
                   #                               style = "info", size = "extra-small"), "Check epi-timing for seasons"), 
                   #             choices = "", size=1,
                   #             selectize = FALSE),
                   # bsPopover(id = "q4", title = "Evaluating MEM",
                   #           content = "MEM is applied ob historical seasons. Here you can evaluate the fit on MEM by studying the fit of pre- and post season on previous seasons.",
                   #           placement = "right", 
                   #           trigger = "hover", 
                   #           options = list(container = "body")),
                   
                   box(title="Model", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,

                   ### This portion is experimental
                   #h3(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Model"),
                   # From
                   selectInput("SelectFrom", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "From"), size=1, selectize = FALSE, choices = ""),
                   # To
                   selectInput("SelectTo", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "To"), size=1, selectize = FALSE, choices = ""),
                   # Excluding
                   selectInput('SelectExclude', h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Exclude"), multiple = TRUE, choices = NULL),
                   # Pandemic
                   # checkboxInput("SelectPandemic", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Pandemic"), value = FALSE),
                   # Maximum seasons
                   numericInput("SelectMaximum", "Max. seasons:", 10, step=1)
                   ),
                   
                   box(title="Surveillance", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                   
                   #h3(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Surveillance"),
                   # Surveillance
                   
                   selectInput("SelectSurveillance", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                       bsButton("ssb", label = "", 
                                                icon = icon("question"), 
                                                style = "info", size = "extra-small"), "Surveillance"), size=1,
                               selectize = FALSE, choices = ""),
                   bsPopover(id = "ssb", title = "Current season",
                             content = "Choose a season to apply MEM thresholds on. Your choosen seasen will be excluded when calculating the MEM thresholds for pre- and post season.",
                             placement = "top", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   # Surveillance week
                   selectInput("SelectSurveillanceWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                        bsButton("sswb", label = "", 
                                                                 icon = icon("question"), 
                                                                 style = "info", size = "extra-small"), "Surveillance Week"), size=1,
                               selectize = FALSE, choices = ""),
                   bsPopover(id = "sswb", title = "Current season",
                             content = "Choose a season to apply MEM thresholds on. Your choosen seasen will be excluded when calculating the MEM thresholds for pre- and post season.",
                             placement = "top", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   # Force surveillance week start
                   selectInput("SelectSurveillanceForceEpidemic", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                            bsButton("ssfb", label = "", 
                                                                     icon = icon("question"), 
                                                                     style = "info", size = "extra-small"), "Force epidemic start"), size=1,
                               selectize = FALSE, choices = NULL, select = NULL),
                   bsPopover(id = "ssfb", title = "Current season",
                             content = "Choose a season to apply MEM thresholds on. Your choosen seasen will be excluded when calculating the MEM thresholds for pre- and post season.",
                             placement = "top", 
                             trigger = "hover", 
                             options = list(container = "body"))
                   ),
                   
                   box(title="Visualize", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                       
                   #h3(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Visualize"),
                   # Add seasons
                   selectInput('SelectSeasons', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                        bsButton("spb", label = "", 
                                                 icon = icon("question"), 
                                                 style = "info", size = "extra-small"), "Seasons to graph"),
                               choices = NULL, multiple = TRUE),
                   bsPopover(id = "spb", title = "Multiple seasons",
                             content = "By clicking at the seasons you can display multiple seasons in the graph. To delete a season click on it and press delete on your keyboard. You cannot apply MEM on this graph. To get MEM back delete the choices you have made.",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body"))
                   ),
                   
                   ##########################################
                   ###  Include MEM-thresholds in graph  ####
                   ##########################################
                   
                   box(title="Thresholds", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                       
                   #h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Thresholds"),
                   checkboxInput("preepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                         bsButton("q5", label = "", 
                                                                  icon = icon("question"), 
                                                                  style = "info", size = "extra-small"),
                                                         "Pre-epidemic threshold"),
                                 value = TRUE),
                   bsPopover(id = "q5", title = "Pre-epidemic threshold",
                             content = "Check the box if you want to include the MEM-thresholds in the plot output.",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),

                   checkboxInput("postepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                         bsButton("q5b", label = "", 
                                                                  icon = icon("question"), 
                                                                  style = "info", size = "extra-small"),
                                                         "Post-epidemic threshold"),
                                 value = FALSE),
                   bsPopover(id = "q5b", title = "Post-epidemic threshold",
                             content = "Check the box if you want to include the MEM-thresholds in the plot output.",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                   
                   ##########################################
                   ###  Include intensity                ####
                   ##########################################
                   
                   checkboxInput("intensitythr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                              bsButton("q7", label = "", 
                                                                       icon = icon("question"), 
                                                                       style = "info", size = "extra-small"), "Intensity thresholds/levels"), 
                                 value = TRUE),
                   bsPopover(id = "q7", title = "Intensity thresholds/levels",
                             content = "Click if you would like to include the intensity of the desease. The intensity is a geometrical mean of the seasons you have choosen for calculating MEM. You can change graphical parameters for the displayed intensity unde Graph options",
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body"))
                   )
                  
                   ),
  
  ###################################
  ### BODY/MAIN SECTION           ###
  ###################################
  
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    fluidPage(

  # Application title
  titlePanel(h1("The Moving Epidemics Method")),
  
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
         
         
         
         #######################################
         ### BODY/MAIN SECTION               ###
         ###   FIRST COLUMN DEFINITION       ###
         ###      SECOND PART: OUTPUTS       ###
         #######################################
         
         # box(title="Outputs", status = "primary", solidHeader = TRUE, width = 12,  background = "black",
         #     collapsible = FALSE, uiOutput("tb")),
         tabBox(
           # Title can include an icon
           title = "MEM", width = 12, height = "800px",
           tabPanel("Check & describe", "Check data series, timing and describe the data", uiOutput("tbData")),
           tabPanel("Model", "Summary, graphs, goodness and optimization of the MEM model", uiOutput("tbModel")),
           tabPanel("Surveillance", "Surveillance tools", uiOutput("tbSurveillance")),
           tabPanel("Visualize", "Visualize different sets of data with a MEM model", uiOutput("tbVisualize"))
         ),
         
         #######################################
         ### BODY/MAIN SECTION               ###
         ###   FIRST COLUMN DEFINITION       ###
         ###      THIRD PART: MEM OPTIONS    ###
         #######################################
         
         box(title="MEM options", status = "primary", solidHeader = TRUE, width = 12,  background = "black",
             collapsible = TRUE, collapsed=TRUE,
             h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Timing"),
             fluidRow(column(4, selectInput("method",
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
                      column(2,numericInput("param", "Slope parameter:", 2.8, step=0.1))),
             h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Thresholds"),
             fluidRow(column(3, selectInput("type.threshold",
                                            h5("Epidemic thr.", 
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
                      column(3, selectInput("type.intensity",
                                            h5("intensity thr.", 
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
                                       options = list(container = "body"))),
                      column(3, selectInput("type.curve",
                                            h5("Curve thr.", 
                                               tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                               bsButton("q13c", label = "", 
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
                                            selected = 2),
                             bsPopover(id = "q13c", title = "95% upper confidence limit",
                                       content = "There are six different methods for calculating the 95% upper confidence limit for the MEM thresholds.",
                                       placement = "right", 
                                       trigger = "hover", 
                                       options = list(container = "body"))),
                      column(3, selectInput("type.other",
                                            h5("Other thr.", 
                                               tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                               bsButton("q13d", label = "", 
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
                                            selected = 3),
                             bsPopover(id = "q13d", title = "95% upper confidence limit",
                                       content = "There are six different methods for calculating the 95% upper confidence limit for the MEM thresholds.",
                                       placement = "right", 
                                       trigger = "hover", 
                                       options = list(container = "body")))
                      
                      
                      ),
             fluidRow(column(2, numericInput("n.max", "Values per season:", -1, step=1, min = -1, max = 100)),
                      column(2, numericInput("level.intensity.m", "Level medium:", 40, step=0.5, min = 0.5, max = 99.5)),
                      column(2, numericInput("level.intensity.h", "Level high:", 90, step=0.5, min = 0.5, max = 99.5)),
                      column(2, numericInput("level.intensity.v", "Level very high:", 97.5, step=0.5, min = 0.5, max = 99.5)),                      
                      column(2, numericInput("tails", "Tails:", 1, step=1, min = 1, max = 2)))       
             
             )),
  
  ###################################
  ### BODY/MAIN SECTION           ###
  ###   SECOND COLUMN DEFINITION  ###
  ###################################
  
  column(3,
        box(title="Graph text", status = "primary", solidHeader = TRUE, width = 12,  background = "black",
            collapsible = TRUE, collapsed=TRUE,
            textInput("textMain", label = h5("Main title"), 
                      value = "Main title"),
            textInput("textY", label = h5("Y-axis"), 
           value = "Y-axis"),
           textInput("textX", label = h5("X-axis"),value = "X-axis")
           ),
        box(title="Graph options", status = "primary", solidHeader = TRUE, width = 12,  background = "black",
            collapsible = TRUE, collapsed=TRUE,
            #fluidRow(column(2, 
                            selectInput("colObservedLines",
                                           h5("Observed (line)",
                                              tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                              bsButton("q9", label = "", 
                                                       icon = icon("question"), 
                                                       style = "info", size = "extra-small")), 
                                        #width = '130px',
                                           choices = sapply(colors(), function(x) list(x)),
                                           size=1,
                                           selectize = FALSE,
                                           selected = "black"),
            selectInput("colObservedPoints",
                        h5("Observed (points)",
                           tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                           bsButton("q9", label = "", 
                                    icon = icon("question"), 
                                    style = "info", size = "extra-small")), 
                        #width = '130px',
                        choices = sapply(colors(), function(x) list(x)),
                        size=1,
                        selectize = FALSE,
                        selected = "grey")
                            #),column(2
                                     ,selectInput("colEpidemicStart",
                                           h5("Epidemic start",
                                              tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                              bsButton("q10", label = "", 
                                                       icon = icon("question"), 
                                                       style = "info", size = "extra-small")), 
                                           #width = '130px',
                                           choices = sapply(colors(), function(x) list(x)),
                                           size=1,
                                           selectize = FALSE,
                                           selected = "red")
                                     #),column(2
                                              ,selectInput("colEpidemicStop",
                                          h5("Epidemic end",
                                             tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                             bsButton("q11", label = "", 
                                                      icon = icon("question"), 
                                                      style = "info", size = "extra-small")), 
                                          #width = '130px',
                                          choices = sapply(colors(), function(x) list(x)),
                                          size=1,
                                          selectize = FALSE,
                                          selected = "green")
                                          #),column(2
                                                   ,selectInput("colThresholds",
                                          h5("Thresholds palette",
                                             tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                             bsButton("q111", label = "", 
                                                      icon = icon("question"), 
                                                      style = "info", size = "extra-small")), 
                                          #width = '130px',
                                          choices = sapply(rownames(brewer.pal.info), function(x) list(x)),
                                          size=1,
                                          selectize = FALSE,
                                          selected = "Blues")
                                          #),column(2
                                                   ,selectInput("colseries",
                                          h5("Series palette",
                                             tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                             bsButton("q111b", label = "", 
                                                      icon = icon("question"), 
                                                      style = "info", size = "extra-small")), 
                                          #width = '130px',
                                          choices = sapply(rownames(brewer.pal.info), function(x) list(x)),
                                          size=1,
                                          selectize = FALSE,
                                          selected = "Accent")
            ,selectInput("colEpidemic",
                         h5("Timing palette",
                            tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                            bsButton("q111b", label = "", 
                                     icon = icon("question"), 
                                     style = "info", size = "extra-small")), 
                         #width = '130px',
                         choices = sapply(rownames(brewer.pal.info), function(x) list(x)),
                         size=1,
                         selectize = FALSE,
                         selected = "Accent")
            #),column(2
                     ,selectInput("colTransparency",
                                          h5("Transparency",
                                             tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                             bsButton("q112", label = "", 
                                                      icon = icon("question"), 
                                                      style = "info", size = "extra-small")), 
                                  #width = '130px',
                                          choices = sapply(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), function(x) list(x)),
                                          size=1,
                                          selectize = FALSE,
                                          selected = 1)
            #))
                        
            )
        )
  ))))
  )










  
