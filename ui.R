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
                                       
                                       fileInput('file', 'Load file', accept = c("csv","dat","prn","txt","xls","xlsx","mdb","accdb")),
                                       uiOutput('loaddata'),
                                       
                                       ################################
                                       ###    Model                ####
                                       ################################
                                       
                                       box(title="Model", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                           selectInput("SelectFrom", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpmf", label = "", icon = icon("question"), style = "info", size = "extra-small"), "From"), size=1, selectize = FALSE, choices = ""),
                                           bsPopover(id = "helpmf", title = "Select seasons for the model", content = "First column to include in the model selection.", placement = "top", trigger = "hover", options = list(container = "body")),
                                           selectInput("SelectTo", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpmt", label = "", icon = icon("question"), style = "info", size = "extra-small"), "To"), size=1, selectize = FALSE, choices = ""),
                                           bsPopover(id = "helpmt", title = "Select seasons for the model", content = "Last column to include in the model selection.", placement = "top", trigger = "hover", options = list(container = "body")),
                                           selectInput('SelectExclude', h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpme", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Exclude"), multiple = TRUE, choices = NULL),
                                           bsPopover(id = "helpme", title = "Select seasons for the model", content = "Select any number of seasons to be excluded from the model.", placement = "top", trigger = "hover", options = list(container = "body")),
                                           numericInput("SelectMaximum", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpmm", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Max. seasons:"), 10, step=1),
                                           bsPopover(id = "helpmm", title = "Select seasons for the model", content = "Maximum number of seasons to be used in the model.<br>Note that this will probably override the rest options, since it will restrict data to the last number of seasons from the selection already made with From/To/Exclude.<br>For influenza it is not recommended to use more than 10 seasons to avoid cyclical trends.", placement = "top", trigger = "hover", options = list(container = "body"))
                                       ),
                                       
                                       ################################
                                       ###    Surveillance         ####
                                       ################################
                                       
                                       box(title="Surveillance", 
                                           status = "primary", 
                                           solidHeader = TRUE, 
                                           width = 12,  
                                           background = "black", 
                                           collapsible = TRUE, 
                                           collapsed=TRUE,
                                           selectInput("SelectSurveillance", 
                                                       h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpss", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Surveillance"), 
                                                       size=1,
                                                       selectize = FALSE, 
                                                       choices = ""),
                                           bsPopover(id = "helpss", 
                                                     title = "Current season",
                                                     content = "Season you want to use for surveillance applying the MEM thresholds.<br>This season can be incomplete.<br> It is recommended not to use the surveillance season in the model selection.",
                                                     placement = "top", 
                                                     trigger = "hover", 
                                                     options = list(container = "body")),
                                           selectInput("SelectSurveillanceWeek", 
                                                       h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpsw", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Surveillance Week"), 
                                                       size=1,
                                                       selectize = FALSE, 
                                                       choices = ""),
                                           bsPopover(id = "helpsw", 
                                                     title = "Surveillance week",
                                                     content = "Week you want to create the surveillance graph for. It can be any week from the first week of the surveillance season to the last one that have data",
                                                     placement = "top", 
                                                     trigger = "hover", 
                                                     options = list(container = "body")),
                                           selectInput("SelectSurveillanceForceEpidemic", 
                                                       h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpse", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Force epidemic start"), 
                                                       size=1,
                                                       selectize = FALSE, 
                                                       choices = NULL, 
                                                       select = NULL),
                                           bsPopover(id = "helpse", 
                                                     title = "Epidemic start",
                                                     content = "Chose a week to force the start of the epidemic period.<br>The epidemic will start at the week selected and not at the first week over the epidemic threshold.",
                                                     placement = "top", 
                                                     trigger = "hover", 
                                                     options = list(container = "body"))
                                       ),
                                       
                                       ################################
                                       ###    Visualize            ####
                                       ################################
                                       
                                       box(title="Visualize", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                           selectInput('SelectSeasons', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                                           bsButton("helpvs", label = "", 
                                                                                    icon = icon("question"), 
                                                                                    style = "info", size = "extra-small"), "Seasons to graph"),
                                                       choices = NULL, multiple = TRUE),
                                           bsPopover(id = "helpvs", title = "Multiple seasons",
                                                     content = "Select any number of seasons to display series, seasons and timing graphs and to apply thresholds from the current model.<br>To delete a season click on it and press delete on your keyboard.",
                                                     placement = "right", 
                                                     trigger = "hover", 
                                                     options = list(container = "body"))
                                       ),
                                       
                                       ################################
                                       ###  Thresholds             ####
                                       ################################
                                       
                                       box(title="Thresholds", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                           checkboxInput("preepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helptp", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Pre-epidemic threshold"), value = TRUE),
                                           bsPopover(id = "helptp", title = "Pre-epidemic threshold",
                                                     content = "Check this tickbox if you want to include epidemic thresholds in the graphs.<br>This is a global option that will work on most graphs.",
                                                     placement = "right", 
                                                     trigger = "hover", 
                                                     options = list(container = "body")),
                                           checkboxInput("postepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpto", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Post-epidemic threshold"), value = FALSE),
                                           bsPopover(id = "helpto", title = "Post-epidemic threshold",
                                                     content = "Check this tickbox if you want to include post-epidemic thresholds in the graphs.<br>This  is a global option that will work on most graphs.",
                                                     placement = "right", 
                                                     trigger = "hover", 
                                                     options = list(container = "body")),
                                           checkboxInput("intensitythr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpti", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Intensity thresholds/levels"), value = TRUE),
                                           bsPopover(id = "helpti", title = "Intensity thresholds",
                                                     content = "Check this tickbox if you want to include intensity thresholds in the graphs.<br>This  is a global option that will work on most graphs.",
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
                            
                            column(9,
                                   
                                   #######################################
                                   ### BODY/MAIN SECTION               ###
                                   ###   FIRST COLUMN DEFINITION       ###
                                   ###      FIRST PART: OUTPUTS        ###
                                   #######################################
                                   
                                   tabBox(
                                     title = "MEM", width = 12, height = "800px",
                                     tabPanel("Check & describe", "Check data series, timing and describe the data", uiOutput("tbData")),
                                     tabPanel("Model", "Summary, graphs, goodness and optimization of the MEM model", uiOutput("tbModel")),
                                     tabPanel("Surveillance", "Surveillance tools", uiOutput("tbSurveillance")),
                                     tabPanel("Visualize", "Visualize different sets of data with a MEM model", uiOutput("tbVisualize"))
                                   ),
                                   
                                   #######################################
                                   ### BODY/MAIN SECTION               ###
                                   ###   FIRST COLUMN DEFINITION       ###
                                   ###      SECOND PART: MEM OPTIONS   ###
                                   #######################################
                                   
                                   box(title="MEM options", 
                                       status = "primary", 
                                       solidHeader = TRUE, 
                                       width = 12,  
                                       background = "black",
                                       collapsible = TRUE, 
                                       collapsed=TRUE,
                                       fluidRow(
                                         column(6,
                                                h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Timing"),
                                                column(8, 
                                                       selectInput("method", h6("Method for timing", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpotim", label = "", icon = icon("question"), style = "info", size = "extra-small")), width = '230px', choices = list("Original method"=1, "Fixed criterium method"=2, "Slope method"=3, "Second derivative method"=4), size=1, selectize = FALSE, selected = 2),
                                                       bsPopover(id = "helpotim", title = "Method for epidemic timing", content = "Original: uses the process shown in the original paper.<br>Fixed criterium: uses the slope of the MAP curve fo find the optimum, which is the point where the slope is lower than a predefined value.<br>Slope: calculates the slope of the MAP curve, but the optimum is the one that matches the global mean slope.<br>Second derivative: calculates the second derivative and equals to zero to search an inflexion point in the original curve.", placement = "right", trigger = "hover", options = list(container = "body"))
                                                ),
                                                column(4,
                                                       conditionalPanel(
                                                         condition = "input.method == 2",
                                                         numericInput("param", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpopar", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Slope parameter"), 2.8, step=0.1),
                                                         bsPopover(id = "helpopar", title = "Window parameter", content = "Window parameter used in fixed criterium method.", placement = "top", trigger = "hover", options = list(container = "body"))
                                                       )
                                                )
                                         ),
                                         column(6,
                                                h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Goodness & optimize"),
                                                sliderInput("paramrange", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpoopt", label = "", icon = icon("question"), style = "info", size = "extra-small"), "Parameter range"), min = 0.1, max = 10, value = c(2, 4), step=0.1),
                                                bsPopover(id = "helpoopt", title = "Window parameter range", content = "Range of possible of values of the window parameter used by goodness and optimize functions.", placement = "top", trigger = "hover", options = list(container = "body"))
                                                )
                                       ),
                                       h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Thresholds"),
                                       fluidRow(
                                         column(3, 
                                                selectInput("type.threshold", h6("Epidemic thr.", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpoepi", label = "", icon = icon("question"), style = "info", size = "extra-small")), width = '420px', choices = list("Arithmetic mean and mean confidence interval"=1, "Geometric mean and mean confidence interval"=2, "Median and the KC Method to calculate its confidence interval"=3, "Median and bootstrap confidence interval"=4, "Arithmetic mean and point confidence interval"=5, "Geometric mean and point confidence interval"=6), size=1, selectize = FALSE, selected = 5),
                                                bsPopover(id = "helpoepi", title = "Epidemic threshold", content = "Method for calculating the epidemic threshold.", placement = "right", trigger = "hover", options = list(container = "body"))
                                                ),
                                         column(3, 
                                                selectInput("type.intensity", h6("intensity thr.", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpoint", label = "", icon = icon("question"), style = "info", size = "extra-small")), width = '420px', choices = list("Arithmetic mean and mean confidence interval"=1, "Geometric mean and mean confidence interval"=2, "Median and the KC Method to calculate its confidence interval"=3, "Median and bootstrap confidence interval"=4, "Arithmetic mean and point confidence interval"=5, "Geometric mean and point confidence interval"=6), size=1, selectize = FALSE, selected = 6),
                                                bsPopover(id = "helpoint", title = "Intensity thresholds", content = "Method for calculating the intensity threshold.", placement = "right", trigger = "hover", options = list(container = "body"))
                                                ),
                                         column(3, 
                                                selectInput("type.curve", h6("Curve thr.", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpotyp", label = "", icon = icon("question"), style = "info", size = "extra-small")), width = '420px', choices = list("Arithmetic mean and mean confidence interval"=1, "Geometric mean and mean confidence interval"=2, "Median and the KC Method to calculate its confidence interval"=3, "Median and bootstrap confidence interval"=4, "Arithmetic mean and point confidence interval"=5, "Geometric mean and point confidence interval"=6), size=1, selectize = FALSE, selected = 2),
                                                bsPopover(id = "helpotyp", title = "Average curve intervals", content = "Method for calculating the average/typical curve confidence intervals.", placement = "right", trigger = "hover", options = list(container = "body"))
                                                ),
                                         column(3, 
                                                selectInput("type.other", h6("Other thr.", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpooth", label = "", icon = icon("question"), style = "info", size = "extra-small")), width = '420px', choices = list("Arithmetic mean and mean confidence interval"=1, "Geometric mean and mean confidence interval"=2, "Median and the KC Method to calculate its confidence interval"=3, "Median and bootstrap confidence interval"=4, "Arithmetic mean and point confidence interval"=5, "Geometric mean and point confidence interval"=6), size=1, selectize = FALSE, selected = 3),
                                                bsPopover(id = "helpooth", title = "Otrher confidence intervals", content = "Method for calculating other confidence intervals: duration, epidemic percentage, epidemic start, etc.", placement = "right", trigger = "hover", options = list(container = "body"))
                                                )
                                         ),
                                       fluidRow(
                                         column(2, numericInput("n.max", h6("Values per season", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpoval", label = "", icon = icon("question"), style = "info", size = "extra-small")), -1, step=1, min = -1, max = 100)),
                                         bsPopover(id = "helpoval", title = "Number of values per seasons", content = "Number of values taken each season for calculate thresholds. If -1, a total of 30 points are used (30/numberofseasons). If 0, all available points are used.", placement = "right", trigger = "hover", options = list(container = "body")),
                                         column(2, numericInput("level.intensity.m", h6("Level medium", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpomed", label = "", icon = icon("question"), style = "info", size = "extra-small")), 40, step=0.5, min = 0.5, max = 99.5)),
                                         bsPopover(id = "helpomed", title = "Medium intensity threshold", content = "Level of the confidence interval used to calculate the medium threshold.", placement = "right", trigger = "hover", options = list(container = "body")),
                                         column(2, numericInput("level.intensity.h", h6("Level high", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpohig", label = "", icon = icon("question"), style = "info", size = "extra-small")), 90, step=0.5, min = 0.5, max = 99.5)),
                                         bsPopover(id = "helpohig", title = "High intensity threshold", content = "Level of the confidence interval used to calculate the high threshold.", placement = "right", trigger = "hover", options = list(container = "body")),
                                         column(2, numericInput("level.intensity.v", h6("Level very high", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpover", label = "", icon = icon("question"), style = "info", size = "extra-small")), 97.5, step=0.5, min = 0.5, max = 99.5)),
                                         bsPopover(id = "helpover", title = "Very high intensity threshold", content = "Level of the confidence interval used to calculate the very high threshold.", placement = "right", trigger = "hover", options = list(container = "body")),
                                         column(2, numericInput("tails", h6("Tails", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpotai", label = "", icon = icon("question"), style = "info", size = "extra-small")), 1, step=1, min = 1, max = 2))),
                                         bsPopover(id = "helpotai", title = "One or two-tailed confidence intervals", content = "Choose if you want to use one-tailed or two-tailed confidence intervals for thresholds.", placement = "right", trigger = "hover", options = list(container = "body"))
                                       )
                                   ),
                            
                            ###################################
                            ### BODY/MAIN SECTION           ###
                            ###   SECOND COLUMN DEFINITION  ###
                            ###################################
                            
                            column(3,
                                   box(
                                     title="Graph text", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                     textInput("textMain", label = h6("Main title", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgmai", label = "", icon = icon("question"), style = "info", size = "extra-small")), value = "Main title"),
                                     bsPopover(id = "helpgmai", title = "Main title", content = "Change the main title in most graphs.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     textInput("textY", label = h6("Y-axis", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgxla", label = "", icon = icon("question"), style = "info", size = "extra-small")), value = "Y-axis"),
                                     bsPopover(id = "helpgxla", title = "Y-axis label", content = "Change the y-axis label in most graphs.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     textInput("textX", label = h6("X-axis", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgyla", label = "", icon = icon("question"), style = "info", size = "extra-small")), value = "X-axis"),
                                     bsPopover(id = "helpgyla", title = "X-axis label", content = "Change the x-axis label in most graphs.", placement = "right", trigger = "hover", options = list(container = "body"))
                                   ),
                                   box(
                                     title="Graph options", status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                     selectInput("colObservedLines", h6("Observed (line)", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpglin", label = "", icon = icon("question"), style = "info", size = "extra-small")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "helpglin", title = "Observed (line)", content = "Color of the line of observed data.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     selectInput("colObservedPoints", h6("Observed (points)", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgpoi", label = "", icon = icon("question"), style = "info", size = "extra-small")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "helpgpoi", title = "Observed (points)", content = "Color of the points of observed data.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     selectInput("colEpidemicStart", h6("Epidemic start", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgsta", label = "", icon = icon("question"), style = "info", size = "extra-small")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "helpgsta", title = "Epidemic start", content = "Color of the point of the epidemic start marker.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     selectInput("colEpidemicStop", h6("Epidemic end", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgsto", label = "", icon = icon("question"), style = "info", size = "extra-small")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "helpgsto", title = "Epidemic end", content = "Color of the point of the epidemic end marker.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     selectInput("colThresholds", h6("Thresholds palette", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgthr", label = "", icon = icon("question"), style = "info", size = "extra-small")), choices = c("default",rownames(brewer.pal.info)), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "helpgthr", title = "Thresholds palette", content = "Palette used to generate color for epidemic and intensity thresholds.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     selectInput("colSeries", h6("Series palette", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgser", label = "", icon = icon("question"), style = "info", size = "extra-small")), choices = c("default",rownames(brewer.pal.info)), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "helpgser", title = "Series palette", content = "Palette used to generate the colors of the lines of the series graphs and other graphs with multiple lines.", placement = "right", trigger = "hover", options = list(container = "body")),
                                     selectInput("colEpidemic", h6("Timing palette", tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("helpgepi", label = "", icon = icon("question"), style = "info", size = "extra-small")), choices = c("default",rownames(brewer.pal.info)), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "helpgepi", title = "Timing palette", content = "Palette used to generate the colors of the points of pre, epidemic and post markers in timing graphs.", placement = "right", trigger = "hover", options = list(container = "body"))
                                     )
                                   )
                            )
                          )
                        )
                      )
)

