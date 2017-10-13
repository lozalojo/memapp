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

load("lang/translation.bin")
# print(translation)
source("helpers.R")
transformation.list<-list("No transformation"=1, "Odd"=2, "Fill missings"=3, "Loess"=4, "Two waves (observed)"=5, "Two waves (expected)"=6)
names(transformation.list)<-tr(c("No transformation", "Odd", "Fill missings", "Loess", "Two waves (observed)", "Two waves (expected)"))
method.list<-list("Original method"=1, "Fixed criterium method"=2, "Slope method"=3, "Second derivative method"=4)
names(method.list)<-tr(c("Original method", "Fixed criterium method", "Slope method", "Second derivative method"))
nvalues.list<-list("30 in total"=-1,"All"=0,"1/season"=1,"2/season"=2,"3/season"=3,"4/season"=4,"5/season"=5,"6/season"=6,"7/season"=7,"8/season"=8,"9/season"=9,"10/season"=10)
names(nvalues.list)<-tr(c("30 in total","All","1/season","2/season","3/season","4/season","5/season","6/season","7/season","8/season","9/season","10/season"))
validation.list<-list("Cross"="cross", "Sequential"="sequential")
names(validation.list)<-tr(c("Cross", "Sequential"))
optimmethod.list<-list("Positive likehood"="pos.likehood", "Negative likehood"="neg.likehood", "Aditive"="aditive", "Multiplicative"="multiplicative", "Mixed"="mixed", "Percent agreement"="percent","Matthews Correlation Coefficient"="matthews","Youden's Index"="youden")
names(optimmethod.list)<-tr(c("Positive likehood", "Negative likehood", "Aditive", "Multiplicative", "Mixed", "Percent agreement","Matthews Correlation Coefficient","Youden's Index"))
type.list<-list("Arithmetic mean and mean confidence interval"=1, "Geometric mean and mean confidence interval"=2, "Median and the KC Method to calculate its confidence interval"=3, "Median and bootstrap confidence interval"=4, "Arithmetic mean and point confidence interval"=5, "Geometric mean and point confidence interval"=6)
names(type.list)<-tr(c("Arithmetic mean and mean confidence interval", "Geometric mean and mean confidence interval", "Median and the KC Method to calculate its confidence interval", "Median and bootstrap confidence interval", "Arithmetic mean and point confidence interval", "Geometric mean and point confidence interval"))

shinyUI(dashboardPage(skin = "black",
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
                                       fileInput('file', label=h4(tr.item("Load file"), tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("file_b", label = "", icon = icon("question"), style = "info", size = "extra-small")), accept = c("csv","dat","prn","txt","xls","xlsx","mdb","accdb", "rdata")),
                                       bsPopover(id = "file_b", title = tr.item("Load file"),      content = "memapp is able to read text, excel, access and R.", placement = "right", trigger = "hover", options = list(container = "body")),
                                        box(title=tr.item("Dataset"), status = "warning", solidHeader = FALSE, width = 12, background = "navy", collapsible = TRUE, collapsed=TRUE,
                                           selectInput('dataset', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Dataset")), size=1, selectize = FALSE, choices = "", selected = NULL),
                                           bsPopover(id = "dataset", title = tr.item("Dataset"), content = "If the format is able to store different datasets, select the one you want to open.", placement = "right", trigger = "hover", options = list(container = "body")),
                                           selectInput("firstWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("First Week")), size=1, selectize = FALSE, choices = "", selected = NULL),
                                           bsPopover(id = "firstWeek", title = tr.item("First Week"), content = "First week of the datasets` surveillance period.",                                    placement = "right", trigger = "hover", options = list(container = "body")),
                                           selectInput("lastWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Last Week")), size=1, selectize = FALSE, choices = "", selected = NULL),
                                           bsPopover(id = "lastWeek", title = tr.item("Last Week"), content = "Last week of the datasets surveillance period.",                                     placement = "right", trigger = "hover", options = list(container = "body")),
                                           selectInput("transformation", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Transform")), size=1, selectize = FALSE, choices = transformation.list, selected = 1),
                                           bsPopover(id = "transformation", title = tr.item("Transform"), content = "Select the transformation to apply to the original data.",                            placement = "right", trigger = "hover", options = list(container = "body"))
                                       ),
                                       ################################
                                       ###    Model                ####
                                       ################################
                                       box(title=tr.item("Model"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                           selectInput("SelectFrom", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("From")), size=1, selectize = FALSE, choices = ""),
                                           bsPopover(id = "SelectFrom", title = tr.item("From"), content = "First column to include in the model selection.", placement = "right", trigger = "hover", options = list(container = "body")),
                                           selectInput("SelectTo", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("To")), size=1, selectize = FALSE, choices = ""),
                                           bsPopover(id = "SelectTo", title = tr.item("To"), content = "Last column to include in the model selection.", placement = "right", trigger = "hover", options = list(container = "body")),
                                           selectInput('SelectExclude', h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Exclude")), multiple = TRUE, choices = NULL),
                                           bsPopover(id = "SelectExclude", title = tr.item("Exclude"), content = "Select any number of seasons to be excluded from the model.", placement = "right", trigger = "hover", options = list(container = "body")),
                                           numericInput("SelectMaximum", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Maximum seasons")), 10, step=1),
                                           bsPopover(id = "SelectMaximum", title = tr.item("Maximum seasons"), content = "Maximum number of seasons to be used in the model.<br>Note that this will probably override the rest options, since it will restrict data to the last number of seasons from the selection already made with From/To/Exclude.<br>For influenza it is not recommended to use more than 10 seasons to avoid cyclical trends.", placement = "right", trigger = "hover", options = list(container = "body"))
                                       ),
                                       
                                       ################################
                                       ###    Surveillance         ####
                                       ################################
                                       
                                       box(title=tr.item("Surveillance"), status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed=TRUE,
                                           selectInput("SelectSurveillance", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Season")), size=1, selectize = FALSE, choices = ""),
                                           bsPopover(id = "SelectSurveillance", title = tr.item("Season"), content = "Season you want to use for surveillance applying the MEM thresholds.<br>This season can be incomplete.<br> It is recommended not to use the surveillance season in the model selection.", placement = "right", trigger = "hover", options = list(container = "body")),
                                           selectInput("SelectSurveillanceWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Week")), size=1, selectize = FALSE, choices = ""),
                                           bsPopover(id = "SelectSurveillanceWeek", title = tr.item("Week"), content = "Week you want to create the surveillance graph for. It can be any week from the first week of the surveillance season to the last one that have data", placement = "right", trigger = "hover", options = list(container = "body")),
                                           selectInput("SelectSurveillanceForceEpidemic", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Force epidemic start")), size=1, selectize = FALSE, choices = NULL, select = NULL),
                                           bsPopover(id = "SelectSurveillanceForceEpidemic", title = tr.item("Force epidemic start"), content = "Chose a week to force the start of the epidemic period.<br>The epidemic will start at the week selected and not at the first week over the epidemic threshold.", placement = "right", trigger = "hover", options = list(container = "body"))
                                       ),
                                       
                                       ################################
                                       ###    Visualize            ####
                                       ################################
                                       
                                       box(title=tr.item("Visualize"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                           selectInput('SelectSeasons', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Seasons")), choices = NULL, multiple = TRUE),
                                           bsPopover(id = "SelectSeasons", title = tr.item("Seasons"), content = "Select any number of seasons to display series, seasons and timing graphs and to apply thresholds from the current model.<br>To delete a season click on it and press delete on your keyboard.", placement = "right", trigger = "hover", options = list(container = "body"))
                                       ),
                                       
                                       ################################
                                       ###  Thresholds             ####
                                       ################################
                                       
                                       box(title=tr.item("Thresholds"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                           checkboxInput("preepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Pre-epidemic threshold")), value = TRUE),
                                           bsPopover(id = "preepidemicthr", title = tr.item("Pre-epidemic threshold"), content = "Check this tickbox if you want to include epidemic thresholds in the graphs.<br>This is a global option that will work on most graphs.", placement = "right", trigger = "hover", options = list(container = "body")),
                                           checkboxInput("postepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Post-epidemic threshold")), value = FALSE),
                                           bsPopover(id = "postepidemicthr", title = tr.item("Post-epidemic threshold"), content = "Check this tickbox if you want to include post-epidemic thresholds in the graphs.<br>This  is a global option that will work on most graphs.", placement = "right", trigger = "hover", options = list(container = "body")),
                                           checkboxInput("intensitythr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Intensity thresholds/levels")), value = TRUE),
                                           bsPopover(id = "intensitythr", title = tr.item("Intensity thresholds/levels"), content = "Check this tickbox if you want to include intensity thresholds in the graphs.<br>This  is a global option that will work on most graphs.", placement = "right", trigger = "hover", options = list(container = "body"))
                                       )
                                       
                      ),
                      
                      ###################################
                      ### BODY/MAIN SECTION           ###
                      ###################################
                      
                      dashboardBody(
                        # All credits of the busy-indicator goes to AnalytixWare/ShinySky, I was forced to include the code
                        # here because the package does not have a CRAN release.
                        tags$body(inlineCSS(list(".shinysky-busy-indicator" = "position: absolute !important; z-index:800; "))),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                        fluidPage(
                          # Application title
                          titlePanel(h1(tr.item("The Moving Epidemic Method Web Application"))),
                          tagList(
                            singleton(tags$head(
                              tags$link(rel="stylesheet", type="text/css",href="busyIndicator.css")
                            ))
                            ,div(class="shinysky-busy-indicator",p("Calculation in progress. This may take a while..."),img(src="ajaxloaderq.gif"))
                            ,tags$script(sprintf(
                              "	setInterval(function(){
                              if ($('html').hasClass('shiny-busy')) {
                              setTimeout(function() {
                              if ($('html').hasClass('shiny-busy')) {
                              $('div.shinysky-busy-indicator').show()
                              }
                              }, %d)  		    
                              } else {
                              $('div.shinysky-busy-indicator').hide()
                              }
                              },100)
                              ",500)
                            )
                          ),
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
                                     title = h3(tr.item("Procedures"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), width = 12, height = "800px",
                                     tabPanel(h4(tr.item("Check & describe"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), "Check data series, timing and describe the data", uiOutput("tbData")),
                                     tabPanel(h4(tr.item("Model"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), "Summary, graphs, goodness and optimization of the MEM model", uiOutput("tbModel")),
                                     tabPanel(h4(tr.item("Surveillance"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), "Surveillance tools", uiOutput("tbSurveillance")),
                                     tabPanel(h4(tr.item("Visualize"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), "Visualize different sets of data with a MEM model", uiOutput("tbVisualize"))
                                   )
                            ),
                            ###################################
                            ### BODY/MAIN SECTION           ###
                            ###   SECOND COLUMN DEFINITION  ###
                            ###################################
                            column(3,
                                   box(
                                     title=tr.item("Text options"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                     textInput("textMain", label = h6(tr.item("Main title"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = tr.item("Main title")),
                                     bsPopover(id = "textMain", title = tr.item("Main title"), content = "Change the main title in most graphs.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     textInput("textY", label = h6(tr.item("Y-axis"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = tr.item("Y-axis")),
                                     bsPopover(id = "textY", title = tr.item("Y-axis"), content = "Change the y-axis label in most graphs.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     textInput("textX", label = h6(tr.item("X-axis"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = tr.item("X-axis")),
                                     bsPopover(id = "textX", title = tr.item("X-axis"), content = "Change the x-axis label in most graphs.", placement = "left", trigger = "hover", options = list(container = "body"))
                                   ),
                                   box(
                                     title=tr.item("Graph options"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                     selectInput("colObservedLines", h6(tr.item("Observed (line)"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "colObservedLines", title = tr.item("Observed (line)"), content = "Color of the line of observed data.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("colObservedPoints", h6(tr.item("Observed (points)"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "colObservedPoints", title = tr.item("Observed (points)"), content = "Color of the points of observed data.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("colEpidemicStart", h6(tr.item("Epidemic start"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "colEpidemicStart", title = tr.item("Epidemic start"), content = "Color of the point of the epidemic start marker.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("colEpidemicStop", h6(tr.item("Epidemic end"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "colEpidemicStop", title = tr.item("Epidemic end"), content = "Color of the point of the epidemic end marker.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("colThresholds", h6(tr.item("Thresholds palette"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",rownames(brewer.pal.info),colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "colThresholds", title = tr.item("Thresholds palette"), content = "Palette used to generate color for epidemic and intensity thresholds.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("colSeasons", h6(tr.item("Seasons palette"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",rownames(brewer.pal.info),colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "colSeasons", title = tr.item("Seasons palette"), content = "Palette used to generate the colors of the lines of the series graphs and other graphs with multiple lines.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("colEpidemic", h6(tr.item("Timing palette"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",rownames(brewer.pal.info),colors()), size=1, selectize = FALSE, selected = "default"),
                                     bsPopover(id = "colEpidemic", title = tr.item("Timing palette"), content = "Palette used to generate the colors of the points of pre, epidemic and post markers in timing graphs.", placement = "left", trigger = "hover", options = list(container = "body"))
                                   ),
                                   box(
                                     title=tr.item("MEM options"), status = "danger", solidHeader = FALSE, width = 12,  background = "navy", collapsible = TRUE, collapsed=TRUE,
                                     h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Timing")),
                                     selectInput("method", h6(tr.item("Method for timing"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = method.list, size=1, selectize = FALSE, selected = 2),
                                     bsPopover(id = "method", title = "Method for epidemic timing", content = "Original: uses the process shown in the original paper.<br>Fixed criterium: uses the slope of the MAP curve fo find the optimum, which is the point where the slope is lower than a predefined value.<br>Slope: calculates the slope of the MAP curve, but the optimum is the one that matches the global mean slope.<br>Second derivative: calculates the second derivative and equals to zero to search an inflexion point in the original curve.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     conditionalPanel(condition = "input.method == 2", 
                                                      numericInput("param", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Slope parameter")), 2.8, step=0.1),
                                                      bsPopover(id = "param", title = "Window parameter", content = "Window parameter used in fixed criterium method.", placement = "left", trigger = "hover", options = list(container = "body"))),
                                     h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Thresholds"),
                                     fluidRow(
                                       column(6,
                                              selectInput("nvalues", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Values per season")), choices = nvalues.list, size=1, selectize = FALSE, selected = -1),
                                              bsPopover(id = "nvalues", title = "Number of values per seasons", content = "Number of values taken each season for calculate thresholds. If -1, a total of 30 points are used (30/numberofseasons). If 0, all available points are used.", placement = "left", trigger = "hover", options = list(container = "body"))
                                       ),
                                       column(6,
                                              numericInput("ntails", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Tails")), 1, step=1, min = 1, max = 2),
                                              bsPopover(id = "ntails", title = "Confidence intervals tails", content = "Choose if you want to use one-tailed or two-tailed confidence intervals for thresholds.", placement = "left", trigger = "hover", options = list(container = "body"))
                                       )
                                     ),
                                     selectInput("typethreshold", h6(tr.item("Epidemic threshold"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 5),
                                     bsPopover(id = "typethreshold", title = tr.item("Epidemic threshold"), content = "Method for calculating the epidemic threshold.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("typeintensity", h6("Intensity thresholds", tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 6),
                                     bsPopover(id = "typeintensity", title = "Intensity thresholds", content = "Method for calculating the intensity threshold.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     fluidRow(
                                       column(4,
                                              numericInput("levelintensitym", h6(tr.item("Medium lvl"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 40, step=0.5, min = 0.5, max = 99.5),
                                              bsPopover(id = "levelintensitym", title = "Medium intensity threshold", content = "Level of the confidence interval used to calculate the medium threshold.", placement = "left", trigger = "hover", options = list(container = "body"))
                                       ),
                                       column(4,
                                              numericInput("levelintensityh", h6(tr.item("High lvl"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 90, step=0.5, min = 0.5, max = 99.5),
                                              bsPopover(id = "levelintensityh", title = "High intensity threshold", content = "Level of the confidence interval used to calculate the high threshold.", placement = "left", trigger = "hover", options = list(container = "body"))
                                       ),
                                       column(4,
                                              numericInput("levelintensityv", h6(tr.item("Very high lvl"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 97.5, step=0.5, min = 0.5, max = 99.5),
                                              bsPopover(id = "levelintensityv", title = "Very high intensity threshold", content = "Level of the confidence interval used to calculate the very high threshold.", placement = "left", trigger = "hover", options = list(container = "body"))
                                       )
                                     ),
                                     h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Goodness & optimize")),
                                     fluidRow(
                                       column(6,
                                              selectInput("validation", h6(tr.item("Validation"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = validation.list, size=1, selectize = FALSE, selected = "cross"),
                                              bsPopover(id = "validation", title = "Method for validation", content = "Cross: Extracts one season and the model is calculated with the remaining seasons.<br>Sequential: Extract a season and the model is calculated with previous seasons only.", placement = "left", trigger = "hover", options = list(container = "body"))
                                       ),
                                       column(6,
                                              selectInput("optimmethod", h6(tr.item("Optimization method"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = optimmethod.list, size=1, selectize = FALSE, selected = "matthews"),
                                              bsPopover(id = "optimmethod", title = tr.item("Optimization method"), content = "Method to choose the optimum parameter.", placement = "left", trigger = "hover", options = list(container = "body"))
                                       )
                                     ),
                                     sliderInput("paramrange", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Parameter range")), min = 0.1, max = 10, value = c(2, 4), step=0.1),
                                     bsPopover(id = "paramrange", title = "Window parameter range", content = "Range of possible of values of the window parameter used by goodness and optimize functions.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Other")),
                                     selectInput("typecurve", h6(tr.item("Average curve CI."), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 2),
                                     bsPopover(id = "typecurve", title = "Average curve intervals", content = "Method for calculating the average curve confidence intervals.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     selectInput("typeother", h6(tr.item("Other CI."), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 3),
                                     bsPopover(id = "typeother", title = "Other confidence intervals", content = "Method for calculating other confidence intervals: duration, epidemic percentage, epidemic start, etc.", placement = "left", trigger = "hover", options = list(container = "body")),
                                     numericInput("levelaveragecurve", h6(tr.item("Average curve/Other CI. level"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 95.0, step=0.5, min = 0.5, max = 99.5),
                                     bsPopover(id = "levelaveragecurve", title = "Average curve intervals", content = "Level of the confidence interval used to calculate the average curve and other intervals.", placement = "left", trigger = "hover", options = list(container = "body"))
                                   ),
                                   shinydashboard::box(
                                     title=tr.item("Support"), status = "info", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
                                     #h5(a("Surveillance guidelines", href="NULL", target="_blank")),
                                     h5(a(tr.item("Technical manual"), href="https://drive.google.com/file/d/0B0IUo_0NhTOoX29zc2p5RmlBUWc/view?usp=sharing", target="_blank")),
                                     h5(a(tr.item("Submit issues"), href="https://github.com/lozalojo/memapp/issues", target="_blank")),
                                     checkboxInput("advancedfeatures", label = h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), tr.item("Show advanced features")), value = FALSE),
                                     bsPopover(id = "advancedfeatures", title = "Advanced features", content = "Show advanced features of memapp.", placement = "right", trigger = "hover", options = list(container = "body"))
                                   )
                            )
                          )
                        )
                      )
)
)

