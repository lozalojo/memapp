library(shiny)

shinyServer(function(input, output, session) {
  
data_read <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()}
  else
  #dat <- as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  #dat <- as.data.frame(dat)
  fileextension<-str_match(file1$name,"^(.*)\\.([^\\.]*)$")[3]
  if (!(fileextension %in% c("csv","dat","prn","txt","xls","xlsx"))) return()
  dat<-read.data(i.file=file1$datapath, i.extension=fileextension)
  dat$vecka<-rownames(dat)
  dat<-dat[c(NCOL(dat),1:(NCOL(dat)-1))]
  dat$num <- 1:nrow(dat)
  dat2 <- as.data.frame(apply(dat, 2, function(x) as.numeric(x)))
  rownames(dat2) <- rownames(dat)

  datanames<-names(dat2)[!(names(dat2) %in% c("vecka","num"))]  
  
  lapply(datanames, function(s){output[[paste0("tbdTiming_",s)]] <- renderPlot({plotTiming(s)})})
  lapply(datanames, function(s){output[[paste0("tbmTiming_",s)]] <- renderPlot({plotTiming(s)})})
  lapply(datanames, function(s){output[[paste0("tbvTiming_",s)]] <- renderPlot({plotTiming(s)})})

  dat2
})

data_model <- reactive({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from=input$SelectFrom, 
                                  i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude,
                                  i.include="",
                                  #i.pandemic=as.logical(input$SelectPandemic),
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  #cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  # cat("i.from:->",input$SelectFrom,"<-\n",sep="")
  # cat("i.to:->",input$SelectTo,"<-\n",sep="")
  # cat("i.exclude:->",input$SelectExclude,"<-\n",sep="")
  # cat("i.include:->",as.character(c(input$K,input$K4)),"<-\n",sep="")
  # cat("i.pandemic:->",as.logical(input$SelectPandemic),"<-\n",sep="")
  # cat("i.seasons:->",input$SelectSeasons,"<-\n",sep="")
  # cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  if (length(selectedcolumns)<2){return()}
  epi <- memmodel(datfile[selectedcolumns],
                  i.type.threshold=as.numeric(input$i.type.threshold),
                  i.type.intensity=as.numeric(input$i.type.intensity), 
                  i.method = as.numeric(input$i.method),
                  i.param = as.numeric(input$memparameter), i.seasons = NA)
  epi
})


data_good <- reactive({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  if (length(selectedcolumns)<3){return()}
  
  good<-memgoodness(datfile[,selectedcolumns],
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter),i.graph=F, i.seasons=NA, i.min.seasons = length(selectedcolumns))
  good
})

data_optim <- reactive({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  if (length(selectedcolumns)<3){return()}
  roca<-roc.analysis(datfile[,selectedcolumns],
                     i.param.values = seq(2, 3, 0.1), 
                     i.graph.file = F,
                     i.type.threshold=as.numeric(input$i.type.threshold),
                     i.type.intensity=as.numeric(input$i.type.intensity), 
                     i.seasons=NA, 
                     i.min.seasons = length(selectedcolumns))
  roca
})

observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile)) return(NULL)
  #dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  fileextension<-str_match(file1$name,"^(.*)\\.([^\\.]*)$")[3]
  if (!(fileextension %in% c("csv","dat","prn","txt","xls","xlsx"))) return(NULL)
  dt<-read.data(i.file=file1$datapath, i.extension=fileextension)
  dt$vecka<-rownames(dt)
  dt<-dt[c(NCOL(dt),1:(NCOL(dt)-1))]  
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K", choices = names(dt)[2:ncol(dt)])
})

observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile)) return(NULL)
  #dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  fileextension<-str_match(file1$name,"^(.*)\\.([^\\.]*)$")[3]
  if (!(fileextension %in% c("csv","dat","prn","txt","xls","xlsx"))) return(NULL)
  dt<-read.data(i.file=file1$datapath, i.extension=fileextension)
  dt$vecka<-rownames(dt)
  dt<-dt[c(NCOL(dt),1:(NCOL(dt)-1))]  
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K2", choices = names(dt)[2:ncol(dt)])
})

observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile)) return(NULL)
  #dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  fileextension<-str_match(file1$name,"^(.*)\\.([^\\.]*)$")[3]
  if (!(fileextension %in% c("csv","dat","prn","txt","xls","xlsx"))) return(NULL)
  dt<-read.data(i.file=file1$datapath, i.extension=fileextension)
  dt$vecka<-rownames(dt)
  dt<-dt[c(NCOL(dt),1:(NCOL(dt)-1))]  
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K3", choices = names(dt)[2:ncol(dt)])
})

observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile)) return(NULL)
  #dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  fileextension<-str_match(file1$name,"^(.*)\\.([^\\.]*)$")[3]
  if (!(fileextension %in% c("csv","dat","prn","txt","xls","xlsx"))) return(NULL)
  dt<-read.data(i.file=file1$datapath, i.extension=fileextension)
  dt$vecka<-rownames(dt)
  dt<-dt[c(NCOL(dt),1:(NCOL(dt)-1))]  
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K4", choices = names(dt)[2:ncol(dt)])
})

observe({
  file1 <- input$file
  inFile <- input$file
  if(is.null(inFile)) return(NULL)
  fileextension<-str_match(file1$name,"^(.*)\\.([^\\.]*)$")[3]
  if (!(fileextension %in% c("csv","dat","prn","txt","xls","xlsx"))) return(NULL)
  dt<-read.data(i.file=file1$datapath, i.extension=fileextension)
  dt$vecka<-rownames(dt)
  dt<-dt[c(NCOL(dt),1:(NCOL(dt)-1))]
  seasons<-names(dt)[2:ncol(dt)]
  weeks<-rownames(dt)
  updateSelectInput(session, "SelectFrom", choices = seasons, selected=seasons[1])
  updateSelectInput(session, "SelectTo", choices = seasons, selected=rev(seasons)[min(2,length(seasons))])
  updateSelectInput(session, "SelectExclude", choices = seasons, selected=NULL)
  updateSelectInput(session, "SelectSurveillance", choices = seasons, selected=rev(seasons)[1])
  updateSelectInput(session, "SelectSurveillanceWeek", choices =weeks, selected=rev(weeks)[1])
  updateSelectInput(session, "SelectSeasons", choices = seasons, selected=NULL)
})

# Define main output structure
# output$tb <- renderUI({
#   if(is.null(data_read())){return()}
#   else
#     tabsetPanel(tabPanel("File name", DT::dataTableOutput("filedf")),
#                 tabPanel("Data", DT::dataTableOutput("table")),
#                 tabPanel("Plot", plotlyOutput("distPlot")),
#                 tabPanel("Seasons", plotlyOutput("distSeasons")),
#                 tabPanel("MEM", verbatimTextOutput("memdf")),
#                 tabPanel("Timing",plotOutput("distPlot2")),
#                 tabPanel("Series",plotOutput("distSeries")),
#                 tabPanel("Surveillance",plotOutput("distSurveillance")),
#                 tabPanel("Animated",imageOutput("distAnimated")),
#                 tabPanel("Goodness",DT::dataTableOutput("tableGoodness")),
#                 tabPanel("Optimize",DT::dataTableOutput("tableOptimize"))
#                 )
# })

#####################################
### DEFINING TABS STRUCTURE
#####################################

output$tbData <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("File", tableOutput("tbdFile")),
                tabPanel("Data", DT::dataTableOutput("tbdData")),
                #tabPanel("Plot", plotlyOutput("tbdPlot")),
                tabPanel("Seasons", plotlyOutput("tbdSeasons")),
                tabPanel("Series",plotOutput("tbdSeries")),
                tabPanel("Timing",uiOutput("tbdTiming"))
    )
})

output$tbModel <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("Data", DT::dataTableOutput("tbmData")),
                #tabPanel("Plot", plotlyOutput("tbmPlot")),
                tabPanel("Seasons", plotlyOutput("tbmSeasons")),
                tabPanel("Series",plotOutput("tbmSeries")),
                tabPanel("Series2",plotlyOutput("tbmSeries2")),
                tabPanel("Timing",uiOutput("tbmTiming")),
                tabPanel("MEM", uiOutput("tbmMem")),
                tabPanel("Goodness",uiOutput("tbmGoodness")),
                tabPanel("Optimize",uiOutput("tbmOptimize"))
    )
})

output$tbSurveillance <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("Data", DT::dataTableOutput("tbsData")),
                #tabPanel("Plot", plotlyOutput("tbsPlot")),
                tabPanel("Seasons", plotlyOutput("tbsSeasons")),
                tabPanel("Timing",plotOutput("tbsTiming")),
                tabPanel("Surveillance",plotOutput("tbsSurveillance")),
                tabPanel("Surveillance",uiOutput("tbsSurveillance2")),
                tabPanel("Animated",imageOutput("tbsAnimated"))
    )
})

output$tbVisualize <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("Data", DT::dataTableOutput("tbvData")),
                #tabPanel("Plot", plotlyOutput("tbvPlot")),
                tabPanel("Seasons", plotlyOutput("tbvSeasons")),
                tabPanel("Series",plotOutput("tbvSeries")),
                tabPanel("Timing",uiOutput("tbvTiming"))
    )
})

#####################################
### DATA TAB
#####################################

output$tbdFile <- renderTable({
  if(is.null(data_read())){return()}
  input$file[1]
}) 

output$tbdData <- DT::renderDataTable({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
  #toinclude<-names(datfile)
  #if (!is.null(input$SelectSeasons)) toinclude<-c(toinclude,input$SelectSeasons)
  selectedcolumns<-select.columns(i.names=names(datfile), 
                 i.from="", 
                 i.to="", 
                 i.exclude="",
                 i.include="",
                 i.pandemic=T,
                 i.seasons=NA)
  # cat("i.from:->",input$SelectFrom,"<-\n",sep="")
  # cat("i.to:->",input$SelectTo,"<-\n",sep="")
  # cat("i.exclude:->",input$SelectExclude,"<-\n",sep="")
  # cat("i.include:->",as.character(c(input$K,input$K4)),"<-\n",sep="")
  # cat("i.pandemic:->",as.logical(input$SelectPandemic),"<-\n",sep="")
  # cat("i.seasons:->",input$SelectSeasons,"<-\n",sep="")
  #cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  if (length(selectedcolumns)>0) datatoshow<-datfile[selectedcolumns] else datatoshow<-data.frame(Message="No data selected",row.names = NULL)
  roundF(datatoshow,2)
},
  options = list(scrollX = TRUE, scrollY = '300px', paging = FALSE))  

output$tbdPlot <- renderPlotly({
  datfile <- data_read()
  p <- plotInput()
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  z
})

output$tbdSeasons <- renderPlotly({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
                                #i.pandemic=as.logical(input$SelectPandemic), 
                                i.pandemic=T, 
                                i.seasons=input$SelectMaximum)
  if (length(model.columns)>1){
    epi <- memmodel(datfile[model.columns],
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
  }else{
    e.thr<-NA
    i.thr<-NA
  }
  datfile.plot<-datfile[!(names(datfile) %in% c("num","vecka"))]
  p <- plotSeasons(datfile.plot,i.epidemic.thr=e.thr, i.intensity.thr=i.thr, i.pre.epidemic = as.logical(input$preepidemicthr), i.post.epidemic = as.logical(input$postepidemicthr), i.intensity = as.logical(input$intensitythr))
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  z
})

output$tbdSeries <- renderPlot({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
                                #i.pandemic=as.logical(input$SelectPandemic), 
                                i.pandemic=T,
                                i.seasons=input$SelectMaximum)
  if (length(model.columns)>1){
    epi <- memmodel(datfile[model.columns],
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
    ei.thr<-as.logical(input$intensitythr)
  }else{
    e.thr<-NA
    i.thr<-NA
    ei.thr<-F
  }
  datfile.plot<-datfile[!(names(datfile) %in% c("num","vecka"))]
  plotSeries(datfile.plot, i.epidemic.thr=e.thr, i.intensity.thr=i.thr, i.timing = T, i.threholds = ei.thr)
})

output$tbdTiming = renderUI({
  datfile <- data_read()
  if(is.null(datfile)) {return()}
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from="", 
                                  i.to="", 
                                  i.exclude="",
                                  i.include="",
                                  i.pandemic=T,
                                  i.seasons=NA)
  datfile.plot<-datfile[selectedcolumns]
  tabnames<-names(datfile.plot)
  do.call(tabsetPanel,
          ## Create a set of tabPanel functions dependent on tabnames
          lapply(tabnames,function(s){
            ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
            ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
            call("tabPanel",s,call('plotOutput',paste0("tbdTiming_",s)))
          })
  )

})

#####################################
### MODEL TAB
#####################################

output$tbmData <- DT::renderDataTable({
  #datfile <- data_read()
  #if(is.null(datfile)){return()}
  # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
  # selectedcolumns<-select.columns(i.names=names(datfile), 
  #                                 i.from=input$SelectFrom, 
  #                                 i.to=input$SelectTo, 
  #                                 i.exclude=input$SelectExclude,
  #                                 i.include="",
  #                                 #i.pandemic=as.logical(input$SelectPandemic),
  #                                 i.pandemic=T,
  #                                 i.seasons=input$SelectMaximum)
  #cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  # cat("i.from:->",input$SelectFrom,"<-\n",sep="")
  # cat("i.to:->",input$SelectTo,"<-\n",sep="")
  # cat("i.exclude:->",input$SelectExclude,"<-\n",sep="")
  # cat("i.include:->",as.character(c(input$K,input$K4)),"<-\n",sep="")
  # cat("i.pandemic:->",as.logical(input$SelectPandemic),"<-\n",sep="")
  # cat("i.seasons:->",input$SelectSeasons,"<-\n",sep="")
  # cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  datamodel<-data_model()
  if(is.null(datamodel)) datatoshow<-data.frame(Message="No data selected",row.names = NULL) else datatoshow<-datamodel$param.data
  roundF(datatoshow,2)
},
  options = list(scrollX = TRUE, scrollY = '300px', paging = FALSE))  

output$tbmPlot <- renderPlotly({
  datfile <- data_read()
  p <- plotInput()
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  z
})

output$tbmSeasons <- renderPlotly({
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
  #                               #i.pandemic=as.logical(input$SelectPandemic), 
  #                               i.pandemic=T,
  #                               i.seasons=input$SelectMaximum)
  # if (length(model.columns)>1){
  #   epi <- memmodel(datfile[model.columns],
  #                   i.type.threshold=as.numeric(input$i.type.threshold),
  #                   i.type.intensity=as.numeric(input$i.type.intensity), 
  #                   i.method = as.numeric(input$i.method),
  #                   i.param = as.numeric(input$memparameter), i.seasons = NA)
  #   e.thr<-epi$epidemic.thresholds
  #   i.thr<-epi$intensity.thresholds
  # }else{
  #   e.thr<-NA
  #   i.thr<-NA
  # }
  datamodel<-data_model()
  if(is.null(datamodel)){return()}
  datfile.plot<-datamodel$param.data
  e.thr<-datamodel$epidemic.thresholds
  i.thr<-datamodel$intensity.thresholds
  #datfile.plot<-datfile[model.columns]
  p <- plotSeasons(datfile.plot,i.epidemic.thr=e.thr, i.intensity.thr=i.thr, i.pre.epidemic = as.logical(input$preepidemicthr), i.post.epidemic = as.logical(input$postepidemicthr), i.intensity = as.logical(input$intensitythr))
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", rownames(datfile.plot))}
  z
})

output$tbmSeries <- renderPlot({
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
  #                               #i.pandemic=as.logical(input$SelectPandemic), 
  #                               i.pandemic=T,
  #                               i.seasons=input$SelectMaximum)
  # cat(model.columns,"-",length(model.columns),"\n")
  # if (length(model.columns)>1){
  #   cat("1\n")
  #   epi <- memmodel(datfile[model.columns],
  #                   i.type.threshold=as.numeric(input$i.type.threshold),
  #                   i.type.intensity=as.numeric(input$i.type.intensity), 
  #                   i.method = as.numeric(input$i.method),
  #                   i.param = as.numeric(input$memparameter), i.seasons = NA)
  #   e.thr<-epi$epidemic.thresholds
  #   i.thr<-epi$intensity.thresholds
  #   ei.thr<-as.logical(input$intensitythr)
  # }else{
  #   cat("2\n")
  #   e.thr<-NA
  #   i.thr<-NA
  #   ei.thr<-F
  # }
  # datfile.plot<-datfile[model.columns]
  datamodel<-data_model()
  if(is.null(datamodel)){return()}
  datfile.plot<-datamodel$param.data
  e.thr<-datamodel$epidemic.thresholds
  i.thr<-datamodel$intensity.thresholds
  plotSeries(datfile.plot, i.epidemic.thr=e.thr, i.intensity.thr=i.thr, i.timing = T, i.threholds = as.logical(input$intensitythr))
})

output$tbmSeries2 <- renderPlotly({
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
  #                               #i.pandemic=as.logical(input$SelectPandemic), 
  #                               i.pandemic=T,
  #                               i.seasons=input$SelectMaximum)
  # if (length(model.columns)>1){
  #   epi <- memmodel(datfile[model.columns],
  #                   i.type.threshold=as.numeric(input$i.type.threshold),
  #                   i.type.intensity=as.numeric(input$i.type.intensity), 
  #                   i.method = as.numeric(input$i.method),
  #                   i.param = as.numeric(input$memparameter), i.seasons = NA)
  #   e.thr<-epi$epidemic.thresholds
  #   i.thr<-epi$intensity.thresholds
  # }else{
  #   e.thr<-NA
  #   i.thr<-NA
  # }
  # datfile.plot<-datfile[model.columns]
  datamodel<-data_model()
  if(is.null(datamodel)){return()}
  datfile.plot<-datamodel$param.data
  e.thr<-datamodel$epidemic.thresholds
  i.thr<-datamodel$intensity.thresholds
  p <- plotSeries3(i.data=datfile.plot, i.plot.timing = T, i.range.x=NA, i.pre.epidemic=as.logical(input$preepidemicthr),
                   i.post.epidemic=as.logical(input$postepidemicthr), i.epidemic.thr=e.thr, 
                   i.intensity= as.logical(input$intensitythr), i.intensity.thr=i.thr, i.range.y=NA)
  
  
  # z <- plotly_build(p)
  # # Fix legend
  # for (j in length(z$x$data):1){
  #   z$x$data[[j]]$name<-sub("\\(([^\\,]*).*","\\1",z$x$data[[j]]$name)
  #   z$x$data[[j]]$legendgroup<-sub("\\(([^\\,]*).*","\\1",z$x$data[[j]]$legendgroup)
  #   if (z$x$data[[j]]$name=="NA") z$x$data[[j]]<-NULL
  # }
  # # Fix margins
  # z$x$layout$margin$b<-2*z$x$layout$margin$b
  # # for(j in 1:length(z$x$data)){
  # #   z$x$data[[j]]$text <- print(paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka))}
  # for(j in 1:length(z$x$data)){
  #   z$x$data[[j]]$text <- paste("Y:", roundF(z$x$data[[j]]$y,1))}
  # z
  
  z <- plotly_build(p$plot)
  zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
  zfix
  
  
})

output$tbmTiming = renderUI({
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
  #                               #i.pandemic=as.logical(input$SelectPandemic), 
  #                               i.pandemic=T,
  #                               i.seasons=input$SelectMaximum)
  # if (length(model.columns)<1){return()}
  # datfile.plot<-datfile[model.columns]
  datamodel<-data_model()
  if(is.null(datamodel)){return()}
  datfile.plot<-datamodel$param.data
  tabnames<-names(datfile.plot)
  do.call(tabsetPanel,
          ## Create a set of tabPanel functions dependent on tabnames
          lapply(tabnames,function(s){
            ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
            ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
            call("tabPanel",s,call('plotOutput',paste0("tbmTiming_",s)))
          })
  )
  
})

output$tbmMem <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("Summary", uiOutput("tbmMemSummary")),
                tabPanel("Output", verbatimTextOutput("tbmMemOutput")),
                tabPanel("Graph",plotOutput("tbmMemModel"))
    )
})

output$tbmMemSummary <- renderUI({
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # selectedcolumns<-select.columns(i.names=names(datfile), 
  #                                 i.from=input$SelectFrom, 
  #                                 i.to=input$SelectTo, 
  #                                 i.exclude=input$SelectExclude,
  #                                 i.include="",
  #                                 #i.pandemic=as.logical(input$SelectPandemic),
  #                                 i.pandemic=T,
  #                                 i.seasons=input$SelectMaximum)
  # 
  # if (length(selectedcolumns)<2){return()}
  # datfile.model<-datfile[selectedcolumns]
  # nam.t <- memmodel(datfile.model,
  #                     # nam.t <- memmodel(data_read()[,c(grep(input$K2, 
  #                     #                                           colnames(data_read())):(grep(input$K, colnames(data_read()))-1))],
  #                     i.type.threshold=as.numeric(input$i.type.threshold),
  #                     i.type.intensity=as.numeric(input$i.type.intensity),
  #                     i.method = as.numeric(input$i.method),
  #                     i.param = as.numeric(input$memparameter), i.seasons = NA) 
  
  datamodel<-data_model()
  if(is.null(datamodel)){return()}

  #roundF(t(object$pre.post.intervals[1:2,3]),2)
  #roundF(object$epi.intervals[,1]*100,1)
  #100*object$param.level

  fluidRow(
    valueBox(datamodel$n.seasons, "Number of seasons", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(datamodel$ci.length[1,2],1), "Epidemic length", icon = icon("heartbeat"), width=3, color="green"),
    valueBox(paste0(roundF(datamodel$ci.percent[2],1), "%"), "Epidemic percentage", icon = icon("heartbeat"), width=3, color="green"),
    valueBox(datamodel$ci.start[2,2], "Mean start", icon = icon("heartbeat"), width=3, color="maroon"),
    valueBox(roundF(datamodel$pre.post.intervals[1,3],1), "Epidemic threshold", icon = icon("thermometer-1"), width=3, color="aqua"),
    valueBox(roundF(datamodel$epi.intervals[1,4],1), "Medium threshold", icon = icon("thermometer-2"), width=3, color="light-blue"),
    valueBox(roundF(datamodel$epi.intervals[2,4],1), "High threshold", icon = icon("thermometer-3"), width=3, color="blue"),
    valueBox(roundF(datamodel$epi.intervals[3,4],1), "Very high threshold", icon = icon("thermometer-4"), width=3, color="navy")
  )
})

output$tbmMemOutput <- renderPrint({
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # selectedcolumns<-select.columns(i.names=names(datfile), 
  #                                 i.from=input$SelectFrom, 
  #                                 i.to=input$SelectTo, 
  #                                 i.exclude=input$SelectExclude,
  #                                 i.include="",
  #                                 #i.pandemic=as.logical(input$SelectPandemic),
  #                                 i.pandemic=T,
  #                                 i.seasons=input$SelectMaximum)
  # 
  # if (length(selectedcolumns)>1){
  # datfile.model<-datfile[selectedcolumns]
    
  datamodel<-data_model()
  if(!is.null(datamodel)){
  
  nam.t<-datamodel
  
    #if(grep(input$K, colnames(data_read()))>1){
    #   if(grep(input$K2, colnames(data_read())) < grep(input$K, colnames(data_read()))-1){
    # nam.t <- memmodel(datfile.model,
    #     # nam.t <- memmodel(data_read()[,c(grep(input$K2, 
    #     #                                           colnames(data_read())):(grep(input$K, colnames(data_read()))-1))],
    #                            i.type.threshold=as.numeric(input$i.type.threshold),
    #                            i.type.intensity=as.numeric(input$i.type.intensity),
    #                            i.method = as.numeric(input$i.method),
    #                            i.param = as.numeric(input$memparameter), i.seasons = NA)
        nam.ttt <- rbind(c("Epidemic threshold:","           Pre Post"),
                         c("",paste0("Threshold ", 
                                     roundF(nam.t$"pre.post.intervals"[1,3],2)," ", 
                                     roundF(nam.t$"pre.post.intervals"[2,3],2))),
                         c("", ""),
                         c("Intensity thresholds:",""),
                         c("                  Threshold", ""),
                         c(paste0("Medium (40%)          ", roundF(nam.t$"epi.intervals"[1,4],2)), ""),
                         c(paste0("High (90%)            ", roundF(nam.t$"epi.intervals"[2,4],2)), ""),
                         c(paste0("Very high (97.5%)     ", roundF(nam.t$"epi.intervals"[3,4],2)), ""))
        
        nam.ttt <- format(nam.ttt, justify = "left")
        nam.ttt <- as.data.frame(nam.ttt)
        names(nam.ttt) <- NULL 
        #print(noquote(nam.ttt), row.names = FALSE)
        summary(datamodel)
      # }else{war.text <- as.data.frame("MEM needs at least two seasons.")
      #         names(war.text) <- NULL 
      #           print(noquote(war.text), row.names = FALSE)}
    }else{war.text <- as.data.frame("MEM needs at least two seasons.")
            names(war.text) <- NULL
              print(noquote(war.text), row.names = FALSE)}
  })

output$tbmMemModel <- renderPlot({
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # selectedcolumns<-select.columns(i.names=names(datfile), 
  #                                 i.from=input$SelectFrom, 
  #                                 i.to=input$SelectTo, 
  #                                 i.exclude=input$SelectExclude,
  #                                 i.include="",
  #                                 #i.pandemic=as.logical(input$SelectPandemic),
  #                                 i.pandemic=T,
  #                                 i.seasons=input$SelectMaximum)
  # 
  # if (length(selectedcolumns)<3){return()}
  # datfile.model<-datfile[selectedcolumns]
  # nam.t <- memmodel(datfile.model,
  #                   # nam.t <- memmodel(data_read()[,c(grep(input$K2, 
  #                   #                                           colnames(data_read())):(grep(input$K, colnames(data_read()))-1))],
  #                   i.type.threshold=as.numeric(input$i.type.threshold),
  #                   i.type.intensity=as.numeric(input$i.type.intensity),
  #                   i.method = as.numeric(input$i.method),
  #                   i.param = as.numeric(input$memparameter), i.seasons = NA)
  datamodel<-data_model()
  if(is.null(datamodel)){return()}
  plot(datamodel)
})

output$tbmGoodness <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("Summary", uiOutput("tbmGoodnessSummary")),
                tabPanel("By season", DT::dataTableOutput("tbmGoodnessDetail"))
    )
})

output$tbmGoodnessSummary <- renderUI({
  good <- data_good()
  if(is.null(good)){return()}
  fluidRow(
    valueBox(roundF(good$results["Sensitivity"],2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(good$results["Specificity"],2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(good$results["Positive predictive value"],2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(good$results["Negative predictive value"],2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(good$results["Percent agreement"],2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
    valueBox(roundF(good$results["Matthews correlation coefficient"],2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua")
  )
})


output$tbmGoodnessDetail<-DT::renderDataTable({
  # datfile <- data_read()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  # datacolumns<-c(grep(input$K2,colnames(datfile)):(grep(input$K, colnames(datfile))-1))
  
  good <- data_good()
  if(!is.null(good)){
    good.table<-as.data.frame(good$validity.data)
    good.table$Total<-good$results
    good.table<-as.data.frame(t(good.table))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
  }else{
    good.table<-data.frame(Error="Number of columns must be greater than 2")
  }
  roundF(good.table,2)
},
  options = list(scrollX = TRUE, scrollY = '300px', paging = FALSE))

output$tbmOptimize <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("Summary", uiOutput("tbmOptimizeSummary")),
                tabPanel("Detail", DT::dataTableOutput("tbmOptimizeDetail")),
                tabPanel("Graph",plotOutput("tbmOptimizeGraph"))
    )
})

output$tbmOptimizeSummary <- renderUI({
  dataoptim <- data_optim()
  if(is.null(dataoptim)){return()}
  doptim<-dataoptim$roc.data
  optim<-doptim[doptim$value==as.numeric(dataoptim$optimum["matthews"]),]
  fluidRow(
    valueBox(roundF(optim["sensitivity"],2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(optim["specificity"],2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(optim["positive.predictive.value"],2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(optim["negative.predictive.value"],2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
    valueBox(roundF(optim["percent.agreement"],2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
    valueBox(roundF(optim["matthews.correlation.coefficient"],2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua"),
    valueBox(roundF(input$memparameter,1), "Current parameter", icon = icon("heartbeat"), width=3, color="red"),
    valueBox(roundF(as.numeric(dataoptim$optimum["matthews"]),1), "Optimum parameter", icon = icon("heartbeat"), width=3, color="olive")
  )
})

output$tbmOptimizeDetail<-DT::renderDataTable({
  # datfile <- data_read()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  # datacolumns<-c(grep(input$K2,colnames(datfile)):(grep(input$K, colnames(datfile))-1))
  # 
  # if(length(datacolumns)>2){
  # datfile <- data_read()
  # if(is.null(datfile)){return()}
  # selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
  #                                 i.exclude=input$SelectExclude, i.include="", 
  #                                 #i.pandemic=as.logical(input$SelectPandemic), 
  #                                 i.pandemic=T,
  #                                 i.seasons=input$SelectMaximum)
  
  dataoptim <- data_optim()
  if(!is.null(dataoptim)){
  
  # if (length(selectedcolumns)>2){
  #   
  #   
  #   
  #   roca<-roc.analysis(datfile[,selectedcolumns],
  #                      i.param.values = seq(2, 3, 0.1), 
  #                      i.graph.file = F,
  #                      i.type.threshold=as.numeric(input$i.type.threshold),
  #                      i.type.intensity=as.numeric(input$i.type.intensity), 
  #                      i.seasons=NA, 
  #                      i.min.seasons = length(selectedcolumns))
    roca.table<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient")]
    names(roca.table)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")    
  }else{
    roca.table<-data.frame(Error="Number of columns must be greater than 2")  
  }
  rownames(roca.table)<-NULL
  roundF(roca.table,2)
},
  options = list(scrollX = TRUE, scrollY = '300px', paging = FALSE))

output$tbmOptimizeGraph<- renderPlot({

  dataoptim <- data_optim()
  if(is.null(dataoptim)){return()}
  
  resultados<-dataoptim$roc.data
  i.graph.title<-""
  i.graph.subtitle<-""
  
  colores<-c("#EBEAEA","#5B9BD5","#ED7D31")
  
  opar<-par(mar=c(5,3,3,3)+0.1,mgp=c(3,0.5,0),xpd=T,mfrow=c(2,2))
  
  if (any(!is.na(resultados$sensitivity)) & any(!is.na(resultados$specificity))){
    d.x<-resultados$value
    d.y<-cbind(resultados$sensitivity,resultados$specificity)
    etiquetas<-c("Sensitivity","Specificity")
    otick<-optimal.tickmarks(0,1,10)
    range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
    matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,ylim=range.y,main=i.graph.title)
    points(d.x,d.y[,1],pch=19,type="p",col=colores[2],cex=0.5)
    points(d.x,d.y[,2],pch=19,type="p",col=colores[3],cex=0.5)
    axis(1,at=d.x,labels=d.x,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
    axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
    mtext(1,text="Parameter",line=1.3,cex=0.8,col="#000040")
    mtext(2,text="Value",line=1.3,cex=0.8,col="#000040")
    mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
    mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
    legend(x="topright",y=NULL,inset=c(0,-0.05),xjust=0,legend=etiquetas,bty="n",lty=c(1,1),lwd=c(1,1),col=colores[c(1,1)],pch=c(21,21),pt.bg=colores[c(2,3)],cex=1,x.intersp=0.5,y.intersp=0.7,text.col="#000000",ncol=1)
    
  }
  
  if (any(!is.na(resultados$positive.predictive.value)) & any(!is.na(resultados$negative.predictive.value))){
    
    d.x<-resultados$value
    d.y<-cbind(resultados$positive.predictive.value,resultados$negative.predictive.value)
    etiquetas<-c("Positive predictive value","Negative predictive value")
    otick<-optimal.tickmarks(0,1,10)
    range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
    matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,ylim=range.y,main=i.graph.title)
    points(d.x,d.y[,1],pch=19,type="p",col=colores[2],cex=0.5)
    points(d.x,d.y[,2],pch=19,type="p",col=colores[3],cex=0.5)
    axis(1,at=d.x,labels=d.x,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
    axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
    mtext(1,text="Parameter",line=1.3,cex=0.8,col="#000040")
    mtext(2,text="Value",line=1.3,cex=0.8,col="#000040")
    mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
    mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
    legend(x="topright",y=NULL,inset=c(0,-0.05),xjust=0,legend=etiquetas,bty="n",lty=c(1,1),lwd=c(1,1),col=colores[c(1,1)],pch=c(21,21),pt.bg=colores[c(2,3)],cex=1,x.intersp=0.5,y.intersp=0.7,text.col="#000000",ncol=1)
  }
  
  if (any(!is.na(resultados$percent.agreement)) & any(!is.na(resultados$matthews.correlation.coefficient))){
    
    d.x<-resultados$value
    d.y<-cbind(resultados$percent.agreement,resultados$matthews.correlation.coefficient)
    etiquetas<-c("Percent agreement","Matthews correlation coefficient")
    otick<-optimal.tickmarks(0,1,10)
    range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
    matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,ylim=range.y,main=i.graph.title)
    points(d.x,d.y[,1],pch=19,type="p",col=colores[2],cex=0.5)
    points(d.x,d.y[,2],pch=19,type="p",col=colores[3],cex=0.5)
    axis(1,at=d.x,labels=d.x,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
    axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
    mtext(1,text="Parameter",line=1.3,cex=0.8,col="#000040")
    mtext(2,text="Value",line=1.3,cex=0.8,col="#000040")
    mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
    mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
    legend(x="topright",y=NULL,inset=c(0,-0.05),xjust=0,legend=etiquetas,bty="n",lty=c(1,1),lwd=c(1,1),col=colores[c(1,1)],pch=c(21,21),pt.bg=colores[c(2,3)],cex=1,x.intersp=0.5,y.intersp=0.7,text.col="#000000",ncol=1)
  }
  
  if (any(!is.na(resultados$specificity)) & any(!is.na(resultados$sensitivity))){
    d.x<-1-resultados$specificity
    d.y<-resultados$sensitivity[order(d.x)]
    d.x<-d.x[order(d.x)]
    otick<-optimal.tickmarks(0,1,10)
    range.x<-c(otick$range[1],otick$range[2]+otick$by/2)
    range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
    matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,xlim=range.x,ylim=range.y,main=i.graph.title)
    points(d.x,d.y,pch=19,type="p",col=colores[2],cex=0.5)
    axis(1,at=otick$tickmarks,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
    axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
    mtext(1,text="1 - specificity",line=1.3,cex=0.8,col="#000040")
    mtext(2,text="Sensitivity",line=1.3,cex=0.8,col="#000040")
    mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
    mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
  }
  par(opar)
  
  
})

#####################################
### SURVEILLANCE TAB
#####################################

output$tbsData <- DT::renderDataTable({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
  # toinclude<-names(datfile)
  #if (!is.null(input$SelectSeasons)) toinclude<-c(toinclude,input$SelectSeasons)
  if (is.null(input$SelectSurveillance)) {return()}
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from=input$SelectSurveillance, 
                                  i.to=input$SelectSurveillance, 
                                  i.exclude="",
                                  i.include=input$SelectSurveillance,
                                  i.pandemic=as.logical("TRUE"),
                                  i.seasons=NA)
  # cat("i.from:->",input$SelectFrom,"<-\n",sep="")
  # cat("i.to:->",input$SelectTo,"<-\n",sep="")
  # cat("i.exclude:->",input$SelectExclude,"<-\n",sep="")
  # cat("i.include:->",as.character(c(input$K,input$K4)),"<-\n",sep="")
  # cat("i.pandemic:->",as.logical(input$SelectPandemic),"<-\n",sep="")
  # cat("i.seasons:->",input$SelectSeasons,"<-\n",sep="")
  #cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  if (length(selectedcolumns)>0) datatoshow<-datfile[selectedcolumns] else datatoshow<-data.frame(Message="No data selected",row.names = NULL)
  roundF(datatoshow,2)
},
  options = list(scrollX = TRUE, scrollY = '300px', paging = FALSE))  

output$tbsPlot <- renderPlotly({
  datfile <- data_read()
  p <- plotInput()
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  z
})

output$tbsSeasons <- renderPlotly({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
                                #i.pandemic=as.logical(input$SelectPandemic), 
                                i.pandemic=T,
                                i.seasons=input$SelectMaximum)
  if (length(model.columns)>1){
    epi <- memmodel(datfile[model.columns],
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
  }else{
    e.thr<-NA
    i.thr<-NA
  }
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from=input$SelectSurveillance, 
                                  i.to=input$SelectSurveillance, 
                                  i.exclude="",
                                  i.include=input$SelectSurveillance,
                                  i.pandemic=as.logical("TRUE"),
                                  i.seasons=NA)
  datfile.plot<-datfile[selectedcolumns]
  if (length(selectedcolumns)==0){return()}
  #cat("Seleccion:->",paste(selectedcolumns,collapse=","),"<-\n",sep="")
  #cat("Seleccion:->",length(selectedcolumns),"<-\n",sep="")
  p <- plotSeasons(datfile.plot,i.epidemic.thr=e.thr, i.intensity.thr=i.thr, i.pre.epidemic = as.logical(input$preepidemicthr), i.post.epidemic = as.logical(input$postepidemicthr), i.intensity = as.logical(input$intensitythr))
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  z
})

output$tbsTiming <- renderPlot({
  plotTiming(input$SelectSurveillance)
})

output$tbsSurveillance <- renderPlot({
  plotSurveillance()
})

output$tbsSurveillance2 <- renderUI({
  if(is.null(data_read())){return()}
  else
    tabsetPanel(tabPanel("Week", plotlyOutput("tbsSurveillanceWeek")),
                tabPanel("Animated", imageOutput("tbsSurveillanceAnimated"))
    )
})

output$tbsSurveillanceAnimated <- renderImage({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  if(!(input$SelectSurveillance %in% names(datfile))){return()}
  if(!(input$SelectSurveillanceWeek %in% rownames(datfile))){return()}
  
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  if (length(selectedcolumns)>1){
    datfile.model<-datfile[selectedcolumns]
    epi <- memmodel(datfile.model,
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
  }else{
    e.thr<-NA
    i.thr<-NA
  }
  datfile.plot<-datfile[input$SelectSurveillance]
  
  max.y<-max(datfile.plot,na.rm=T)
  if (as.logical(input$preepidemicthr)) max.y<-max(max.y,e.thr[1],na.rm=T)
  if (as.logical(input$postepidemicthr)) max.y<-max(max.y,e.thr[2],na.rm=T)
  if (as.logical(input$intensitythr)) max.y<-max(max.y,i.thr,na.rm=T)

  n.weeks<-NROW(datfile)
  n.surveillance.week<-min((1:n.weeks)[input$SelectSurveillanceWeek==rownames(datfile)])
  
  # Option 1: using animation, it uses imagemagick, needs it to be installed!
  # plot.one<-function(i){
  #   print(plotSurveillance2(i.data=datfile.plot, i.week.report=rownames(datfile)[i], i.pre.epidemic=as.logical(input$preepidemicthr),
  #                           i.post.epidemic=as.logical(input$postepidemicthr), i.epidemic.thr = e.thr, i.intensity = as.logical(input$intensitythr),
  #                           i.intensity.thr = i.thr, i.range.y=c(0,max.y)))
  # }
  # plot.all <- function() {
  #   lapply(1:n.surveillance.week, function(i) {
  #     plot.one(i)
  #     ani.record()
  #   })
  # }
  # imgfile<-paste(tempdir(),"/animated.gif",sep="")
  # saveGIF(plot.all(), interval = 0.5, movie.name=imgfile,
  #         ani.width = 1200, 
  #         ani.height = 600,
  #         nmax=n.weeks,
  #         autobrowse=F,
  #         loop=0
  #         )
  # outdistAnimated<-list(src = imgfile,
  #                       contentType = 'image/gif',
  #                       width = 800,
  #                       height = 600,
  #                       alt = "This is alternate text")
  # outdistAnimated
  
  # Option 2: Uses magick, its a kind of R imagemagick, should work out of the box
 for (i in 1:n.surveillance.week){
     p<-plotSurveillance2(i.data=datfile.plot, i.week.report=rownames(datfile)[i], i.pre.epidemic=as.logical(input$preepidemicthr),
                             i.post.epidemic=as.logical(input$postepidemicthr), i.epidemic.thr = e.thr, i.intensity = as.logical(input$intensitythr),
                             i.intensity.thr = i.thr, i.range.y=c(0,max.y),
                          i.end=as.logical(input$postepidemicthr))
     imgfile<-paste(tempdir(),"/animatedplot_",i,".png",sep="")
     ggsave(imgfile, plot=p$plot, width=8, height=6, dpi=150)
     if (i==1) imgfilem<-image_read(imgfile) else imgfilem<-c(imgfilem,image_read(imgfile))
     cat(imgfile,"\n")
 }
  #frames <- image_morph(imgfilem, frames = 10)
  imgfilegif<-paste(tempdir(),"/animated.gif",sep="")
  anim <- image_animate(imgfilem, fps = 2)
  image_write(anim,path=imgfilegif)
  cat(imgfilegif,"\n")
  outdistAnimated<-list(src = paste(tempdir(),"/animated.gif",sep=""),
                         contentType = 'image/gif',
                         width = 800,
                         height = 600,
                         alt = "This is alternate text")
  outdistAnimated
  
  
}, deleteFile = TRUE)

output$tbsSurveillanceWeek <- renderPlotly({
  
  datfile <- data_read()
  if(is.null(datfile)){return()}
  if(!(input$SelectSurveillance %in% names(datfile))){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  if (length(selectedcolumns)>1){
    datfile.model<-datfile[selectedcolumns]
    epi <- memmodel(datfile.model,
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
  }else{
    e.thr<-NA
    i.thr<-NA
  }
  
  datfile.plot<-datfile[input$SelectSurveillance]
  
  p <- plotSurveillance2(i.data=datfile.plot, i.week.report=input$SelectSurveillanceWeek, i.pre.epidemic=as.logical(input$preepidemicthr),
                         i.post.epidemic=as.logical(input$postepidemicthr), i.epidemic.thr = e.thr, i.intensity = as.logical(input$intensitythr),
                         i.intensity.thr = i.thr,
                         i.end=as.logical(input$postepidemicthr))
  
  z <- plotly_build(p$plot)
  zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
  # Fix legend
  # for (j in length(z$x$data):1){
  #   z$x$data[[j]]$name<-sub("\\(([^\\,]*).*","\\1",z$x$data[[j]]$name)
  #   z$x$data[[j]]$legendgroup<-sub("\\(([^\\,]*).*","\\1",z$x$data[[j]]$legendgroup)
  #   if (z$x$data[[j]]$name=="NA") z$x$data[[j]]<-NULL
  # }
  # for(j in 1:length(z$x$data)){
  #   z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  # z
  zfix
})

output$tbsAnimated <- renderImage({
  # datfile <- data_read()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  datfile <- data_read()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  datfile.model<-datfile[selectedcolumns]
  if (NCOL(datfile.model)>1 & input$SelectSurveillance %in% names(datfile)){
    epi <- memmodel(datfile.model,
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
    range.x<- as.numeric(rownames(datfile)[c(1,NROW(datfile))])
    
    if (!as.logical(input$preepidemicthr)) e.thr<-NA

   sura<-memsurveillance.animated(datfile[input$SelectSurveillance], e.thr, i.thr, i.remove = T,
                           i.animated.graph.file.name = "animated", 
                           i.output = tempdir(), 
                           i.pos.epidemic = as.logical(input$postepidemicthr), 
                           i.range.x=range.x,
                           i.no.intensity=!as.logical(input$intensitythr))
  imgfile<-sura$graph.name
  #cat(imgfile,"\n")
  
  outdistAnimated<-list(src = imgfile,
       contentType = 'image/gif',
       width = 800,
       height = 600,
       alt = "This is alternate text")
  }else{
    outdistAnimated<-NULL
  }
  outdistAnimated
}, deleteFile = TRUE)

#####################################
### VISUALIZE TAB
#####################################

output$tbvData <- DT::renderDataTable({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
  if (is.null(input$SelectSeasons)) {return()}
  toinclude<-input$SelectSeasons
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from=input$SelectSeasons[1], 
                                  i.to=input$SelectSeasons[1], 
                                  i.exclude="",
                                  i.include=toinclude,
                                  i.pandemic=as.logical("TRUE"),
                                  i.seasons=NA)
  # cat("i.from:->",input$SelectFrom,"<-\n",sep="")
  # cat("i.to:->",input$SelectTo,"<-\n",sep="")
  # cat("i.exclude:->",input$SelectExclude,"<-\n",sep="")
  # cat("i.include:->",as.character(c(input$K,input$K4)),"<-\n",sep="")
  # cat("i.pandemic:->",as.logical(input$SelectPandemic),"<-\n",sep="")
  # cat("i.seasons:->",input$SelectSeasons,"<-\n",sep="")
  #cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  if (length(selectedcolumns)>0) datatoshow<-datfile[selectedcolumns] else datatoshow<-data.frame(Message="No data selected",row.names = NULL)
  roundF(datatoshow,2)
},
  options = list(scrollX = TRUE, scrollY = '300px', paging = FALSE))  

output$tbvPlot <- renderPlotly({
  datfile <- data_read()
  p <- plotInput()
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  z
})

output$tbvSeasons <- renderPlotly({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
                                #i.pandemic=as.logical(input$SelectPandemic), 
                                i.pandemic=T,
                                i.seasons=input$SelectMaximum)
  if (length(model.columns)>1){
    epi <- memmodel(datfile[model.columns],
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
  }else{
    e.thr<-NA
    i.thr<-NA
  }
  if (is.null(input$SelectSeasons)) {return()}
  toinclude<-input$SelectSeasons
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from=input$SelectSeasons[1], 
                                  i.to=input$SelectSeasons[1], 
                                  i.exclude="",
                                  i.include=toinclude,
                                  i.pandemic=as.logical("TRUE"),
                                  i.seasons=NA)
  if (length(selectedcolumns)==0){return()}
  datfile.plot<-datfile[selectedcolumns]
  #cat("Seleccion:->",paste(selectedcolumns,collapse=","),"<-\n",sep="")
  #cat("Seleccion:->",length(selectedcolumns),"<-\n",sep="")
  p <- plotSeasons(datfile.plot,i.epidemic.thr=e.thr, i.intensity.thr=i.thr, i.pre.epidemic = as.logical(input$preepidemicthr), i.post.epidemic = as.logical(input$postepidemicthr), i.intensity = as.logical(input$intensitythr))
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- paste(z$x$data[[j]]$name,"Y:", roundF(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka)}
  z
})

output$tbvSeries <- renderPlot({
  datfile <- data_read()
  if(is.null(datfile)){return()}
  model.columns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
                                #i.pandemic=as.logical(input$SelectPandemic), 
                                i.pandemic=T,
                                i.seasons=input$SelectMaximum)
  if (length(model.columns)>1){
    epi <- memmodel(datfile[model.columns],
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
    ei.thr<-as.logical(input$intensitythr)
  }else{
    e.thr<-NA
    i.thr<-NA
    ei.thr<-F
  }
  if (is.null(input$SelectSeasons)) {return()}
  toinclude<-input$SelectSeasons
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from=input$SelectSeasons[1], 
                                  i.to=input$SelectSeasons[1], 
                                  i.exclude="",
                                  i.include=toinclude,
                                  i.pandemic=as.logical("TRUE"),
                                  i.seasons=NA)
  if (length(selectedcolumns)==0){return()}
  datfile.plot<-datfile[selectedcolumns]
  plotSeries(datfile.plot, i.epidemic.thr=e.thr, i.intensity.thr=i.thr, i.timing = T, i.threholds = ei.thr)
})

output$tbvTiming = renderUI({
  tabnames<-input$SelectSeasons
  do.call(tabsetPanel,
          ## Create a set of tabPanel functions dependent on tabnames
          lapply(tabnames,function(s){
            ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
            ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
            call("tabPanel",s,call('plotOutput',paste0("tbvTiming_",s)))
          })
  )
  
})



plotInput <-function(){
  # datfile <- data_read()
  # dat3 <- datfile
  # datafil <- dat3
  
  datfile <- data_read()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  datfile.model<-datfile[selectedcolumns]
  datfile.seasons<-datfile[input$SelectSeasons]

  if (NCOL(datfile.model)>1){
    epi <- memmodel(datfile.model,
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
  }

  # Axis format for all the graphs
  # Calculate values if we want to place 20 tickmarks in the graph in the x-axis.
  axis.x.range.original <- c(min(datfile$num),max(datfile$num))
  axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 20, 1:axis.x.range.original[2],T,T)  
  axis.x.range <- axis.x.otick$range
  axis.x.values <- as.numeric(datfile$num)
  axis.x.ticks <- axis.x.otick$tickmarks
  axis.x.labels <- rownames(datfile)[axis.x.otick$tickmarks]
  # Same, for 10 tickmarks in the y-axis
  axis.y.range.original <- c(0,1.05*max(cbind(datfile.model, datfile.seasons), na.rm=T))
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
  axis.y.range <- axis.y.otick$range
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks
  
  #cat(paste(axis.y.ticks,collapse=","),"\n",sep="")
  #########Graf med observerade data
  
  # CASE 1: if there are no 2 seasons to calculate thresholds AND addmoreplots=NOT
  # OR
  # addintensity=NOT AND addthreshold=NOT and addmoreplots=NOT
  if(NCOL(datfile.model)<2 & is.null(input$SelectSeasons)|(input$preepidemicthr=="FALSE" & is.null(input$SelectSeasons) & input$intensitythr=="FALSE"))
  {
    #cat("Case #1\n")
    g.plot <- 
      ggplot(datfile) +
      #geom_line(aes(x=as.numeric(rownames(dat3)), y=as.numeric(dat3[,input$K]), group=1, color=input$K)) +
        ggtitle(input$textMain) +
        theme(plot.title = element_text(hjust = 0.1, size=22))+
        geom_line(aes(x=datfile$num, y=datfile[,input$SelectSurveillance], group=1, color=input$SelectSurveillance)) +
        labs(x=input$textX, y=input$textY, color='Season') +
        scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels)+
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range)+
        scale_color_manual(values="black", labels=input$SelectSurveillance)+
        ggthemes::theme_few()
    print(ggplotly(g.plot, tooltip = "text"))
    #start
# CASE 2: addthreshold=YES AND 2 or more seasons AND addmoreplots=NOT and addintensity=NOT
  }else if(input$preepidemicthr=="TRUE" & 
           NCOL(datfile.model)>1 & is.null(input$SelectSeasons) & input$intensitythr=="FALSE"){
    #cat("Case #2\n")
    # epi <- memmodel(datfile[,c(grep(input$K2, 
    #                               colnames(datfile)):(grep(input$K, 
    #                                                        colnames(datfile))-1))], 
    #               i.type.threshold=as.numeric(input$i.type.threshold),
    #               i.type.intensity=as.numeric(input$i.type.intensity), 
    #               i.method = as.numeric(input$i.method),
    #               i.param = as.numeric(input$memparameter), i.seasons = NA)
    g.plot <-
      ggplot(datfile) +
        ggtitle(input$textMain) +
        theme(plot.title = element_text(hjust = 0.1, size=22))+
        geom_line(aes(x=datfile$num, y=datfile[,input$SelectSurveillance], group=1, color=input$SelectSurveillance)) +
        labs(x=input$textX,y=input$textY, color='Season') +
        scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels)+
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range)+
        scale_color_manual(values="black", labels=input$SelectSurveillance)+
        geom_hline(aes(yintercept=e.thr[1]), color = input$colMEMstart) +
        geom_hline(aes(yintercept=e.thr[2]), color = input$colMEMstop) +
        ggthemes::theme_few()
    print(ggplotly(g.plot, tooltip = "text"))
    # CASE 3: addthreshold=NO AND 2 or more seasons AND  addmoreplots=NOT AND addintensity=YES
  }else if(input$preepidemicthr=="FALSE" & 
           NCOL(datfile.model)>1 & is.null(input$SelectSeasons) & input$intensitythr=="TRUE"){
    # epi <- memmodel(datfile[,c(grep(input$K2, 
    #                               colnames(datfile)):(grep(input$K, 
    #                                                        colnames(datfile))-1))], 
    #               i.type.threshold=as.numeric(input$i.type.threshold),
    #               i.type.intensity=as.numeric(input$i.type.intensity), 
    #               i.method = as.numeric(input$i.method),
    #               i.param = as.numeric(input$memparameter), i.seasons = NA)
    #cat("Case #3\n")
    col.pal <- colorRampPalette(brewer.pal(5,input$colpal))(5)
    g.plot <-
      ggplot(datfile) +
        ggtitle(input$textMain) +
        theme(plot.title = element_text(hjust = 0.1, size=22))+
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = 0,        ymax = e.thr[1], fill = col.pal[1], alpha=as.numeric(input$colpalTran))+
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = e.thr[1], ymax = i.thr[1], fill = col.pal[2], alpha=as.numeric(input$colpalTran))+
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[1], ymax = i.thr[2], fill = col.pal[3], alpha=as.numeric(input$colpalTran))+
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[2], ymax = i.thr[3], fill = col.pal[4], alpha=as.numeric(input$colpalTran))+
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[3], ymax = axis.y.range[2], fill = col.pal[5], alpha=as.numeric(input$colpalTran))+
        geom_line(aes(x=datfile$num, y=datfile[,input$SelectSurveillance], group=1, color=input$SelectSurveillance)) +
        labs(x=input$textX,y=input$textY, color='Season') +
        scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels)+
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range)+
        scale_color_manual(values="black", labels=input$SelectSurveillance)+
        ggthemes::theme_few()
    print(ggplotly(g.plot, tooltip = "text"))
    # CASE 4: addthreshold=YES AND 2 or more seasons AND  addmoreplots=NOT AND addintensity=YES
  }else if(input$preepidemicthr=="TRUE" & 
           NCOL(datfile.model)>1 & is.null(input$SelectSeasons) & input$intensitythr=="TRUE"){
    # epi <- memmodel(datfile[,c(grep(input$K2, 
    #                               colnames(datfile)):(grep(input$K, 
    #                                                        colnames(datfile))-1))], 
    #               i.type.threshold=as.numeric(input$i.type.threshold),
    #               i.type.intensity=as.numeric(input$i.type.intensity), 
    #               i.method = as.numeric(input$i.method),
    #               i.param = as.numeric(input$memparameter), i.seasons = NA)
    #cat("Case #4\n")
    col.pal <- colorRampPalette(brewer.pal(5,input$colpal))(5)
    g.plot <-
      ggplot(datfile) +
      ggtitle(input$textMain) +
      theme(plot.title = element_text(hjust = 0.1, size=22))+
      annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = 0,        ymax = e.thr[1], fill = col.pal[1], alpha=as.numeric(input$colpalTran))+
      annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = e.thr[1], ymax = i.thr[1], fill = col.pal[2], alpha=as.numeric(input$colpalTran))+
      annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[1], ymax = i.thr[2], fill = col.pal[3], alpha=as.numeric(input$colpalTran))+
      annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[2], ymax = i.thr[3], fill = col.pal[4], alpha=as.numeric(input$colpalTran))+
      annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[3], ymax = axis.y.range[2], fill = col.pal[5], alpha=as.numeric(input$colpalTran))+
      geom_line(aes(x=datfile$num, y=datfile[,input$SelectSurveillance], group=1, color=input$SelectSurveillance)) +
      labs(x=input$textX,y=input$textY, color='Season') +
      scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels)+
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range)+
      scale_color_manual(values="black", input$SelectSurveillance)+
      geom_hline(aes(yintercept=e.thr[1]), color = input$colMEMstart) +
      geom_hline(aes(yintercept=e.thr[2]), color = input$colMEMstop) +
      #scale_color_manual(values="blue", labels=roundF(i.thr[1]))+
      ggthemes::theme_few()
    print(ggplotly(g.plot, tooltip = "text"))
# Condition: CASE 5
  }else{
    # lis <- list()
    # for(i in input$SelectSeasons){
    #   dat3 <- datfile
    #   dat3$tid <- rownames(dat3)
    #   selectedxaxis = colnames(dat3)[ncol(dat3)]
    #   selectedcolumns = colnames(dat3)[which(colnames(dat3)==i)]
    #   widedata = subset(dat3, select = c(selectedxaxis, selectedcolumns))
    #   longdata = melt(widedata, id.vars=selectedxaxis, variable.name='Cases', value.name='Count')
    #   lis <- rbind(lis, longdata)
    # }
    #cat("Case #5\n")
    lis<-melt(datfile, id.vars="num", measure.vars=input$SelectSeasons, variable.name='Season', value.name='Rates')     
    g.plot <- 
      ggplot(lis) + 
      ggtitle(input$textMain) +
      theme(plot.title = element_text(hjust = 0.1, size=22))+
      geom_line(aes(x=num, y=Rates, group=Season, color=Season, linetype = Season))+ 
      labs(x=input$textX,y=input$textY, color='Season') +
      scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels)+
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range)+
      ggthemes::theme_few()
    print(ggplotly(g.plot, tooltip = "text"))    
  }
  
}

plotSeasons <- function(i.data, i.epidemic.thr=NA, i.intensity.thr=NA, i.pre.epidemic=TRUE, i.post.epidemic=TRUE, i.intensity=TRUE){
  #cat("plotSeasons function\n")
  if(is.null(i.data)){return()}
  dataplot<-i.data
  dataplot$num<-1:NROW(dataplot)
  
  # Axis format for all the graphs
  # Calculate values if we want to place 20 tickmarks in the graph in the x-axis.
  axis.x.range.original <- c(min(dataplot$num),max(dataplot$num))
  axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 20, 1:axis.x.range.original[2],T,T)  
  axis.x.range <- axis.x.otick$range
  axis.x.values <- as.numeric(dataplot$num)
  axis.x.ticks <- axis.x.otick$tickmarks
  axis.x.labels <- rownames(dataplot)[axis.x.otick$tickmarks]
  # Same, for 10 tickmarks in the y-axis
  
  temp1<-max(i.data,na.rm=T)
  if (length(i.epidemic.thr)==2 & i.pre.epidemic) temp1<-max(c(temp1,i.epidemic.thr[1]),na.rm=T)
  if (length(i.epidemic.thr)==2 & i.post.epidemic) temp1<-max(c(temp1,i.epidemic.thr[2]),na.rm=T)
  if (length(i.intensity.thr)==3) temp1<-max(c(temp1,i.intensity.thr),na.rm=T)
  axis.y.range.original <- c(0,1.05*temp1)
  rm("temp1")
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
  axis.y.range <- axis.y.otick$range
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks
  
  if (NCOL(i.data)>0){
    lis<-melt(dataplot, id.vars="num", measure.vars=names(i.data), variable.name='Season', value.name='Rates')     
    col.pal <- colorRampPalette(brewer.pal(5,input$colpal))(5)
    col.ser <- colorRampPalette(brewer.pal(max(3,min(8,NCOL(i.data))),input$colpalseries))(NCOL(i.data))
    # Basic plot structure
    g.plot <- 
      ggplot(lis) +
      ggtitle(input$textMain) +
      theme(plot.title = element_text(hjust = 0.1, size=22)) +
      labs(x=input$textX,y=input$textY, color='Season')
    # Add intensity, this goes first not to overwrite the series
    if(i.intensity & length(i.intensity.thr)==3){
      g.plot <- g.plot + 
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = 0,        ymax = i.epidemic.thr[1], fill = col.pal[1], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.epidemic.thr[1], ymax = i.intensity.thr[1], fill = col.pal[2], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.intensity.thr[1], ymax = i.intensity.thr[2], fill = col.pal[3], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.intensity.thr[2], ymax = i.intensity.thr[3], fill = col.pal[4], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.intensity.thr[3], ymax = axis.y.range[2], fill = col.pal[5], alpha=as.numeric(input$colpalTran))
    }
    # Add all series selected
    g.plot <- g.plot +
      geom_line(aes(x=num, y=Rates, group=Season, color=Season)) + 
      scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range) +
      scale_color_manual(values=col.ser, labels=names(i.data))
    # Add pre-epidemic thresholds
    if(i.pre.epidemic & length(i.epidemic.thr)==2){
      g.plot <- g.plot +
        geom_hline(aes(yintercept=i.epidemic.thr[1]), color = input$colMEMstart)
    }
    # Add postepidemic thresholds
    if(i.post.epidemic & length(i.epidemic.thr)==2){
      g.plot <- g.plot +
        geom_hline(aes(yintercept=i.epidemic.thr[2]), color = input$colMEMstop)
    }
    # Finishing
    g.plot <- g.plot + 
      ggthemes::theme_few()
    print(ggplotly(g.plot, tooltip = "text"))
  }
}

plotSeries2<-function(i.data, i.plot.timing = T, i.range.x=NA, i.pre.epidemic=T, i.post.epidemic=T,
                      i.epidemic.thr=NA, i.intensity= T, i.intensity.thr=NA, i.range.y=NA,
                      i.color.pattern=c("#C0C0C0","#606060","#000000","#808080","#000000","#001933",
                                        "#00C000","#800080","#FFB401",
                                        "#8c6bb1","#88419d","#810f7c","#4d004b"), ...){
  
  if(is.null(i.data)){return()}
  
  week.f<-as.numeric(rownames(i.data)[1])
  week.l<-as.numeric(rownames(i.data)[NROW(i.data)])
  if (week.f>week.l){
    i.range.x<-c(min(week.f,30),min(week.f,30)-1)
    i.range.x.values<-data.frame(week.lab=c(min(week.f,30):52,1:(min(week.f,30)-1)),week.no=1:52)
  }else{
    i.range.x<-c(1,52)
    i.range.x.values<-data.frame(week.lab=1:52,week.no=1:52)
  }
  if (NCOL(i.data)>1){
    epi<-memmodel(i.data, i.seasons=NA,...)
    epidata<-epi$data
    epiindex<-as.data.frame(epi$season.indexes[,,1])
    rownames(epiindex)<-rownames(i.data)
    colnames(epiindex)<-colnames(i.data)
    epithresholds<-epi$epidemic.thresholds
    intthresholds<-epi$intensity.thresholds
    i.data<-i.data[names(i.data) %in% names(epi$data)]
  }else{
    # I need the epi object to extract the data dataframe, which includes the original data + filled missing data and
    # the timing (which would be extracted with memtiming also)
    epi<-memmodel(cbind(i.data,i.data), i.seasons=NA,...)
    epidata<-epi$data[1]
    epiindex<-as.data.frame(epi$season.indexes[,1,1])
    rownames(epiindex)<-rownames(i.data)
    colnames(epiindex)<-colnames(i.data)
    epithresholds<-NA
    intthresholds<-NA
    i.data<-i.data[names(i.data) %in% names(epi$data)]
  }
  rm("epi")
  
  # To have continuity between seasons I have to inflate original data to the global squeme. That's it: If
  # original data format is from 40 to 20, the inflated data would be 30 to 29, so that when a season ends
  # at 29, next one will start at 30 and there would be continuity between both
  
  data.full<-i.data
  data.full$week.lab<-rownames(data.full)
  data.full<-merge(data.full,i.range.x.values,by="week.lab",all.y=T)
  data.full<-data.full[order(data.full$week.no),]
  row.names(data.full)<-data.full$week.lab
  data.full$week.lab<-NULL
  data.full$week.no<-NULL
  
  data.full.epi<-epidata
  data.full.epi$week.lab<-rownames(data.full.epi)
  data.full.epi<-merge(data.full.epi,i.range.x.values,by="week.lab",all.y=T)
  data.full.epi<-data.full.epi[order(data.full.epi$week.no),]
  row.names(data.full.epi)<-data.full.epi$week.lab
  data.full.epi$week.lab<-NULL
  data.full.epi$week.no<-NULL
  
  data.full.index<-epiindex
  data.full.index[is.na(epidata)]<-NA
  data.full.index$week.lab<-rownames(data.full.index)
  data.full.index<-merge(data.full.index,i.range.x.values,by="week.lab",all.y=T)
  data.full.index<-data.full.index[order(data.full.index$week.no),]
  row.names(data.full.index)<-data.full.index$week.lab
  data.full.index$week.lab<-NULL
  data.full.index$week.no<-NULL
  
  # Data to plot
  data.orig<-transformdata.back(data.full,i.name="rates",i.range.x=i.range.x,i.fun=sum)
  data.y<-as.numeric(data.orig[,"rates"])
  # Data to plot, filling in missing with data imputed by mem (using loess)
  data.fixed<-transformdata.back(data.full.epi,i.name="rates",i.range.x=i.range.x,i.fun=sum)
  data.y.fixed<-as.numeric(data.fixed[,"rates"])
  # Data that have been imputed, to mark them as a circle with a cross
  data.missing<-data.fixed
  data.missing[!(is.na(data.orig) & !is.na(data.fixed))]<-NA
  data.y.missing<-as.numeric(data.missing[,"rates"])
  # Indexes for pre, epi and post epidemic
  data.indexes<-transformdata.back(data.full.index,i.name="rates",i.range.x=i.range.x,i.fun=function(x,...) if (all(is.na(x))) return(NA) else if (any(x==2,...)) return(2) else if (any(x==1,...)) return(1) else return(3))
  data.y.indexes<-as.numeric(data.indexes[,names(data.indexes)=="rates"])
  
  if (length(i.epidemic.thr)==2){
    epidemic<-i.epidemic.thr
  }else{
    if (NCOL(i.data)>1){
      epidemic<-as.numeric(epithresholds)
    }else{
      i.pre.epidemic<-F
      i.post.epidemic<-F
      epidemic<-NA
    }
  }
  
  if (length(i.intensity.thr)==3){
    intensity<-i.intensity.thr
  }else{
    if (NCOL(i.data)>1){
      intensity<-as.numeric(intthresholds)
    }else{
      i.intensity<-F
      intensity<-NA
    }
  }
  
  
  # Calculate ticks for x
  
  data.x <- 1:NROW(data.orig)
  axis.x.range <- range(data.x)
  
  # Axis format for all the graphs
  # First: WEEK NAME
  # Calculate values as if we want to place 20 tickmarks in the graph in the x-axis.
  # temp1 <- range(i.range.x.values$week.no)
  # temp2 <- optimal.tickmarks(temp1[1], temp1[2], 4, 1:temp1[2],T,F)  
  # axis.x1.ticks<-data.x[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  # axis.x1.labels<-data.orig$week[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  # rm("temp1","temp2")  
  # # Second: SEASONS NAME 
  # temp1<-i.range.x.values$week.lab[floor(quantile(i.range.x.values$week.no,0.5))]
  # axis.x2.ticks<-data.x[data.orig$week==temp1]
  # axis.x2.labels<-data.orig$season[data.orig$week==temp1]
  # rm("temp1")
  
  temp1 <- range(i.range.x.values$week.no)
  temp2 <- optimal.tickmarks(temp1[1], temp1[2], 4, 1:temp1[2],T,F)  
  axis.x.ticks<-data.x[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  axis.x.labels1<-data.orig$week[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  axis.x.labels2<-data.orig$season[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  axis.x.labels2[axis.x.labels1!=i.range.x.values$week.lab[temp2$tickmarks][floor(temp2$number/2+1)]]<-""
  axis.x.labels<-paste(axis.x.labels1,axis.x.labels2,sep="\n")
  rm("temp1","temp2")  
  
  # Same, for 10 tickmarks in the y-axis
  
  if (is.numeric(i.range.y)){
    temp1<-i.range.y
  }else if (i.intensity){
    #temp1<-c(0,max.fix.na(c(data.y.fixed,intensity)))
    temp1<-c(0,max(c(data.y.fixed,intensity, na.rm=T)))
  }else{
    #temp1<-c(0,max.fix.na(data.y.fixed))
    temp1<-c(0,max(data.y.fixed, na.rm=T))
  }
  if (length(epidemic)==2 & i.pre.epidemic) temp1<-max(c(temp1,epidemic[1]),na.rm=T)
  if (length(epidemic)==2 & i.post.epidemic) temp1<-max(c(temp1,epidemic[2]),na.rm=T)
  if (length(intensity)==3 & i.intensity) temp1<-max(c(temp1,intensity),na.rm=T)
  axis.y.range.original <- c(0,1.05*temp1)
  rm("temp1")
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
  axis.y.range <- axis.y.otick$range
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks
  
  temp1<-rep("No",length(data.y.missing))
  temp1[!is.na(data.y.missing)]<-"Yes"
  
  data.plot<-data.frame(data.x,data.y.fixed,data.y.missing=temp1,data.y.indexes,dummy=1)
  
  g.plot <- 
    ggplot(data.plot) +
    ggtitle(input$textMain) +
    theme(plot.title = element_text(hjust = 0.1, size=22)) +
    labs(x=input$textX,y=input$textY, color=i.color.pattern[6]) +
    geom_line(aes(x=data.x, y=data.y.fixed, group=1), color=i.color.pattern[6]) + 
    scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels) +
    scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range)
  
  if (i.plot.timing){
    g.plot <- 
      g.plot + 
      geom_point(aes(x=data.x, y=data.y.fixed, 
                     color=factor(data.y.indexes, levels=1:3, labels = c("Pre", "Epi","Post")), 
                     shape=data.y.missing, 
                     size=data.y.missing)) + 
      scale_color_manual(name="Timing", values=i.color.pattern[7:9]) +
      scale_shape_manual(name="Missing", values=c(19, 13)) +
      scale_size_manual(name="Missing", values=c(1, 2))
  }else{
    g.plot <- 
      g.plot + 
      geom_point(aes(x=data.x, y=data.y.fixed, 
                     color=factor(dummy, levels=1, labels = "Serie"),
                     shape=data.y.missing, 
                     size=data.y.missing)) + 
      scale_color_manual(name="Timing", values=i.color.pattern[4]) +
      scale_shape_manual(name="Missing", values=c(19, 13)) +
      scale_size_manual(name="Missing", values=c(1, 2))
  }
  
  
  if (i.pre.epidemic) g.plot <- g.plot + geom_hline(aes(yintercept=epidemic[1]), color = i.color.pattern[10], linetype=2, size=1)
  if (i.post.epidemic) g.plot <- g.plot + geom_hline(aes(yintercept=epidemic[2]), color = i.color.pattern[10], linetype=3, size=1)
  if (i.intensity) g.plot <- g.plot + geom_hline(aes(yintercept=intensity[1]), color = i.color.pattern[11], linetype=2, size=1) +
    geom_hline(aes(yintercept=intensity[2]), color = i.color.pattern[12], linetype=2, size=1) +
    geom_hline(aes(yintercept=intensity[3]), color = i.color.pattern[13], linetype=2, size=1)
  g.plot <- g.plot + 
    ggthemes::theme_few()
  print(ggplotly(g.plot, tooltip = "text"))
}

plotSeries3<-function(i.data, i.plot.timing = T, i.pre.epidemic=T, i.post.epidemic=T, i.epidemic.thr=NA, 
                      i.intensity= T, i.intensity.thr=NA, i.range.x=NA, i.range.y=NA, ...){
  
  if(is.null(i.data)){return()}
  
  week.f<-as.numeric(rownames(i.data)[1])
  week.l<-as.numeric(rownames(i.data)[NROW(i.data)])
  if (week.f>week.l){
    i.range.x<-c(min(week.f,30),min(week.f,30)-1)
    i.range.x.values<-data.frame(week.lab=c(min(week.f,30):52,1:(min(week.f,30)-1)),week.no=1:52)
  }else{
    i.range.x<-c(1,52)
    i.range.x.values<-data.frame(week.lab=1:52,week.no=1:52)
  }
  if (NCOL(i.data)>1){
    epi<-memmodel(i.data, i.seasons=NA,...)
    epidata<-epi$data
    epiindex<-as.data.frame(epi$season.indexes[,,1])
    rownames(epiindex)<-rownames(i.data)
    colnames(epiindex)<-colnames(i.data)
    epithresholds<-epi$epidemic.thresholds
    intthresholds<-epi$intensity.thresholds
    i.data<-i.data[names(i.data) %in% names(epi$data)]
  }else{
    # I need the epi object to extract the data dataframe, which includes the original data + filled missing data and
    # the timing (which would be extracted with memtiming also)
    epi<-memmodel(cbind(i.data,i.data), i.seasons=NA,...)
    epidata<-epi$data[1]
    epiindex<-as.data.frame(epi$season.indexes[,1,1])
    rownames(epiindex)<-rownames(i.data)
    colnames(epiindex)<-colnames(i.data)
    epithresholds<-NA
    intthresholds<-NA
    i.data<-i.data[names(i.data) %in% names(epi$data)]
  }
  rm("epi")
  
  # To have continuity between seasons I have to inflate original data to the global squeme. That's it: If
  # original data format is from 40 to 20, the inflated data would be 30 to 29, so that when a season ends
  # at 29, next one will start at 30 and there would be continuity between both
  
  data.full<-i.data
  data.full$week.lab<-rownames(data.full)
  data.full<-merge(data.full,i.range.x.values,by="week.lab",all.y=T)
  data.full<-data.full[order(data.full$week.no),]
  row.names(data.full)<-data.full$week.lab
  data.full$week.lab<-NULL
  data.full$week.no<-NULL
  
  data.full.epi<-epidata
  data.full.epi$week.lab<-rownames(data.full.epi)
  data.full.epi<-merge(data.full.epi,i.range.x.values,by="week.lab",all.y=T)
  data.full.epi<-data.full.epi[order(data.full.epi$week.no),]
  row.names(data.full.epi)<-data.full.epi$week.lab
  data.full.epi$week.lab<-NULL
  data.full.epi$week.no<-NULL
  
  data.full.index<-epiindex
  data.full.index[is.na(epidata)]<-NA
  data.full.index$week.lab<-rownames(data.full.index)
  data.full.index<-merge(data.full.index,i.range.x.values,by="week.lab",all.y=T)
  data.full.index<-data.full.index[order(data.full.index$week.no),]
  row.names(data.full.index)<-data.full.index$week.lab
  data.full.index$week.lab<-NULL
  data.full.index$week.no<-NULL
  
  # Data to plot
  data.orig<-transformdata.back(data.full,i.name="rates",i.range.x=i.range.x,i.fun=sum)
  data.y<-as.numeric(data.orig[,"rates"])
  # Data to plot, filling in missing with data imputed by mem (using loess)
  data.fixed<-transformdata.back(data.full.epi,i.name="rates",i.range.x=i.range.x,i.fun=sum)
  data.y.fixed<-as.numeric(data.fixed[,"rates"])
  # Data that have been imputed, to mark them as a circle with a cross
  data.missing<-data.fixed
  data.missing[!(is.na(data.orig) & !is.na(data.fixed))]<-NA
  data.y.missing<-as.numeric(data.missing[,"rates"])
  # Indexes for pre, epi and post epidemic
  data.indexes<-transformdata.back(data.full.index,i.name="rates",i.range.x=i.range.x,i.fun=function(x,...) if (all(is.na(x))) return(NA) else if (any(x==2,...)) return(2) else if (any(x==1,...)) return(1) else return(3))
  data.y.indexes<-as.numeric(data.indexes[,names(data.indexes)=="rates"])
  
  if (length(i.epidemic.thr)==2){
    epidemic<-i.epidemic.thr
  }else{
    if (NCOL(i.data)>1){
      epidemic<-as.numeric(epithresholds)
    }else{
      i.pre.epidemic<-F
      i.post.epidemic<-F
      epidemic<-NA
    }
  }
  
  if (length(i.intensity.thr)==3){
    intensity<-i.intensity.thr
  }else{
    if (NCOL(i.data)>1){
      intensity<-as.numeric(intthresholds)
    }else{
      i.intensity<-F
      intensity<-NA
    }
  }
  
  labels<-c("Weekly rates","Pre-epidemic","Pre-epidemic (missing)","Epidemic","Epidemic (missing)","Post-epidemic","Post-epidemic (missing)","Epidemic thr.","Medium thr.","High thr.","Very high thr.","Post thr.")
  haspoints<-c(T,T,T,T,T,T,T,F,F,F,F,F)
  haslines<-c(T,F,F,F,F,F,F,T,T,T,T,T)
  shapes<-c(21,21,24,21,24,21,24,NA,NA,NA,NA,NA)
  colors<-c("#808080","#000000","#000000","#000000","#000000","#000000","#000000","#8c6bb1","#88419d","#810f7c","#4d004b","#8c6bb1")
  fills<-c("#000000","#00C000","#00C000","#800080","#800080","#FFB401","#FFB401","#000000","#000000")
  sizes<-c(2,2,2,2,2,2,2,1,1,1,1,1)
  linetypes<-c("solid","blank","blank","blank","blank","blank","blank", "dashed", "dashed", "dashed", "dashed","dashed")
  
  dgraf<-data.frame(rates=data.y.fixed,
                    prer=data.y,
                    prem=data.y.missing,
                    epir=data.y,
                    epim=data.y.missing,
                    posr=data.y,
                    posm=data.y.missing,
                    epit=epidemic[1],
                    medt=intensity[1],
                    higt=intensity[2],
                    vert=intensity[3],
                    post=epidemic[2]
  )
  dgraf$prer[data.y.indexes!=1]<-NA
  dgraf$prem[data.y.indexes!=1]<-NA
  dgraf$epir[data.y.indexes!=2]<-NA
  dgraf$epim[data.y.indexes!=2]<-NA
  dgraf$posr[data.y.indexes!=3]<-NA
  dgraf$posm[data.y.indexes!=3]<-NA
  names(dgraf)<-labels
  dgraf$week<-1:NROW(dgraf)
  
  dgrafgg<-melt(dgraf,id="week")
  
  selected.indicators<-1
  if (i.plot.timing){
    selected.indicators<-c(selected.indicators,c(2,4,6))
    if (!all(is.na(dgraf[,3]))) selected.indicators<-c(selected.indicators,3)
    if (!all(is.na(dgraf[,5]))) selected.indicators<-c(selected.indicators,5)
    if (!all(is.na(dgraf[,7]))) selected.indicators<-c(selected.indicators,7)
  }
  if (i.pre.epidemic) selected.indicators<-c(selected.indicators,8)
  if (i.post.epidemic) selected.indicators<-c(selected.indicators,12)
  if (i.intensity) selected.indicators<-c(selected.indicators,9:11)
  selected.indicators<-unique(selected.indicators)
  selected.indicators<-selected.indicators[order(selected.indicators)]
  
  labels.s<-labels[selected.indicators]
  haspoints.s<-haspoints[selected.indicators]
  haslines.s<-haslines[selected.indicators]
  dgrafgg.s<-subset(dgrafgg,variable %in% labels.s)
  shapes.s<-shapes[selected.indicators]
  colors.s<-colors[selected.indicators]
  fills.s<-fills[selected.indicators]
  sizes.s<-sizes[selected.indicators]
  linetypes.s<-linetypes[selected.indicators]
  
  # Calculate ticks for x
  data.x <- 1:NROW(data.orig)
  axis.x.range <- range(data.x)
  temp1 <- range(i.range.x.values$week.no)
  temp2 <- optimal.tickmarks(temp1[1], temp1[2], 4, 1:temp1[2],T,F)  
  axis.x.ticks<-data.x[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  axis.x.labels1<-data.orig$week[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  axis.x.labels2<-data.orig$season[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
  axis.x.labels2[axis.x.labels1!=i.range.x.values$week.lab[temp2$tickmarks][floor(temp2$number/2+1)]]<-""
  axis.x.labels<-paste(axis.x.labels1,axis.x.labels2,sep="\n")
  rm("temp1","temp2")  
  
  # Range y fix
  if (length(i.range.y)!=2){
    i.range.y <- c(0,1.05*max(subset(dgrafgg.s,variable!="week",select="value"),na.rm=T))
  }else{
    i.range.y <- 1.05*i.range.y
  }
  axis.y.range.original <- i.range.y
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
  axis.y.range <- axis.y.otick$range
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks
  
  gplot<-ggplot(dgrafgg.s) +
    geom_line(aes(x=week,y=value,group=variable, color=variable, linetype=variable),size=1) +
    geom_point(aes(x=week,y=value,group=variable, color=variable, size=variable, fill=variable, shape=variable), color="#ffffff", stroke = 0.1) +
    scale_shape_manual(values=shapes.s, name="Legend", labels=labels.s) + 
    scale_color_manual(values=colors.s, name="Legend", labels=labels.s) +
    scale_fill_manual(values=fills.s, name="Legend", labels=labels.s) +
    scale_size_manual(values=sizes.s, name="Legend", labels=labels.s) +
    scale_linetype_manual(values=linetypes.s, name="Legend", labels=labels.s) + 
    scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
    scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
    labs(title = input$textMain, x = input$textX, y = input$textY) + 
    ggthemes::theme_few()
  
  list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,weeklabels=paste(data.orig$week,"<br>Season: ",data.orig$season,sep=""))
  
}

plotSeasons.OLD <-function(){
  datfile <- data_read()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  datfile.model<-datfile[selectedcolumns]
  datfile.seasons<-datfile[input$SelectSeasons]
  if (NCOL(datfile.model)>1){
    epi <- memmodel(datfile.model,
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter), i.seasons = NA)
    e.thr<-epi$epidemic.thresholds
    i.thr<-epi$intensity.thresholds
  }
  
  # Axis format for all the graphs
  # Calculate values if we want to place 20 tickmarks in the graph in the x-axis.
  axis.x.range.original <- c(min(datfile$num),max(datfile$num))
  axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 20, 1:axis.x.range.original[2],T,T)  
  axis.x.range <- axis.x.otick$range
  axis.x.values <- as.numeric(datfile$num)
  axis.x.ticks <- axis.x.otick$tickmarks
  axis.x.labels <- rownames(datfile)[axis.x.otick$tickmarks]
  # Same, for 10 tickmarks in the y-axis
  axis.y.range.original <- c(0,1.05*max(cbind(datfile.model, datfile.seasons), na.rm=T))
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
  axis.y.range <- axis.y.otick$range
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks

  if (length(input$SelectSeasons)>0){
    lis<-melt(datfile, id.vars="num", measure.vars=input$SelectSeasons, variable.name='Season', value.name='Rates')     
    col.pal <- colorRampPalette(brewer.pal(5,input$colpal))(5)
    col.ser <- colorRampPalette(brewer.pal(length(input$SelectSeasons),input$colpalseries))(length(input$SelectSeasons))
    # Basic plot structure
    g.plot <- 
      ggplot(lis) +
      ggtitle(input$textMain) +
      theme(plot.title = element_text(hjust = 0.1, size=22)) +
      labs(x=input$textX,y=input$textY, color='Season')
    # Add intensity, this goes first not to overwrite the series
    if(input$intensitythr=="TRUE" & NCOL(datfile.model)>1){
      g.plot <- g.plot + 
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = 0,        ymax = e.thr[1], fill = col.pal[1], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = e.thr[1], ymax = i.thr[1], fill = col.pal[2], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[1], ymax = i.thr[2], fill = col.pal[3], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[2], ymax = i.thr[3], fill = col.pal[4], alpha=as.numeric(input$colpalTran)) +
        annotate("rect", xmin = axis.x.range[1], xmax = axis.x.range[2], ymin = i.thr[3], ymax = axis.y.range[2], fill = col.pal[5], alpha=as.numeric(input$colpalTran))
    }
    # Add all series selected
    g.plot <- g.plot +
      geom_line(aes(x=num, y=Rates, group=Season, color=Season)) + 
      scale_x_continuous(breaks=axis.x.ticks, labels=axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range) +
      scale_color_manual(values=col.ser, labels=input$SelectSurveillance)
    # Add epidemic thresholds
    if(input$preepidemicthr=="TRUE" & NCOL(datfile.model)>1){
      g.plot <- g.plot +
        geom_hline(aes(yintercept=e.thr[1]), color = input$colMEMstart) +
        geom_hline(aes(yintercept=e.thr[2]), color = input$colMEMstop)
    }
    # Finishing
    g.plot <- g.plot + 
      ggthemes::theme_few()
    print(ggplotly(g.plot, tooltip = "text"))
  }
}

plotTiming <-function(kol){
  datfile <- data_read()
    #dat3 <- datfile
    #datafil <- dat3
    #for(i in input$K:input$K2){
    epi.plot <- memtiming(datfile[kol],
                          i.method = as.numeric(input$i.method),
                          i.param = as.numeric(input$memparameter))
    plot(epi.plot)
}


plotSeries <-function(i.data, i.epidemic.thr=NA, i.intensity.thr=NA, i.timing=TRUE, i.threholds=TRUE){
  datfile <- i.data
  if(is.null(datfile)){return()}
  range.x<- as.numeric(rownames(datfile)[c(1,NROW(datfile))])
  if (length(i.epidemic.thr)==2 & length(i.intensity.thr)==3) intensity<-c(i.epidemic.thr[1],i.intensity.thr) else intensity<-NA
  full.series.graph(datfile,
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter),
                    i.graph.file = F, 
                    i.plot.timing = i.timing, 
                    i.plot.intensity = i.threholds,
                    i.range.x=range.x,
                    i.alternative.thresholds=intensity)
}

plotSurveillance <-function(){
  # datfile <- data_read()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  
  datfile <- data_read()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", 
                                  #i.pandemic=as.logical(input$SelectPandemic), 
                                  i.pandemic=T,
                                  i.seasons=input$SelectMaximum)
  datfile.model<-datfile[selectedcolumns]
  datfile.surveillance<-datfile[input$SelectSurveillance]
  if (NCOL(datfile.model)>1 & input$SelectSurveillance %in% names(datfile)){
    epi <- memmodel(datfile.model,
                i.type.threshold=as.numeric(input$i.type.threshold),
                i.type.intensity=as.numeric(input$i.type.intensity), 
                i.method = as.numeric(input$i.method),
                i.param = as.numeric(input$memparameter), i.seasons = NA)
  e.thr<-epi$epidemic.thresholds
  i.thr<-epi$intensity.thresholds
  range.x<- as.numeric(rownames(datfile)[c(1,NROW(datfile))])

  if (!as.logical(input$preepidemicthr)) e.thr<-NA
  memsurveillance(datfile.surveillance, 
                  e.thr, i.thr, i.graph.file=F, i.pos.epidemic = as.logical(input$postepidemicthr), i.range.x =range.x, i.week.report = input$SelectSurveillanceWeek, i.no.intensity=!as.logical(input$intensitythr))
  }
}

plotSurveillance2<-function(i.data,
                      i.week.report=NA,
                      i.range.x=NA,
                      i.range.y=NA,
                      i.pre.epidemic=T,
                      i.post.epidemic=T,
                      i.epidemic=T,
                      i.start=T,
                      i.end=T,
                      i.epidemic.thr = NA,
                      i.intensity= T,
                      i.intensity.thr=NA,
                      i.mean.length=10,
                      i.force.length=F,
                      i.force.equal=F,
                      i.force.start=NA,
                      i.force.week.53=F){
  
  # check parameters
  if (is.null(i.data)) {return()}
  if (is.null(dim(i.data))) stop('Incorrect number of dimensions, input must be a data.frame.') else if (!(ncol(i.data)==1)) stop('Incorrect number of dimensions, only one season required.')
  
  if (i.force.week.53) last.week<-53 else last.week<-52
  
  # Range x fix
  if (length(i.range.x)!=2) i.range.x<-c(as.numeric(rownames(i.data)[1]),as.numeric(rownames(i.data)[NROW(i.data)]))
  week.f<-i.range.x[1]
  week.l<-i.range.x[2]
  if (!is.numeric(i.range.x) | length(i.range.x)!=2) i.range.x<-c(40,20)
  if (week.f>week.l){
    i.range.x.values<-data.frame(week.lab=c(week.f:last.week,1:week.l),week.no=1:(last.week-week.f+1+week.l))
  }else{
    i.range.x.values<-data.frame(week.lab=week.f:week.l,week.no=1:(week.l-week.f+1))
  }

  if (length(i.epidemic.thr)!=2){
    i.pre.epidemic<-F
    i.post.epidemic<-F
  }
  
  if (length(i.intensity.thr)!=3) i.intensity<-F
  
  if (!is.numeric(i.epidemic.thr) | length(i.epidemic.thr)==1) i.epidemic.thr<-rep(NA,2)
  if (!is.numeric(i.intensity.thr) | length(i.intensity.thr)==1) i.intensity.thr<-rep(NA,3)
  
  # Esquema de las semanas
  
  esquema.temporadas.1<-last.week
  if (i.range.x[1]==i.range.x[2]) i.range.x[2]<-i.range.x[1]-1
  if (i.range.x[1]<i.range.x[2]){
    esquema.temporadas.2<-max(1,i.range.x[1])
    esquema.temporadas.3<-min(esquema.temporadas.1,i.range.x[2])
    esquema.temporadas.4<-c(esquema.temporadas.2:esquema.temporadas.3)
  }else{
    esquema.temporadas.2<-min(esquema.temporadas.1,i.range.x[1])
    esquema.temporadas.3<-max(1,i.range.x[2])
    esquema.temporadas.4<-c(esquema.temporadas.2:esquema.temporadas.1,1:esquema.temporadas.3)
  }
  semanas<-length(esquema.temporadas.4)
  esquema.semanas<-data.frame(numero.semana=1:semanas,nombre.semana=esquema.temporadas.4)
  
  # Acomodamos i.data al esquema
  current.season<-i.data
  names(current.season)<-"rates"
  current.season$nombre.semana<-rownames(i.data)
  rownames(current.season)<-NULL
  current.season<-merge(esquema.semanas,current.season,by="nombre.semana",all.x=T)
  current.season<-current.season[order(current.season$numero.semana),]
  rownames(current.season)<-NULL
  
  # limitamos a la semana del informe (i.week.report)
  if (!is.na(i.week.report) & any(i.week.report==as.numeric(esquema.semanas$nombre.semana))){
    semana.report<-((1:semanas)[i.week.report==as.numeric(esquema.semanas$nombre.semana)])[1]
    if (!is.na(semana.report) & semana.report<semanas) current.season$rates[(semana.report+1):semanas]<-NA
  }else{
    if (all(is.na(current.season$rates))) semana.report<-semanas else semana.report<-max((1:semanas)[!is.na(current.season$rates)],na.rm=T)
  }
  
  # Preparacion de datos necesarios
  umbral.pre<-as.numeric(i.epidemic.thr[1])
  if (i.force.equal) umbral.pos<-as.numeric(i.epidemic.thr[1]) else umbral.pos<-as.numeric(i.epidemic.thr[2])
  duracion.media<-i.mean.length
  
  # Si el inicio forzado de la epidemia es posterior a la semana del informe, quitamos
  if (!is.na(i.force.start)) semana.inicio.forzado<-((1:semanas)[i.force.start==as.numeric(esquema.semanas$nombre.semana)])[1] else semana.inicio.forzado<-NA
  if (any(current.season$rates>umbral.pre,na.rm=T)) semana.inicio.real<-min((1:semanas)[current.season$rates>umbral.pre],na.rm=T) else semana.inicio.real<-NA
  if (!is.na(semana.inicio.forzado)){
    if (semana.inicio.forzado>semana.report) semana.inicio.forzado<-NA
  }
  if (!is.na(semana.inicio.forzado) & !is.na(semana.inicio.real)){
    if (semana.inicio.forzado==semana.inicio.real) semana.inicio.forzado<-NA
  }
  if (!is.na(semana.inicio.forzado)){
    semana.inicio<-semana.inicio.forzado
  }else{
    semana.inicio<-semana.inicio.real
  }
  
  week.peak<-which.max(current.season$rates)
  
  if (!is.na(semana.inicio)){
    # if (!is.na(semana.inicio.real)){
    #   # semana.fin.1<-(1:semanas)[current.season$rates<umbral.pos & semana.inicio.real<(1:semanas)]
    #   punto.de.busqueda<-max(semana.inicio,semana.inicio.real,na.rm=T)
    #   semana.fin.1<-(1:semanas)[current.season$rates<umbral.pos & punto.de.busqueda<(1:semanas)]
    # }else{
    #   semana.fin.1<-(1:semanas)[current.season$rates<umbral.pos & semana.inicio<(1:semanas)]
    # }
    if (i.force.length){
      semana.fin<-semana.inicio+i.mean.length
      if (semana.fin>semanas) semana.fin<-NA
    }else{
      punto.de.busqueda<-max(semana.inicio,semana.inicio.real,week.peak,na.rm=T)
      semana.fin.1<-(1:semanas)[current.season$rates<umbral.pos & punto.de.busqueda<(1:semanas)]
      if (any(semana.fin.1,na.rm=T)) semana.fin<-min(semana.fin.1,na.rm=T) else semana.fin<-NA
    }
  }else{
    semana.fin<-NA
  }
  if (!i.epidemic){
    semana.inicio<-NA
    semana.fin<-NA
  }
  limites.niveles<-as.vector(i.intensity.thr)
  #nombres.niveles<-as.character(i.flu$epi.intervals[,1])
  limites.niveles[limites.niveles<0]<-0
  
  # Datos para el grafico
  if (is.na(semana.inicio)){
    # No iniciada
    pre.umbrales.1<-rep(umbral.pre,semana.report+1)
    pre.umbrales.2<-rep(NA,semanas)
    post.umbrales.1<-rep(NA,semana.report+1)
    post.umbrales.2<-rep(NA,semanas)
    intensidades.1<-array(dim=c(semanas,3))
    intensidades.2<-array(dim=c(semanas,3)) 
  }else{
    if (is.na(semana.fin)){
      # Iniciada y no finalizada
      pre.umbrales.1<-rep(umbral.pre,semana.inicio-1)
      pre.umbrales.2<-rep(NA,max(duracion.media,semana.report-semana.inicio+1))
      post.umbrales.1<-rep(NA,semana.inicio-1)
      post.umbrales.2<-rep(NA,max(duracion.media,semana.report-semana.inicio+1))
      if (i.intensity){
        intensidades.1<-array(dim=c(semana.inicio-1,3))
        intensidades.2<-matrix(rep(limites.niveles,max(duracion.media,semana.report-semana.inicio+1)),ncol=3,byrow=T)
      }else{
        intensidades.1<-array(dim=c(semana.inicio-1,3))
        intensidades.2<-array(dim=c(max(duracion.media,semana.report-semana.inicio+1),3))
      }
    }else{
      # Iniciada y finalizada
      pre.umbrales.1<-rep(umbral.pre,semana.inicio-1)
      pre.umbrales.2<-rep(NA,semana.fin-semana.inicio)
      post.umbrales.1<-rep(NA,semana.inicio-1)
      post.umbrales.2<-rep(NA,semana.fin-semana.inicio)
      if (i.intensity){
        intensidades.1<-array(dim=c(semana.inicio-1,3))
        intensidades.2<-matrix(rep(limites.niveles,semana.fin-semana.inicio),ncol=3,byrow=T)
      }else{
        intensidades.1<-array(dim=c(semana.inicio-1,3))
        intensidades.2<-array(dim=c(semana.fin-semana.inicio,3))
      }
    }
  }
  if (i.post.epidemic){
    pre.umbrales.3<-rep(NA,semanas)
    post.umbrales.3<-rep(umbral.pos,semanas)
  }else{
    pre.umbrales.3<-rep(NA,semanas)
    post.umbrales.3<-rep(NA,semanas)
  }
  pre.umbrales<-c(pre.umbrales.1,pre.umbrales.2,pre.umbrales.3)[1:semanas]
  post.umbrales<-c(post.umbrales.1,post.umbrales.2,post.umbrales.3)[1:semanas]
  intensidades.3<-array(dim=c(semanas,3))
  intensidades<-rbind(intensidades.1,intensidades.2,intensidades.3)[1:semanas,]
  
  labels<-c("Weekly rates","Epidemic thr.","Medium thr.","High thr.","Very high thr.","Post thr.","Start","End")
  haspoints<-c(T,F,F,F,F,F,T,T)
  haslines<-c(T,T,T,T,T,T,F,F)
  shapes<-c(21,NA,NA,NA,NA,NA,21,21)
  colors<-c("#808080","#8c6bb1","#88419d","#810f7c","#4d004b","#8c6bb1","#000000","#000000")
  fills<-c("#000000","#000000","#000000","#000000","#000000","#000000","#FF0000","#40FF40")
  sizes<-c(3,1,1,1,1,1,4,4)
  linetypes<-c("solid", "dashed", "dashed", "dashed", "dashed","dashed","blank","blank")
  
  dgraf<-as.data.frame(cbind(current.season$rates,pre.umbrales,intensidades,post.umbrales))
  dgraf$start<-NA
  dgraf$end<-NA
  if (!is.na(semana.inicio)) dgraf$start[semana.inicio]<-current.season$rates[semana.inicio]
  if (!is.na(semana.fin)) dgraf$end[semana.fin]<-current.season$rates[semana.fin]
  names(dgraf)<-labels
  dgraf$week<-1:semanas
  
  dgrafgg<-melt(dgraf,id="week")
  
  selected.indicators<-1
  if (i.pre.epidemic) selected.indicators<-c(selected.indicators,2)
  if (i.post.epidemic) selected.indicators<-c(selected.indicators,6)
  if (i.intensity) selected.indicators<-c(selected.indicators,3:5)
  if (i.start & i.pre.epidemic) selected.indicators<-c(selected.indicators,7)
  if (i.end & i.post.epidemic) selected.indicators<-c(selected.indicators,8)
  selected.indicators<-unique(selected.indicators)
  selected.indicators<-selected.indicators[order(selected.indicators)]
  
  labels.s<-labels[selected.indicators]
  haspoints.s<-haspoints[selected.indicators]
  haslines.s<-haslines[selected.indicators]
  dgrafgg.s<-subset(dgrafgg,variable %in% labels.s)
  shapes.s<-shapes[selected.indicators]
  colors.s<-colors[selected.indicators]
  fills.s<-fills[selected.indicators]
  sizes.s<-sizes[selected.indicators]
  linetypes.s<-linetypes[selected.indicators]
  
  # Axis format for all the graphs
  # Calculate values if we want to place 20 tickmarks in the graph in the x-axis.
  
  axis.x.range.original <- c(1,semanas)
  axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 20, 1:axis.x.range.original[2],T,T)  
  axis.x.range <- axis.x.otick$range
  axis.x.values <- as.numeric(current.season$numero.semana)
  axis.x.ticks <- axis.x.otick$tickmarks
  axis.x.labels <- (current.season$nombre.semana)[axis.x.otick$tickmarks]
  # Same, for 10 tickmarks in the y-axis
  # Range y fix
  if (length(i.range.y)!=2){
    i.range.y <- c(0,1.05*max(subset(dgrafgg.s,variable!="week",select="value"),na.rm=T))
  }else{
    i.range.y <- 1.05*i.range.y
  }
  axis.y.range.original <- i.range.y
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
  axis.y.range <- axis.y.otick$range
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks
  
  gplot<-ggplot(dgrafgg.s) +
    geom_line(aes(x=week,y=value,group=variable, color=variable, linetype=variable),size=1.2) +
    geom_point(aes(x=week,y=value,group=variable, color=variable, size=variable, fill=variable, shape=variable), color="#ffffff", stroke = 0.1) +
    scale_shape_manual(values=shapes.s, name="Legend", labels=labels.s) + 
    scale_color_manual(values=colors.s, name="Legend", labels=labels.s) +
    scale_fill_manual(values=fills.s, name="Legend", labels=labels.s) +
    scale_size_manual(values=sizes.s, name="Legend", labels=labels.s) +
    scale_linetype_manual(values=linetypes.s, name="Legend", labels=labels.s) + 
    scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
    scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
    labs(title = input$textMain, x = input$textX, y = input$textY) + 
    ggthemes::theme_few()
  
    # theme(axis.text.x=element_text(size=14, col="#000040"), axis.title.x=element_text(size=16, col="#000040"),
    #    axis.text.y=element_text(size=14, col="#000040"), axis.title.y=element_text(size=16, col="#000040"),
    #    plot.title=element_text(size=20, face="bold", color="#000000", hjust=0.5))
  list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,weeklabels=current.season$nombre.semana)
       
}

# custom functions

read.data<-function(i.file,i.extension=NA,i.subset=NA,i.remove.pandemic=F,i.n.max.temp=NA,i.season.limits=NA,i.naweeks.to.remove=1){
  
  if (!file.exists(i.file)) stop("file not found")
  
  # divide the filename between path, name and extension
  temp1<-str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.[^\\.]*))?$")
  temp1[is.na(temp1)]<-""
  filepath<-temp1[2]
  filename<-paste(temp1[3],temp1[4],sep="")
  if (is.na(i.extension)){
    temp2<-str_match(filename,"^(.*)\\.([^\\.]*)$")
    temp2[is.na(temp2)]<-""  
    fileextension<-temp2[3]
    rm("temp2")
  }else fileextension<-i.extension
  
  rm("temp1")
  
  # reading data, depending the extension, different options
  if (tolower(fileextension)=="xlsx"){
    # xlsx files, openxlsx read data perfectly
    cat("Excel 2007+ file detected: ",filename,"\n",sep="")
    wb<-openxlsx::loadWorkbook(i.file)
    sheets<-openxlsx::sheets(wb)
    n.sheets<-length(sheets)
    cat("Number of sheets: ",n.sheets,"\nReading the first one: ",sheets[1],"\n",sep="")    
    data.original<-openxlsx::read.xlsx(wb,sheet=1,rowNames=T)
    cat("Read ",NROW(data.original),"rows and ",NCOL(data.original),"columns\n",sep="")
    rm("sheets","n.sheets")
  }else if (tolower(fileextension)=="xls"){
    # no xls library read headers correctly, so i use a first pass to read headers, then the data
    cat("Excel 97-2003 file detected: ",filename,"\n",sep="")
    wb <- XLConnect::loadWorkbook(i.file)
    sheets<-XLConnect::getSheets(wb)
    n.sheets<-length(sheets)
    cat("Number of sheets: ",n.sheets,"\nReading the first one: ",sheets[1],"\n",sep="")    
    data.headers<-as.character(XLConnect::readWorksheet(wb, sheet = 1, header=F, colTypes=XLC$DATA_TYPE.STRING, endRow=1))[-1]
    data.original<-XLConnect::readWorksheet(wb, sheet = 1,rownames=1, colTypes=XLC$DATA_TYPE.NUMERIC)
    names(data.original)<-data.headers
    rm("data.headers")
    cat("Read ",NROW(data.original),"rows and ",NCOL(data.original),"columns\n",sep="")
  }else if (tolower(fileextension) %in% c("csv","dat","prn","txt")){
    # text files
    # detect encoding
    temp1<-readr::guess_encoding(i.file, n_max = -1)
    temp1<-temp1[order(temp1$confidence,decreasing = T),]
    myencoding<-as.character(temp1$encoding[1])
    rm("temp1")
    # detect separator and decimal separator
    cat("Text file detected: ",filename," (encoding: ",myencoding,")\n",sep="")
    firstline<-readLines(i.file,1,encoding=myencoding)
    separators<-c(',',';','\t','\\|')
    mysep<-separators[which.max(str_count(firstline, separators))]
    restlines<-paste(readLines(i.file,encoding=myencoding)[-1],collapse="")
    decimals<-c(".",",")
    mydec<-decimals[which.max(str_count(gsub(mysep,"",restlines,fixed=T), fixed(decimals)))]
    cat("Separator is ",mysep,"\nDecimal point is ",mydec,"\n",sep="")
    data.headers<-as.character(read.delim(i.file,header=F,sep=mysep,nrows=1,colClasses="character", as.is=T, encoding = myencoding))[-1]
    data.original<-read.delim(i.file,header=T,sep=mysep,dec=mydec,row.names=1,fill=T,colClasses="numeric", as.is=T, encoding = myencoding)
    names(data.original)<-data.headers
    rm("firstline","data.headers","separators","mysep","decimals")
    cat("Read ",NROW(data.original)," rows and ",NCOL(data.original)," columns\n",sep="")
  }else{
    stop(paste("Extension not recognised\n",i.file,"\n",filepath,filename,fileextension,sep="-"))
  }
  
  # dealing with season start and end, extracts information from rownames and gets season start/end
  
  seasons.original<-data.frame(names(data.original),str_match(names(data.original),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[,-1],stringsAsFactors = F)
  names(seasons.original)<-c("season","anioi","aniof","aniow")
  seasons.original[is.na(seasons.original)]<-""
  
  # This is the original data, now we modify it, final colnames is as any of these formats:
  # 2009/2019 (starting year/ending year) - standard format
  # 2010/2011(1) (first part of the 2010/2011 season) - this is done when the season is splitted in two
  # 2009 (year) - for southern hemisphere countries, each season is one single year
  
  # Final modified data  
  data.final<-data.original
  
  # dealing with season start and end
  seasons.final<-seasons.original
  seasonsname<-seasons.final$anioi
  seasonsname[seasons.final$aniof!=""]<-paste(seasonsname[seasons.final$aniof!=""],seasons.final$aniof[seasons.final$aniof!=""],sep="/")
  seasonsname[seasons.final$aniow!=""]<-paste(seasonsname[seasons.final$aniow!=""],seasons.final$aniow[seasons.final$aniow!=""],sep="/")
  seasons.final$season<-seasonsname
  rm("seasonsname")
  
  # The read function also subsets the original dataset, subsetting data
  if (!any(is.na(i.subset))){
    mysubset<-i.subset
    data.final<-data.final[mysubset]
    seasons.final<-seasons.final[mysubset,]    
    rm("mysubset")
  }
  
  # And removes the pandemic seasons if requested to
  if (i.remove.pandemic){
    mysubset<-seasons.final$anioi!="2009"
    data.final<-data.final[mysubset]
    seasons.final<-seasons.final[mysubset,]    
    rm("mysubset")
  }
  
  # Limit to the maximum number of seasons
  if (!is.na(i.n.max.temp)){
    mysubset<-(max(NCOL(data.final)-i.n.max.temp+1,1)):NCOL(data.final)
    data.final<-data.final[mysubset]
    seasons.final<-seasons.final[mysubset,]    
    rm("mysubset")
  }
  
  # Limiting weeks
  if (!any(is.na(i.season.limits))){
    if (i.season.limits[1]<i.season.limits[2]) validweeks<-as.character(i.season.limits[1]:i.season.limits[2]) else validweeks<-as.character(c(i.season.limits[1]:53,1:i.season.limits[2]))
    data.final<-data.final[rownames(data.final) %in% validweeks,]
  }  
  
  # Seasons with more than a given percentage of NAs are removed
  if (!is.na(i.naweeks.to.remove)){
    mysubset<-apply(data.final, 2, function(x) sum(is.na(x))/length(x))<=i.naweeks.to.remove
    data.final<-data.final[mysubset]
    seasons.final<-seasons.final[mysubset,]    
    rm("mysubset")
  }
  
  cat("Subsetting...\n")
  cat("Final data: ",NROW(data.final)," rows and ",NCOL(data.final)," columns\n\n",sep="")
  
  names(data.final)<-seasons.final$season    
  return(data.final)
}

# Function to select the seasons to use MEM using From, To, Exclude, Use pandemic and Maximum number of seasons fields

select.columns<-function(i.names, i.from, i.to, i.exclude="", i.include="", i.pandemic=T, i.seasons=NA){
  # cat("Select columns function\n")
  # cat("i.from:->",i.from,"<-\n",sep="")
  # cat("i.to:->",i.to,"<-\n",sep="")
  # cat("i.exclude:->",paste(i.exclude,collapse=","),"<-\n",sep="")
  # cat("i.include:->",paste(i.include,collapse=","),"<-\n",sep="")
  # cat("i.pandemic:->",i.pandemic,"<-\n",sep="")
  # cat("i.seasons:->",i.seasons,"<-\n",sep="")
  indexes<-1:length(i.names)
  toinclude<-indexes[i.names %in% i.include]
  if (!(i.from=="" | is.na(i.from) | is.null(i.from)) & (i.from %in% i.names)) from<-grep(i.from,i.names,fixed=T) else from<-1  
  if (!(i.to=="" | is.na(i.to) | is.null(i.to)) & (i.to %in% i.names)) to<-grep(i.to,i.names,fixed=T) else to<-length(i.names)
  if (to<from) to<-from
  #cat(to,"-",from,"\n")
  #cat(i.names,"\n")
  seasons<-data.frame(i.names,str_match(i.names,"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[,-1],stringsAsFactors = F)
  names(seasons)<-c("season.original","anioi","aniof","aniow")
  seasons[is.na(seasons)]<-""
  seasonsname<-seasons$anioi
  seasonsname[seasons$aniof!=""]<-paste(seasonsname[seasons$aniof!=""],seasons$aniof[seasons$aniof!=""],sep="/")
  seasonsname[seasons$aniow!=""]<-paste(seasonsname[seasons$aniow!=""],seasons$aniow[seasons$aniow!=""],sep="/")
  seasons$season<-seasonsname
  pandemic<-grep("2009",seasons$anioi,fixed=T)
  indexes<-from:to
  if (!is.null(i.pandemic)) if (!i.pandemic & length(pandemic)>0) indexes<-indexes[pandemic!=indexes]
  if (length(indexes)>0){
    if (!is.null(i.exclude)) if (any(i.exclude!="") & any(!is.na(i.exclude))) indexes<-indexes[!(i.names[indexes] %in% i.exclude)]
    if (!is.null(i.seasons)) if (!is.na(i.seasons)) indexes<-indexes[(max(length(indexes)-i.seasons+1,1)):length(indexes)]
  }
  if (length(toinclude)>0) indexes<-unique(c(indexes,toinclude))
  indexes<-indexes[order(indexes)]
  indexes<-indexes[!(i.names[indexes] %in% c("num","vecka"))]
  #cat(paste(indexes,collapse=","),"\n")
  return(indexes)
}

#' Find tickmarks for a given range of the y-axis that best fit an optimal number of tickmarks
#' you decide. f.i: what if i want to have a graph with 8 tickmarks in a range of 34 to 345

optimal.tickmarks<-function(i.min,i.max,i.number.ticks=10,
                            i.valid.ticks=apply(expand.grid(c(1,2,2.5,5), 10^(-10:10)), 1, FUN = function(x) {x[1] * x[2]}),
                            i.include.min=F,i.include.max=F){
  # Y ahora calculo el tickmark que más se acerca a esos 10 tickmarks objetivo.
  # Option 1: free, I can put tickmarks outside c(i.min,i.max)
  if (!i.include.min){
    ticks.min<-floor(i.min/i.valid.ticks)
    ticks.max<-ceiling(i.max/i.valid.ticks)
    ticks.maxmin<-ticks.max-ticks.min+1
    n.valid.ticks<-length(i.valid.ticks)
    posicion.ticks<-(1:n.valid.ticks)[min(abs(ticks.maxmin-i.number.ticks))==abs(ticks.maxmin-i.number.ticks)][1]
    ini<-(ticks.min*i.valid.ticks)[posicion.ticks]
    fin<-(ticks.max*i.valid.ticks)[posicion.ticks]
    salto<-i.valid.ticks[posicion.ticks]
    # Tickmarks
    tickmarks<-seq(ini,fin,salto)
    # Number of ticks
    numero.ticks<-length(tickmarks)
    # Rank
    range.y<-c(ini,fin)
  }else{
    # Opcion 2: restricted, first tickmark must be i.min, and the last one i.max (if i.include.max=T, else it is the lastest tickmark not greater than i.max)
    ticks.maxmin<-1+floor((i.max-i.min)/i.valid.ticks)
    n.valid.ticks<-length(i.valid.ticks)
    posicion.ticks<-which.min(abs(ticks.maxmin-i.number.ticks))
    ini<-i.min
    fin<-i.min+((ticks.maxmin-1)*i.valid.ticks)[posicion.ticks]
    salto<-i.valid.ticks[posicion.ticks]
    # Tickmarks
    tickmarks<-seq(ini,fin,salto)
    # Number of ticks
    numero.ticks<-length(tickmarks)
    if (i.include.max) {
      fin<-i.max
      tickmarks[numero.ticks]<-i.max
    }
    # Rank
    range.y<-c(ini,fin)
  }  
  # Returning
  return(list(by=salto,number=numero.ticks,range=range.y,tickmarks=tickmarks))
}

# roundF and format

roundF <- function(x, k=2) format(round(x, k), nsmall=k)

# Fix plotly graphs

fixplotly<-function(i.plotly,i.labels,i.lines,i.points,i.xname,i.yname,i.weeklabels){
  
  nlabels<-length(i.labels)
  nlists<-length(i.plotly$x$data)
  if (nlists!=2*nlabels) return(i.plotly)
  # Show all labels
  for (i in 1:nlists) i.plotly$x$data[[i]]$showlegend<-T
  # Fix x.axis labels
  a<-strsplit(as.character(i.plotly$x$layout$xaxis$ticktext),"\\\n")
  a.len <- max(sapply(a, length))
  a.corrected <- lapply(a, function(x) {c(x, rep("", a.len - length(x)))})
  divideit<-matrix(unlist(a.corrected),nrow=length(i.plotly$x$layout$xaxis$ticktext), byrow=T)
  i.plotly$x$layout$margin$b<-(NCOL(divideit))*i.plotly$x$layout$margin$b
  i.plotly$x$layout$xaxis$ticktext<-apply(divideit,1,paste,collapse="<br>")
  # Fix labels names
  sequ<-1:nlists-nlabels*(floor((1:nlists-1)/nlabels))
  for (i in 1:nlists) i.plotly$x$data[[i]]$name<-i.labels[sequ[i]]
  # Fix text to showup
  for (i in 1:nlists){
    i.plotly$x$data[[i]]$text
    dividetext<-matrix(unlist(strsplit(i.plotly$x$data[[i]]$text,"<br>")),nrow=length(i.plotly$x$data[[i]]$text), byrow=T)
    i.plotly$x$data[[i]]$text<-paste("Week: ",i.weeklabels,"<br>",sub(i.yname,i.labels[sequ[i]],dividetext[,2]),sep="")
  }
  # For those with points and labels, i modify the mode and add the marker
  pandl<-i.points & i.lines
  index.pandl<-(1:nlabels)[pandl]
  for (i in 1:length(index.pandl)){
    i.plotly$x$data[[index.pandl[i]]]$mode<-"lines+markers"
    i.plotly$x$data[[index.pandl[i]]]$marker<-i.plotly$x$data[[index.pandl[i]+nlabels]]$marker
  }
  # Remove unnecesary legend entries
  panol<-i.points & !i.lines
  index.panol<-(1:nlabels)[panol]
  nopal<-!i.points & i.lines
  index.nopal<-(1:nlabels)[nopal]
  toremove<-c(index.pandl+nlabels,index.panol,index.nopal+nlabels)
  toremove<-toremove[order(toremove, decreasing = T)]
  # in reverse order, since removing changes order
  for (i in 1:length(toremove)) i.plotly$x$data[[toremove[i]]]<-NULL
  return(i.plotly)
}


session$onSessionEnded(function() {
  stopApp()
})
})

