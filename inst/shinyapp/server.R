source("helpers.R")
set.rzip()

shinyServer(function(input, output, session) {

#####################################
### REACTIVE FUNCTIONS
#####################################

data_model <- reactive({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    epi<-NULL
  }else{
    # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
    selectedcolumns<-select.columns(i.names=names(datfile),
                                    i.from=input$SelectFrom,
                                    i.to=input$SelectTo,
                                    i.exclude=input$SelectExclude,
                                    i.include="",
                                    i.pandemic=T,
                                    i.seasons=as.numeric(input$SelectMaximum))
    if (length(selectedcolumns)<2){
      epi<-NULL
    }else{
      epi <- memmodel(datfile[selectedcolumns],
                      i.seasons=as.numeric(input$SelectMaximum),
                      i.type.threshold=as.numeric(input$typethreshold),
                      i.tails.threshold=as.numeric(input$ntails),
                      i.type.intensity=as.numeric(input$typeintensity),
                      i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                      i.tails.intensity=as.numeric(input$ntails),
                      i.type.curve=as.numeric(input$typecurve),
                      i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                      i.type.other=as.numeric(input$typeother),
                      i.level.other=as.numeric(input$leveltypicalcurve)/100,
                      i.method=as.numeric(input$method),
                      i.param=as.numeric(input$param),
                      i.n.max=as.numeric(input$nvalues))
    }
  }
  epi
})

data_good_model <- reactive({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    good<-NULL
  }else{
    selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo,
                                    i.exclude=input$SelectExclude, i.include="",
                                    i.pandemic=T,
                                    i.seasons=as.numeric(input$SelectMaximum))
    if (length(selectedcolumns)<3){
      good<-NULL
    }else{
      good<-memgoodness(datfile[,selectedcolumns],
                        i.graph=F,
                        i.min.seasons = 3,
                        i.seasons=as.numeric(input$SelectMaximum),
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.detection.values = seq(input$paramrange[1],input$paramrange[2],by=0.1),
                        i.n.max=as.numeric(input$nvalues),
                        i.goodness.method=as.character(input$validation))
    }
  }
  good
})

data_good_global <- reactive({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    good<-NULL
  }else{
    seasons<-names(datfile)
    selectedcolumns<-select.columns(i.names=seasons, 
                                    i.from=head(seasons,1), 
                                    i.to=tail(seasons,1),
                                    i.exclude=input$SelectExclude, 
                                    i.include="",
                                    i.pandemic=T,
                                    i.seasons=NA)
    if (length(selectedcolumns)<3){
      good<-NULL
    }else{
      good<-memgoodness(datfile[,selectedcolumns],
                        i.graph=F,
                        i.min.seasons = 3,
                        i.seasons=as.numeric(input$SelectMaximum),
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.detection.values = seq(input$paramrange[1],input$paramrange[2],by=0.1),
                        i.n.max=as.numeric(input$nvalues),
                        i.goodness.method=as.character(input$validation))
    }
  }
  good
})

data_optim <- reactive({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    roca<-NULL
  }else{
    selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo,
                                    i.exclude=input$SelectExclude, i.include="",
                                    i.pandemic=T,
                                    i.seasons=as.numeric(input$SelectMaximum))
    if (length(selectedcolumns)<3){
      roca<-NULL
    }else{
      roca<-roc.analysis(datfile[,selectedcolumns],
                         i.param.values = seq(input$paramrange[1],input$paramrange[2],by=0.1),
                         i.min.seasons = 3,
                         i.graph = F,
                         i.graph.file = F,
                         i.seasons=as.numeric(input$SelectMaximum),
                         i.type.threshold=as.numeric(input$typethreshold),
                         i.tails.threshold=as.numeric(input$ntails),
                         i.type.intensity=as.numeric(input$typeintensity),
                         i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                         i.tails.intensity=as.numeric(input$ntails),
                         i.type.curve=as.numeric(input$typecurve),
                         i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                         i.type.other=as.numeric(input$typeother),
                         i.level.other=as.numeric(input$leveltypicalcurve)/100,
                         i.detection.values = seq(input$paramrange[1],input$paramrange[2],by=0.1),
                         i.n.max=as.numeric(input$nvalues),
                         i.goodness.method=as.character(input$validation))
    }
  }
  roca
})

data_evolution <- reactive({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    evo<-NULL
  }else if (NCOL(datfile)<2){
    evo<-NULL
  }else{
    evo <- memevolution(i.data=datfile,
                        i.evolution.seasons=as.numeric(input$SelectMaximum),
                        i.evolution.method = as.character(input$validation),
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues))
  }
  evo
})

data_stability <- reactive({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    sta<-NULL
  }else if (NCOL(datfile)<2){
    sta<-NULL
  }else{
    sta <- memstability(i.data=datfile,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues))
  }
  sta
})

# get_datasets <- reactive({
#   infile <- input$file
#   inname <- infile$name
#   cat("get_datasets> Name: ",inname,"\n")
#   if(is.null(infile)){
#     datasets<-NULL
#     cat("Warning: No file\n")
#   } else{
#     datasets<-read.data(i.file=infile$datapath, i.file.name=inname, i.dataset = NA, i.range.x=as.numeric(c(input$firstWeek,input$lastWeek)))$datasets
#   }
#   cat(paste(datasets,collapse=","),"\n")
#   cat("get_datasets>Returning NULL?: ",is.null(datasets),"\n\n")
#   datasets
# })
# 
# get_weeks <- reactive({
#   infile <- input$file
#   indataset <- input$dataset
#   inname <- infile$name
#   cat("get_weeks> Name: ",inname,"\n")
#   cat("get_weeks> Dataset: ",indataset,"\n")
#   if(is.null(infile)){
#     dataweeks<-NULL
#     cat("Warning: No file\n")
#   }else if(is.null(indataset)){
#     dataweeks<-NULL
#     cat("Warning: No dataset\n")
#   }else if (indataset==""){
#     dataweeks<-NULL
#     cat("Warning: No dataset\n")
#   }else{
#     dataweeks<-read.data(i.file=infile$datapath, i.file.name=inname, i.dataset = indataset, i.range.x=as.numeric(c(input$firstWeek,input$lastWeek)))$dataweeks
#   }
#   cat("get_weeks>Returning NULL?: ",is.null(dataweeks),"\n\n")
#   dataweeks
# })

read_data <- reactive({
  infile <- input$file
  indataset <- input$dataset
  inname <- infile$name
  i.range.x<-rep(NA,2)
  i.range.x[1]<-as.numeric(input$firstWeek)
  i.range.x[2]<-as.numeric(input$lastWeek)
  cat("read_data> ------------------------------------------\n")
  cat("read_data> Name: ",inname,"\n")
  cat("read_data> Dataset: ",indataset,"\n")
  cat("read_data> Range: ",i.range.x[1],"-",i.range.x[2],"\n")
  if(is.null(infile)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: No file\n")
  }else if(is.null(indataset)){
    temp1<-read.data(i.file=infile$datapath, i.file.name=inname)
    datasets=temp1$datasets
    datasetread=temp1$datasetread
    dataweeks=temp1$dataweeks
    rm("temp1")
    cat("read_data> Warning: No dataset\n")
  }else if (indataset==""){
    temp1<-read.data(i.file=infile$datapath, i.file.name=inname)
    datasets=temp1$datasets
    datasetread=temp1$datasetread
    dataweeks=temp1$dataweeks
    rm("temp1")
    cat("read_data> Warning: No dataset\n")
  }else{
    temp1<-read.data(i.file=infile$datapath, i.file.name=inname, i.dataset = indataset, i.range.x=i.range.x)
    datasets=temp1$datasets
    datasetread=temp1$datasetread
    dataweeks=temp1$dataweeks
    rm("temp1")
  }
  if(!is.null(datasetread)) datasetread<-transformseries(datasetread, i.transformation=as.numeric(input$transformation))
  cat("read_data> datasets returning NULL?: ",is.null(datasets),"\n")
  cat("read_data> dataweeks returning NULL?: ",is.null(dataweeks),"\n")
  cat("read_data> datasetread NULL?: ",is.null(datasetread),"\n")
  cat("read_data> ------------------------------------------\n")
  readdata<-list(datasets=datasets, datasetread=datasetread, dataweeks=dataweeks)
  readdata
})

observeEvent(input$file, {
  cat("observe/file> begin\n")
  readdata <- read_data()
  datfile <- readdata$datasetread
  datsheets <- readdata$datasets
  datweeks <- readdata$dataweeks
  if (!is.null(datsheets)){
    cat("observe/file> updating dataset list\n")
    updateSelectInput(session, "dataset", choices = datsheets, selected=head(datsheets,1))
  }
  cat("observe/file> end\n")
})

observeEvent(input$firstWeek, {
  cat("observe/firstWeek> begin\n")
  readdata <- read_data()
  datfile <- readdata$datasetread
  datsheets <- readdata$datasets
  datweeks <- readdata$dataweeks
  if (!is.null(datfile)){
    weeks<-rownames(datfile)
    cat("observe/firstWeek> updating surveillance week\n")
    updateSelectInput(session, "SelectSurveillanceWeek", choices =weeks, selected=rev(weeks)[1])
    updateSelectInput(session, "SelectSurveillanceForceEpidemic", choices =c("",weeks), selected="")
  }
  if (!is.null(datfile)){
    seasons<-names(datfile)
    weeks<-rownames(datfile)
    cat("observe/firstWeek> updating from/to, exclude\n")
    updateSelectInput(session, "SelectFrom", choices = seasons, selected=seasons[1])
    updateSelectInput(session, "SelectTo", choices = seasons, selected=rev(seasons)[min(2,length(seasons))])
    updateSelectInput(session, "SelectExclude", choices = seasons, selected=NULL)
    cat("observe/firstWeek> updating surveillance season, week and force epidemic\n")
    updateSelectInput(session, "SelectSurveillance", choices = seasons, selected=rev(seasons)[1])
    updateSelectInput(session, "SelectSurveillanceWeek", choices =weeks, selected=rev(weeks)[1])
    updateSelectInput(session, "SelectSurveillanceForceEpidemic", choices =c("",weeks), selected="")
    cat("observe/firstWeek> updating visualize seasons list\n")
    updateSelectInput(session, "SelectSeasons", choices = seasons, selected=NULL)
    cat("observe/firstWeek> updating timing plots\n")
    lapply(seasons, function(s){output[[paste0("tbdTiming_",as.character(s))]] <- renderPlotly({
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbmTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbvTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
  }  
  cat("observe/firstWeek> end\n")
})

observeEvent(input$lastWeek, {
  cat("observe/lastWeek> begin\n")
  readdata <- read_data()
  datfile <- readdata$datasetread
  datsheets <- readdata$datasets
  datweeks <- readdata$dataweeks
  if (!is.null(datfile)){
    weeks<-rownames(datfile)
    cat("observe/lastWeek> updating surveillance week\n")
    updateSelectInput(session, "SelectSurveillanceWeek", choices =weeks, selected=rev(weeks)[1])
    updateSelectInput(session, "SelectSurveillanceForceEpidemic", choices =c("",weeks), selected="")
  }
  if (!is.null(datfile)){
    seasons<-names(datfile)
    weeks<-rownames(datfile)
    cat("observe/lastWeek> updating from/to, exclude\n")
    updateSelectInput(session, "SelectFrom", choices = seasons, selected=seasons[1])
    updateSelectInput(session, "SelectTo", choices = seasons, selected=rev(seasons)[min(2,length(seasons))])
    updateSelectInput(session, "SelectExclude", choices = seasons, selected=NULL)
    cat("observe/lastWeek> updating surveillance season, week and force epidemic\n")
    updateSelectInput(session, "SelectSurveillance", choices = seasons, selected=rev(seasons)[1])
    updateSelectInput(session, "SelectSurveillanceWeek", choices =weeks, selected=rev(weeks)[1])
    updateSelectInput(session, "SelectSurveillanceForceEpidemic", choices =c("",weeks), selected="")
    cat("observe/lastWeek> updating visualize seasons list\n")
    updateSelectInput(session, "SelectSeasons", choices = seasons, selected=NULL)
    cat("observe/lastWeek> updating timing plots\n")
    lapply(seasons, function(s){output[[paste0("tbdTiming_",as.character(s))]] <- renderPlotly({
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbmTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbvTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
  }
  cat("observe/lastWeek> end\n")
})

observeEvent(input$dataset, {
  cat("observe/dataset> begin\n")
  readdata <- read_data()
  datfile <- readdata$datasetread
  datsheets <- readdata$datasets
  datweeks <- readdata$dataweeks
  if (!is.null(datweeks)){
    cat("observe/dataset> updating first/last week list\n")
    updateSelectInput(session, "firstWeek", choices = datweeks, selected=head(datweeks,1))
    updateSelectInput(session, "lastWeek", choices = datweeks, selected=tail(datweeks,1))
  }
  if (!is.null(datfile)){
    seasons<-names(datfile)
    weeks<-rownames(datfile)
    cat("observe/dataset> updating from/to, exclude\n")
    updateSelectInput(session, "SelectFrom", choices = seasons, selected=seasons[1])
    updateSelectInput(session, "SelectTo", choices = seasons, selected=rev(seasons)[min(2,length(seasons))])
    updateSelectInput(session, "SelectExclude", choices = seasons, selected=NULL)
    cat("observe/dataset> updating surveillance season, week and force epidemic\n")
    updateSelectInput(session, "SelectSurveillance", choices = seasons, selected=rev(seasons)[1])
    updateSelectInput(session, "SelectSurveillanceWeek", choices =weeks, selected=rev(weeks)[1])
    updateSelectInput(session, "SelectSurveillanceForceEpidemic", choices =c("",weeks), selected="")
    cat("observe/dataset> updating visualize seasons list\n")
    updateSelectInput(session, "SelectSeasons", choices = seasons, selected=NULL)
    cat("observe/dataset> updating timing plots\n")
    lapply(seasons, function(s){output[[paste0("tbdTiming_",as.character(s))]] <- renderPlotly({
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbmTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbvTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
  }
  cat("observe/dataset> end\n")
})

observeEvent(input$transformation, {
  cat("observe/transformation> begin\n")
  readdata <- read_data()
  datfile <- readdata$datasetread
  datsheets <- readdata$datasets
  datweeks <- readdata$dataweeks
  if (!is.null(datweeks)){
    cat("observe/transformation> updating first/last week list\n")
    updateSelectInput(session, "firstWeek", choices = datweeks, selected=head(datweeks,1))
    updateSelectInput(session, "lastWeek", choices = datweeks, selected=tail(datweeks,1))
  }
  if (!is.null(datfile)){
    seasons<-names(datfile)
    weeks<-rownames(datfile)
    cat("observe/transformation> updating from/to, exclude\n")
    updateSelectInput(session, "SelectFrom", choices = seasons, selected=seasons[1])
    updateSelectInput(session, "SelectTo", choices = seasons, selected=rev(seasons)[min(2,length(seasons))])
    updateSelectInput(session, "SelectExclude", choices = seasons, selected=NULL)
    cat("observe/transformation> updating surveillance season, week and force epidemic\n")
    updateSelectInput(session, "SelectSurveillance", choices = seasons, selected=rev(seasons)[1])
    updateSelectInput(session, "SelectSurveillanceWeek", choices =weeks, selected=rev(weeks)[1])
    updateSelectInput(session, "SelectSurveillanceForceEpidemic", choices =c("",weeks), selected="")
    cat("observe/transformation> updating visualize seasons list\n")
    updateSelectInput(session, "SelectSeasons", choices = seasons, selected=NULL)
    cat("observe/transformation> updating timing plots\n")
    lapply(seasons, function(s){output[[paste0("tbdTiming_",as.character(s))]] <- renderPlotly({
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbmTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
    lapply(seasons, function(s){output[[paste0("tbvTiming_",as.character(s))]] <- renderPlotly({
      readdata <- isolate(read_data())
      datfile <- readdata$datasetread
      if(is.null(datfile)){
        zfix<-NULL
      }else if (!(as.character(s) %in% names(datfile))){
        zfix<-NULL
      }else{
        datfile.plot<-datfile[as.character(s)]
        colors.palette<-generate_palette(i.number.series=NA,
                                         i.colObservedLines=input$colObservedLines,
                                         i.colObservedPoints=input$colObservedPoints,
                                         i.colEpidemicStart=input$colEpidemicStart,
                                         i.colEpidemicStop=input$colEpidemicStop,
                                         i.colThresholds=input$colThresholds,
                                         i.colSeasons=input$colSeasons,
                                         i.colEpidemic=input$colEpidemic)
        p <- plotSeries(datfile.plot,
                        i.plot.timing = T,
                        i.range.x=NA,
                        i.pre.epidemic=F,
                        i.post.epidemic=F,
                        i.intensity= F,
                        i.replace.x.cr=F,
                        i.textMain=input$textMain,
                        i.textX=input$textX,
                        i.textY=input$textY,
                        i.type.threshold=as.numeric(input$typethreshold),
                        i.tails.threshold=as.numeric(input$ntails),
                        i.type.intensity=as.numeric(input$typeintensity),
                        i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                        i.tails.intensity=as.numeric(input$ntails),
                        i.type.curve=as.numeric(input$typecurve),
                        i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$leveltypicalcurve)/100,
                        i.method=as.numeric(input$method),
                        i.param=as.numeric(input$param),
                        i.n.max=as.numeric(input$nvalues),
                        i.colObservedLines=colors.palette$colObservedLines,
                        i.colThresholds=colors.palette$colThresholds,
                        i.colObservedPoints=colors.palette$colObservedPoints,
                        i.colEpidemic=colors.palette$colEpidemic)
        if (is.null(p)){
          zfix<-NULL
        }else{
          z <- ggplotly(p$plot, width = 800, height = 600)
          zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
        }
      }
      zfix
    })})
  }
  cat("observe/transformation> end\n")
})

#####################################
### DEFINING TABS STRUCTURE
#####################################

#####################################
### DATA TAB
#####################################

output$tbData <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("File", tableOutput("tbdFile")),
                tabPanel("Data", 
                         DT::dataTableOutput("tbdData"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbdData_x","xlsx")),
                           column(1,downloadButton("tbdData_c","csv"))
                         )                         
                         ),
                tabPanel("Seasons", plotlyOutput("tbdSeasons", width ="100%", height ="100%")),
                tabPanel("Series",plotlyOutput("tbdSeries", width ="100%", height ="100%")),
                tabPanel("Timing",uiOutput("tbdTiming")),
                tabPanel("Evolution",uiOutput("tbdEvolution")),
                tabPanel("Stability",uiOutput("tbdStability"))
    )
  }
})

output$tbdFile <- renderTable({
  infile <- input$file
  indataset <- input$dataset
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    data.show<-data.frame(var="No file or dataset selected")
    names(data.show)=""
  }else{
    data.show<-data.frame(var1=c("File","Dataset"),var2=c(infile$name,indataset))
    names(data.show)=c("","")
  }
  data.show
})

output$tbdData <- DT::renderDataTable({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    datatoshow<-NULL
  }else{
    # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
    selectedcolumns<-select.columns(i.names=names(datfile),
                                    i.from="",
                                    i.to="",
                                    i.exclude="",
                                    i.include="",
                                    i.pandemic=T,
                                    i.seasons=NA)
    if (length(selectedcolumns)>0){
      datatoshow<-format(round(datfile[selectedcolumns], 2), nsmall=2)
    }else{
      datatoshow<-data.frame(Message="No data selected",row.names = NULL)
    }
  }
  datatoshow
}, 
#extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))
options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

# observeEvent(input$tbdData_x, {
#   readdata <- read_data()
#   datfile <- readdata$datasetread
#   if(!is.null(datfile)){
#     selectedcolumns<-select.columns(i.names=names(datfile), i.from="", i.to="", i.exclude="", i.include="", i.pandemic=T, i.seasons=NA)
#     if (length(selectedcolumns)>0) export.mydata(i.data=datfile[selectedcolumns], i.sheet=input$dataset, i.rownames="Week no", i.format="xlsx")
#   }
# })

# observeEvent(input$tbdData_c, {
#   readdata <- read_data()
#   datfile <- readdata$datasetread
#   if(!is.null(datfile)){
#     selectedcolumns<-select.columns(i.names=names(datfile), i.from="", i.to="", i.exclude="", i.include="", i.pandemic=T, i.seasons=NA)
#     if (length(selectedcolumns)>0) export.mydata(i.data=datfile[selectedcolumns], i.sheet=input$dataset, i.rownames="Week no", i.format="csv")
#   }
# })

output$tbdData_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(!is.null(datfile)){
      selectedcolumns<-select.columns(i.names=names(datfile), i.from="", i.to="", i.exclude="", i.include="", i.pandemic=T, i.seasons=NA)
      if (length(selectedcolumns)>0) export.mydata(i.data=datfile[selectedcolumns], i.file=file, 
                                                   i.sheet="Data", i.rownames="Week no", i.format="xlsx")
    }
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbdData_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(!is.null(datfile)){
      selectedcolumns<-select.columns(i.names=names(datfile), i.from="", i.to="", i.exclude="", i.include="", i.pandemic=T, i.seasons=NA)
      if (length(selectedcolumns)>0) export.mydata(i.data=datfile[selectedcolumns], i.file=file, 
                                                   i.sheet="Data", i.rownames="Week no", i.format="csv")
    }
  },
  contentType="text/csv"
)

output$tbdSeasons <- renderPlotly({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    zfix<-NULL
  }else{
    datamodel<-data_model()
    if(is.null(datamodel)){
      zfix<-NULL
    }else{
      datfile.plot<-datfile
      e.thr<-datamodel$epidemic.thresholds
      i.thr<-datamodel$intensity.thresholds
      colors.palette<-generate_palette(i.number.series=NCOL(datfile.plot),
                                       i.colObservedLines=input$colObservedLines,
                                       i.colObservedPoints=input$colObservedPoints,
                                       i.colEpidemicStart=input$colEpidemicStart,
                                       i.colEpidemicStop=input$colEpidemicStop,
                                       i.colThresholds=input$colThresholds,
                                       i.colSeasons=input$colSeasons,
                                       i.colEpidemic=input$colEpidemic)
      p <- plotSeasons(datfile.plot,
                       i.epidemic.thr=e.thr,
                       i.intensity.thr=i.thr,
                       i.pre.epidemic = as.logical(input$preepidemicthr),
                       i.post.epidemic = as.logical(input$postepidemicthr),
                       i.intensity = as.logical(input$intensitythr),
                       i.textMain=input$textMain,
                       i.textX=input$textX,
                       i.textY=input$textY,
                       i.type.threshold=as.numeric(input$typethreshold),
                       i.tails.threshold=as.numeric(input$ntails),
                       i.type.intensity=as.numeric(input$typeintensity),
                       i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                       i.tails.intensity=as.numeric(input$ntails),
                       i.type.curve=as.numeric(input$typecurve),
                       i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                       i.type.other=as.numeric(input$typeother),
                       i.level.other=as.numeric(input$leveltypicalcurve)/100,
                       i.method=as.numeric(input$method),
                       i.param=as.numeric(input$param),
                       i.n.max=as.numeric(input$nvalues),
                       i.colObservedPoints=colors.palette$colObservedPoints,
                       i.colSeasons=colors.palette$colSeasons,
                       i.colThresholds=colors.palette$colThresholds)
      if (is.null(p)){
        zfix<-NULL
      }else{
        z <- ggplotly(p$plot, width = 800, height = 600)
        zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
      }
    }
  }
  zfix
})

output$tbdSeries <- renderPlotly({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    zfix<-NULL
  }else{
    datamodel<-data_model()
    if(is.null(datamodel)){
      zfix<-NULL
    }else{
      datfile.plot<-datfile
      e.thr<-datamodel$epidemic.thresholds
      i.thr<-datamodel$intensity.thresholds
      colors.palette<-generate_palette(i.number.series=NA,
                                       i.colObservedLines=input$colObservedLines,
                                       i.colObservedPoints=input$colObservedPoints,
                                       i.colEpidemicStart=input$colEpidemicStart,
                                       i.colEpidemicStop=input$colEpidemicStop,
                                       i.colThresholds=input$colThresholds,
                                       i.colSeasons=input$colSeasons,
                                       i.colEpidemic=input$colEpidemic)
      p <- plotSeries(i.data=datfile.plot,
                      i.plot.timing = T,
                      i.range.x=NA,
                      i.pre.epidemic=as.logical(input$preepidemicthr),
                      i.post.epidemic=as.logical(input$postepidemicthr),
                      i.epidemic.thr=e.thr,
                      i.intensity= as.logical(input$intensitythr),
                      i.intensity.thr=i.thr,
                      i.range.y=NA,
                      i.replace.x.cr=T,
                      i.textMain=input$textMain,
                      i.textX=input$textX,
                      i.textY=input$textY,
                      i.type.threshold=as.numeric(input$typethreshold),
                      i.tails.threshold=as.numeric(input$ntails),
                      i.type.intensity=as.numeric(input$typeintensity),
                      i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                      i.tails.intensity=as.numeric(input$ntails),
                      i.type.curve=as.numeric(input$typecurve),
                      i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                      i.type.other=as.numeric(input$typeother),
                      i.level.other=as.numeric(input$leveltypicalcurve)/100,
                      i.method=as.numeric(input$method),
                      i.param=as.numeric(input$param),
                      i.n.max=as.numeric(input$nvalues),
                      i.colObservedLines=colors.palette$colObservedLines,
                      i.colThresholds=colors.palette$colThresholds,
                      i.colObservedPoints=colors.palette$colObservedPoints,
                      i.colEpidemic=colors.palette$colEpidemic)
      if (is.null(p)){
        zfix<-NULL
      }else{
        z <- ggplotly(p$plot, width = 800, height = 600)
        zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
      }
    }
  }
  zfix
})

output$tbdTiming = renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)) {
    return(NULL)
  }else{
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
              call("tabPanel",s,call('plotlyOutput',outputId=paste0("tbdTiming_",s), width ="100%", height ="100%"))
            })
    )
  }
})

output$tbdEvolution <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Duration", plotlyOutput("tbdEduration", width ="100%", height ="100%")),
                tabPanel("Start",plotlyOutput("tbdEstart", width ="100%", height ="100%")),
                tabPanel("Percentage", plotlyOutput("tbdEpercentage", width ="100%", height ="100%")),
                tabPanel("Thresholds",plotlyOutput("tbdEthresholds", width ="100%", height ="100%")),
                tabPanel("Scheme", formattableOutput("tbdEscheme")),
                tabPanel("Details", 
                         DT::dataTableOutput("tbdEdetails"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbdEdetails_x","xlsx")),
                           column(1,downloadButton("tbdEdetails_c","csv"))
                         )
                         )
    )
  }
})


output$tbdEduration <- renderPlotly({
  dataevolution <- data_evolution()$evolution.data
  if(is.null(dataevolution)){
    zfix<-NULL
  }else{
    indicators<-c("durationll","duration","durationul")
    datfile.plot<-dataevolution[indicators]
    names(datfile.plot)<-c("Lower limit","Duration","Upper limit")
    # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
    #rownames(datfile.plot)<-gsub("/","\n",rownames(datfile.plot))
    colors.palette<-generate_palette(i.number.series=3,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.range.y.labels=NA,
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.colors=colors.palette$colSeasons,
                   i.fills=colors.palette$colSeasons,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.replace.x.cr=T,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
    }
  }
  zfix
})

output$tbdEstart <- renderPlotly({
  dataevolution <- data_evolution()$evolution.data
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(dataevolution)){
    zfix<-NULL
  }else{
    indicators<-c("startll","start","startul")
    datfile.plot<-dataevolution[indicators]
    names(datfile.plot)<-c("Lower limit","Start","Upper limit")
    # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
    #rownames(datfile.plot)<-gsub("/","\n",rownames(datfile.plot))
    colors.palette<-generate_palette(i.number.series=3,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.range.y.labels=rownames(datfile),
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.colors=colors.palette$colSeasons,
                   i.fills=colors.palette$colSeasons,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.replace.x.cr=T,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )

    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
      # fix to replace relative to absolute weeks
      for (i in 1:3) zfix$x$data[[i]]$text<-paste("Week: ",rownames(datfile.plot),"<br />",names(datfile.plot),": ", rownames(datfile)[datfile.plot[,i]],sep="")
    }
  }
  zfix
})

output$tbdEpercentage <- renderPlotly({
  dataevolution <- data_evolution()$evolution.data
  if(is.null(dataevolution)){
    zfix<-NULL
  }else{
    indicators<-c("percentagell","percentage","percentageul")
    datfile.plot<-dataevolution[indicators]
    names(datfile.plot)<-c("Lower limit","Epidemic percentage","Upper limit")
    # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
    #rownames(datfile.plot)<-gsub("/","\n",rownames(datfile.plot))
    colors.palette<-generate_palette(i.number.series=3,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.range.y.labels=NA,
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.colors=colors.palette$colSeasons,
                   i.fills=colors.palette$colSeasons,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.replace.x.cr=T,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
    }
  }
  zfix
})

output$tbdEthresholds <- renderPlotly({
  dataevolution <- data_evolution()$evolution.data
  if(is.null(dataevolution)){
    zfix<-NULL
  }else{
    indicators<-c("epidemic","medium","high","veryhigh","postepidemic")
    datfile.plot<-dataevolution[indicators]
    names(datfile.plot)<-c("Epidemic","Medium int.","High int.","Very high int.","Post-epidemic")
    colors.palette<-generate_palette(i.number.series=NCOL(datfile.plot),
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
    #rownames(datfile.plot)<-gsub("/","\n",rownames(datfile.plot))
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.range.y.labels=NA,
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.colors=colors.palette$colThresholds,
                   i.fills=colors.palette$colThresholds,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.replace.x.cr=T,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
    }
  }
  zfix
})

# output$tbdEscheme <- DT::renderDataTable({
#   dataevolution <- data_evolution()
#   if(is.null(dataevolution)){
#     datashow<-NULL
#   }else{
#     datashow<-dataevolution$evolution.seasons
#   }
#   datashow
# }, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

output$tbdEscheme <- renderFormattable({
  dataevolution <- data_evolution()
  if(is.null(dataevolution)){
    datashow<-NULL
  }else{
    temp1<-dataevolution$evolution.seasons
    datashow<-formattable(temp1, apply(temp1, 2, 
       function(noxneeded) formatter("span", style = x ~ style(color = ifelse(x, "green", "red")), x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))))
    )
  }
  datashow
})

output$tbdEdetails <- DT::renderDataTable({
  dataevolution <- data_evolution()
  if(is.null(dataevolution)){
    datashow<-NULL
  }else{
    datashow<-format(round(dataevolution$evolution.data, 2), nsmall=2)
    names(datashow)<-c("Seasons","Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
  }
  datashow
}, 
#extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))
options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

# observeEvent(input$tbdEdetails_x, {
#   dataevolution <- data_evolution()
#   datashow<-dataevolution$evolution.data
#   names(datashow)<-c("Seasons","Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
#   if(!is.null(dataevolution)) export.mydata(i.data=datashow, i.sheet="Evolution", i.rownames="Season", i.format="xlsx")
# })
# 
# observeEvent(input$tbdEdetails_c, {
#   dataevolution <- data_evolution()
#   datashow<-dataevolution$evolution.data
#   names(datashow)<-c("Seasons","Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
#   if(!is.null(dataevolution)) export.mydata(i.data=datashow, i.sheet="Evolution", i.rownames="Season", i.format="csv")
# })

output$tbdEdetails_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
      dataevolution <- data_evolution()
      datashow<-dataevolution$evolution.data
      names(datashow)<-c("Seasons","Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
      if(!is.null(dataevolution)) export.mydata(i.data=datashow, i.file=file,
                                                i.sheet="Evolution", i.rownames="Season", i.format="xlsx")
    
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbdEdetails_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
      dataevolution <- data_evolution()
      datashow<-dataevolution$evolution.data
      names(datashow)<-c("Seasons","Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
      if(!is.null(dataevolution)) export.mydata(i.data=datashow, i.file=file, 
                                                i.sheet="Evolution", i.rownames="Season", i.format="csv")
  },
  contentType="text/csv"
)

output$tbdStability <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Duration", plotlyOutput("tbdSduration", width ="100%", height ="100%")),
                tabPanel("Start",plotlyOutput("tbdSstart", width ="100%", height ="100%")),
                tabPanel("Percentage", plotlyOutput("tbdSpercentage", width ="100%", height ="100%")),
                tabPanel("Thresholds",plotlyOutput("tbdSthresholds", width ="100%", height ="100%")),
                #tabPanel("Scheme", DT::dataTableOutput("tbdSscheme")),
                tabPanel("Scheme", formattableOutput("tbdSscheme")),
                tabPanel("Details", 
                         DT::dataTableOutput("tbdSdetails"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbdSdetails_x","xlsx")),
                           column(1,downloadButton("tbdSdetails_c","csv"))
                         ))
    )
  }
})

output$tbdSduration <- renderPlotly({
  datastability <- data_stability()$stability.data
  if(is.null(datastability)){
    zfix<-NULL
  }else{
    indicators<-c("durationll","duration","durationul")
    datfile.plot<-datastability[indicators]
    names(datfile.plot)<-c("Lower limit","Duration","Upper limit")
    colors.palette<-generate_palette(i.number.series=3,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.range.y.labels=NA,
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.colors=colors.palette$colSeasons,
                   i.fills=colors.palette$colSeasons,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
    }
  }
  zfix
})

output$tbdSstart <- renderPlotly({
  datastability <- data_stability()$stability.data
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datastability)){
    zfix<-NULL
  }else{
    indicators<-c("startll","start","startul")
    datfile.plot<-datastability[indicators]
    names(datfile.plot)<-c("Lower limit","Start","Upper limit")
    colors.palette<-generate_palette(i.number.series=3,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.range.y.labels=rownames(datfile),
                   i.colors=colors.palette$colSeasons,
                   i.fills=colors.palette$colSeasons,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
      # fix to replace relative to absolute weeks
      for (i in 1:3) zfix$x$data[[i]]$text<-paste("Week: ",rownames(datfile.plot),"<br />",names(datfile.plot),": ", rownames(datfile)[datfile.plot[,i]],sep="")
    }
  }
  zfix
})

output$tbdSpercentage <- renderPlotly({
  datastability <- data_stability()$stability.data
  if(is.null(datastability)){
    zfix<-NULL
  }else{
    indicators<-c("percentagell","percentage","percentageul")
    datfile.plot<-datastability[indicators]
    names(datfile.plot)<-c("Lower limit","Epidemic percentage","Upper limit")
    colors.palette<-generate_palette(i.number.series=3,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.range.y.labels=NA,
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.colors=colors.palette$colSeasons,
                   i.fills=colors.palette$colSeasons,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
    }
  }
  zfix
})

output$tbdSthresholds <- renderPlotly({
  datastability <- data_stability()$stability.data
  if(is.null(datastability)){
    zfix<-NULL
  }else{
    indicators<-c("epidemic","medium","high","veryhigh","postepidemic")
    datfile.plot<-datastability[indicators]
    names(datfile.plot)<-c("Epidemic","Medium int.","High int.","Very high int.","Post-epidemic")
    colors.palette<-generate_palette(i.number.series=NCOL(datfile.plot),
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p<-plotGeneric(datfile.plot,
                   i.range.y=NA,
                   i.range.y.labels=NA,
                   i.shapes=rep(21,NCOL(datfile.plot)),
                   i.colors=colors.palette$colThresholds,
                   i.fills=colors.palette$colThresholds,
                   i.sizes=rep(3,NCOL(datfile.plot)),
                   i.linetypes=rep("solid",NCOL(datfile.plot)),
                   i.linesize=1,
                   i.textMain=input$textMain,
                   i.textX=input$textX,
                   i.textY=input$textY
    )
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,
                      names(datfile.plot),
                      rep(T,NCOL(datfile.plot)),
                      rep(T,NCOL(datfile.plot)),
                      "num",
                      "value",
                      rownames(datfile.plot))
    }
  }
  zfix
})

# output$tbdSscheme <- DT::renderDataTable({
#   datastability <- data_stability()
#   if(is.null(datastability)){
#     datashow<-NULL
#   }else{
#     datashow<-datastability$stability.seasons
#   }
#   datashow
# }, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

output$tbdSscheme <- renderFormattable({
  datastability <- data_stability()
  if(is.null(datastability)){
    datashow<-NULL
  }else{
    temp1<-datastability$stability.seasons
    datashow<-formattable(temp1, apply(temp1, 2,
      function(noxneeded) formatter("span", style = x ~ style(color = ifelse(x, "green", "red")), x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))))
    )
  }
  datashow
})

output$tbdSdetails <- DT::renderDataTable({
  datastability <- data_stability()
  if(is.null(datastability)){
    datashow<-NULL
  }else{
    datashow<-format(round(datastability$stability.data, 2), nsmall=2)
    names(datashow)<-c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
  }
  datashow
}, 
#extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))
options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

# observeEvent(input$tbdSdetails_x, {
#   datastability <- data_stability()
#   datashow<-datastability$stability.data
#   names(datashow)<-c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
#   if(!is.null(datastability)) export.mydata(i.data=datashow, i.sheet="Stability", i.rownames="Seasons", i.format="xlsx")
# })
# 
# observeEvent(input$tbdSdetails_c, {
#   datastability <- data_stability()
#   datashow<-datastability$stability.data
#   names(datashow)<-c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
#   if(!is.null(datastability)) export.mydata(i.data=datashow, i.sheet="Stability", i.rownames="Seasons", i.format="csv")
# })

output$tbdSdetails_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    datastability <- data_stability()
    datashow<-datastability$stability.data
    names(datashow)<-c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
    if(!is.null(datastability)) export.mydata(i.data=datashow, i.file = file, 
                                              i.sheet="Stability", i.rownames="Seasons", i.format="xlsx")
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbdSdetails_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    datastability <- data_stability()
    datashow<-datastability$stability.data
    names(datashow)<-c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
    if(!is.null(datastability)) export.mydata(i.data=datashow, i.file = file, 
                                              i.sheet="Stability", i.rownames="Seasons", i.format="csv")
  },
  contentType="text/csv"
)

#####################################
### MODEL TAB
#####################################

output$tbModel <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Data", 
                         DT::dataTableOutput("tbmData"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbmData_x","xlsx")),
                           column(1,downloadButton("tbmData_c","csv"))
                         )
                         ),
                tabPanel("Seasons", plotlyOutput("tbmSeasons", width ="100%", height ="100%")),
                tabPanel("Series",plotlyOutput("tbmSeries", width ="100%", height ="100%")),
                tabPanel("Timing",uiOutput("tbmTiming")),
                tabPanel("MEM", uiOutput("tbmMem")),
                tabPanel("Goodness",uiOutput("tbmGoodness")),
                tabPanel("Optimize",uiOutput("tbmOptimize"))
    )
  }
})

output$tbmData <- DT::renderDataTable({
  datamodel<-data_model()
  if(is.null(datamodel)) {
    datatoshow<-data.frame(Message="No data selected",row.names = NULL)
  }else{
    datatoshow<-format(round(datamodel$param.data, 2), nsmall=2)
  }
  datatoshow
}, 
#extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))
options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

# observeEvent(input$tbmData_x, {
#   datamodel<-data_model()
#   if(!is.null(datamodel)) export.mydata(i.data=datamodel$param.data, i.sheet=input$dataset, i.rownames="Week no", i.format="xlsx")
# })
# 
# observeEvent(input$tbmData_c, {
#   datamodel<-data_model()
#   if(!is.null(datamodel)) export.mydata(i.data=datamodel$param.data, i.sheet=input$dataset, i.rownames="Week no", i.format="csv")
# })

output$tbmData_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    datamodel<-data_model()
    if(!is.null(datamodel)) export.mydata(i.data=datamodel$param.data, i.file = file, 
                                          i.sheet="Model data", i.rownames="Week no", i.format="xlsx")
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbmData_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    datamodel<-data_model()
    if(!is.null(datamodel)) export.mydata(i.data=datamodel$param.data, i.file = file, 
                                          i.sheet="Model data", i.rownames="Week no", i.format="csv")
  },
  contentType="text/csv"
)

output$tbmSeasons <- renderPlotly({
  datamodel<-data_model()
  if(is.null(datamodel)){
    zfix<-NULL
  }else{
    datfile.plot<-datamodel$param.data
    e.thr<-datamodel$epidemic.thresholds
    i.thr<-datamodel$intensity.thresholds
    colors.palette<-generate_palette(i.number.series=NCOL(datfile.plot),
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p <- plotSeasons(datfile.plot,
                     i.epidemic.thr=e.thr,
                     i.intensity.thr=i.thr,
                     i.pre.epidemic = as.logical(input$preepidemicthr),
                     i.post.epidemic = as.logical(input$postepidemicthr),
                     i.intensity = as.logical(input$intensitythr),
                     i.textMain=input$textMain,
                     i.textX=input$textX,
                     i.textY=input$textY,
                     i.type.threshold=as.numeric(input$typethreshold),
                     i.tails.threshold=as.numeric(input$ntails),
                     i.type.intensity=as.numeric(input$typeintensity),
                     i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                     i.tails.intensity=as.numeric(input$ntails),
                     i.type.curve=as.numeric(input$typecurve),
                     i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                     i.type.other=as.numeric(input$typeother),
                     i.level.other=as.numeric(input$leveltypicalcurve)/100,
                     i.method=as.numeric(input$method),
                     i.param=as.numeric(input$param),
                     i.n.max=as.numeric(input$nvalues),
                     i.colObservedPoints=colors.palette$colObservedPoints,
                     i.colSeasons=colors.palette$colSeasons,
                     i.colThresholds=colors.palette$colThresholds)
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
    }
  }
  zfix

})

output$tbmSeries <- renderPlotly({
  datamodel<-data_model()
  if(is.null(datamodel)){
    zfix<-NULL
  }else{
    datfile.plot<-datamodel$param.data
    e.thr<-datamodel$epidemic.thresholds
    i.thr<-datamodel$intensity.thresholds
    colors.palette<-generate_palette(i.number.series=NA,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p <- plotSeries(i.data=datfile.plot,
                    i.plot.timing = T,
                    i.range.x=NA,
                    i.pre.epidemic=as.logical(input$preepidemicthr),
                    i.post.epidemic=as.logical(input$postepidemicthr),
                    i.epidemic.thr=e.thr,
                    i.intensity= as.logical(input$intensitythr),
                    i.intensity.thr=i.thr,
                    i.range.y=NA,
                    i.replace.x.cr=T,
                    i.textMain=input$textMain,
                    i.textX=input$textX,
                    i.textY=input$textY,
                    i.type.threshold=as.numeric(input$typethreshold),
                    i.tails.threshold=as.numeric(input$ntails),
                    i.type.intensity=as.numeric(input$typeintensity),
                    i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                    i.tails.intensity=as.numeric(input$ntails),
                    i.type.curve=as.numeric(input$typecurve),
                    i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                    i.type.other=as.numeric(input$typeother),
                    i.level.other=as.numeric(input$leveltypicalcurve)/100,
                    i.method=as.numeric(input$method),
                    i.param=as.numeric(input$param),
                    i.n.max=as.numeric(input$nvalues),
                    i.colObservedLines=colors.palette$colObservedLines,
                    i.colThresholds=colors.palette$colThresholds,
                    i.colObservedPoints=colors.palette$colObservedPoints,
                    i.colEpidemic=colors.palette$colEpidemic)
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
    }
  }
  zfix
})

output$tbmTiming = renderUI({
  datamodel<-data_model()
  if(is.null(datamodel)){
    return(NULL)
  }else{
    datfile.plot<-datamodel$param.data
    tabnames<-names(datfile.plot)
    do.call(tabsetPanel,
            ## Create a set of tabPanel functions dependent on tabnames
            lapply(tabnames,function(s){
              ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
              ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
              call("tabPanel",s,call('plotlyOutput',outputId=paste0("tbmTiming_",s), width ="100%", height ="100%"))
            })
    )
  }
})

output$tbmMem <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Estimators", uiOutput("tbmMemSummary")),
                tabPanel("Detailed", verbatimTextOutput("tbmMemOutput")),
                tabPanel("Graphs",uiOutput("tbmMemGraph"))
    )
  }
})

output$tbmMemSummary <- renderUI({
  datamodel<-data_model()
  if(is.null(datamodel)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(datamodel$n.seasons, "Seasons in the model", icon = icon("heartbeat"), width=3, color="light-blue"),
      valueBox(datamodel$ci.start[2,2], "Average epidemic start week", icon = icon("heartbeat"), width=3, color="light-blue"),
      valueBox(format(round(datamodel$ci.length[1,2], 2), nsmall=1), "Average epidemic length", icon = icon("heartbeat"), width=3, color="light-blue"),
      valueBox(paste0(format(round(datamodel$ci.percent[2], 2), nsmall=1), "%"), "Epidemic percentage", icon = icon("heartbeat"), width=3, color="light-blue"),
      valueBox(format(round(datamodel$pre.post.intervals[1,3], 2), nsmall=1), "Epidemic threshold", icon = icon("thermometer-1"), width=3, color="green"),
      valueBox(format(round(datamodel$epi.intervals[1,4], 2), nsmall=1), "Medium threshold", icon = icon("thermometer-2"), width=3, color="yellow"),
      valueBox(format(round(datamodel$epi.intervals[2,4], 2), nsmall=1), "High threshold", icon = icon("thermometer-3"), width=3, color="orange"),
      valueBox(format(round(datamodel$epi.intervals[3,4], 2), nsmall=1), "Very high threshold", icon = icon("thermometer-4"), width=3, color="red")
    )
  }
})

output$tbmMemOutput <- renderPrint({
  datamodel<-data_model()
  if(!is.null(datamodel)){
    nam.t<-datamodel
    nam.ttt <- rbind(c("Epidemic threshold:","           Pre Post"),
                     c("",paste0("Threshold ",
                                 format(round(nam.t$"pre.post.intervals"[1,3], 2), nsmall=2)," ",
                                 format(round(nam.t$"pre.post.intervals"[2,3], 2), nsmall=2))),
                     c("", ""),
                     c("Intensity thresholds:",""),
                     c("                  Threshold", ""),
                     c(paste0("Medium (40%)          ", format(round(nam.t$"epi.intervals"[1,4], 2), nsmall=2)), ""),
                     c(paste0("High (90%)            ", format(round(nam.t$"epi.intervals"[2,4], 2), nsmall=2)), ""),
                     c(paste0("Very high (97.5%)     ", format(round(nam.t$"epi.intervals"[3,4], 2), nsmall=2)), ""))

    nam.ttt <- format(nam.ttt, justify = "left")
    nam.ttt <- as.data.frame(nam.ttt)
    names(nam.ttt) <- NULL
    summary(datamodel)
  }else{
    war.text <- as.data.frame("MEM needs at least two seasons.")
    names(war.text) <- NULL
    print(noquote(war.text), row.names = FALSE)}
})

output$tbmMemGraph <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Moving epidemics", plotlyOutput("tbmMemGraphMoving", width ="100%", height ="100%")),
                tabPanel("Average curve", plotlyOutput("tbmMemGraphTypical", width ="100%", height ="100%"))
    )
  }
})

output$tbmMemGraphMoving <- renderPlotly({
  datamodel<-data_model()
  if(is.null(datamodel)){
    zfix<-NULL
  }else{
    e.thr<-datamodel$epidemic.thresholds
    i.thr<-datamodel$intensity.thresholds
    datfile.plot<-data.frame(datamodel$moving.epidemics,row.names = rownames(datamodel$param.data))
    names(datfile.plot)<-names(datamodel$param.data)
    datfile.plot$Typical<-datamodel$typ.curve[,2]
    colors.palette<-generate_palette(i.number.series=NCOL(datfile.plot),
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p <- plotSeasons(datfile.plot,
                     i.epidemic.thr=e.thr,
                     i.intensity.thr=i.thr,
                     i.pre.epidemic = as.logical(input$preepidemicthr),
                     i.post.epidemic = as.logical(input$postepidemicthr),
                     i.intensity = as.logical(input$intensitythr),
                     i.textMain=input$textMain,
                     i.textX=input$textX,
                     i.textY=input$textY,
                     i.type.threshold=as.numeric(input$typethreshold),
                     i.tails.threshold=as.numeric(input$ntails),
                     i.type.intensity=as.numeric(input$typeintensity),
                     i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                     i.tails.intensity=as.numeric(input$ntails),
                     i.type.curve=as.numeric(input$typecurve),
                     i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                     i.type.other=as.numeric(input$typeother),
                     i.level.other=as.numeric(input$leveltypicalcurve)/100,
                     i.method=as.numeric(input$method),
                     i.param=as.numeric(input$param),
                     i.n.max=as.numeric(input$nvalues),
                     i.colObservedPoints=colors.palette$colObservedPoints,
                     i.colSeasons=colors.palette$colSeasons,
                     i.colThresholds=colors.palette$colThresholds)
    if (is.null(p)){
      zfix<-NULL
    }else{
      p0<-p$plot +
        geom_vline(xintercept = datamodel$ci.start[1,2]-0.5,
                   col=colors.palette$colEpidemicStart, linetype="longdash", size=0.5) +
        geom_vline(xintercept = datamodel$ci.start[1,2]+datamodel$mean.length-1+0.5,
                   col=colors.palette$colEpidemicStop, linetype="longdash", size=0.5)
      z <- ggplotly(p0, width = 800, height = 600)
      # Typical curve, more width and dot stype
      z$x$data[[NCOL(datfile.plot)]]$line$width<-2*z$x$data[[NCOL(datfile.plot)]]$line$width
      z$x$data[[NCOL(datfile.plot)]]$line$dash<-"dot"
      # Rename name and text for vertical lines I've just added
      z$x$data[[2*length(p$labels)+1]]$name<-"Mean start"
      z$x$data[[2*length(p$labels)+2]]$name<-"Mean end"
      z$x$data[[2*length(p$labels)+1]]$text<-paste("Mean start: ",rownames(datfile.plot)[datamodel$ci.start[1,2]],sep="")
      z$x$data[[2*length(p$labels)+2]]$text<-paste("Mean end: ",rownames(datfile.plot)[datamodel$ci.start[1,2]+datamodel$mean.length-1],sep="")
      # And I need to rearrange the order of the z list for fixplotly to work
      names(z$x$data)<-as.character(1:(2*length(p$labels)+2))
      z$x$data<-z$x$data[as.character(c(1:length(p$labels),2*length(p$labels)+1,2*length(p$labels)+2,(length(p$labels)+1):(2*length(p$labels)),2*length(p$labels)+1,2*length(p$labels)+2))]
      names(z$x$data)<-NULL
      zfix<-fixplotly(z,
                      c(p$labels,"Mean start","Mean end"),
                      c(p$haslines,T,T),
                      c(p$haspoints,F,F),
                      "week","value",p$weeklabels)
    }
  }
  zfix
})

output$tbmMemGraphTypical <- renderPlotly({
  datamodel<-data_model()
  if(is.null(datamodel)){
    zfix<-NULL
  }else{
    datfile.plot<-data.frame(typical=datamodel$typ.curve[,2],row.names = rownames(datamodel$param.data))
    e.thr<-datamodel$epidemic.thresholds
    i.thr<-datamodel$intensity.thresholds
    colors.palette<-generate_palette(i.number.series=NA,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p <- plotSurveillance(i.data=datfile.plot,
                          i.week.report=rev(rownames(datfile.plot))[1],
                          i.pre.epidemic=as.logical(input$preepidemicthr),
                          i.post.epidemic=as.logical(input$postepidemicthr),
                          i.start=as.logical(input$preepidemicthr),
                          i.end=as.logical(input$postepidemicthr),
                          i.epidemic.thr = e.thr,
                          i.intensity= as.logical(input$intensitythr),
                          i.intensity.thr=i.thr,
                          i.mean.length=datamodel$mean.length,
                          i.force.length=T,
                          i.force.equal=F,
                          i.force.start=datamodel$ci.start[2,2],
                          i.force.week.53=F,
                          i.textMain=input$textMain,
                          i.textX=input$textX,
                          i.textY=input$textY,
                          i.colObservedLines=colors.palette$colObservedLines,
                          i.colObservedPoints=colors.palette$colObservedPoints,
                          i.colEpidemicStart=colors.palette$colEpidemicStart,
                          i.colEpidemicStop=colors.palette$colEpidemicStop,
                          i.colThresholds=colors.palette$colThresholds)
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
    }
  }
  zfix
})

output$tbmGoodness <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }
  else
    tabsetPanel(tabPanel("Model", uiOutput("tbmGoodnessModel")),
                tabPanel("Global", uiOutput("tbmGoodnessGlobal"))
    )
})

output$tbmGoodnessModel <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }
  else
    tabsetPanel(tabPanel("Indicators", uiOutput("tbmGoodnessModelSummary")),
                tabPanel("Detailed", 
                         formattableOutput("tbmGoodnessModelDetail1"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbmGoodnessModelDetail1_x","xlsx")),
                           column(1,downloadButton("tbmGoodnessModelDetail1_c","csv"))
                         )
                         ),
                tabPanel("Intensity", uiOutput("tbmGoodnessModelIntensity")),
                tabPanel("Detailed", 
                         formattableOutput("tbmGoodnessModelDetail2"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbmGoodnessModelDetail2_x","xlsx")),
                           column(1,downloadButton("tbmGoodnessModelDetail2_c","csv"))
                         )
                         )
    )
})

output$tbmGoodnessModelSummary <- renderUI({
  good <- data_good_model()
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(format(round(good$results["Sensitivity"], 2), nsmall=2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Specificity"], 2), nsmall=2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Positive predictive value"], 2), nsmall=2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Negative predictive value"], 2), nsmall=2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Percent agreement"], 2), nsmall=2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(format(round(good$results["Matthews correlation coefficient"], 2), nsmall=2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua")
    )
  }
})

output$tbmGoodnessModelDetail1<-renderFormattable({
  good <- data_good_model()
  if(!is.null(good)){
    temp1<-as.data.frame(good$validity.data)
    temp1$Total<-good$results
    temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
    # temp1[is.na(temp1)]<--1
    good.table<-formattable(temp1, list(
      # area(col = names(temp1)[1:4]) ~ normalize_bar("#FFBBFF", 0.2),
      # area(col = names(temp1)[5:6]) ~ normalize_bar("#A5DBEB", 0.2)
      "Sensitivity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Specificity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Positive predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Negative predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Percent agreement" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
      "Matthews correlation coefficient" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5)
    ), digits = 2, format = "f")    
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    good.table<-formattable(temp1)
  }
  good.table
})

# observeEvent(input$tbmGoodnessModelDetail1_x, {
#   good <- data_good_model()
#   if(!is.null(good)){
#     temp1<-as.data.frame(good$validity.data)
#     temp1$Total<-good$results
#     temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
#     export.mydata(i.data=temp1, i.sheet="Goodness_model_1", i.rownames="Season", i.format="xlsx")
#   }
# })
# 
# observeEvent(input$tbmGoodnessModelDetail1_c, {
#   good <- data_good_model()
#   if(!is.null(good)){
#     temp1<-as.data.frame(good$validity.data)
#     temp1$Total<-good$results
#     temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
#     export.mydata(i.data=temp1, i.sheet="Goodness_model_1", i.rownames="Season", i.format="csv")
#   }
# })

output$tbmGoodnessModelDetail1_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    good <- data_good_model()
    if(!is.null(good)){
      temp1<-as.data.frame(good$validity.data)
      temp1$Total<-good$results
      temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Model goodness indicators", i.rownames="Season", i.format="xlsx")
    }
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbmGoodnessModelDetail1_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    good <- data_good_model()
    if(!is.null(good)){
      temp1<-as.data.frame(good$validity.data)
      temp1$Total<-good$results
      temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Model goodness indicators", i.rownames="Season", i.format="csv")
    }
  },
  contentType="text/csv"
)

output$tbmGoodnessModelIntensity <- renderUI({
  good <- data_good_model()
  peaks <- good$peaks
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==1]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==1]," level"), icon = icon("heartbeat"), width=2, color="lime"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==2]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==2]," level"), icon = icon("thermometer-1"), width=2, color="green"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==3]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==3]," level"), icon = icon("thermometer-2"), width=2, color="yellow"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==4]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==4]," level"), icon = icon("thermometer-3"), width=2, color="orange"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==5]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==5]," level"), icon = icon("thermometer-4"), width=2, color="red"),
      valueBox(peaks$Count[peaks[,1]==-1], peaks$Description[peaks[,1]==-1], icon = icon("heartbeat"), width=3, color="teal"),
      valueBox(peaks$Count[peaks[,1]==0], peaks$Description[peaks[,1]==0], icon = icon("heartbeat"), width=3, color="teal")
    )
  }
})

output$tbmGoodnessModelDetail2<-renderFormattable({
  good <- data_good_model()
  if(!is.null(good)){
    temp1 <- good$peaks.data
    temp1$Level<-as.character(temp1$Level)
    thr.c<-generate_palette(i.colThresholds=input$colThresholds)$colThresholds
    lvl.n<-as.character(c(1:5))
    lvl.t<-c("Baseline","Low","Medium","High","Very high")
    lvl.c<-c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")
    peaks.data<-formattable(temp1, list(
      Level = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.n[1], lvl.c[1] , ifelse(x==lvl.n[2], lvl.c[2], ifelse(x==lvl.n[3], lvl.c[3], ifelse(x==lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      Description = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.t[1], lvl.c[1] , ifelse(x==lvl.t[2], lvl.c[2], ifelse(x==lvl.t[3], lvl.c[3], ifelse(x==lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      "Epidemic threshold"=formatter("span", style = style(color = thr.c[1], font.weight = "bold")),
      "Medium threshold"=formatter("span", style = style(color = thr.c[2], font.weight = "bold")),
      "High threshold"=formatter("span", style = style(color = thr.c[3], font.weight = "bold")),
      "Very high threshold"=formatter("span", style = style(color = thr.c[4], font.weight = "bold"))
    ), digits = 2, format = "f")
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    peaks.data<-formattable(temp1)
  }
  peaks.data
})

# observeEvent(input$tbmGoodnessModelDetail2_x, {
#   good <- data_good_model()
#   if(!is.null(good)){
#     temp1 <- good$peaks.data
#     temp1$Level<-as.character(temp1$Level)
#     export.mydata(i.data=temp1, i.sheet="Goodness_model_2", i.rownames="Season", i.format="xlsx")
#   }
# })
# 
# observeEvent(input$tbmGoodnessModelDetail2_c, {
#   good <- data_good_model()
#   if(!is.null(good)){
#     temp1 <- good$peaks.data
#     temp1$Level<-as.character(temp1$Level)
#     export.mydata(i.data=temp1, i.sheet="Goodness_model_2", i.rownames="Season", i.format="csv")
#   }
# })

output$tbmGoodnessModelDetail2_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    good <- data_good_model()
    if(!is.null(good)){
      temp1 <- good$peaks.data
      temp1$Level<-as.character(temp1$Level)
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Model goodness intensity", i.rownames="Season", i.format="xlsx")
    }
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbmGoodnessModelDetail2_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    good <- data_good_model()
    if(!is.null(good)){
      temp1 <- good$peaks.data
      temp1$Level<-as.character(temp1$Level)
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Model goodness intensity", i.rownames="Season", i.format="csv")
    }
  },
  contentType="text/csv"
)

output$tbmGoodnessGlobal <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }
  else
    tabsetPanel(tabPanel("Indicators", uiOutput("tbmGoodnessGlobalSummary")),
                tabPanel("Detailed", 
                         formattableOutput("tbmGoodnessGlobalDetail1"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbmGoodnessGlobalDetail1_x","xlsx")),
                           column(1,downloadButton("tbmGoodnessGlobalDetail1_c","csv"))
                         )
                         ),
                tabPanel("Intensity", uiOutput("tbmGoodnessGlobalIntensity")),
                tabPanel("Detailed", 
                         formattableOutput("tbmGoodnessGlobalDetail2"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbmGoodnessGlobalDetail2_x","xlsx")),
                           column(1,downloadButton("tbmGoodnessGlobalDetail2_c","csv"))
                         )
                         )
    )
})

output$tbmGoodnessGlobalSummary <- renderUI({
  good <- data_good_global()
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(format(round(good$results["Sensitivity"], 2), nsmall=2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Specificity"], 2), nsmall=2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Positive predictive value"], 2), nsmall=2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Negative predictive value"], 2), nsmall=2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(good$results["Percent agreement"], 2), nsmall=2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(format(round(good$results["Matthews correlation coefficient"], 2), nsmall=2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua")
    )
  }
})

output$tbmGoodnessGlobalDetail1<-renderFormattable({
  good <- data_good_global()
  if(!is.null(good)){
    temp1<-as.data.frame(good$validity.data)
    temp1$Total<-good$results
    temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
    # temp1[is.na(temp1)]<--1
    good.table<-formattable(temp1, list(
      # area(col = names(temp1)[1:4]) ~ normalize_bar("#FFBBFF", 0.2),
      # area(col = names(temp1)[5:6]) ~ normalize_bar("#A5DBEB", 0.2)
      "Sensitivity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Specificity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Positive predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Negative predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Percent agreement" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
      "Matthews correlation coefficient" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5)
    ), digits = 2, format = "f")    
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    good.table<-formattable(temp1)
  }
  good.table
})

# observeEvent(input$tbmGoodnessGlobalDetail1_x, {
#   good <- data_good_global()
#   if(!is.null(good)){
#     temp1<-as.data.frame(good$validity.data)
#     temp1$Total<-good$results
#     temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
#     export.mydata(i.data=temp1, i.sheet="Goodness_global_1", i.rownames="Season", i.format="xlsx")
#   }
# })
# 
# observeEvent(input$tbmGoodnessGlobalDetail1_c, {
#   good <- data_good_global()
#   if(!is.null(good)){
#     temp1<-as.data.frame(good$validity.data)
#     temp1$Total<-good$results
#     temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
#     export.mydata(i.data=temp1, i.sheet="Goodness_global_1", i.rownames="Season", i.format="csv")
#   }
# })

output$tbmGoodnessGlobalDetail1_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    good <- data_good_global()
    if(!is.null(good)){
      temp1<-as.data.frame(good$validity.data)
      temp1$Total<-good$results
      temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Global goodness indicators", i.rownames="Season", i.format="xlsx")
    }
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbmGoodnessGlobalDetail1_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    good <- data_good_global()
    if(!is.null(good)){
      temp1<-as.data.frame(good$validity.data)
      temp1$Total<-good$results
      temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Global goodness indicators", i.rownames="Season", i.format="csv")
    }
  },
  contentType="text/csv"
)

output$tbmGoodnessGlobalIntensity <- renderUI({
  good <- data_good_global()
  peaks <- good$peaks
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==1]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==1]," level"), icon = icon("heartbeat"), width=2, color="lime"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==2]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==2]," level"), icon = icon("thermometer-1"), width=2, color="green"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==3]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==3]," level"), icon = icon("thermometer-2"), width=2, color="yellow"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==4]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==4]," level"), icon = icon("thermometer-3"), width=2, color="orange"),
      valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==5]*100, 2), nsmall=1), "%"), paste0(peaks$Description[peaks[,1]==5]," level"), icon = icon("thermometer-4"), width=2, color="red"),
      valueBox(peaks$Count[peaks[,1]==-1], peaks$Description[peaks[,1]==-1], icon = icon("heartbeat"), width=3, color="teal"),
      valueBox(peaks$Count[peaks[,1]==0], peaks$Description[peaks[,1]==0], icon = icon("heartbeat"), width=3, color="teal")
    )
  }
})

output$tbmGoodnessGlobalDetail2<-renderFormattable({
  good <- data_good_global()
  if(!is.null(good)){
    temp1 <- good$peaks.data
    temp1$Level<-as.character(temp1$Level)
    thr.c<-generate_palette(i.colThresholds=input$colThresholds)$colThresholds
    lvl.n<-as.character(c(1:5))
    lvl.t<-c("Baseline","Low","Medium","High","Very high")
    lvl.c<-c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")
    peaks.data<-formattable(temp1, list(
      Level = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.n[1], lvl.c[1] , ifelse(x==lvl.n[2], lvl.c[2], ifelse(x==lvl.n[3], lvl.c[3], ifelse(x==lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      Description = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.t[1], lvl.c[1] , ifelse(x==lvl.t[2], lvl.c[2], ifelse(x==lvl.t[3], lvl.c[3], ifelse(x==lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      "Epidemic threshold"=formatter("span", style = style(color = thr.c[1], font.weight = "bold")),
      "Medium threshold"=formatter("span", style = style(color = thr.c[2], font.weight = "bold")),
      "High threshold"=formatter("span", style = style(color = thr.c[3], font.weight = "bold")),
      "Very high threshold"=formatter("span", style = style(color = thr.c[4], font.weight = "bold"))
    ), digits = 2, format = "f")
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    peaks.data<-formattable(temp1)
  }
  peaks.data
})

# observeEvent(input$tbmGoodnessGlobalDetail2_x, {
#   good <- data_good_global()
#   if(!is.null(good)){
#     temp1 <- good$peaks.data
#     temp1$Level<-as.character(temp1$Level)
#     export.mydata(i.data=temp1, i.sheet="Goodness_global_2", i.rownames="Season", i.format="xlsx")
#   }
# })
# 
# observeEvent(input$tbmGoodnessGlobalDetail2_c, {
#   good <- data_good_global()
#   if(!is.null(good)){
#     temp1 <- good$peaks.data
#     temp1$Level<-as.character(temp1$Level)
#     export.mydata(i.data=temp1, i.sheet="Goodness_global_2", i.rownames="Season", i.format="csv")
#   }
# })

output$tbmGoodnessGlobalDetail2_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    good <- data_good_global()
    if(!is.null(good)){
      temp1 <- good$peaks.data
      temp1$Level<-as.character(temp1$Level)
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Global goodness intensity", i.rownames="Season", i.format="xlsx")
    }
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbmGoodnessGlobalDetail2_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    good <- data_good_global()
    if(!is.null(good)){
      temp1 <- good$peaks.data
      temp1$Level<-as.character(temp1$Level)
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Global goodness intensity", i.rownames="Season", i.format="csv")
    }
  },
  contentType="text/csv"
)

output$tbmOptimize <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Indicators", uiOutput("tbmOptimizeSummary")),
                tabPanel("Detailed", 
                         formattableOutput("tbmOptimizeDetail"),
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbmOptimizeDetail_x","xlsx")),
                           column(1,downloadButton("tbmOptimizeDetail_c","csv"))
                         )
                         ),
                tabPanel("Graphs",plotlyOutput("tbmOptimizeGraph"))
    )
  }
})

output$tbmOptimizeSummary <- renderUI({
  dataoptim <- data_optim()
  if(is.null(dataoptim)){
    return(NULL)
  }else{
    doptim<-dataoptim$roc.data
    optim<-doptim[doptim$value==as.numeric(dataoptim$optimum["matthews"]),]
    fluidRow(
      valueBox(format(round(optim["sensitivity"], 2), nsmall=2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(optim["specificity"], 2), nsmall=2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(optim["positive.predictive.value"], 2), nsmall=2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(optim["negative.predictive.value"], 2), nsmall=2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(format(round(optim["percent.agreement"], 2), nsmall=2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(format(round(optim["matthews.correlation.coefficient"], 2), nsmall=2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(format(round(input$param, 2), nsmall=1), "Current parameter", icon = icon("heartbeat"), width=3, color="red"),
      valueBox(format(round(as.numeric(dataoptim$optimum["matthews"]), 2), nsmall=1), "Optimum parameter", icon = icon("heartbeat"), width=3, color="olive")
    )
  }
})

output$tbmOptimizeDetail<-renderFormattable({
  dataoptim <- data_optim()
  if(!is.null(dataoptim)){
    temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient")]
    names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")
    rownames(temp1)<-NULL
    # temp1[is.na(temp1)]<--1
    roca.table<-formattable(temp1, list(
      # area(col = names(roca.table)[2:5]) ~ normalize_bar("#FFBBFF", 0.2),
      # area(col = names(roca.table)[6:7]) ~ normalize_bar("#A5DBEB", 0.2)
      "Sensitivity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Specificity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Positive predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Negative predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
      "Percent agreement" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
      "Matthews correlation coefficient" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5)
    ), digits = 2, format = "f")
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2",row.names = NULL)
    roca.table<-formattable(temp1)
  }
  roca.table
})

# observeEvent(input$tbmOptimizeDetail_x, {
#   dataoptim <- data_optim()
#   if(!is.null(dataoptim)){
#     temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient")]
#     names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")
#     rownames(temp1)<-NULL
#     export.mydata(i.data=temp1, i.sheet="Optimize", i.rownames=NA, i.format="xlsx")
#   }
# })
# 
# observeEvent(input$tbmOptimizeDetail_c, {
#   dataoptim <- data_optim()
#   if(!is.null(dataoptim)){
#     temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient")]
#     names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")
#     rownames(temp1)<-NULL
#     export.mydata(i.data=temp1, i.sheet="Optimize", i.rownames=NA, i.format="csv")
#   }
# })

output$tbmOptimizeDetail_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    dataoptim <- data_optim()
    if(!is.null(dataoptim)){
      temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient")]
      names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")
      rownames(temp1)<-NULL
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Optimization", i.rownames=NA, i.format="xlsx")
    }
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbmOptimizeDetail_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    dataoptim <- data_optim()
    if(!is.null(dataoptim)){
      temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient")]
      names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")
      rownames(temp1)<-NULL
      export.mydata(i.data=temp1, i.file = file, 
                    i.sheet="Optimization", i.rownames=NA, i.format="csv")
    }
  },
  contentType="text/csv"
)

output$tbmOptimizeGraph<- renderPlotly({
  dataoptim <- data_optim()
  if(is.null(dataoptim)){
    z<-NULL
  }else{
    dgraf<-subset(dataoptim$roc.data,select=c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient"))
    names(dgraf)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")
    dgrafgg<-melt(dgraf,id="Parameter", value.name = "Value", variable.name = "Indicator")
    colors.palette<-generate_palette(i.number.series=NCOL(dgraf)-1,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    axis.x.range.original <- range(dgraf$Parameter)
    axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 10, seq(0.1,10,0.1),T,F)
    axis.x.range <- axis.x.otick$range
    axis.x.ticks <- axis.x.otick$tickmarks
    axis.x.labels <- axis.x.otick$tickmarks

    axis.y.range.original <- c(0,1)
    axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
    axis.y.range <- axis.y.otick$range
    axis.y.ticks <- axis.y.otick$tickmarks
    axis.y.labels <- axis.y.otick$tickmarks

    p<-ggplot(dgrafgg, aes(x=Parameter,y=Value, color=Indicator)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      scale_color_manual(values=colors.palette$colSeasons) +
      labs(title = input$textMain, x = input$textX, y = input$textY) +
      ggthemes::theme_few()
    z<-ggplotly(p, width = 800, height = 600)
  }
  z
})

#####################################
### SURVEILLANCE TAB
#####################################

output$tbSurveillance <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Data", DT::dataTableOutput("tbsData")),
                tabPanel("Surveillance",uiOutput("tbsSurveillance"))
    )
  }
})

output$tbsData <- DT::renderDataTable({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    datatoshow<-NULL
  }else if (is.null(input$SelectSurveillance)) {
    datatoshow<-NULL
  }else{
    # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
    selectedcolumns<-select.columns(i.names=names(datfile),
                                    i.from=input$SelectSurveillance,
                                    i.to=input$SelectSurveillance,
                                    i.exclude="",
                                    i.include=input$SelectSurveillance,
                                    i.pandemic=as.logical("TRUE"),
                                    i.seasons=NA)
    if (length(selectedcolumns)>0){
      datatoshow<-format(round(datfile[selectedcolumns], 2), nsmall=2)
    }else{
      datatoshow<-data.frame(Message="No data selected",row.names = NULL)
    }
  }
  datatoshow
}, 
#extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))
options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

output$tbsSurveillance <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else if(!(input$SelectSurveillance %in% names(datfile))){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Week", plotlyOutput("tbsSurveillanceWeek", width ="100%", height ="100%")),
                tabPanel("Animated", imageOutput("tbsSurveillanceAnimated")),
                tabPanel("Average", 
                         plotlyOutput("tbsSurveillanceAverage", width ="100%", height ="100%")
                         ,
                         fluidRow(
                           column(10),
                           column(1,if (zip.present()) downloadButton("tbsSurveillanceAverage_x","xlsx")),
                           column(1,downloadButton("tbsSurveillanceAverage_c","csv"))
                         )
                         )
    )
  }
})

output$tbsSurveillanceWeek <- renderPlotly({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    zfix<-NULL
  }else if(!(input$SelectSurveillance %in% names(datfile))){
    zfix<-NULL
  }else{
    if(is.null(input$SelectSurveillanceWeek)){
      SurveillanceWeek<-tail(row.names(datfile),1)
    }else if(!(input$SelectSurveillanceWeek %in% row.names(datfile))){
      SurveillanceWeek<-tail(row.names(datfile),1)
    }else{
      SurveillanceWeek<-input$SelectSurveillanceWeek
    }
    if (is.null(input$SelectSurveillanceForceEpidemic)){
      force.start<-NA
    }else if(!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))){
      force.start<-NA
    }else{
      force.start<-input$SelectSurveillanceForceEpidemic
    } 
    datamodel<-data_model()
    if(!is.null(datamodel)){
      e.thr<-datamodel$epidemic.thresholds
      i.thr<-datamodel$intensity.thresholds
    }else{
      e.thr<-NA
      i.thr<-NA
    }
    datfile.plot<-datfile[input$SelectSurveillance]
    colors.palette<-generate_palette(i.number.series=NA,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)
    p <- plotSurveillance(i.data=datfile.plot,
                          i.week.report=SurveillanceWeek,
                          i.pre.epidemic=as.logical(input$preepidemicthr),
                          i.post.epidemic=as.logical(input$postepidemicthr),
                          i.epidemic.thr = e.thr,
                          i.intensity = as.logical(input$intensitythr),
                          i.intensity.thr = i.thr,
                          i.start=as.logical(input$preepidemicthr),
                          i.end=as.logical(input$postepidemicthr),
                          i.force.start = force.start,
                          i.textMain=input$textMain,
                          i.textX=input$textX,
                          i.textY=input$textY,
                          i.colObservedLines=colors.palette$colObservedLines,
                          i.colObservedPoints=colors.palette$colObservedPoints,
                          i.colEpidemicStart=colors.palette$colEpidemicStart,
                          i.colEpidemicStop=colors.palette$colEpidemicStop,
                          i.colThresholds=colors.palette$colThresholds)
    if (is.null(p)){
      zfix<-NULL
    }else{
      z <- ggplotly(p$plot, width = 800, height = 600)
      zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
    }
  }
  zfix
})

output$tbsSurveillanceAnimated <- renderImage({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    outdistAnimated<-NULL
  }else if(!(input$SelectSurveillance %in% names(datfile))){
    outdistAnimated<-NULL
  }else{
    if(is.null(input$SelectSurveillanceWeek)){
      SurveillanceWeek<-tail(row.names(datfile),1)
    }else if(!(input$SelectSurveillanceWeek %in% row.names(datfile))){
      SurveillanceWeek<-tail(row.names(datfile),1)
    }else{
      SurveillanceWeek<-input$SelectSurveillanceWeek
    }
    if (is.null(input$SelectSurveillanceForceEpidemic)){
      force.start<-NA
    }else if(!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))){
      force.start<-NA
    }else{
      force.start<-input$SelectSurveillanceForceEpidemic
    } 
    datamodel<-data_model()
    if(!is.null(datamodel)){
      e.thr<-datamodel$epidemic.thresholds
      i.thr<-datamodel$intensity.thresholds
    }else{
      e.thr<-NA
      i.thr<-NA
    }
    # cat(paste(row.names(datfile),collapse=","),"\n")
    # cat(SurveillanceWeek,"\n")
    datfile.plot<-datfile[input$SelectSurveillance]
    max.y<-max(datfile.plot,na.rm=T)
    if (as.logical(input$preepidemicthr)) max.y<-max(max.y,e.thr[1],na.rm=T)
    if (as.logical(input$postepidemicthr)) max.y<-max(max.y,e.thr[2],na.rm=T)
    if (as.logical(input$intensitythr)) max.y<-max(max.y,i.thr,na.rm=T)
    n.surveillance.week<-min((1:(NROW(datfile)))[SurveillanceWeek==rownames(datfile)])
    # Uses magick, its a kind of R imagemagick, should work out of the box
    colors.palette<-generate_palette(i.number.series=NA,
                                     i.colObservedLines=input$colObservedLines,
                                     i.colObservedPoints=input$colObservedPoints,
                                     i.colEpidemicStart=input$colEpidemicStart,
                                     i.colEpidemicStop=input$colEpidemicStop,
                                     i.colThresholds=input$colThresholds,
                                     i.colSeasons=input$colSeasons,
                                     i.colEpidemic=input$colEpidemic)

    for (i in 1:n.surveillance.week){
      p<-plotSurveillance(i.data=datfile.plot,
                          i.week.report=rownames(datfile)[i],
                          i.pre.epidemic=as.logical(input$preepidemicthr),
                          i.post.epidemic=as.logical(input$postepidemicthr),
                          i.epidemic.thr = e.thr,
                          i.intensity = as.logical(input$intensitythr),
                          i.intensity.thr = i.thr,
                          i.range.y=c(0,max.y),
                          i.start=as.logical(input$preepidemicthr),
                          i.end=as.logical(input$postepidemicthr),
                          i.force.start = force.start,
                          i.textMain=input$textMain,
                          i.textX=input$textX,
                          i.textY=input$textY,
                          i.colObservedLines=colors.palette$colObservedLines,
                          i.colObservedPoints=colors.palette$colObservedPoints,
                          i.colEpidemicStart=colors.palette$colEpidemicStart,
                          i.colEpidemicStop=colors.palette$colEpidemicStop,
                          i.colThresholds=colors.palette$colThresholds)
      imgfile<-paste(tempdir(),"/animatedplot_",i,".png",sep="")
      ggsave(imgfile, plot=p$plot, width=8, height=6, dpi=150)
      if (i==1) imgfilem<-image_read(imgfile) else imgfilem<-c(imgfilem,image_read(imgfile))
      #cat(imgfile,"\n")
    }
    imgfilegif<-paste(tempdir(),"/animated.gif",sep="")
    anim <- image_animate(imgfilem, fps = 2)
    image_write(anim,path=imgfilegif)
    #cat(imgfilegif,"\n")
    outdistAnimated<-list(src = paste(tempdir(),"/animated.gif",sep=""),
                          contentType = 'image/gif',
                          width = 800,
                          height = 600,
                          alt = "This is alternate text")
  }
  outdistAnimated
}, deleteFile = TRUE)

output$tbsSurveillanceAverage <- renderPlotly({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    zfix<-NULL
  }else if(!(input$SelectSurveillance %in% names(datfile))){
    zfix<-NULL
  }else{
    if(is.null(input$SelectSurveillanceWeek)){
      SurveillanceWeek<-tail(row.names(datfile),1)
    }else if(!(input$SelectSurveillanceWeek %in% row.names(datfile))){
      SurveillanceWeek<-tail(row.names(datfile),1)
    }else{
      SurveillanceWeek<-input$SelectSurveillanceWeek
    }
    if (is.null(input$SelectSurveillanceForceEpidemic)){
      force.start<-NA
    }else if(!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))){
      force.start<-NA
    }else{
      force.start<-input$SelectSurveillanceForceEpidemic
    } 
    datamodel<-data_model()
    if(is.null(datamodel)){
      zfix<-NULL
    }else{
      e.thr<-datamodel$epidemic.thresholds
      i.thr<-datamodel$intensity.thresholds
      datfile.plot<-data.frame(datfile[input$SelectSurveillance],datamodel$typ.curve)
      survweek<-(1:(NROW(datfile)))[SurveillanceWeek==rownames(datfile)]
      datfile.plot[-(1:survweek),1]<-NA
      names(datfile.plot)<-c(input$SelectSurveillance,"Lower interval","Average curve","Higher interval")
      colors.palette<-generate_palette(i.number.series=3,
                                       i.colObservedLines=input$colObservedLines,
                                       i.colObservedPoints=input$colObservedPoints,
                                       i.colEpidemicStart=input$colEpidemicStart,
                                       i.colEpidemicStop=input$colEpidemicStop,
                                       i.colThresholds=input$colThresholds,
                                       i.colSeasons=input$colSeasons,
                                       i.colEpidemic=input$colEpidemic)
      p <- plotSeasons(datfile.plot,
                       i.epidemic.thr=e.thr,
                       i.intensity.thr=i.thr,
                       i.pre.epidemic = as.logical(input$preepidemicthr),
                       i.post.epidemic = as.logical(input$postepidemicthr),
                       i.intensity = as.logical(input$intensitythr),
                       i.textMain=input$textMain,
                       i.textX=input$textX,
                       i.textY=input$textY,
                       i.type.threshold=as.numeric(input$typethreshold),
                       i.tails.threshold=as.numeric(input$ntails),
                       i.type.intensity=as.numeric(input$typeintensity),
                       i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                       i.tails.intensity=as.numeric(input$ntails),
                       i.type.curve=as.numeric(input$typecurve),
                       i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                       i.type.other=as.numeric(input$typeother),
                       i.level.other=as.numeric(input$leveltypicalcurve)/100,
                       i.method=as.numeric(input$method),
                       i.param=as.numeric(input$param),
                       i.n.max=as.numeric(input$nvalues),
                       i.colObservedPoints=colors.palette$colObservedPoints,
                       i.colSeasons=c(colors.palette$colObservedLines,colors.palette$colSeasons[c(3,2,3)]),
                       i.colThresholds=colors.palette$colThresholds)
      if (is.null(p)){
        zfix<-NULL
      }else{
        z <- ggplotly(p$plot, width = 800, height = 600)
        zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
      }
    }
  }
  zfix
})

# observeEvent(input$tbsSurveillanceAverage_x, {
#   readdata <- read_data()
#   datfile <- readdata$datasetread
#   if(!is.null(datfile)){
#     if(input$SelectSurveillance %in% names(datfile)){
#       if(is.null(input$SelectSurveillanceWeek)){
#         SurveillanceWeek<-tail(row.names(datfile),1)
#       }else if(!(input$SelectSurveillanceWeek %in% row.names(datfile))){
#         SurveillanceWeek<-tail(row.names(datfile),1)
#       }else{
#         SurveillanceWeek<-input$SelectSurveillanceWeek
#       }
#       if (is.null(input$SelectSurveillanceForceEpidemic)){
#         force.start<-NA
#       }else if(!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))){
#         force.start<-NA
#       }else{
#         force.start<-input$SelectSurveillanceForceEpidemic
#       } 
#       datamodel<-data_model()
#       if(!is.null(datamodel)){
#         e.thr<-datamodel$epidemic.thresholds
#         i.thr<-datamodel$intensity.thresholds
#         datfile.plot<-data.frame(datfile[input$SelectSurveillance],datamodel$typ.curve)
#         survweek<-(1:(NROW(datfile)))[SurveillanceWeek==rownames(datfile)]
#         datfile.plot[-(1:survweek),1]<-NA
#         names(datfile.plot)<-c(input$SelectSurveillance,"Lower interval","Average curve","Higher interval")
#         colors.palette<-generate_palette(i.number.series=3,
#                                          i.colObservedLines=input$colObservedLines,
#                                          i.colObservedPoints=input$colObservedPoints,
#                                          i.colEpidemicStart=input$colEpidemicStart,
#                                          i.colEpidemicStop=input$colEpidemicStop,
#                                          i.colThresholds=input$colThresholds,
#                                          i.colSeasons=input$colSeasons,
#                                          i.colEpidemic=input$colEpidemic)
#         p <- plotSeasons(datfile.plot,
#                          i.epidemic.thr=e.thr,
#                          i.intensity.thr=i.thr,
#                          i.pre.epidemic = as.logical(input$preepidemicthr),
#                          i.post.epidemic = as.logical(input$postepidemicthr),
#                          i.intensity = as.logical(input$intensitythr),
#                          i.textMain=input$textMain,
#                          i.textX=input$textX,
#                          i.textY=input$textY,
#                          i.type.threshold=as.numeric(input$typethreshold),
#                          i.tails.threshold=as.numeric(input$ntails),
#                          i.type.intensity=as.numeric(input$typeintensity),
#                          i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
#                          i.tails.intensity=as.numeric(input$ntails),
#                          i.type.curve=as.numeric(input$typecurve),
#                          i.level.curve=as.numeric(input$leveltypicalcurve)/100,
#                          i.type.other=as.numeric(input$typeother),
#                          i.level.other=as.numeric(input$leveltypicalcurve)/100,
#                          i.method=as.numeric(input$method),
#                          i.param=as.numeric(input$param),
#                          i.n.max=as.numeric(input$nvalues),
#                          i.colObservedPoints=colors.palette$colObservedPoints,
#                          i.colSeasons=c(colors.palette$colObservedLines,colors.palette$colSeasons[c(3,2,3)]),
#                          i.colThresholds=colors.palette$colThresholds)
#         if (!is.null(p)){
#           temp1<-p$gdata
#           temp2<-dcast(temp1, week ~ variable, value.var = "value", drop = FALSE, fill = NA)
#           temp2<-temp2[order(temp2$week),p$labels]
#           row.names(temp2)<-p$weeklabels
#           temp2$week<-NULL
#           export.mydata(i.data=temp2, i.sheet="Average", i.rownames="Week no", i.format="xlsx") 
#         }
#       }
#     }
#   }
# })
# 
# observeEvent(input$tbsSurveillanceAverage_c, {
#   readdata <- read_data()
#   datfile <- readdata$datasetread
#   if(!is.null(datfile)){
#     if(input$SelectSurveillance %in% names(datfile)){
#       if(is.null(input$SelectSurveillanceWeek)){
#         SurveillanceWeek<-tail(row.names(datfile),1)
#       }else if(!(input$SelectSurveillanceWeek %in% row.names(datfile))){
#         SurveillanceWeek<-tail(row.names(datfile),1)
#       }else{
#         SurveillanceWeek<-input$SelectSurveillanceWeek
#       }
#       if (is.null(input$SelectSurveillanceForceEpidemic)){
#         force.start<-NA
#       }else if(!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))){
#         force.start<-NA
#       }else{
#         force.start<-input$SelectSurveillanceForceEpidemic
#       } 
#       datamodel<-data_model()
#       if(!is.null(datamodel)){
#         e.thr<-datamodel$epidemic.thresholds
#         i.thr<-datamodel$intensity.thresholds
#         datfile.plot<-data.frame(datfile[input$SelectSurveillance],datamodel$typ.curve)
#         survweek<-(1:(NROW(datfile)))[SurveillanceWeek==rownames(datfile)]
#         datfile.plot[-(1:survweek),1]<-NA
#         names(datfile.plot)<-c(input$SelectSurveillance,"Lower interval","Average curve","Higher interval")
#         colors.palette<-generate_palette(i.number.series=3,
#                                          i.colObservedLines=input$colObservedLines,
#                                          i.colObservedPoints=input$colObservedPoints,
#                                          i.colEpidemicStart=input$colEpidemicStart,
#                                          i.colEpidemicStop=input$colEpidemicStop,
#                                          i.colThresholds=input$colThresholds,
#                                          i.colSeasons=input$colSeasons,
#                                          i.colEpidemic=input$colEpidemic)
#         p <- plotSeasons(datfile.plot,
#                          i.epidemic.thr=e.thr,
#                          i.intensity.thr=i.thr,
#                          i.pre.epidemic = as.logical(input$preepidemicthr),
#                          i.post.epidemic = as.logical(input$postepidemicthr),
#                          i.intensity = as.logical(input$intensitythr),
#                          i.textMain=input$textMain,
#                          i.textX=input$textX,
#                          i.textY=input$textY,
#                          i.type.threshold=as.numeric(input$typethreshold),
#                          i.tails.threshold=as.numeric(input$ntails),
#                          i.type.intensity=as.numeric(input$typeintensity),
#                          i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
#                          i.tails.intensity=as.numeric(input$ntails),
#                          i.type.curve=as.numeric(input$typecurve),
#                          i.level.curve=as.numeric(input$leveltypicalcurve)/100,
#                          i.type.other=as.numeric(input$typeother),
#                          i.level.other=as.numeric(input$leveltypicalcurve)/100,
#                          i.method=as.numeric(input$method),
#                          i.param=as.numeric(input$param),
#                          i.n.max=as.numeric(input$nvalues),
#                          i.colObservedPoints=colors.palette$colObservedPoints,
#                          i.colSeasons=c(colors.palette$colObservedLines,colors.palette$colSeasons[c(3,2,3)]),
#                          i.colThresholds=colors.palette$colThresholds)
#         if (!is.null(p)){
#           temp1<-p$gdata
#           temp2<-dcast(temp1, week ~ variable, value.var = "value", drop = FALSE, fill = NA)
#           temp2<-temp2[order(temp2$week),p$labels]
#           row.names(temp2)<-p$weeklabels
#           temp2$week<-NULL
#           export.mydata(i.data=temp2, i.sheet="Average", i.rownames="Week no", i.format="csv")
#         } 
#       }
#     }
#   }
# })

output$tbsSurveillanceAverage_x <- downloadHandler(
  filename = function() { paste(input$dataset, '.xlsx', sep='') },
  content = function(file) {
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(!is.null(datfile)){
      if(input$SelectSurveillance %in% names(datfile)){
        if(is.null(input$SelectSurveillanceWeek)){
          SurveillanceWeek<-tail(row.names(datfile),1)
        }else if(!(input$SelectSurveillanceWeek %in% row.names(datfile))){
          SurveillanceWeek<-tail(row.names(datfile),1)
        }else{
          SurveillanceWeek<-input$SelectSurveillanceWeek
        }
        if (is.null(input$SelectSurveillanceForceEpidemic)){
          force.start<-NA
        }else if(!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))){
          force.start<-NA
        }else{
          force.start<-input$SelectSurveillanceForceEpidemic
        } 
        datamodel<-data_model()
        if(!is.null(datamodel)){
          e.thr<-datamodel$epidemic.thresholds
          i.thr<-datamodel$intensity.thresholds
          datfile.plot<-data.frame(datfile[input$SelectSurveillance],datamodel$typ.curve)
          survweek<-(1:(NROW(datfile)))[SurveillanceWeek==rownames(datfile)]
          datfile.plot[-(1:survweek),1]<-NA
          names(datfile.plot)<-c(input$SelectSurveillance,"Lower interval","Average curve","Higher interval")
          colors.palette<-generate_palette(i.number.series=3,
                                           i.colObservedLines=input$colObservedLines,
                                           i.colObservedPoints=input$colObservedPoints,
                                           i.colEpidemicStart=input$colEpidemicStart,
                                           i.colEpidemicStop=input$colEpidemicStop,
                                           i.colThresholds=input$colThresholds,
                                           i.colSeasons=input$colSeasons,
                                           i.colEpidemic=input$colEpidemic)
          p <- plotSeasons(datfile.plot,
                           i.epidemic.thr=e.thr,
                           i.intensity.thr=i.thr,
                           i.pre.epidemic = as.logical(input$preepidemicthr),
                           i.post.epidemic = as.logical(input$postepidemicthr),
                           i.intensity = as.logical(input$intensitythr),
                           i.textMain=input$textMain,
                           i.textX=input$textX,
                           i.textY=input$textY,
                           i.type.threshold=as.numeric(input$typethreshold),
                           i.tails.threshold=as.numeric(input$ntails),
                           i.type.intensity=as.numeric(input$typeintensity),
                           i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                           i.tails.intensity=as.numeric(input$ntails),
                           i.type.curve=as.numeric(input$typecurve),
                           i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                           i.type.other=as.numeric(input$typeother),
                           i.level.other=as.numeric(input$leveltypicalcurve)/100,
                           i.method=as.numeric(input$method),
                           i.param=as.numeric(input$param),
                           i.n.max=as.numeric(input$nvalues),
                           i.colObservedPoints=colors.palette$colObservedPoints,
                           i.colSeasons=c(colors.palette$colObservedLines,colors.palette$colSeasons[c(3,2,3)]),
                           i.colThresholds=colors.palette$colThresholds)
          if (!is.null(p)){
            temp1<-p$gdata
            temp2<-dcast(temp1, week ~ variable, value.var = "value", drop = FALSE, fill = NA)
            temp2<-temp2[order(temp2$week),p$labels]
            row.names(temp2)<-p$weeklabels
            temp2$week<-NULL
            export.mydata(i.data=temp2, i.file = file, 
                          i.sheet="Average curve", i.rownames="Week no", i.format="xlsx") 
          }
        }
      }
    }
  },
  contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$tbsSurveillanceAverage_c <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep='') },
  content = function(file) {
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(!is.null(datfile)){
      if(input$SelectSurveillance %in% names(datfile)){
        if(is.null(input$SelectSurveillanceWeek)){
          SurveillanceWeek<-tail(row.names(datfile),1)
        }else if(!(input$SelectSurveillanceWeek %in% row.names(datfile))){
          SurveillanceWeek<-tail(row.names(datfile),1)
        }else{
          SurveillanceWeek<-input$SelectSurveillanceWeek
        }
        if (is.null(input$SelectSurveillanceForceEpidemic)){
          force.start<-NA
        }else if(!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))){
          force.start<-NA
        }else{
          force.start<-input$SelectSurveillanceForceEpidemic
        } 
        datamodel<-data_model()
        if(!is.null(datamodel)){
          e.thr<-datamodel$epidemic.thresholds
          i.thr<-datamodel$intensity.thresholds
          datfile.plot<-data.frame(datfile[input$SelectSurveillance],datamodel$typ.curve)
          survweek<-(1:(NROW(datfile)))[SurveillanceWeek==rownames(datfile)]
          datfile.plot[-(1:survweek),1]<-NA
          names(datfile.plot)<-c(input$SelectSurveillance,"Lower interval","Average curve","Higher interval")
          colors.palette<-generate_palette(i.number.series=3,
                                           i.colObservedLines=input$colObservedLines,
                                           i.colObservedPoints=input$colObservedPoints,
                                           i.colEpidemicStart=input$colEpidemicStart,
                                           i.colEpidemicStop=input$colEpidemicStop,
                                           i.colThresholds=input$colThresholds,
                                           i.colSeasons=input$colSeasons,
                                           i.colEpidemic=input$colEpidemic)
          p <- plotSeasons(datfile.plot,
                           i.epidemic.thr=e.thr,
                           i.intensity.thr=i.thr,
                           i.pre.epidemic = as.logical(input$preepidemicthr),
                           i.post.epidemic = as.logical(input$postepidemicthr),
                           i.intensity = as.logical(input$intensitythr),
                           i.textMain=input$textMain,
                           i.textX=input$textX,
                           i.textY=input$textY,
                           i.type.threshold=as.numeric(input$typethreshold),
                           i.tails.threshold=as.numeric(input$ntails),
                           i.type.intensity=as.numeric(input$typeintensity),
                           i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                           i.tails.intensity=as.numeric(input$ntails),
                           i.type.curve=as.numeric(input$typecurve),
                           i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                           i.type.other=as.numeric(input$typeother),
                           i.level.other=as.numeric(input$leveltypicalcurve)/100,
                           i.method=as.numeric(input$method),
                           i.param=as.numeric(input$param),
                           i.n.max=as.numeric(input$nvalues),
                           i.colObservedPoints=colors.palette$colObservedPoints,
                           i.colSeasons=c(colors.palette$colObservedLines,colors.palette$colSeasons[c(3,2,3)]),
                           i.colThresholds=colors.palette$colThresholds)
          if (!is.null(p)){
            temp1<-p$gdata
            temp2<-dcast(temp1, week ~ variable, value.var = "value", drop = FALSE, fill = NA)
            temp2<-temp2[order(temp2$week),p$labels]
            row.names(temp2)<-p$weeklabels
            temp2$week<-NULL
            export.mydata(i.data=temp2, i.file = file, 
                          i.sheet="Average curve", i.rownames="Week no", i.format="csv")
          } 
        }
      }
    }
  },
  contentType="text/csv"
)

#####################################
### VISUALIZE TAB
#####################################

output$tbVisualize <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else if (is.null(input$SelectSeasons)) {
    return(NULL)
  }else{
    toinclude<-input$SelectSeasons
    selectedcolumns<-select.columns(i.names=names(datfile),
                                    i.from=input$SelectSeasons[1],
                                    i.to=input$SelectSeasons[1],
                                    i.exclude="",
                                    i.include=toinclude,
                                    i.pandemic=as.logical("TRUE"),
                                    i.seasons=NA)
    if (length(selectedcolumns)>0){
      tabsetPanel(tabPanel("Data", DT::dataTableOutput("tbvData")),
                  tabPanel("Seasons", plotlyOutput("tbvSeasons", width ="100%", height ="100%")),
                  tabPanel("Series",plotlyOutput("tbvSeries", width ="100%", height ="100%")),
                  tabPanel("Timing",uiOutput("tbvTiming"))
      )
    }else{
      return(NULL)
    }
  }
})

output$tbvData <- DT::renderDataTable({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    datatoshow<-NULL
  }else if (is.null(input$SelectSeasons)) {
    datatoshow<-NULL
  }else{
    # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
    toinclude<-input$SelectSeasons
    selectedcolumns<-select.columns(i.names=names(datfile),
                                    i.from=input$SelectSeasons[1],
                                    i.to=input$SelectSeasons[1],
                                    i.exclude="",
                                    i.include=toinclude,
                                    i.pandemic=as.logical("TRUE"),
                                    i.seasons=NA)
    if (length(selectedcolumns)>0){
      datatoshow<-format(round(datfile[selectedcolumns], 2), nsmall=2)
    }else{
      datatoshow<-data.frame(Message="No data selected",row.names = NULL)
    }
  }
  datatoshow
}, 
#extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))
options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

output$tbvSeasons <- renderPlotly({
  readdata <- read_data()
  datfile <- readdata$datasetread
  datamodel<-data_model()
  if(is.null(datfile)){
    zfix<-NULL
  }else if(is.null(datamodel)){
    zfix<-NULL
  }else if (is.null(input$SelectSeasons)) {
    zfix<-NULL
  }else{
    e.thr<-datamodel$epidemic.thresholds
    i.thr<-datamodel$intensity.thresholds
    toinclude<-input$SelectSeasons
    selectedcolumns<-select.columns(i.names=names(datfile),
                                    i.from=input$SelectSeasons[1],
                                    i.to=input$SelectSeasons[1],
                                    i.exclude="",
                                    i.include=toinclude,
                                    i.pandemic=as.logical("TRUE"),
                                    i.seasons=NA)
    if (length(selectedcolumns)==0){
      zfix<-NULL
    }else{
      datfile.plot<-datfile[selectedcolumns]
      colors.palette<-generate_palette(i.number.series=NCOL(datfile.plot),
                                       i.colObservedLines=input$colObservedLines,
                                       i.colObservedPoints=input$colObservedPoints,
                                       i.colEpidemicStart=input$colEpidemicStart,
                                       i.colEpidemicStop=input$colEpidemicStop,
                                       i.colThresholds=input$colThresholds,
                                       i.colSeasons=input$colSeasons,
                                       i.colEpidemic=input$colEpidemic)
      p <- plotSeasons(datfile.plot,
                       i.epidemic.thr=e.thr,
                       i.intensity.thr=i.thr,
                       i.pre.epidemic = as.logical(input$preepidemicthr),
                       i.post.epidemic = as.logical(input$postepidemicthr),
                       i.intensity = as.logical(input$intensitythr),
                       i.textMain=input$textMain,
                       i.textX=input$textX,
                       i.textY=input$textY,
                       i.type.threshold=as.numeric(input$typethreshold),
                       i.tails.threshold=as.numeric(input$ntails),
                       i.type.intensity=as.numeric(input$typeintensity),
                       i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                       i.tails.intensity=as.numeric(input$ntails),
                       i.type.curve=as.numeric(input$typecurve),
                       i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                       i.type.other=as.numeric(input$typeother),
                       i.level.other=as.numeric(input$leveltypicalcurve)/100,
                       i.method=as.numeric(input$method),
                       i.param=as.numeric(input$param),
                       i.n.max=as.numeric(input$nvalues),
                       i.colObservedPoints=colors.palette$colObservedPoints,
                       i.colSeasons=colors.palette$colSeasons,
                       i.colThresholds=colors.palette$colThresholds)
      if (is.null(p)){
        zfix<-NULL
      }else{
        z <- ggplotly(p$plot, width = 800, height = 600)
        zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
      }
    }
  }
  zfix
})

output$tbvSeries <- renderPlotly({
  readdata <- read_data()
  datfile <- readdata$datasetread
  datamodel<-data_model()
  if(is.null(datfile)){
    zfix<-NULL
  }else if(is.null(datamodel)){
    zfix<-NULL
  }else if (is.null(input$SelectSeasons)) {
    zfix<-NULL
  }else{
    e.thr<-datamodel$epidemic.thresholds
    i.thr<-datamodel$intensity.thresholds
    toinclude<-input$SelectSeasons
    selectedcolumns<-select.columns(i.names=names(datfile),
                                    i.from=input$SelectSeasons[1],
                                    i.to=input$SelectSeasons[1],
                                    i.exclude="",
                                    i.include=toinclude,
                                    i.pandemic=as.logical("TRUE"),
                                    i.seasons=NA)
    if (length(selectedcolumns)==0) {
      zfix<-NULL
    }else{
      datfile.plot<-datfile[selectedcolumns]
      colors.palette<-generate_palette(i.number.series=NA,
                                       i.colObservedLines=input$colObservedLines,
                                       i.colObservedPoints=input$colObservedPoints,
                                       i.colEpidemicStart=input$colEpidemicStart,
                                       i.colEpidemicStop=input$colEpidemicStop,
                                       i.colThresholds=input$colThresholds,
                                       i.colSeasons=input$colSeasons,
                                       i.colEpidemic=input$colEpidemic)
      p <- plotSeries(i.data=datfile.plot,
                      i.plot.timing = T,
                      i.range.x=NA,
                      i.pre.epidemic=as.logical(input$preepidemicthr),
                      i.post.epidemic=as.logical(input$postepidemicthr),
                      i.epidemic.thr=e.thr,
                      i.intensity= as.logical(input$intensitythr),
                      i.intensity.thr=i.thr,
                      i.range.y=NA,
                      i.replace.x.cr=T,
                      i.textMain=input$textMain,
                      i.textX=input$textX,
                      i.textY=input$textY,
                      i.type.threshold=as.numeric(input$typethreshold),
                      i.tails.threshold=as.numeric(input$ntails),
                      i.type.intensity=as.numeric(input$typeintensity),
                      i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                      i.tails.intensity=as.numeric(input$ntails),
                      i.type.curve=as.numeric(input$typecurve),
                      i.level.curve=as.numeric(input$leveltypicalcurve)/100,
                      i.type.other=as.numeric(input$typeother),
                      i.level.other=as.numeric(input$leveltypicalcurve)/100,
                      i.method=as.numeric(input$method),
                      i.param=as.numeric(input$param),
                      i.n.max=as.numeric(input$nvalues),
                      i.colObservedLines=colors.palette$colObservedLines,
                      i.colThresholds=colors.palette$colThresholds,
                      i.colObservedPoints=colors.palette$colObservedPoints,
                      i.colEpidemic=colors.palette$colEpidemic)
      if (is.null(p)){
        zfix<-NULL
      }else{
        z <- ggplotly(p$plot, width = 800, height = 600)
        zfix<-fixplotly(z,p$labels,p$haslines,p$haspoints,"week","value",p$weeklabels)
      }
    }
  }
  zfix
})

output$tbvTiming = renderUI({
  tabnames<-input$SelectSeasons
  if (is.null(tabnames)) {
    return(NULL)
  }else{
    do.call(tabsetPanel,
            ## Create a set of tabPanel functions dependent on tabnames
            lapply(tabnames,function(s){
              ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
              ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
              call("tabPanel",s,call('plotlyOutput',outputId=paste0("tbvTiming_",s), width ="100%", height ="100%"))
            })
    )
  }
})


session$onSessionEnded(function() {
  stopApp()
})
})

