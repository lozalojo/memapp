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
                tabPanel("Data", DT::dataTableOutput("tbdData")),
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
      datatoshow<-roundF(datfile[selectedcolumns],2)
    }else{
      datatoshow<-data.frame(Message="No data selected",row.names = NULL)
    }
  }
  datatoshow
}, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

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
                #tabPanel("Scheme", DT::dataTableOutput("tbdEscheme")),
                tabPanel("Scheme", formattableOutput("tbdEscheme")),
                tabPanel("Details", DT::dataTableOutput("tbdEdetails"))
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
      z <- ggplotly(p, width = 800, height = 600)
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
      z <- ggplotly(p, width = 800, height = 600)
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
      z <- ggplotly(p, width = 800, height = 600)
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
      z <- ggplotly(p, width = 800, height = 600)
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
    datashow<-roundF(dataevolution$evolution.data,2)
    names(datashow)<-c("Seasons","Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
  }
  datashow
}, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

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
                tabPanel("Details", DT::dataTableOutput("tbdSdetails"))
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
      z <- ggplotly(p, width = 800, height = 600)
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
      z <- ggplotly(p, width = 800, height = 600)
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
      z <- ggplotly(p, width = 800, height = 600)
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
      z <- ggplotly(p, width = 800, height = 600)
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
    datashow<-roundF(datastability$stability.data,2)
    names(datashow)<-c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic percentage (lower limit)","Epidemic percentage","Epidemic percentage (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr.")
  }
  datashow
}, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

#####################################
### MODEL TAB
#####################################

output$tbModel <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Data", DT::dataTableOutput("tbmData")),
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
    datatoshow<-roundF(datatoshow<-datamodel$param.data,2)
  }
  datatoshow
}, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

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
      valueBox(roundF(datamodel$ci.length[1,2],1), "Average epidemic length", icon = icon("heartbeat"), width=3, color="light-blue"),
      valueBox(paste0(roundF(datamodel$ci.percent[2],1), "%"), "Epidemic percentage", icon = icon("heartbeat"), width=3, color="light-blue"),
      valueBox(roundF(datamodel$pre.post.intervals[1,3],1), "Epidemic threshold", icon = icon("thermometer-1"), width=3, color="green"),
      valueBox(roundF(datamodel$epi.intervals[1,4],1), "Medium threshold", icon = icon("thermometer-2"), width=3, color="yellow"),
      valueBox(roundF(datamodel$epi.intervals[2,4],1), "High threshold", icon = icon("thermometer-3"), width=3, color="orange"),
      valueBox(roundF(datamodel$epi.intervals[3,4],1), "Very high threshold", icon = icon("thermometer-4"), width=3, color="red")
    )
  }
})

output$tbmMemOutput <- renderPrint({
  datamodel<-data_model()
  if(!is.null(datamodel)){
    nam.t<-datamodel
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
                tabPanel("Detailed", formattableOutput("tbmGoodnessModelDetail1")),
                tabPanel("Intensity", uiOutput("tbmGoodnessModelIntensity")),
                tabPanel("Detailed", formattableOutput("tbmGoodnessModelDetail2"))
    )
})

output$tbmGoodnessModelSummary <- renderUI({
  good <- data_good_model()
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(roundF(good$results["Sensitivity"],2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Specificity"],2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Positive predictive value"],2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Negative predictive value"],2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Percent agreement"],2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(roundF(good$results["Matthews correlation coefficient"],2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua")
    )
  }
})

output$tbmGoodnessModelDetail1<-renderFormattable({
  good <- data_good_model()
  if(!is.null(good)){
    temp1<-as.data.frame(good$validity.data)
    temp1$Total<-good$results
    temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
    temp1[is.na(temp1)]<--1
    temp1<-roundF(temp1,2)
    good.table<-formattable(temp1, list(
      area(col = names(temp1)[1:4]) ~ normalize_bar("#FFBBFF", 0.2),
      area(col = names(temp1)[5:6]) ~ normalize_bar("#A5DBEB", 0.2)
    ))    
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    good.table<-formattable(temp1)
  }
  good.table
})

output$tbmGoodnessModelIntensity <- renderUI({
  good <- data_good_model()
  peaks <- good$peaks
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==1]*100,1), "%"), paste0(peaks$Description[peaks[,1]==1]," level"), icon = icon("heartbeat"), width=2, color="lime"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==2]*100,1), "%"), paste0(peaks$Description[peaks[,1]==2]," level"), icon = icon("thermometer-1"), width=2, color="green"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==3]*100,1), "%"), paste0(peaks$Description[peaks[,1]==3]," level"), icon = icon("thermometer-2"), width=2, color="yellow"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==4]*100,1), "%"), paste0(peaks$Description[peaks[,1]==4]," level"), icon = icon("thermometer-3"), width=2, color="orange"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==5]*100,1), "%"), paste0(peaks$Description[peaks[,1]==5]," level"), icon = icon("thermometer-4"), width=2, color="red"),
      valueBox(peaks$Count[peaks[,1]==-1], peaks$Description[peaks[,1]==-1], icon = icon("heartbeat"), width=3, color="teal"),
      valueBox(peaks$Count[peaks[,1]==0], peaks$Description[peaks[,1]==0], icon = icon("heartbeat"), width=3, color="teal")
    )
  }
})

output$tbmGoodnessModelDetail2<-renderFormattable({
  good <- data_good_model()
  if(!is.null(good)){
    temp1 <- good$peaks.data
    temp1[c(1,3:6)]<-roundF(temp1[c(1,3:6)],2)
    thr.c<-c("#8c6bb1","#88419d","#810f7c","#4d004b")
    lvl.n<-c(1:5)
    lvl.t<-c("Baseline","Low","Medium","High","Very high")
    lvl.c<-c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")
    peaks.data<-formattable(temp1, list(
      Level = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.n[1], lvl.c[1] , ifelse(x==lvl.n[2], lvl.c[2], ifelse(x==lvl.n[3], lvl.c[3], ifelse(x==lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      Description = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.t[1], lvl.c[1] , ifelse(x==lvl.t[2], lvl.c[2], ifelse(x==lvl.t[3], lvl.c[3], ifelse(x==lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      "Epidemic threshold"=formatter("span", style = style(color = thr.c[1], font.weight = "bold")),
      "Medium threshold"=formatter("span", style = style(color = thr.c[2], font.weight = "bold")),
      "High threshold"=formatter("span", style = style(color = thr.c[3], font.weight = "bold")),
      "Very high threshold"=formatter("span", style = style(color = thr.c[4], font.weight = "bold"))
    ))
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    peaks.data<-formattable(temp1)
  }
  peaks.data
})

output$tbmGoodnessGlobal <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }
  else
    tabsetPanel(tabPanel("Indicators", uiOutput("tbmGoodnessGlobalSummary")),
                tabPanel("Detailed", formattableOutput("tbmGoodnessGlobalDetail1")),
                tabPanel("Intensity", uiOutput("tbmGoodnessGlobalIntensity")),
                tabPanel("Detailed", formattableOutput("tbmGoodnessGlobalDetail2"))
    )
})

output$tbmGoodnessGlobalSummary <- renderUI({
  good <- data_good_global()
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(roundF(good$results["Sensitivity"],2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Specificity"],2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Positive predictive value"],2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Negative predictive value"],2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(good$results["Percent agreement"],2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(roundF(good$results["Matthews correlation coefficient"],2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua")
    )
  }
})

output$tbmGoodnessGlobalDetail1<-renderFormattable({
  good <- data_good_global()
  if(!is.null(good)){
    temp1<-as.data.frame(good$validity.data)
    temp1$Total<-good$results
    temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")]
    temp1[is.na(temp1)]<--1
    temp1<-roundF(temp1,2)
    good.table<-formattable(temp1, list(
      area(col = names(temp1)[1:4]) ~ normalize_bar("#FFBBFF", 0.2),
      area(col = names(temp1)[5:6]) ~ normalize_bar("#A5DBEB", 0.2)
    ))
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    good.table<-formattable(temp1)
  }
  good.table
})

output$tbmGoodnessGlobalIntensity <- renderUI({
  good <- data_good_global()
  peaks <- good$peaks
  if(is.null(good)){
    return(NULL)
  }else{
    fluidRow(
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==1]*100,1), "%"), paste0(peaks$Description[peaks[,1]==1]," level"), icon = icon("heartbeat"), width=2, color="lime"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==2]*100,1), "%"), paste0(peaks$Description[peaks[,1]==2]," level"), icon = icon("thermometer-1"), width=2, color="green"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==3]*100,1), "%"), paste0(peaks$Description[peaks[,1]==3]," level"), icon = icon("thermometer-2"), width=2, color="yellow"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==4]*100,1), "%"), paste0(peaks$Description[peaks[,1]==4]," level"), icon = icon("thermometer-3"), width=2, color="orange"),
      valueBox(paste0(roundF(peaks$Percentage[peaks[,1]==5]*100,1), "%"), paste0(peaks$Description[peaks[,1]==5]," level"), icon = icon("thermometer-4"), width=2, color="red"),
      valueBox(peaks$Count[peaks[,1]==-1], peaks$Description[peaks[,1]==-1], icon = icon("heartbeat"), width=3, color="teal"),
      valueBox(peaks$Count[peaks[,1]==0], peaks$Description[peaks[,1]==0], icon = icon("heartbeat"), width=3, color="teal")
    )
  }
})

output$tbmGoodnessGlobalDetail2<-renderFormattable({
  good <- data_good_global()
  if(!is.null(good)){
    temp1 <- good$peaks.data
    temp1[c(1,3:6)]<-roundF(temp1[c(1,3:6)],2)
    peaks.data<-thr.c<-c("#8c6bb1","#88419d","#810f7c","#4d004b")
    lvl.n<-c(1:5)
    lvl.t<-c("Baseline","Low","Medium","High","Very high")
    lvl.c<-c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")
    peaks.data<-formattable(temp1, list(
      Level = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.n[1], lvl.c[1] , ifelse(x==lvl.n[2], lvl.c[2], ifelse(x==lvl.n[3], lvl.c[3], ifelse(x==lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      Description = formatter("span", style = x ~ style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.t[1], lvl.c[1] , ifelse(x==lvl.t[2], lvl.c[2], ifelse(x==lvl.t[3], lvl.c[3], ifelse(x==lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
      "Epidemic threshold"=formatter("span", style = style(color = thr.c[1], font.weight = "bold")),
      "Medium threshold"=formatter("span", style = style(color = thr.c[2], font.weight = "bold")),
      "High threshold"=formatter("span", style = style(color = thr.c[3], font.weight = "bold")),
      "Very high threshold"=formatter("span", style = style(color = thr.c[4], font.weight = "bold"))
    ))
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2")
    peaks.data<-formattable(temp1)
  }
  peaks.data
})

output$tbmOptimize <- renderUI({
  readdata <- read_data()
  datfile <- readdata$datasetread
  if(is.null(datfile)){
    return(NULL)
  }else{
    tabsetPanel(tabPanel("Indicators", uiOutput("tbmOptimizeSummary")),
                tabPanel("Detailed", formattableOutput("tbmOptimizeDetail")),
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
      valueBox(roundF(optim["sensitivity"],2), "Sensitivity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(optim["specificity"],2), "Specificity", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(optim["positive.predictive.value"],2), "Positive predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(optim["negative.predictive.value"],2), "Negative predictive value", icon = icon("heartbeat"), width=3, color="yellow"),
      valueBox(roundF(optim["percent.agreement"],2), "Percent agreement", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(roundF(optim["matthews.correlation.coefficient"],2), "Matthews correlation coefficient", icon = icon("heartbeat"), width=3, color="aqua"),
      valueBox(roundF(input$param,1), "Current parameter", icon = icon("heartbeat"), width=3, color="red"),
      valueBox(roundF(as.numeric(dataoptim$optimum["matthews"]),1), "Optimum parameter", icon = icon("heartbeat"), width=3, color="olive")
    )
  }
})

output$tbmOptimizeDetail<-renderFormattable({
  dataoptim <- data_optim()
  if(!is.null(dataoptim)){
    temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient")]
    names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient")
    rownames(temp1)<-NULL
    temp1[is.na(temp1)]<--1
    temp1<-roundF(temp1,2)
    roca.table<-formattable(temp1, list(
      area(col = names(roca.table)[2:5]) ~ normalize_bar("#FFBBFF", 0.2),
      area(col = names(roca.table)[6:7]) ~ normalize_bar("#A5DBEB", 0.2)
    ))
  }else{
    temp1<-data.frame(Error="Number of columns must be greater than 2",row.names = NULL)
    roca.table<-formattable(temp1)
  }
  roca.table
})

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
      datatoshow<-roundF(datfile[selectedcolumns],2)
    }else{
      datatoshow<-data.frame(Message="No data selected",row.names = NULL)
    }
  }
  datatoshow
}, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

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
                tabPanel("Average", plotlyOutput("tbsSurveillanceAverage", width ="100%", height ="100%"))
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
      datatoshow<-roundF(datfile[selectedcolumns],2)
    }else{
      datatoshow<-data.frame(Message="No data selected",row.names = NULL)
    }
  }
  datatoshow
}, extensions = 'Buttons', options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs=list(list(targets="_all", class="dt-right"))))

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

#####################################
### CUSTOM FUNCTIONS
#####################################

generate_palette <- function(i.number.series=NA,
                             i.colObservedLines=NULL,
                             i.colObservedPoints=NULL,
                             i.colEpidemicStart=NULL,
                             i.colEpidemicStop=NULL,
                             i.colThresholds=NULL,
                             i.colSeasons=NULL,
                             i.colEpidemic=NULL){
  params.default<-list(colObservedLines="#808080",
                       colObservedPoints="#000000",
                       colEpidemicStart="#FF0000",
                       colEpidemicStop="#40FF40",
                       colThresholds=c("#8c6bb1","#88419d","#810f7c","#4d004b","#c0c0ff"),
                       colSeasons="Accent",
                       colEpidemic=c("#00C000","#800080","#FFB401")
  )
  if (is.na(i.number.series)) i.number.series<-10
  if (is.null(i.colObservedLines)) i.colObservedLines<-"default" else if (is.na(i.colObservedLines)) i.colObservedLines<-"default"
  if (is.null(i.colObservedPoints)) i.colObservedPoints<-"default" else if (is.na(i.colObservedPoints)) i.colObservedPoints<-"default"
  if (is.null(i.colEpidemicStart)) i.colEpidemicStart<-"default" else if (is.na(i.colEpidemicStart)) i.colEpidemicStart<-"default"
  if (is.null(i.colEpidemicStop)) i.colEpidemicStop<-"default" else if (is.na(i.colEpidemicStop)) i.colEpidemicStop<-"default"
  if (is.null(i.colThresholds)) i.colThresholds<-"default" else if (is.na(i.colThresholds)) i.colThresholds<-"default"
  if (is.null(i.colSeasons)) i.colSeasons<-"default" else if (is.na(i.colSeasons)) i.colSeasons<-"default"
  if (is.null(i.colEpidemic)) i.colEpidemic<-"default" else if (is.na(i.colEpidemic)) i.colEpidemic<-"default"

  # First four are simple colors
  if (i.colObservedLines=="default") i.colObservedLines<-params.default$colObservedLines else i.colObservedLines<-col2hex(i.colObservedLines)
  if (i.colObservedPoints=="default") i.colObservedPoints<-params.default$colObservedPoints else i.colObservedPoints<-col2hex(i.colObservedPoints)
  if (i.colEpidemicStart=="default") i.colEpidemicStart<-params.default$colEpidemicStart else i.colEpidemicStart<-col2hex(i.colEpidemicStart)
  if (i.colEpidemicStop=="default") i.colEpidemicStop<-params.default$colEpidemicStop else i.colEpidemicStop<-col2hex(i.colEpidemicStop)
  # Fifth to Seventh are palettes that I must create
  if (i.colThresholds=="default") i.colThresholds<-params.default$colThresholds else i.colThresholds<-brewer.pal(7,i.colThresholds)[2:6]
  if (i.colSeasons=="default") i.colSeasons<-params.default$colSeasons
  i.colSeasons <- colorRampPalette(brewer.pal(max(3,min(8,i.number.series)),i.colSeasons))(i.number.series)
  if (i.colEpidemic=="default") i.colEpidemic<-params.default$colEpidemic else i.colEpidemic<-brewer.pal(5,i.colEpidemic)[2:4]
  # Last one is a number between 0 and 1
  # colTransparency<-input$colTransparency
  # if (is.null(colTransparency)) colTransparency<-1 else if (is.na(colTransparency)) colTransparency<-1
  colors.final<-list(colObservedLines=i.colObservedLines, colObservedPoints=i.colObservedPoints,
                     colEpidemicStart=i.colEpidemicStart, colEpidemicStop=i.colEpidemicStop,
                     colThresholds=i.colThresholds, colSeasons=i.colSeasons,colEpidemic=i.colEpidemic
  )
  colors.final
}

plotSeasons <- function(i.data,
                        i.pre.epidemic=TRUE,
                        i.post.epidemic=TRUE,
                        i.epidemic.thr=NA,
                        i.intensity=TRUE,
                        i.intensity.thr=NA,
                        i.range.x=NA,
                        i.range.y=NA,
                        i.tickmarks=30,
                        i.textMain="",
                        i.textX="",
                        i.textY="",
                        i.colObservedPoints="#000000",
                        i.colSeasons="Accent",
                        i.colThresholds=c("#8c6bb1","#88419d","#810f7c","#4d004b","#c0c0ff"),
                        ...){

  if(is.null(i.data)){
    p<-NULL
  }else{
    if (any(is.na(i.range.x)) | !is.numeric(i.range.x) | length(i.range.x)!=2) i.range.x<-c(min(as.numeric(rownames(i.data)[1:(min(3,NROW(i.data)))])),max(as.numeric(rownames(i.data)[(max(1,NROW(i.data)-2)):NROW(i.data)])))
    if (i.range.x[1] < 1) i.range.x[1] <- 1
    if (i.range.x[1] > 52) i.range.x[1] <- 52
    if (i.range.x[2] < 1) i.range.x[2] <- 1
    if (i.range.x[2] > 52) i.range.x[2] <- 52
    if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
    if (i.range.x[2]==0) i.range.x[2]<-52
    # Input scheme numbering
    week.f<-i.range.x[1]
    week.l<-i.range.x[2]
    last.week<-52
    if (week.f>week.l){
      i.range.x.values<-data.frame(week.lab=c(week.f:last.week,1:week.l),week.no=1:(last.week-week.f+1+week.l))
    }else{
      i.range.x.values<-data.frame(week.lab=week.f:week.l,week.no=1:(week.l-week.f+1))
    }

    if (NCOL(i.data)>1){
      epi<-memmodel(i.data,
                    i.seasons = NA,
                    ...)
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
      epi<-memmodel(cbind(i.data,i.data),
                    i.seasons = NA,
                    ...)
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

    data.full.missing<-data.full.epi
    data.full.missing[!(is.na(data.full) & !is.na(data.full.epi))]<-NA

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

    labels<-c(names(data.full),
              paste(names(data.full)," (missing)",sep=""),
              "Epidemic thr.","Medium thr.","High thr.","Very high thr.","Post thr.")
    haspoints<-c(rep(F,NCOL(data.full)),rep(T,NCOL(data.full)),F,F,F,F,F)
    haslines<-c(rep(T,NCOL(data.full)),rep(F,NCOL(data.full)),T,T,T,T,T)
    shapes<-c(rep(NA,NCOL(data.full)),rep(24,NCOL(data.full)),NA,NA,NA,NA,NA)
    colors<-c(rep(i.colSeasons,2),i.colThresholds)
    fills<-c(rep(i.colSeasons,2),rep(i.colObservedPoints,5))
    sizes<-c(rep(2,NCOL(data.full)),rep(2,NCOL(data.full)),1,1,1,1,1)
    linetypes<-c(rep("solid",NCOL(data.full)),rep("solid",NCOL(data.full)), "dashed", "dashed", "dashed", "dashed","dashed")

    # Data to plot

    dgraf<-cbind(data.full.epi,data.full.missing,
                 epit=epidemic[1],
                 medt=intensity[1],
                 higt=intensity[2],
                 vert=intensity[3],
                 post=epidemic[2]
    )
    names(dgraf)<-labels
    dgraf$week<-1:NROW(dgraf)

    dgrafgg<-melt(dgraf,id="week")

    selected.indicators<-(1:(2*NCOL(data.full)))[apply(dgraf[1:(2*NCOL(data.full))],2,function(x) !all(is.na(x)))]
    if (i.pre.epidemic) selected.indicators<-c(selected.indicators,2*NCOL(data.full)+1)
    if (i.post.epidemic) selected.indicators<-c(selected.indicators,2*NCOL(data.full)+5)
    if (i.intensity) selected.indicators<-c(selected.indicators,2*NCOL(data.full)+2:4)
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
    axis.x.range.original <- range(i.range.x.values$week.no)
    axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], i.tickmarks, 1:axis.x.range.original[2],T,F)
    axis.x.range <- axis.x.otick$range
    axis.x.ticks<- axis.x.otick$tickmarks
    axis.x.labels<-i.range.x.values$week.lab[axis.x.otick$tickmarks]

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
      geom_line(aes(x=week,y=value,group=variable, color=variable, linetype=variable),size=0.5) +
      geom_point(aes(x=week,y=value,group=variable, color=variable, size=variable, fill=variable, shape=variable), color="#ffffff", stroke = 0.1) +
      scale_shape_manual(values=shapes.s, name="Legend", labels=labels.s) +
      scale_color_manual(values=colors.s, name="Legend", labels=labels.s) +
      scale_fill_manual(values=fills.s, name="Legend", labels=labels.s) +
      scale_size_manual(values=sizes.s, name="Legend", labels=labels.s) +
      scale_linetype_manual(values=linetypes.s, name="Legend", labels=labels.s) +
      scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = i.textMain, x = i.textX, y = i.textY) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggthemes::theme_few()

    p<-list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,weeklabels=i.range.x.values$week.lab)
  }
  p
}

plotSeries<-function(i.data,
                     i.plot.timing = T,
                     i.pre.epidemic=T,
                     i.post.epidemic=T,
                     i.epidemic.thr=NA,
                     i.intensity= T,
                     i.intensity.thr=NA,
                     i.range.x=NA,
                     i.range.y=NA,
                     i.tickmarks=30,
                     i.replace.x.cr=F,
                     i.textMain="",
                     i.textX="",
                     i.textY="",
                     i.colObservedLines="#808080",
                     i.colThresholds=c("#8c6bb1","#88419d","#810f7c","#4d004b","#c0c0ff"),
                     i.colObservedPoints="#000000",
                     i.colEpidemic=c("#00C000","#800080","#FFB401"),
                     ...){

  if(is.null(i.data)){
    p<-NULL
  }else{
    # Range x fix
    i.cutoff.original<-min(as.numeric(rownames(i.data)[1:(min(3,NROW(i.data)))]))
    if (i.cutoff.original < 1) i.cutoff.original <- 1
    if (i.cutoff.original > 52) i.cutoff.original <- 52
    if (any(is.na(i.range.x)) | !is.numeric(i.range.x) | length(i.range.x)!=2) i.range.x<-c(min(as.numeric(rownames(i.data)[1:(min(3,NROW(i.data)))])),max(as.numeric(rownames(i.data)[(max(1,NROW(i.data)-2)):NROW(i.data)])))
    if (i.range.x[1] < 1) i.range.x[1] <- 1
    if (i.range.x[1] > 52) i.range.x[1] <- 52
    if (i.range.x[2] < 1) i.range.x[2] <- 1
    if (i.range.x[2] > 52) i.range.x[2] <- 52
    if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
    if (i.range.x[2]==0) i.range.x[2]<-52
    week.f<-i.range.x[1]
    week.l<-i.range.x[2]
    last.week<-52
    if (week.f>week.l){
      i.range.x.values<-data.frame(week.lab=c(week.f:last.week,1:week.l),week.no=1:(last.week-week.f+1+week.l))
    }else{
      i.range.x.values<-data.frame(week.lab=week.f:week.l,week.no=1:(week.l-week.f+1))
    }

    if (NCOL(i.data)>1){
      epi<-memmodel(i.data,
                    i.seasons = NA,
                    ...)
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
      epi<-memmodel(cbind(i.data,i.data),
                    i.seasons = NA,
                    ...)
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
    data.orig<-transformdata.back(data.full,i.name="rates", i.cutoff.original=i.cutoff.original, i.range.x.final=i.range.x, i.fun=sum)$data
    data.y<-as.numeric(data.orig[,"rates"])
    # Data to plot, filling in missing with data imputed by mem (using loess)
    data.fixed<-transformdata.back(data.full.epi,i.name="rates", i.cutoff.original=i.cutoff.original, i.range.x.final=i.range.x, i.fun=sum)$data
    data.y.fixed<-as.numeric(data.fixed[,"rates"])
    # Data that have been imputed, to mark them as a circle with a cross
    data.missing<-data.fixed
    data.missing[!(is.na(data.orig) & !is.na(data.fixed))]<-NA
    data.y.missing<-as.numeric(data.missing[,"rates"])
    # Indexes for pre, epi and post epidemic
    data.indexes<-transformdata.back(data.full.index,i.name="rates", i.cutoff.original=i.cutoff.original, i.range.x.final=i.range.x, i.fun=function(x,...) if (all(is.na(x))) return(NA) else if (any(x==2,...)) return(2) else if (any(x==1,...)) return(1) else return(3))$data
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
    haspoints<-c(F,T,T,T,T,T,T,F,F,F,F,F)
    haslines<-c(T,F,F,F,F,F,F,T,T,T,T,T)
    shapes<-c(21,21,24,21,24,21,24,NA,NA,NA,NA,NA)
    colors<-c(rep(i.colObservedLines,7),i.colThresholds)
    fills<-c(i.colObservedPoints,rep(i.colEpidemic,each=2),rep(i.colObservedPoints,5))
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
    temp2 <- optimal.tickmarks(temp1[1], temp1[2], floor(i.tickmarks/NCOL(i.data)), 1:temp1[2],T,F)
    axis.x.ticks<-data.x[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
    axis.x.labels1<-data.orig$week[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
    axis.x.labels2<-data.orig$season[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
    axis.x.labels2[axis.x.labels1!=i.range.x.values$week.lab[temp2$tickmarks][floor(temp2$number/2+1)]]<-""
    axis.x.labels<-paste(axis.x.labels1,axis.x.labels2,sep="\n")
    if (i.replace.x.cr) axis.x.labels<-gsub("/","\n",axis.x.labels)
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
      geom_line(aes(x=week,y=value,group=variable, color=variable, linetype=variable),size=0.5) +
      geom_point(aes(x=week,y=value,group=variable, color=variable, size=variable, fill=variable, shape=variable), color="#ffffff", stroke = 0.1) +
      scale_shape_manual(values=shapes.s, name="Legend", labels=labels.s) +
      scale_color_manual(values=colors.s, name="Legend", labels=labels.s) +
      scale_fill_manual(values=fills.s, name="Legend", labels=labels.s) +
      scale_size_manual(values=sizes.s, name="Legend", labels=labels.s) +
      scale_linetype_manual(values=linetypes.s, name="Legend", labels=labels.s) +
      scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = i.textMain, x = i.textX, y = i.textY) +
      ggthemes::theme_few()

    p<-list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,weeklabels=paste(data.orig$week,"<br />Season: ",data.orig$season,sep=""))
  }
  p
}

plotSurveillance<-function(i.data,
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
                           i.force.week.53=F,
                           i.textMain="",
                           i.textX="",
                           i.textY="",
                           i.colObservedLines="#808080",
                           i.colObservedPoints="#000000",
                           i.colEpidemicStart="#FF0000",
                           i.colEpidemicStop="#40FF40",
                           i.colThresholds=c("#8c6bb1","#88419d","#810f7c","#4d004b","#c0c0ff")){

  # check parameters
  if (is.null(i.data)) {
    p<-NULL
  }else if (is.null(dim(i.data))){
    p<-NULL
    #cat("Incorrect number of dimensions, input must be a data.frame.\n")
  }else if (!(ncol(i.data)==1)){
    p<-NULL
    #cat("Incorrect number of dimensions, only one season required.\n")
  }else{
    if (i.force.week.53) last.week<-53 else last.week<-52

    if (!is.numeric(i.range.x) | length(i.range.x)!=2) i.range.x<-c(max(1,as.numeric(rownames(i.data)[1])),min(52,as.numeric(rownames(i.data)[NROW(i.data)])))
    week.f<-i.range.x[1]
    week.l<-i.range.x[2]
    if (week.f < 1) week.f <- 1
    if (week.f > 52) week.f <- 52
    if (week.l < 1) week.l <- 1
    if (week.l > 52) week.l <- 52
    if (week.f == week.l) week.l <- week.l - 1
    last.week<-52
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
    colors<-c(i.colObservedLines,i.colThresholds,rep(i.colObservedLines,2))
    fills<-c(rep(i.colObservedPoints,6),i.colEpidemicStart,i.colEpidemicStop)
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
    if (i.start) selected.indicators<-c(selected.indicators,7)
    if (i.end) selected.indicators<-c(selected.indicators,8)
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
      labs(title = i.textMain, x = i.textX, y = i.textY) +
      ggthemes::theme_few()

    p<-list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,weeklabels=current.season$nombre.semana)
  }
  p
}

plotGeneric <- function(i.data,
                        i.range.y,
                        i.range.y.labels=NA,
                        i.shapes,
                        i.colors,
                        i.fills,
                        i.sizes,
                        i.linetypes,
                        i.linesize,
                        i.replace.x.cr=F,
                        i.textMain="",
                        i.textX="",
                        i.textY=""){
  if(is.null(i.data)){
    p<-NULL
  }else{
    dgraf<-i.data
    labels<-names(dgraf)
    dgraf$num<-1:NROW(dgraf)
    dgrafgg<-melt(dgraf,id="num")

    # Calculate ticks for x
    axis.x.range <- c(1,NROW(dgraf))
    axis.x.ticks<- 1:NROW(dgraf)
    axis.x.labels<-rownames(dgraf)
    if (i.replace.x.cr) axis.x.labels<-gsub("/","\n", axis.x.labels)
    # Range y fix
    if (length(i.range.y.labels)<2){
      if (length(i.range.y)!=2){
        i.range.y <- c(0,1.05*max(dgrafgg$value,na.rm=T))
      }else{
        i.range.y <- 1.05*i.range.y
      }
      axis.y.range.original <- i.range.y
      axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
      axis.y.range <- axis.y.otick$range
      axis.y.ticks <- axis.y.otick$tickmarks
      axis.y.labels <- axis.y.otick$tickmarks
    }else{
      axis.y.range.original <- c(1,length(i.range.y.labels))
      axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10,
                                        i.valid.ticks=1:(length(i.range.y.labels)),  i.include.min=T, i.include.max=T)
      axis.y.range <- axis.y.otick$range
      axis.y.ticks <- axis.y.otick$tickmarks
      axis.y.labels <- i.range.y.labels[axis.y.otick$tickmarks]
    }

    p<-ggplot(dgrafgg) +
      geom_line(aes(x=num,y=value,group=variable, color=variable, linetype=variable),size=i.linesize) +
      geom_point(aes(x=num,y=value,group=variable, color=variable, size=variable, fill=variable, shape=variable), color="#ffffff", stroke = 0.1) +
      scale_shape_manual(values=i.shapes, name="Legend", labels=labels) +
      scale_color_manual(values=i.colors, name="Legend", labels=labels) +
      scale_fill_manual(values=i.fills, name="Legend", labels=labels) +
      scale_size_manual(values=i.sizes, name="Legend", labels=labels) +
      scale_linetype_manual(values=i.linetypes, name="Legend", labels=labels) +
      scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = i.textMain, x = i.textX, y = i.textY) +
      ggthemes::theme_few()
  }
  p
}

read.data<-function(i.file,
                    i.file.name=NA,
                    i.dataset=NA,
                    i.range.x=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    if (fileextension=="xlsx"){
      temp2<-read.data.xlsx(i.file, filenameextension, i.dataset)
      datasets=temp2$datasets
      datasetread=temp2$datasetread
      dataweeks=temp2$dataweeks
      rm("temp2")
    }else if (fileextension=="xls"){
      temp2<-read.data.xls(i.file, filenameextension, i.dataset)
      datasets=temp2$datasets
      datasetread=temp2$datasetread
      dataweeks=temp2$dataweeks
      rm("temp2")
    }else if (fileextension %in% c("mdb","accdb")){
      temp2<-read.data.access(i.file, filenameextension, i.dataset)
      datasets=temp2$datasets
      datasetread=temp2$datasetread
      dataweeks=temp2$dataweeks
      rm("temp2")
    }else if (fileextension %in% c("csv","dat","prn","txt")){
      temp2<-read.data.text(i.file, filenameextension, i.dataset)
      datasets=temp2$datasets
      datasetread=temp2$datasetread
      dataweeks=temp2$dataweeks
      rm("temp2")
    }else if (fileextension %in% c("rds")){
      temp2<-read.data.rds(i.file, filenameextension, i.dataset)
      datasets=temp2$datasets
      datasetread=temp2$datasetread
      dataweeks=temp2$dataweeks
      rm("temp2")
    }else{
      datasets=NULL
      datasetread=NULL
      dataweeks=NULL
      cat(paste("read_data> Warning: Extension not recognised\t", filenameextension,"\n",sep=""));
    }
    rm("temp1","filename","fileextension","filenameextension")
  }
  if (!(is.null(datasetread))){
    # Remove columns only with NA
    naonlycolumns<-apply(datasetread, 2, function(x) all(is.na(x)))
    if (any(naonlycolumns)){
      cat("read_data> Note: Columns ",paste(names(datasetread)[naonlycolumns], collapse=",")," contain only NAs, removing...\n")
      datasetread<-datasetread[!naonlycolumns]
    }
    rm("naonlycolumns")
    # Remove character only columns
    nonnumericcolumns<-sapply(datasetread, function(x) !is.numeric(x))
    if (any(nonnumericcolumns)){
      cat("read_data> Note: Columns ",paste(names(datasetread)[nonnumericcolumns], collapse=",")," are not numeric, removing...\n")
      datasetread<-datasetread[!nonnumericcolumns]
    }
    rm("nonnumericcolumns")
    # dealing with season start and end, extracts information from rownames and gets season start/end
    if (NCOL(datasetread)>1){
      seasons<-data.frame(names(datasetread),matrix(str_match(names(datasetread),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?"),nrow=NCOL(datasetread),byrow=F)[,-1],stringsAsFactors = F)
    }else{
      seasons<-data.frame(t(c(names(datasetread),str_match(names(datasetread),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[-1])),stringsAsFactors = F)
    }
    names(seasons)<-c("column","anioi","aniof","aniow")
    seasons[is.na(seasons)]<-""
    seasons$aniof[seasons$aniof==""]<-seasons$anioi[seasons$aniof==""]
    seasonsname<-seasons$anioi
    seasonsname[seasons$aniof!=""]<-paste(seasonsname[seasons$aniof!=""],seasons$aniof[seasons$aniof!=""],sep="/")
    seasonsname[seasons$aniow!=""]<-paste(seasonsname[seasons$aniow!=""],"(",seasons$aniow[seasons$aniow!=""],")",sep="")
    seasons$season<-seasonsname
    rm("seasonsname")
    names(datasetread)<-seasons$season
    # Remove columns not detected as seasons
    noseasondetected<-(names(datasetread)=="")
    if (any(noseasondetected)){
      cat("read_data> Note: Columns ",paste((1:NCOL(datasetread))[noseasondetected], collapse=",")," does not have a correct header (2001, 2001/2002 or 2001/2001), removing...\n")
      datasetread<-datasetread[!noseasondetected]
    }
    rm("noseasondetected")
    if (NCOL(datasetread)==0){
      datasetread<-NULL
    }else{
      # Fix when reading access files, sometimes it changes the order of the weeks
      # This (i.range.x<-NA) is in case i implement the "week range option" to select the surveillance
      # period, if i implement it, i only have to substitute i.range.x for input$somethinstart/end
      i.cutoff.original<-min(as.numeric(rownames(datasetread)[1:(min(3,NROW(datasetread)))]))
      if (any(is.na(i.range.x)) | !is.numeric(i.range.x) | length(i.range.x)!=2) i.range.x<-c(min(as.numeric(rownames(datasetread)[1:(min(3,NROW(datasetread)))])),max(as.numeric(rownames(datasetread)[(max(1,NROW(datasetread)-2)):NROW(datasetread)])))
      if (i.range.x[1] < 1) i.range.x[1] <- 1
      if (i.range.x[1] > 52) i.range.x[1] <- 52
      if (i.range.x[2] < 1) i.range.x[2] <- 1
      if (i.range.x[2] > 52) i.range.x[2] <- 52
      if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
      if (i.range.x[2]==0) i.range.x[2]<-52
      datasetread<-transformdata.back(datasetread, i.name = "rates", i.cutoff.original=i.cutoff.original, i.range.x.final=i.range.x)$data
      datasetread<-transformdata(datasetread, i.name = "rates", i.range.x = i.range.x)$data
    }
  }
  readdata<-list(datasets=datasets, datasetread=datasetread, dataweeks=dataweeks)
  readdata
}

read.data.xlsx<-function(i.file, i.file.name=NA, i.dataset=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    cat("read_data> Excel 2007+ file detected: ", filenameextension, "\n", sep="")
    wb<-openxlsx::loadWorkbook(i.file)
    datasets<-openxlsx::sheets(wb)
    n.datasets<-length(datasets)
    if (is.na(i.dataset)){
      datasetread<-NULL
      dataweeks=NULL
    }else if (!(i.dataset %in% datasets)) {
      datasetread<-NULL
      dataweeks=NULL
      cat("read_data> Warning: Table ",i.dataset," not found\n");
    }else{
      cat("read_data> Number of datasets: ",n.datasets,"\tReading dataset: ",i.dataset,"\n",sep="")
      datasetread<-openxlsx::read.xlsx(wb,sheet=i.dataset,rowNames=F)
      # First column is the week name
      if (all(datasetread[,1] %in% 1:53)){
        rownames(datasetread)<-as.character(datasetread[,1])
        datasetread<-datasetread[-1]
        cat("read_data> Note: First column is the week name\n")
      }else rownames(datasetread)<-1:NROW(datasetread)
      dataweeks<-as.numeric(row.names(datasetread))
      cat("read_data> Read ",NROW(datasetread)," rows and ",NCOL(datasetread)," columns\n",sep="")
    }
  }
  list(datasets=datasets, datasetread=datasetread, dataweeks=dataweeks)
}

read.data.xls<-function(i.file, i.file.name=NA, i.dataset=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    cat("read_data> Excel 97-2003 file detected: ",filenameextension,"\n",sep="")
    wb <- XLConnect::loadWorkbook(i.file)
    datasets<-XLConnect::getSheets(wb)
    n.datasets<-length(datasets)
    if (is.na(i.dataset)){
      datasetread<-NULL
      dataweeks=NULL
    }else if (!(i.dataset %in% datasets)){
      datasetread<-NULL
      dataweeks=NULL
      cat("read_data> Warning: Table ",i.dataset," not found\n")
    }else{
      cat("read_data> Number of datasets: ",n.datasets,"\tReading table: ",i.dataset,"\n",sep="")
      temp1<-as.character(XLConnect::readWorksheet(wb, sheet = i.dataset, header=F, colTypes=XLC$DATA_TYPE.STRING, endRow=1))
      datasetread<-XLConnect::readWorksheet(wb, sheet = i.dataset, rownames=NA, colTypes=XLC$DATA_TYPE.NUMERIC)
      names(datasetread)<-temp1
      if (all(datasetread[,1] %in% 1:53)){
        rownames(datasetread)<-as.character(datasetread[,1])
        datasetread<-datasetread[-1]
        cat("read_data> Note: First column is the week name\n")
      }else rownames(datasetread)<-1:NROW(datasetread)
      dataweeks<-as.numeric(row.names(datasetread))
      cat("read_data> Read ",NROW(datasetread)," rows and ",NCOL(datasetread)," columns\n",sep="")
    }
  }
  list(datasets=datasets, datasetread=datasetread, dataweeks=dataweeks)
}

read.data.access<-function(i.file, i.file.name=NA, i.dataset=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    cat("read_data> Access file detected: ",filenameextension,"\n",sep="")
    if (.Platform$OS.type=="windows"){
      connectstring<-paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",i.file,sep="")
      channel<-odbcDriverConnect(connectstring)
      datasets<-subset(sqlTables(channel),TABLE_TYPE!="SYSTEM TABLE")[,"TABLE_NAME"]
      n.datasets<-length(datasets)
      if (is.na(i.dataset)){
        datasetread<-NULL
        dataweeks=NULL
      }else if (!(i.dataset %in% datasets)) {
        datasetread<-NULL
        dataweeks=NULL
        cat("read_data> Warning: Table ",i.dataset," not found\n")
      }else{
        cat("read_data> Number of datasets: ",n.datasets,"\tReading table: ",i.dataset,"\n",sep="")
        datasetread<-sqlFetch(channel,i.dataset,rownames=T)
        if (all(datasetread[,1] %in% 1:53)){
          rownames(datasetread)<-as.character(datasetread[,1])
          datasetread<-datasetread[-1]
          cat("read_data> Note: First column is the week name\n")
        }else rownames(datasetread)<-1:NROW(datasetread)
        dataweeks<-as.numeric(row.names(datasetread))
        cat("read_data> Read ",NROW(datasetread)," rows and ",NCOL(datasetread)," columns\n",sep="")
      }
      odbcCloseAll()
    }else if (.Platform$OS.type=="unix"){
      # check if mdbtools is installed
      if (system("mdb-tables")==127){
        datasets <- NULL
        datasetread<-NULL
        dataweeks=NULL
        cat("read_data> Error: mdb tools not installed.\nFor debian/ubuntu:\nsudo apt-get install mdbtools mdbtools-gmdb")
      }else{
        # read tables in file
        datasets <- system(paste('mdb-tables -1', shQuote(i.file)), intern=TRUE)
        n.datasets<-length(datasets)
        if (is.na(i.dataset)){
          datasetread<-NULL
          dataweeks=NULL
        }else if (!(i.dataset %in% datasets)) {
          datasetread<-NULL
          dataweeks=NULL
          cat("read_data> Warning: Table ",i.dataset," not found\n")
        }else{
          cat("read_data> Number of datasets: ",n.datasets,"\tReading table: ",i.dataset,"\n",sep="")
          # read selected table schema
          tableschema <- system(paste('mdb-schema -T', shQuote(i.dataset), shQuote(i.file)), intern=TRUE)
          start <- grep('^ \\($', tableschema) + 1
          end   <- grep('^\\);$', tableschema) - 1
          tableschema <- tableschema[start:end]
          tableschema <- strsplit(tableschema, '\t')
          vnames <- sapply(tableschema, function(x)x[2])
          vnames <- substring(vnames, 2,nchar(vnames)-1)
          filecsv <- tempfile()
          system(paste('mdb-export -b strip', shQuote(i.file), shQuote(i.dataset), '>', filecsv))
          # detect encoding
          encodings<-readr::guess_encoding(filecsv, n_max = -1)
          encodings<-encodings[order(encodings$confidence,decreasing = T),]
          myencoding<-as.character(encodings$encoding[1])
          # detect separator and decimal separator
          firstline<-readLines(filecsv,1,encoding=myencoding)
          separators<-c(',',';','\t','\\|')
          mysep<-separators[which.max(stringr::str_count(firstline, separators))]
          restlines<-paste(readLines(filecsv,encoding=myencoding)[-1],collapse="")
          decimals<-c(".",",")
          mydec<-decimals[which.max(stringr::str_count(gsub(mysep,"",restlines,fixed=T), stringr::fixed(decimals)))]
          datasetread<-read.delim(filecsv,header=T,sep=mysep,dec=mydec,row.names=NULL,fill=T,colClasses="numeric", as.is=T, encoding = myencoding)
          names(datasetread)<-vnames
          if (all(datasetread[,1] %in% 1:53)){
            rownames(datasetread)<-as.character(datasetread[,1])
            datasetread<-datasetread[-1]
            cat("read_data> Note: First column is the week name\n")
          }else rownames(datasetread)<-1:NROW(datasetread)
          dataweeks<-as.numeric(row.names(datasetread))
          cat("read_data> Read ",NROW(datasetread)," rows and ",NCOL(datasetread)," columns\n",sep="")
        }
      }
    }else{
      datasets=NULL
      datasetread<-NULL
      dataweeks=NULL
      cat("read_data> Warning: Access file only supported in windows and *nix systems\n")
    }
  }
  list(datasets=datasets, datasetread=datasetread, dataweeks=dataweeks)
}

read.data.text<-function(i.file, i.file.name=NA, i.dataset=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    datasets<-filename
    n.datasets<-length(datasets)
    # text files
    # detect encoding
    temp1<-readr::guess_encoding(i.file, n_max = -1)
    temp1<-temp1[order(temp1$confidence,decreasing = T),]
    myencoding<-as.character(temp1$encoding[1])
    cat("read_data> Text file detected: ",filenameextension," (encoding: ",myencoding,")\n",sep="")
    if (is.na(i.dataset)){
      datasetread<-NULL
      dataweeks=NULL
    }else if (!(i.dataset %in% datasets)) {
      datasetread<-NULL
      dataweeks=NULL
      cat("read_data> Warning: Table ",i.dataset," not found\n");
    }else{
      cat("read_data> Number of datasets: ",n.datasets,"\tReading dataset: ",i.dataset,"\n",sep="")
      # detect separator and decimal separator
      firstline<-readLines(i.file,1,encoding=myencoding)
      separators<-c(',',';','\t','\\|')
      mysep<-separators[which.max(str_count(firstline, separators))]
      restlines<-paste(readLines(i.file,encoding=myencoding)[-1],collapse="")
      decimals<-c(".",",")
      mydec<-decimals[which.max(str_count(gsub(mysep,"",restlines,fixed=T), fixed(decimals)))]
      cat("read_data> Separator is ",mysep,"\tDecimal point is ",mydec,"\n",sep="")
      temp1<-as.character(read.delim(i.file,header=F,sep=mysep,nrows=1,colClasses="character", as.is=T, encoding = myencoding))
      datasetread<-read.delim(i.file,header=T,sep=mysep,dec=mydec,row.names=NULL,fill=T,colClasses="numeric", as.is=T, encoding = myencoding)
      names(datasetread)<-temp1
      if (all(datasetread[,1] %in% 1:53)){
        rownames(datasetread)<-as.character(datasetread[,1])
        datasetread<-datasetread[-1]
        cat("read_data> Note: First column is the week name\n")
      }else rownames(datasetread)<-1:NROW(datasetread)
      dataweeks<-as.numeric(row.names(datasetread))
      cat("read_data> Read ",NROW(datasetread)," rows and ",NCOL(datasetread)," columns\n",sep="")
    }
  }
  list(datasets=datasets, datasetread=datasetread, dataweeks=dataweeks)
}

read.data.rds<-function(i.file, i.file.name=NA, i.dataset=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    datasets<-filename
    n.datasets<-length(datasets)
    # rds files
    cat("read_data> R file detected: ",filenameextension,")\n",sep="")
    if (is.na(i.dataset)){
      datasetread<-NULL
      dataweeks=NULL
    }else if (!(i.dataset %in% datasets)) {
      datasetread<-NULL
      dataweeks=NULL
      cat("read_data> Warning: Table ",i.dataset," not found\n");
    }else{
      cat("read_data> Number of datasets: ",n.datasets,"\tReading dataset: ",i.dataset,"\n",sep="")
      # detect separator and decimal separator
      datasetread<-readRDS(i.file)
      dataweeks<-as.numeric(row.names(datasetread))
      #if (is.null(rownames(datasetread))) rownames(datasetread)<-1:NROW(datasetread)
      cat("read_data> Read ",NROW(datasetread)," rows and ",NCOL(datasetread)," columns\n",sep="")
    }
  }
  list(datasets=datasets, datasetread=datasetread, dataweeks=dataweeks)
}

# Function to select the seasons to use MEM using From, To, Exclude, Use pandemic and Maximum number of seasons fields

select.columns<-function(i.names, i.from, i.to, i.exclude="", i.include="", i.pandemic=T, i.seasons=NA){
  indexes<-1:length(i.names)
  toinclude<-indexes[i.names %in% i.include]
  if (!(i.from=="" | is.na(i.from) | is.null(i.from)) & (i.from %in% i.names)) from<-grep(i.from,i.names,fixed=T) else from<-1
  if (!(i.to=="" | is.na(i.to) | is.null(i.to)) & (i.to %in% i.names)) to<-grep(i.to,i.names,fixed=T) else to<-length(i.names)
  if (to<from) to<-from
  if (length(i.names)>1){
    seasons<-data.frame(i.names,matrix(str_match(i.names,"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?"),nrow=length(i.names),byrow=F)[,-1],stringsAsFactors = F)
  }else{
    seasons<-data.frame(t(c(i.names,str_match(i.names,"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[-1])),stringsAsFactors = F)
  }
  names(seasons)<-c("season.original","anioi","aniof","aniow")
  seasons[is.na(seasons)]<-""
  seasons$aniof[seasons$aniof==""]<-seasons$anioi[seasons$aniof==""]
  seasonsname<-seasons$anioi
  seasonsname[seasons$aniof!=""]<-paste(seasonsname[seasons$aniof!=""],seasons$aniof[seasons$aniof!=""],sep="/")
  seasonsname[seasons$aniow!=""]<-paste(seasonsname[seasons$aniow!=""],"(",seasons$aniow[seasons$aniow!=""],")",sep="")
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
  i.plotly$x$layout$xaxis$ticktext<-apply(divideit,1,paste,collapse="<br />")
  # Fix labels names
  sequ<-1:nlists-nlabels*(floor((1:nlists-1)/nlabels))
  for (i in 1:nlists) i.plotly$x$data[[i]]$name<-i.labels[sequ[i]]
  # Fix text to showup
  for (i in 1:nlists){
    if (length(grep(i.yname,i.plotly$x$data[[i]]$text))>0){
      #i.plotly$x$data[[i]]$text
      dividetext<-matrix(unlist(strsplit(i.plotly$x$data[[i]]$text,"<br>|<br />")),nrow=length(i.plotly$x$data[[i]]$text), byrow=T)
      i.plotly$x$data[[i]]$text<-paste("Week: ",i.weeklabels,"<br />",sub(i.yname,i.labels[sequ[i]],dividetext[,2]),sep="")
    }
  }
  # For those with points and labels, i modify the mode and add the marker
  pandl<-i.points & i.lines
  index.pandl<-(1:nlabels)[pandl]
  if (length(index.pandl)>0){
    for (i in 1:length(index.pandl)){
      i.plotly$x$data[[index.pandl[i]]]$mode<-"lines+markers"
      i.plotly$x$data[[index.pandl[i]]]$marker<-i.plotly$x$data[[index.pandl[i]+nlabels]]$marker
    }
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

