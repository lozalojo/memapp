library(shiny)

shinyServer(function(input, output, session) {
  
dat_funk <- reactive({
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
  dat2
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
  updateSelectInput(session, "SelectFrom", choices = names(dt)[2:ncol(dt)])
  updateSelectInput(session, "SelectTo", choices = names(dt)[2:ncol(dt)])
  updateSelectInput(session, "SelectExclude", choices = names(dt)[2:ncol(dt)])
  updateSelectInput(session, "SelectSurveillance", choices = names(dt)[2:ncol(dt)])
  updateSelectInput(session, "SelectSurveillanceWeek", choices = rownames(dt))
  updateSelectInput(session, "SelectSeasons", choices = names(dt)[2:ncol(dt)])
})


# Define main output structure
output$tb <- renderUI({
  if(is.null(dat_funk())){return()}
  else
    tabsetPanel(tabPanel("File name", tableOutput("filedf")),
                tabPanel("Data", tableOutput("table")),
                tabPanel("Plot", plotlyOutput("distPlot")),
                tabPanel("MEM", verbatimTextOutput("memdf")),
                tabPanel("Timing",plotOutput("distPlot2")),
                tabPanel("Series",plotOutput("distSeries")),
                tabPanel("Surveillance",plotOutput("distSurveillance")),
                tabPanel("Animated",imageOutput("distAnimated")),
                tabPanel("Goodness",tableOutput("tableGoodness")),
                tabPanel("Optimize",tableOutput("tableOptimize"))
                )
})

output$filedf <- renderTable({
  if(is.null(dat_funk())){return()}
  input$file[1]
}) 

output$table <- renderTable({
  datfile <- dat_funk()
  cat(rownames(datfile),"\n")
  if(is.null(datfile)){return()}
  # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
  #cat("---->",paste(c(input$K,input$K3,input$K4),collapse="*",sep=""),"<----\n",length(c(input$K,input$K3,input$K4)),"\n")
  toinclude<-input$SelectSurveillance
  if (!is.null(input$SelectSeasons)) toinclude<-c(toinclude,input$SelectSeasons)
  # print(is.character(input$K))
  # print(is.character(input$K3))
  # print(is.character(input$K4))
  # print(is.character(toinclude))
  # print(length(toinclude))
  # cat("-",toinclude[1],"-",toinclude[2],"-","\n",sep="")
  selectedcolumns<-select.columns(i.names=names(datfile), 
                 i.from=input$SelectFrom, 
                 i.to=input$SelectTo, 
                 i.exclude=input$SelectExclude,
                 i.include=toinclude,
                 i.pandemic=as.logical(input$SelectPandemic),
                 i.seasons=input$SelectMaximum)
  cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  cat("i.from:->",input$SelectFrom,"<-\n",sep="")
  cat("i.to:->",input$SelectTo,"<-\n",sep="")
  cat("i.exclude:->",input$SelectExclude,"<-\n",sep="")
  cat("i.include:->",as.character(c(input$K,input$K4)),"<-\n",sep="")
  cat("i.pandemic:->",as.logical(input$SelectPandemic),"<-\n",sep="")
  cat("i.seasons:->",input$SelectSeasons,"<-\n",sep="")
  cat("Seleccion:->",selectedcolumns,"<-\n",sep="")
  if (length(selectedcolumns)>0) datatoshow<-datfile[selectedcolumns] else datatoshow<-data.frame(Message="No data selected",row.names = NULL)
}, rownames = T, digits = 2)  

output$distPlot <- renderPlotly({
  datfile <- dat_funk()
  p <- plotInput()
  z <- plotly_build(p)
  for(j in 1:length(z$x$data)){
    z$x$data[[j]]$text <- print(paste(z$x$data[[j]]$name,"Y:", round(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka))}
  z
})

output$memdf <- renderPrint({
  datfile <- dat_funk()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), 
                                  i.from=input$SelectFrom, 
                                  i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude,
                                  i.include="",
                                  i.pandemic=as.logical(input$SelectPandemic),
                                  i.seasons=input$SelectMaximum)
  
  if (length(selectedcolumns)>1){
  datfile.model<-datfile[selectedcolumns]
    
    # if(grep(input$K, colnames(dat_funk()))>1){
    #   if(grep(input$K2, colnames(dat_funk())) < grep(input$K, colnames(dat_funk()))-1){
    nam.t <- memmodel(datfile.model,
        # nam.t <- memmodel(dat_funk()[,c(grep(input$K2, 
        #                                           colnames(dat_funk())):(grep(input$K, colnames(dat_funk()))-1))],
                               i.type.threshold=as.numeric(input$i.type.threshold),
                               i.type.intensity=as.numeric(input$i.type.intensity),
                               i.method = as.numeric(input$i.method),
                               i.param = as.numeric(input$memparameter), i.seasons = NA)
        nam.ttt <- rbind(c("Epidemic threshold:","           Pre Post"),
                         c("",paste0("Threshold ", 
                                     round(nam.t$"pre.post.intervals"[1,3],2)," ", 
                                     round(nam.t$"pre.post.intervals"[2,3],2))),
                         c("", ""),
                         c("Intensity thresholds:",""),
                         c("                  Threshold", ""),
                         c(paste0("Medium (40%)          ", round(nam.t$"epi.intervals"[1,4],2)), ""),
                         c(paste0("High (90%)            ", round(nam.t$"epi.intervals"[2,4],2)), ""),
                         c(paste0("Very high (97.5%)     ", round(nam.t$"epi.intervals"[3,4],2)), ""))
        
        nam.ttt <- format(nam.ttt, justify = "left")
        nam.ttt <- as.data.frame(nam.ttt)
        names(nam.ttt) <- NULL 
        #print(noquote(nam.ttt), row.names = FALSE)
        summary(nam.t)
      # }else{war.text <- as.data.frame("MEM needs at least two seasons.")
      #         names(war.text) <- NULL 
      #           print(noquote(war.text), row.names = FALSE)}
    }else{war.text <- as.data.frame("MEM needs at least two seasons.")
            names(war.text) <- NULL
              print(noquote(war.text), row.names = FALSE)}
  })

output$distPlot2 <- renderPlot({
  plotInput2()
})

output$distSeries <- renderPlot({
  plotSeries()
})

output$distSurveillance <- renderPlot({
  plotSurveillance()
})

output$distAnimated <- renderImage({
  # datfile <- dat_funk()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  datfile <- dat_funk()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", i.pandemic=as.logical(input$SelectPandemic), i.seasons=input$SelectMaximum)
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
   sura<-memsurveillance.animated(datfile[input$SelectSurveillance], e.thr, i.thr, i.remove = T,
                           i.animated.graph.file.name = "animated", i.output = tempdir(), i.pos.epidemic = T, i.range.x=range.x)
  
  imgfile<-sura$graph.name
  cat(imgfile,"\n")
  
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


output$tableGoodness<-renderTable({
  # datfile <- dat_funk()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  # datacolumns<-c(grep(input$K2,colnames(datfile)):(grep(input$K, colnames(datfile))-1))
  
  datfile <- dat_funk()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", i.pandemic=as.logical(input$SelectPandemic), i.seasons=input$SelectMaximum)
  if (length(selectedcolumns)>2){
  good<-memgoodness(datfile[,selectedcolumns],
                    i.type.threshold=as.numeric(input$i.type.threshold),
                    i.type.intensity=as.numeric(input$i.type.intensity), 
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter),i.graph=F, i.seasons=NA, i.min.seasons = length(selectedcolumns))
  good.table<-as.data.frame(good$validity.data)
  good.table$Total<-good$results    
  }else{
    good.table<-data.frame(Error="Number of columns must be greater than 2")  
  }
  good.table
}, rownames = T, digits = 2)

output$tableOptimize<-renderTable({
  # datfile <- dat_funk()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  # datacolumns<-c(grep(input$K2,colnames(datfile)):(grep(input$K, colnames(datfile))-1))
  # 
  # if(length(datacolumns)>2){
    datfile <- dat_funk()
    if(is.null(datfile)){return()}
    selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                    i.exclude=input$SelectExclude, i.include="", i.pandemic=as.logical(input$SelectPandemic), i.seasons=input$SelectMaximum)
    if (length(selectedcolumns)>2){
      
    
    
    roca<-roc.analysis(datfile[,selectedcolumns],
                       i.param.values = seq(2, 3, 0.1), 
                       i.graph.file = F,
                      i.type.threshold=as.numeric(input$i.type.threshold),
                      i.type.intensity=as.numeric(input$i.type.intensity), 
                      i.seasons=NA, 
                      i.min.seasons = length(selectedcolumns))
    roca.table<-as.data.frame(t(roca$optimum))
    names(roca.table)<-"Optimum"    
  }else{
    roca.table<-data.frame(Error="Number of columns must be greater than 2")  
  }
  roca.table
}, rownames = T, digits = 2)


plotInput <-function(){
  datfile <- dat_funk()
  dat3 <- datfile
  datafil <- dat3
  #########Graf med observerade data
  
  if((grep(input$K,colnames(datfile))-grep(input$K2,colnames(datfile)))<2 & is.null(input$K3)|(input$mem_knapp=="FALSE" & is.null(input$K3) & input$mem_intensitet=="FALSE"))
  {
    g.plot <- 
      ggplot(dat3) +
        geom_line(aes(x=as.numeric(rownames(dat3)), y=as.numeric(dat3[,input$K]), group=1, color=input$K)) +
        ggtitle(input$textMain) +
        theme(plot.title = element_text(hjust = 0.1, size=22))+
        labs(x=input$textX, y=input$textY, color='Season') +
        scale_y_continuous(limits = c(0, max(dat3[,2:(ncol(dat3)-1)]*1.4, na.rm=T)))+
        scale_x_continuous(breaks=seq(1,52,3),
                           labels=c(as.character(dat3$vecka[seq(1,52,3)])))+
        scale_color_manual(values="black", labels=input$K)+
        ggthemes::theme_few()

print(ggplotly(g.plot, tooltip = "text"))
    #start
  }else if(input$mem_knapp=="TRUE" & 
           (grep(input$K,colnames(datfile))-grep(input$K2,colnames(datfile)))>1 & is.null(input$K3) & input$mem_intensitet=="FALSE"){
    epi <- memmodel(datfile[,c(grep(input$K2, 
                                  colnames(datfile)):(grep(input$K, 
                                                           colnames(datfile))-1))], 
                  i.type.threshold=as.numeric(input$i.type.threshold),
                  i.type.intensity=as.numeric(input$i.type.intensity), 
                  i.method = as.numeric(input$i.method),
                  i.param = as.numeric(input$memparameter), i.seasons = NA)
    print(ggplotly(
      ggplot(dat3) +
        geom_line(aes(x=as.numeric(rownames(dat3)), y=as.numeric(dat3[,input$K]), group=1, color=input$K)) +
        geom_hline(aes(yintercept=epi$pre.post.intervals[1,3]), color = input$colMEMstart) +
        geom_hline(aes(yintercept=epi$pre.post.intervals[2,3]), color = input$colMEMstop) +
        ggtitle(input$textMain) +
        labs(x=input$textX,y=input$textY, color='Season') +
        scale_y_continuous(limits = c(0, max(dat3[,2:(ncol(dat3)-1)]*1.4, na.rm=T)))+
        scale_x_continuous(breaks=seq(1,52,3),
                           labels=c(as.character(dat3$vecka[seq(1,52,3)])))+
        scale_color_manual(values="black", labels=input$K)+
        ggthemes::theme_few(), tooltip = "text"))
  }else if(input$mem_knapp=="FALSE" & 
           (grep(input$K,colnames(datfile))-grep(input$K2,colnames(datfile)))>1 & is.null(input$K3) & input$mem_intensitet=="TRUE"){
    epi <- memmodel(datfile[,c(grep(input$K2, 
                                  colnames(datfile)):(grep(input$K, 
                                                           colnames(datfile))-1))], 
                  i.type.threshold=as.numeric(input$i.type.threshold),
                  i.type.intensity=as.numeric(input$i.type.intensity), 
                  i.method = as.numeric(input$i.method),
                  i.param = as.numeric(input$memparameter), i.seasons = NA)
    col.pal <- colorRampPalette(brewer.pal(3,input$colpal))(3)
    print(ggplotly(
      ggplot(dat3) +
        annotate("rect", xmin = 1, xmax = nrow(dat3), ymin = 0, ymax = epi$"epi.intervals"[1,4], 
                 fill = col.pal[1], alpha=as.numeric(input$colpalTran))+
        annotate("rect", xmin = 1, xmax = nrow(dat3), ymin = epi$"epi.intervals"[1,4], ymax = epi$"epi.intervals"[2,4],
                 fill = col.pal[2], alpha=as.numeric(input$colpalTran))+
      annotate("rect", xmin = 1, xmax = nrow(dat3), ymin = epi$"epi.intervals"[2,4], ymax = epi$"epi.intervals"[3,4],
               fill = col.pal[3], alpha=as.numeric(input$colpalTran))+
        ggtitle(input$textMain) +
        labs(x=input$textX,y=input$textY, color='Season') +
        geom_line(aes(x=as.numeric(rownames(dat3)), y=as.numeric(dat3[,input$K]), group=1, color=input$K)) +
        scale_y_continuous(limits = c(0, max(dat3[,2:(ncol(dat3)-1)]*1.4, na.rm=T)))+
        scale_color_manual(values="black", labels=input$K)+
        scale_x_continuous(breaks=seq(1,52,3),
                           labels=c(as.character(dat3$vecka[seq(1,52,3)])))+
        ggtitle(input$textMain) +
        ggthemes::theme_few(), tooltip = "text"))
  }else if(input$mem_knapp=="TRUE" & 
           (grep(input$K,colnames(datfile))-grep(input$K2,colnames(datfile)))>1 & is.null(input$K3) & input$mem_intensitet=="TRUE"){
    epi <- memmodel(datfile[,c(grep(input$K2, 
                                  colnames(datfile)):(grep(input$K, 
                                                           colnames(datfile))-1))], 
                  i.type.threshold=as.numeric(input$i.type.threshold),
                  i.type.intensity=as.numeric(input$i.type.intensity), 
                  i.method = as.numeric(input$i.method),
                  i.param = as.numeric(input$memparameter), i.seasons = NA)
    col.pal <- colorRampPalette(brewer.pal(3,input$colpal))(3)
    print(ggplotly(
      ggplot(dat3) +
        annotate("rect", xmin = 1, xmax = nrow(dat3), ymin = 0, ymax = epi$"epi.intervals"[1,4], 
                 fill = col.pal[1], alpha=as.numeric(input$colpalTran))+
        annotate("rect", xmin = 1, xmax = nrow(dat3), ymin = epi$"epi.intervals"[1,4], ymax = epi$"epi.intervals"[2,4],
                 fill = col.pal[2],alpha=as.numeric(input$colpalTran))+
        annotate("rect", xmin = 1, xmax = nrow(dat3), ymin = epi$"epi.intervals"[2,4], ymax = epi$"epi.intervals"[3,4],
                 fill = col.pal[3],alpha=as.numeric(input$colpalTran))+
        geom_hline(aes(yintercept=epi$pre.post.intervals[1,3]), color = input$colMEMstart) +
        geom_hline(aes(yintercept=epi$pre.post.intervals[2,3]), color = input$colMEMstop) +
        ggtitle(input$textMain) +
        labs(x=input$textX,y=input$textY, color='Season') +
        geom_line(aes(x=as.numeric(rownames(dat3)), y=as.numeric(dat3[,input$K]), group=1, color=input$K))+
        scale_x_continuous(breaks=seq(1,52,3),
                           labels=c(as.character(dat3$vecka[seq(1,52,3)])))+
        ggtitle(input$textMain) +
        scale_y_continuous(limits = c(0, max(dat3[,2:(ncol(dat3)-1)]*1.4, na.rm=T)))+
        scale_color_manual(values="blue", labels=round(epi$"epi.intervals"[1,4]))+
        scale_color_manual(values="black", labels=input$K)+
        ggthemes::theme_few()))

    
  }else{
    lis <- list()
    for(i in input$K3){
      dat3$tid <- rownames(dat3)
      selectedxaxis = colnames(dat3)[ncol(dat3)]
      selectedcolumns = colnames(dat3)[which(colnames(dat3)==i)]
      widedata = subset(dat3, select = c(selectedxaxis, selectedcolumns))
      
      longdata = melt(widedata, id.vars=selectedxaxis, variable.name='Cases', value.name='Count')
      lis <- rbind(lis, longdata)
      
      p <- ggplotly(ggplot(lis) + geom_line(aes(x=as.numeric(tid), y=Count, group=Cases, color=Cases, linetype = Cases))+ 
                      scale_x_continuous(breaks=seq(1,52,3),
                                         labels=c(as.character(dat3$vecka[seq(1,52,3)])))+
                      ggtitle(input$textMain) +
                      labs(x=input$textX,y=input$textY, color='Season') +
                      ggthemes::theme_few())
      
    }
    print(p)
  }
  
}

plotInput2 <-function(){
  datfile <- dat_funk()
  DLM <- function(kol){
    dat3 <- datfile
    datafil <- dat3
    #for(i in input$K:input$K2){
    epi.plot <- memtiming(dat3[kol],
                          i.method = as.numeric(input$i.method),
                          i.param = as.numeric(input$memparameter))
    plot(epi.plot)
  }
  DLM(input$SelectSurveillance)
}

plotSeries <-function(){
  datfile <- dat_funk()
  if(is.null(datfile)){return()}
  toinclude<-input$SelectSurveillance
  if (!is.null(input$SelectSeasons)) toinclude<-c(toinclude,input$SelectSeasons)
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include=toinclude, i.pandemic=as.logical(input$SelectPandemic), i.seasons=input$SelectMaximum)
  datfile<-datfile[selectedcolumns]
  range.x<- as.numeric(rownames(datfile)[c(1,NROW(datfile))])
  if (length(selectedcolumns)>1) full.series.graph(datfile,
                    i.method = as.numeric(input$i.method),
                    i.param = as.numeric(input$memparameter),i.graph.file = F, i.plot.timing = T, i.plot.intensity = T,
                    i.range.x=range.x)
}

plotSurveillance <-function(){
  # datfile <- dat_funk()
  # rownames(datfile)<-datfile$vecka
  # datfile$vecka<-NULL
  
  datfile <- dat_funk()
  if(is.null(datfile)){return()}
  selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo, 
                                  i.exclude=input$SelectExclude, i.include="", i.pandemic=as.logical(input$SelectPandemic), i.seasons=input$SelectMaximum)
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
  memsurveillance(datfile[input$SelectSurveillance], 
                  e.thr, i.thr, i.graph.file=F, i.pos.epidemic = T, i.range.x =range.x, i.week.report = input$SelectSurveillanceWeek)
  }
}

session$onSessionEnded(function() {
  stopApp()
})
})

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

select.columns<-function(i.names, i.from, i.to, i.exclude="", i.include="", i.pandemic=T,i.seasons=NA){
  indexes<-1:length(i.names)
  toinclude<-indexes[i.names %in% i.include]
  if (!(i.from=="" | is.na(i.from) | is.null(i.from)) & (i.from %in% i.names)) from<-grep(i.from,i.names,fixed=T) else from<-1  
  if (!(i.to=="" | is.na(i.to) | is.null(i.to)) & (i.to %in% i.names)) to<-grep(i.to,i.names,fixed=T) else to<-length(i.names)
  if (to<from) to<-from
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
  return(indexes)
}

