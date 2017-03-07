

shinyServer(function(input, output, session) {
dat_funk <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()}
  else
  dat <- as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  dat <- as.data.frame(dat)
  dat$num <- 1:nrow(dat)
  dat <- apply(dat, 2, function(x) as.numeric(x))
  dat <- as.data.frame(dat)
})

observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile))
    return(NULL)
  dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K", choices = names(dt)[2:ncol(dt)])
})

observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile))
    return(NULL)
  dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K2", choices = names(dt)[2:ncol(dt)])
})
observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile))
    return(NULL)
  dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K3", choices = names(dt)[2:ncol(dt)])
})

observe({
  file1 <- input$file
  inFile<-input$file
  if(is.null(inFile))
    return(NULL)
  dt = as.data.frame(read.csv2(file=file1$datapath, sep=";", header=TRUE))
  ## Decide later what to do with the data, here we just fill
  updateSelectInput(session, "K4", choices = names(dt)[2:ncol(dt)])
})


output$filedf <- renderTable({
  if(is.null(dat_funk())){return()}
  input$file[1]
}) 

output$table <- renderTable({
  if(is.null(dat_funk())){return()}
  dat_funk()[,-c(ncol(dat_funk()))]
})  

output$memdf <- renderPrint({
    if(grep(input$K, colnames(dat_funk()))>1){
      if(grep(input$K2, colnames(dat_funk())) < grep(input$K, colnames(dat_funk()))-1){
      
    
        nam.t <- epimem(dat_funk()[,c(grep(input$K2, 
                                                  colnames(dat_funk())):(grep(input$K, colnames(dat_funk()))-1))],
                               i.type.threshold=as.numeric(input$i.type.threshold), 
                               i.method = as.numeric(input$i.method))
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
        print(noquote(nam.ttt), row.names = FALSE)
      }else{war.text <- as.data.frame("MEM needs at least two seasons.")
              names(war.text) <- NULL 
                print(noquote(war.text), row.names = FALSE)}
    }else{war.text <- as.data.frame("MEM needs at least two seasons.")
            names(war.text) <- NULL
              print(noquote(war.text), row.names = FALSE)}
  })

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
    epi <- epimem(datfile[,c(grep(input$K2, 
                                  colnames(datfile)):(grep(input$K, 
                                                           colnames(datfile))-1))], 
                  i.type.threshold=as.numeric(input$i.type.threshold), 
                  i.method = as.numeric(input$i.method))
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
    epi <- epimem(datfile[,c(grep(input$K2, 
                                  colnames(datfile)):(grep(input$K, 
                                                           colnames(datfile))-1))], 
                  i.type.threshold=as.numeric(input$i.type.threshold), 
                  i.method = as.numeric(input$i.method))
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
    epi <- epimem(datfile[,c(grep(input$K2, 
                                  colnames(datfile)):(grep(input$K, 
                                                           colnames(datfile))-1))], 
                  i.type.threshold=as.numeric(input$i.type.threshold), 
                  i.method = as.numeric(input$i.method))
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
          epi.plot <- epitiming(dat3[,kol],
                                i.method = as.numeric(input$i.method))
          plot(epi.plot)
    }
  DLM(input$K4)
}
  
    

output$distPlot <- renderPlotly({
  datfile <- dat_funk()
   p <- plotInput()
   z <- plotly_build(p)
   for(j in 1:length(z$x$data)){
     z$x$data[[j]]$text <- print(paste(z$x$data[[j]]$name,"Y:", round(z$x$data[[j]]$y,1),"\nWeek:", datfile$vecka))}
   z
   })

# for(i in 1:length(z)){z$x$data[[1]]$text <- print(paste("Y:", round(z$x$data[[1]]$y,1),"\nWeek:", z$x$data[[1]]$x)) z}


output$distPlot2 <- renderPlot({
  plotInput2()
})

output$tb <- renderUI({
  if(is.null(dat_funk())){return()}
  else
    tabsetPanel(tabPanel("File name", tableOutput("filedf")),
                tabPanel("Data", tableOutput("table")),
                tabPanel("Plot", plotlyOutput("distPlot")),
                tabPanel("MEM", verbatimTextOutput("memdf")),
                tabPanel("Epi-timing",plotOutput("distPlot2")))
})


session$onSessionEnded(function() {
  stopApp()
})
})


