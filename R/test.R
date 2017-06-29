library(ggplot2)
library(shiny)

inputdata<-data.frame(weekno=1:20, weekna=letters[1:20])
inputdata$normal<-dnorm(inputdata$weekno,10)
inputdata$beta<-dbeta(inputdata$weekno, 1, 1)
inputdata$gamma<-dgamma(inputdata$weekno, 1, 1)
inputdata$logistic<-dlogis(inputdata$weekno,10)
inputdata$poisson<-dpois(inputdata$weekno, 2)
namescol<-names(inputdata)[-(1:2)]
nnamescol<-length(namescol)
for (s in namescol) eval(parse(text=paste0("inputdata$'",s,"_color'<-'1'")))

makeReactiveBinding('inputdata')

ui <- fluidPage(

  fluidRow(  
    do.call(column, c(width=6,
            c(          
            lapply(namescol,function(s){
              call("plotOutput", outputId=paste0("plot_",as.character(s),"_first"), 
                   height = 300, click = paste0("plot_",as.character(s),"_first_click"))
            }),
            lapply(namescol,function(s){
            call("tableOutput", outputId=paste0("results_",as.character(s),"_first"))
            })
            )[c(rbind(1:nnamescol,1:nnamescol+nnamescol))]
            
            )),
    do.call(column, c(width=6,
            c(
            lapply(namescol,function(s){
              call("plotOutput", outputId=paste0("plot_",as.character(s),"_last"), 
                   height = 300, click = paste0("plot_",as.character(s),"_last_click"))
            }),
            lapply(namescol,function(s){
              call("tableOutput", outputId=paste0("results_",as.character(s),"_last"))
            })
            )[c(rbind(1:nnamescol,1:nnamescol+nnamescol))]
            
            ))
  ),
  fluidRow(
    column(width = 12,
           tableOutput("results_final")
    )
  )
)

server <- function(input, output) {

  
  lapply(namescol, function(s){output[[paste0("plot_",as.character(s),"_first")]] <- renderPlot({
    # dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    ggplot(inputdata, aes_string("weekno", s, color=paste0(s,"_color"))) + geom_point() + geom_line() +
      scale_x_continuous(breaks=inputdata$weekno, labels = inputdata$weekna)+
      labs(title = s, x = "week", y = "rate")
  })})
  
  lapply(namescol, function(s){output[[paste0("plot_",as.character(s),"_last")]] <- renderPlot({
    # dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    ggplot(inputdata, aes_string("weekno", s, color=paste0(s,"_color"))) + geom_point() + geom_line() +
      scale_x_continuous(breaks=inputdata$weekno, labels = inputdata$weekna)+
      labs(title = s, x = "week", y = "rate") 
  })})
  
  lapply(namescol, function(s){output[[paste0("results_",as.character(s),"_first")]] <- renderTable({
    # dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    sname<-paste0("plot_",as.character(s),"_first_click")
    if (is.null(input[[sname]])){
      out1<-as.data.frame(t(rep(NA,4)),stringsAsFactors=F)
    }else{
      out1<-data.frame(var1=s,nearPoints(inputdata, input[[sname]], addDist = F, threshold=1000, maxpoints=1)[c("weekno","weekna",s)],stringsAsFactors=F)
    }
    names(out1)<-c("var1","weekno","weekna",s)
    out1
  })})
  
  lapply(namescol, function(s){output[[paste0("results_",as.character(s),"_last")]] <- renderTable({
    # dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    sname<-paste0("plot_",as.character(s),"_last_click")
    if (is.null(input[[sname]])){
      out1<-as.data.frame(t(rep(NA,4)),stringsAsFactors=F)
    }else{
      out1<-data.frame(var1=s,nearPoints(inputdata, input[[sname]], addDist = F, threshold=1000, maxpoints=1)[c("weekno","weekna",s)],stringsAsFactors=F)
    }
    names(out1)<-c("var1","weekno","weekna",s)
    out1
  })})

  output$results_final <- renderTable({
    out1<-data.frame()
    for (s in namescol){
      # dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
      sname<-paste0("plot_",as.character(s),"_first_click")
      if (is.null(input[[sname]])){
        out1x<-as.data.frame(t(rep(NA,4)),stringsAsFactors=F) 
      }else{
        
        out1x<-data.frame(var1=s,nearPoints(inputdata, input[[sname]], addDist = F, threshold=1000, maxpoints=1)[c("weekno","weekna",s)],stringsAsFactors=F)
      }
      names(out1x)<-c("var1","weekno_first","weekna_first","value_first")
      out1<-rbind(out1,out1x)
    }
    out2<-data.frame()
    for (s in namescol){
      # dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
      sname<-paste0("plot_",as.character(s),"_last_click")
      if (is.null(input[[sname]])){
        out2x<-as.data.frame(t(rep(NA,4)),stringsAsFactors=F)
      }else{
        out2x<-data.frame(var1=s,nearPoints(inputdata, input[[sname]], addDist = F, threshold=1000, maxpoints=1)[c("weekno","weekna",s)],stringsAsFactors=F)
      }
      names(out2x)<-c("var1","weekno_last","weekna_last","value_last")
      out2<-rbind(out2,out2x)
    }
    out3<-data.frame(var1=namescol)
    outf<-merge(out3,out1,all.x=T, by="var1")
    outf<-merge(outf,out2,all.x=T, by="var1")
    outf
  })
  
  # observeEvent(input$plot_click, {
  #   np <- nearPoints(data, input$plot_click, maxpoints=1 , threshold = 15)
  #   data$color <<- rep("1",length(data$x))
  #   data$color[data$values==np$values] <<- "2"
  # })
  
  lapply(namescol, function(s){observeEvent(input[[paste0("plot_",as.character(s),"_first_click")]], {
    sname<-paste0("plot_",as.character(s),"_first_click")
    if (!is.null(input[[sname]])){
      np <- nearPoints(inputdata, input[[sname]], addDist = F, threshold=1000, maxpoints=1)
      # inputdata[paste0(s,"_color")] <- rep("1",NROW(inputdata))
      inputdata[inputdata$weekno==np$weekno, paste0(s,"_color")] <- "2"
      print(inputdata)
      }
  })}) 
  lapply(namescol, function(s){observeEvent(input[[paste0("plot_",as.character(s),"_last_click")]], {
    sname<-paste0("plot_",as.character(s),"_last_click")
    if (!is.null(input[[sname]])){
      np <- nearPoints(inputdata, input[[sname]], addDist = F, threshold=1000, maxpoints=1)
      # inputdata[paste0(s,"_color")] <- rep("1",NROW(inputdata))
      inputdata[inputdata$weekno==np$weekno, paste0(s,"_color")] <- "3"
      print(inputdata)
    }
  })}) 
  
}

shinyApp(ui, server)
