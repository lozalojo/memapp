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
    dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    ggplot(dgraf, aes(weekno, rate)) + geom_point() + geom_line() +
      scale_x_continuous(breaks=inputdata$weekno, labels = inputdata$weekna)+
      labs(title = s, x = "week", y = "rate")
  })})
  
  lapply(namescol, function(s){output[[paste0("plot_",as.character(s),"_last")]] <- renderPlot({
    dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    ggplot(dgraf, aes(weekno, rate)) + geom_point() + geom_line() +
      scale_x_continuous(breaks=inputdata$weekno, labels = inputdata$weekna)+
      labs(title = s, x = "week", y = "rate") 
  })})
  
  lapply(namescol, function(s){output[[paste0("results_",as.character(s),"_first")]] <- renderTable({
    dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    sname<-paste0("plot_",as.character(s),"_first_click")
    if (is.null(input[[sname]])) out1<-data.frame(var1=NA,weekno=NA,weekna=NA,rate=NA) else out1<-data.frame(var1=s,nearPoints(dgraf, input[[sname]], addDist = F, threshold=1000, maxpoints=1),stringsAsFactors=F)
    out1
  })})
  
  lapply(namescol, function(s){output[[paste0("results_",as.character(s),"_last")]] <- renderTable({
    dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
    sname<-paste0("plot_",as.character(s),"_last_click")
    if (is.null(input[[sname]])) out1<-data.frame(var1=NA,weekno=NA,weekna=NA,rate=NA) else out1<-data.frame(var1=s,nearPoints(dgraf, input[[sname]], addDist = F, threshold=1000, maxpoints=1),stringsAsFactors=F)
    out1
  })})

  output$results_final <- renderTable({
    out1<-data.frame()
    for (s in namescol){
      dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
      sname<-paste0("plot_",as.character(s),"_first_click")
      if (is.null(input[[sname]])) out1x<-data.frame(var1=NA,weekno=NA,weekna=NA,rate=NA) else out1x<-data.frame(var1=s,nearPoints(dgraf, input[[sname]], addDist = F, threshold=1000, maxpoints=1),stringsAsFactors=F)
      out1<-rbind(out1,out1x)
    }
    out2<-data.frame()
    for (s in namescol){
      dgraf<-data.frame(weekno=inputdata$weekno, weekna=inputdata$weekna, rate=inputdata[[s]])
      sname<-paste0("plot_",as.character(s),"_last_click")
      if (is.null(input[[sname]])) out2x<-data.frame(var1=NA,weekno=NA,weekna=NA,rate=NA) else out2x<-data.frame(var1=s,nearPoints(dgraf, input[[sname]], addDist = F, threshold=1000, maxpoints=1),stringsAsFactors=F)
      out2<-rbind(out2,out2x)
    }
    subset(merge(out1, out2, by="var1", all=T, suffixes = c("_first","_last")),!is.na(var1))
  })  
}

shinyApp(ui, server)
