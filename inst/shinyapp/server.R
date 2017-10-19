options(warn =-1)
# route messages to output in the server
if (!interactive()) sink(stderr(), type = "output")

source("helpers.R")

set.rzip()
animationmethod<-animation.method()

translation.loc<-c("lang","inst/shinyapp/lang",paste0(.libPaths(),"/memapp/shinyapp/lang"))
translation.dir<-utils::head(translation.loc[dir.exists(translation.loc)],1)
translation.fil<-paste0(translation.dir,"/translation.bin")

load(translation.fil)
cat(paste0("Translation file loaded from: ",tools::file_path_as_absolute(translation.fil)," (",NROW(translation)," items)"),"\n")

shinyServer(function(input, output, session) {
  
  #####################################
  ### GLOBAL VARIABLES TO BE USED IN THE SERVER
  #####################################
  
  values <- reactiveValues(origdata = NULL, plotdata = NULL, clickdata=NULL, idscreated = NULL, 
                           optimizegraphs = NULL)
  
  #####################################
  ### SERVER-SIDE FUNCTIONS
  #####################################
  
  trloc <- function(text){ # translates text into current language
    as.character(sapply(text,function(s){
      o.text<-tail(translation[translation$original==s,input$lang])
      if (NROW(o.text)!=1) o.text<-s
      if (is.na(o.text)) o.text<-s
      o.text
    }, USE.NAMES=FALSE))
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
                          i.colSeasons=NA,
                          i.colThresholds=c("#8c6bb1","#88419d","#810f7c","#4d004b","#c0c0ff"),
                          ...){
    
    if(is.null(i.data)){
      p<-NULL
    }else{
      if (any(is.na(i.colSeasons))) i.colSeasons <- colorRampPalette(RColorBrewer::brewer.pal(max(3,min(8,NCOL(i.data))),"Accent"))(NCOL(i.data))
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
                paste(names(data.full)," (",trloc("missing"),")",sep=""),
                trloc(c("Epidemic thr.","Medium thr.","High thr.","Very high thr.","Post thr.")))
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
        scale_shape_manual(values=shapes.s, name=trloc("Legend"), labels=labels.s) +
        scale_color_manual(values=colors.s, name=trloc("Legend"), labels=labels.s) +
        scale_fill_manual(values=fills.s, name=trloc("Legend"), labels=labels.s) +
        scale_size_manual(values=sizes.s, name=trloc("Legend"), labels=labels.s) +
        scale_linetype_manual(values=linetypes.s, name=trloc("Legend"), labels=labels.s) +
        scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        ggthemes::theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
      p<-list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,
              weeklabels=i.range.x.values$week.lab, gdata=dgrafgg.s)
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
      
      labels<-trloc(c("Weekly data", "Pre-epidemic", "Pre-epidemic (missing)", "Epidemic", "Epidemic (missing)", "Post-epidemic", "Post-epidemic (missing)", "Epidemic thr.", "Medium thr.", "High thr.", "Very high thr.", "Post thr."))
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
        scale_shape_manual(values=shapes.s, name=trloc("Legend"), labels=labels.s) +
        scale_color_manual(values=colors.s, name=trloc("Legend"), labels=labels.s) +
        scale_fill_manual(values=fills.s, name=trloc("Legend"), labels=labels.s) +
        scale_size_manual(values=sizes.s, name=trloc("Legend"), labels=labels.s) +
        scale_linetype_manual(values=linetypes.s, name=trloc("Legend"), labels=labels.s) +
        scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        ggthemes::theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
      p<-list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,
              weeklabels=paste(data.orig$week,"<br />Season: ",data.orig$season,sep=""), gdata=dgrafgg.s)
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
    }else if (!(ncol(i.data)==1)){
      p<-NULL
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
      
      labels<-trloc(c(names(i.data),"Epidemic thr.","Medium thr.","High thr.","Very high thr.","Post thr.","Start","End"))
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
        scale_shape_manual(values=shapes.s, name=trloc("Legend"), labels=labels.s) +
        scale_color_manual(values=colors.s, name=trloc("Legend"), labels=labels.s) +
        scale_fill_manual(values=fills.s, name=trloc("Legend"), labels=labels.s) +
        scale_size_manual(values=sizes.s, name=trloc("Legend"), labels=labels.s) +
        scale_linetype_manual(values=linetypes.s, name=trloc("Legend"), labels=labels.s) +
        scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        ggthemes::theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
      p<-list(plot=gplot,labels=labels.s,haspoints=haspoints.s,haslines=haslines.s,
              weeklabels=current.season$nombre.semana, gdata=dgrafgg.s)
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
      
      gplot<-ggplot(dgrafgg) +
        geom_line(aes(x=num,y=value,group=variable, color=variable, linetype=variable),size=i.linesize) +
        geom_point(aes(x=num,y=value,group=variable, color=variable, size=variable, fill=variable, shape=variable), color="#ffffff", stroke = 0.1) +
        scale_shape_manual(values=i.shapes, name=trloc("Legend"), labels=labels) +
        scale_color_manual(values=i.colors, name=trloc("Legend"), labels=labels) +
        scale_fill_manual(values=i.fills, name=trloc("Legend"), labels=labels) +
        scale_size_manual(values=i.sizes, name=trloc("Legend"), labels=labels) +
        scale_linetype_manual(values=i.linetypes, name=trloc("Legend"), labels=labels) +
        scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        ggthemes::theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
      p<-list(plot=gplot, gdata=dgrafgg)
    }
    p
  }
  
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
                        i.level.curve=as.numeric(input$levelaveragecurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$levelaveragecurve)/100,
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
        tfile<-tempfile()
        tfile.div<-extract.pfe(tfile)
        good<-memgoodness(datfile[selectedcolumns],
                          i.graph=as.logical(input$advancedfeatures), i.prefix = tfile.div$name, i.output = tfile.div$path,
                          i.min.seasons = 3,
                          i.seasons=as.numeric(input$SelectMaximum),
                          i.type.threshold=as.numeric(input$typethreshold),
                          i.tails.threshold=as.numeric(input$ntails),
                          i.type.intensity=as.numeric(input$typeintensity),
                          i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                          i.tails.intensity=as.numeric(input$ntails),
                          i.type.curve=as.numeric(input$typecurve),
                          i.level.curve=as.numeric(input$levelaveragecurve)/100,
                          i.type.other=as.numeric(input$typeother),
                          i.level.other=as.numeric(input$levelaveragecurve)/100,
                          i.method=as.numeric(input$method),
                          i.param=as.numeric(input$param),
                          i.detection.values = seq(input$paramrange[1],input$paramrange[2],by=0.1),
                          i.n.max=as.numeric(input$nvalues),
                          i.goodness.method=as.character(input$validation))
      }
    }
    # Update goodness graphs tabs
    no.seasons<-NCOL(good$param.data)
    if (good$param.goodness.method=="sequential") se.seasons<-3:no.seasons else se.seasons<-1:no.seasons
    nu.seasons<-(1:no.seasons)[se.seasons]
    na.seasons<-(names(good$param.data))[se.seasons]
    lapply(data.frame(rbind(nu.seasons,na.seasons)), function(s){output[[paste0("tbmGoodnessGraphs_",as.character(s[2]))]] <- renderImage({
      graph.file<-paste(good$param.output, "/", good$param.prefix," Goodness ", s[1], " (",format(round(input$param,1),digits=3,nsmall=1),").png", sep="")
      if (!file.exists(graph.file)){
        gfile<-NULL
      }else{
        gfile<-list(src = graph.file,
                    contentType = 'image/png',
                    width = 800,
                    height = 600,
                    alt = "No image found")
      }
      gfile
    })})
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
        tfile<-tempfile()
        tfile.div<-extract.pfe(tfile)
        good<-memgoodness(datfile[selectedcolumns],
                          i.graph=as.logical(input$advancedfeatures), i.prefix=tfile.div$name, i.output = tfile.div$path,
                          i.min.seasons = 3,
                          i.seasons=as.numeric(input$SelectMaximum),
                          i.type.threshold=as.numeric(input$typethreshold),
                          i.tails.threshold=as.numeric(input$ntails),
                          i.type.intensity=as.numeric(input$typeintensity),
                          i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                          i.tails.intensity=as.numeric(input$ntails),
                          i.type.curve=as.numeric(input$typecurve),
                          i.level.curve=as.numeric(input$levelaveragecurve)/100,
                          i.type.other=as.numeric(input$typeother),
                          i.level.other=as.numeric(input$levelaveragecurve)/100,
                          i.method=as.numeric(input$method),
                          i.param=as.numeric(input$param),
                          i.detection.values = seq(input$paramrange[1],input$paramrange[2],by=0.1),
                          i.n.max=as.numeric(input$nvalues),
                          i.goodness.method=as.character(input$validation),
                          i.calculation.method="threshold")
      }
    }
    # Update goodness graphs tabs
    no.seasons<-NCOL(good$param.data)
    if (good$param.goodness.method=="sequential") se.seasons<-3:no.seasons else se.seasons<-1:no.seasons
    nu.seasons<-(1:no.seasons)[se.seasons]
    na.seasons<-(names(good$param.data))[se.seasons]
    lapply(data.frame(rbind(nu.seasons,na.seasons)), function(s){output[[paste0("tbdGoodnessGraphs_",as.character(s[2]))]] <- renderImage({
      graph.file<-paste(good$param.output, "/", good$param.prefix," Goodness ", s[1], " (",format(round(input$param,1),digits=3,nsmall=1),").png", sep="")
      if (!file.exists(graph.file)){
        gfile<-NULL
      }else{
        gfile<-list(src = graph.file,
                    contentType = 'image/png',
                    width = 800,
                    height = 600,
                    alt = "No image found")
      }
      gfile
    })})
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
        
        roca<-roc.analysis(datfile[selectedcolumns],
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
                           i.level.curve=as.numeric(input$levelaveragecurve)/100,
                           i.type.other=as.numeric(input$typeother),
                           i.level.other=as.numeric(input$levelaveragecurve)/100,
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
                          i.level.curve=as.numeric(input$levelaveragecurve)/100,
                          i.type.other=as.numeric(input$typeother),
                          i.level.other=as.numeric(input$levelaveragecurve)/100,
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
                          i.level.curve=as.numeric(input$levelaveragecurve)/100,
                          i.type.other=as.numeric(input$typeother),
                          i.level.other=as.numeric(input$levelaveragecurve)/100,
                          i.method=as.numeric(input$method),
                          i.param=as.numeric(input$param),
                          i.n.max=as.numeric(input$nvalues))
    }
    sta
  })
  
  read_data <- reactive({
    infile <- input$file
    indataset <- input$dataset
    inname <- infile$name
    i.range.x<-rep(NA,2)
    if (!is.null(input$firstWeek)) i.range.x[1]<-as.numeric(input$firstWeek)
    if (!is.null(input$lastWeek)) i.range.x[2]<-as.numeric(input$lastWeek)
    cat("read_data> ------------------------------------------\n")
    cat("read_data> Name: ",inname,"\n")
    cat("read_data> Dataset: ",indataset,"\n")
    cat("read_data> Range: ",i.range.x[1],"-",i.range.x[2],"\n")
    if(is.null(infile)){
      datasets=NULL
      datasetread=NULL
      cat("read_data> Warning: No file\n")
    }else if(is.null(indataset)){
      temp1<-read.data(i.file=infile$datapath, i.file.name=inname)
      datasets=temp1$datasets
      datasetread=temp1$datasetread
      rm("temp1")
      cat("read_data> Warning: No dataset\n")
    }else if (indataset==""){
      temp1<-read.data(i.file=infile$datapath, i.file.name=inname)
      datasets=temp1$datasets
      datasetread=temp1$datasetread
      rm("temp1")
      cat("read_data> Warning: No dataset\n")
    }else{
      temp1<-read.data(i.file=infile$datapath, i.file.name=inname, i.dataset = indataset, i.range.x=i.range.x)
      temp2<-read.data(i.file=infile$datapath, i.file.name=inname, i.dataset = indataset)
      datasets=temp1$datasets
      datasetread=temp1$datasetread
      rm("temp1")
    }
    if(!is.null(datasetread)){
      dataweeksoriginal<-row.names(temp2$datasetread)
      dataweeksfiltered<-row.names(datasetread)
      datasetread<-transformseries(datasetread, i.transformation=as.numeric(input$transformation))
    }else{
      dataweeksoriginal=NULL
      dataweeksfiltered=NULL
    }
    cat("read_data> datasets returning NULL?: ",is.null(datasets),"\n")
    cat("read_data> dataweeksoriginal returning NULL?: ",is.null(dataweeksoriginal),"\n")
    cat("read_data> dataweeksfiltered returning NULL?: ",is.null(dataweeksfiltered),"\n")
    cat("read_data> datasetread NULL?: ",is.null(datasetread),"\n")
    cat("read_data> ------------------------------------------\n")
    readdata<-list(datasets=datasets, datasetread=datasetread, dataweeksoriginal=dataweeksoriginal, dataweeksfiltered=dataweeksfiltered)
    readdata
  })
  
  getDatasets <- eventReactive(input$file, {
    cat("reactive/getDatasets> begin\n")
    readdata <- read_data()
    # datfile <- readdata$datasetread
    datsheets <- readdata$datasets
    # datweeks <- readdata$dataweeks
    if (!is.null(datsheets)) cat("reactive/getDatasets> updating dataset list\n")    
    cat("reactive/getDatasets> end\n")
    return(datsheets)
  })
  
  getWeeksOriginal <- eventReactive(input$dataset, {
    cat("reactive/getWeeksOriginal> begin\n")
    readdata <- read_data()
    dataweeksoriginal <- readdata$dataweeksoriginal
    if (!is.null(dataweeksoriginal)) cat("reactive/getWeeksOriginal> updating first/last week list\n")
    cat("reactive/getWeeksOriginal> end\n")
    return(dataweeksoriginal)
  })
  
  getWeeksFiltered <- eventReactive(c(input$dataset,input$firstWeek,input$lastWeek), {
    cat("reactive/getWeeksFiltered> begin\n")
    readdata <- read_data()
    dataweeksfiltered <- readdata$dataweeksfiltered
    if (!is.null(dataweeksfiltered)) cat("reactive/getWeeksFiltered> updating first/last week list\n")
    cat("reactive/getWeeksFiltered> end\n")
    return(dataweeksfiltered)
  })  
  
  getSeasons <- reactive({
    cat("reactive/getSeasons> begin\n")
    readdata <- read_data()
    datfile <- readdata$datasetread
    seasons<-names(datfile)
    if (!is.null(seasons)) cat("reactive/getSeasons> updating from/to/exclude\n")
    cat("reactive/getSeasons> end\n")
    return(seasons)
  })
  
  #####################################
  ### OBSERVERS
  #####################################

  observeEvent(read_data(), {
    cat("observe/read_data> begin\n")
    readdata <- read_data()
    datfile <- readdata$datasetread
    datsheets <- readdata$datasets
    if (!is.null(datfile)){
      seasons<-names(datfile)
      cat("observe/read_data> updating timing plots\n")
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
                          i.level.curve=as.numeric(input$levelaveragecurve)/100,
                          i.type.other=as.numeric(input$typeother),
                          i.level.other=as.numeric(input$levelaveragecurve)/100,
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
                          i.level.curve=as.numeric(input$levelaveragecurve)/100,
                          i.type.other=as.numeric(input$typeother),
                          i.level.other=as.numeric(input$levelaveragecurve)/100,
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
                          i.level.curve=as.numeric(input$levelaveragecurve)/100,
                          i.type.other=as.numeric(input$typeother),
                          i.level.other=as.numeric(input$levelaveragecurve)/100,
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
      cat("observe/read_data> updating manual optimization plots\n")
      selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo,
                                      i.exclude=input$SelectExclude, i.include="",
                                      i.pandemic=T,
                                      i.seasons=as.numeric(input$SelectMaximum))
      if (length(selectedcolumns)>0){
        modfile<-datfile[selectedcolumns]
        modseasons<-names(modfile)
        
        plotdata<-cbind(data.frame(weekno=1:NROW(modfile),weekna=rownames(modfile), stringsAsFactors = F), modfile)
        if (NCOL(modfile)>1){
          epi<-memmodel(modfile, i.seasons = NA)
          epidata<-epi$data
        }else{
          epi<-memmodel(cbind(modfile, modfile), i.seasons = NA)
          epidata<-epi$data[1]
        }
        names(epidata)<-paste0(names(epidata),"_fixed")
        epidata$weekna<-rownames(epidata)
        plotdata<-merge(plotdata,epidata,"weekna",all.x=T, sort=F)
        # I have to duplicate the dataset since the plotdata will be changing when I add colors to the _color
        # column, if I use plotdata for detect nearpoint, each time I change a color, it reevaluates the
        # expression nearpoint, producing duplicate points
        origdata<-plotdata
        for (s in modseasons) eval(parse(text=paste0("plotdata$'",as.character(s),"_color'<-'1'")))
        values$origdata <- origdata
        values$plotdata <- plotdata
        values$clickdata <- data.frame()
        values$optimizegraphs <- data.frame()
        rm("origdata", "plotdata", "epidata")
        lapply(modseasons, function(s){output[[paste0("tbmOptimizeM_",as.character(s))]] <- renderUI({
          imgfileok<-F
          if (NROW(values$optimizegraphs)>0){
            imgtmp<-values$optimizegraphs
            imgtmp2<-subset(imgtmp,imgtmp$season==as.character(s))
            if (NROW(imgtmp2)>0){
              if (file.exists(imgtmp2$file)){
                imgfile<-imgtmp2$file
                imgfileok<-T
              } 
            }
          }           
          if (imgfileok){
            fluidRow(
              plotOutput(outputId=paste0("tbmOptimizeM_",as.character(s),"_plot"), 
                         click = paste0("tbmOptimizeM_",as.character(s),"_click"),
                         width = "800px", height = "600px"),
              tableOutput(paste0("tbmOptimizeM_",as.character(s),"_table")),
              imageOutput(paste0("tbmOptimizeM_",as.character(s),"_image"))
            )            
          }else{
            fluidRow(
              plotOutput(outputId=paste0("tbmOptimizeM_",as.character(s),"_plot"), 
                         click = paste0("tbmOptimizeM_",as.character(s),"_click"),
                         width = "800px", height = "600px"),
              tableOutput(paste0("tbmOptimizeM_",as.character(s),"_table"))
            )            
          }
        })})
        lapply(modseasons, function(s){output[[paste0("tbmOptimizeM_",as.character(s),"_table")]] <- renderTable({
          if (NROW(values$clickdata)>0){
            etwo<-extract.two(values$clickdata,"weekno","season")
            etwo<-merge(etwo,data.frame(id.tail=c(1,2),point=trloc(c("Start","End")), stringsAsFactors = F),by="id.tail")
            etwo2<-subset(etwo,etwo$season==as.character(s))[c("season","weekno","point",paste0(as.character(s),"_fixed"))]
            names(etwo2)[1:3]<-trloc(c("Season","Week","Point"))
            names(etwo2)[4]<-as.character(s)
          }else{
            etwo2<-data.frame(message="No data")
          }
          etwo2
        })})        
        lapply(modseasons, function(s){output[[paste0("tbmOptimizeM_",as.character(s),"_image")]] <- renderImage({
          imgfile<-""
          if (NROW(values$optimizegraphs)>0){
            imgtmp<-values$optimizegraphs
            imgtmp2<-subset(imgtmp,imgtmp$season==as.character(s))
            if (NROW(imgtmp2)>0){
              if (file.exists(imgtmp2$file)){
                imgfile<-imgtmp2$file
              } 
            }
          }          
          gfile<-list(src = imgfile,
                      contentType = 'image/png',
                      width = 800,
                      height = 600,
                      alt = "No image found")
          gfile
        })})
        lapply(modseasons, function(s){output[[paste0("tbmOptimizeM_",as.character(s),"_plot")]] <- renderPlot({
          i.cutoff.original<-min(as.numeric(values$origdata$weekna[1:(min(3,NROW(values$origdata)))]))
          if (i.cutoff.original < 1) i.cutoff.original <- 1
          if (i.cutoff.original > 52) i.cutoff.original <- 52
          i.range.x<-c(min(as.numeric(values$origdata$weekna[1:(min(3,NROW(values$origdata)))])),max(as.numeric(values$origdata$weekna[(max(1,NROW(values$origdata)-2)):NROW(values$origdata)])))
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
          # Calculate ticks for x
          data.x <- values$origdata$weekno
          axis.x.range <- range(data.x)
          temp1 <- range(i.range.x.values$week.no)
          temp2 <- optimal.tickmarks(temp1[1], temp1[2], 30, 1:temp1[2],T,F)
          axis.x.ticks<-data.x[values$origdata$weekna %in% i.range.x.values$week.lab[temp2$tickmarks]]
          axis.x.labels<-values$origdata$weekna[values$origdata$weekna %in% i.range.x.values$week.lab[temp2$tickmarks]]
          rm("temp1","temp2")
          # Range y fix
          i.range.y <- c(0,1.05*max(values$origdata[s],na.rm=T))
          axis.y.range.original <- i.range.y
          axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2],10)
          axis.y.range <- axis.y.otick$range
          axis.y.ticks <- axis.y.otick$tickmarks
          axis.y.labels <- axis.y.otick$tickmarks
          colors.palette<-generate_palette(i.number.series=NCOL(values$plotdata),
                                           i.colObservedLines=input$colObservedLines,
                                           i.colObservedPoints=input$colObservedPoints,
                                           i.colEpidemicStart=input$colEpidemicStart,
                                           i.colEpidemicStop=input$colEpidemicStop,
                                           i.colThresholds=input$colThresholds,
                                           i.colSeasons=input$colSeasons,
                                           i.colEpidemic=input$colEpidemic)
          colormixed<-"#FF00FF"
          ggplot(values$plotdata, aes_(x=as.name("weekno"), y=as.name(paste0(s,"_fixed")))) +
            geom_point(aes_(as.name("weekno"), as.name(s), colour=as.name(paste0(s,"_color")), size=as.name(paste0(s,"_color")))) +
            scale_color_manual(values = c("1" = colors.palette$colObservedPoints, "2" = colors.palette$colEpidemicStart, "3" = colors.palette$colEpidemicStop, "4" = colormixed)) +
            scale_size_manual(values = c("1" = 4, "2" = 6, "3" = 6, "4" = 8)) +
            geom_line(aes_(x=as.name("weekno"), y=as.name(s)), color=colors.palette$colObservedLines) +
            scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
            scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
            labs(title = input$textMain, x = input$textX, y = input$textY) +
            ggthemes::theme_few() +
            theme(plot.title = element_text(hjust = 0.5)) +
            guides(color=FALSE, size=FALSE)
        })})
        lapply(modseasons,function(s){
          nameid<-paste0("tbmOptimizeM_",as.character(s),"_click")
          if (!(nameid %in% values$idscreated)){
            values$idscreated<-c(values$idscreated,nameid)
            observeEvent(input[[nameid]], {
              np <- nearPoints(values$origdata, input[[nameid]], maxpoints=1 , threshold = 10000)
              values$clickdata<-rbind(values$clickdata,cbind(data.frame(season=as.character(s), stringsAsFactors = F), np))
              if (NROW(values$clickdata)>0){
                p0<-extract.two(values$clickdata,"weekno","season")
                p1<-subset(p0, season==as.character(s) & id.tail==1)
                p2<-subset(p0, season==as.character(s) & id.tail==2)
                if (NROW(p1)>0) {
                  values$plotdata[values$plotdata[,paste0(as.character(s),"_color")]!="3", paste0(as.character(s),"_color")]<-"1"
                  values$plotdata[values$origdata$weekno==p1$weekno,paste0(as.character(s),"_color")]<-"2"
                }
                if (NROW(p2)>0){
                  values$plotdata[values$plotdata[,paste0(as.character(s),"_color")]!="2", paste0(as.character(s),"_color")]<-"1"
                  values$plotdata[values$origdata$weekno==p2$weekno,paste0(as.character(s),"_color")]<-"3"
                }
                if (NROW(p1)>0 & NROW(p2)>0){
                  if (p1$weekno==p2$weekno){
                    values$plotdata[, paste0(as.character(s),"_color")]<-"1"
                    values$plotdata[values$origdata$weekno==p1$weekno,paste0(as.character(s),"_color")]<-"4"
                  }
                }
              }
            })
          }
        })
      }
    }
    cat("observe/read_data> end\n")
  })
  
  # Pass url parameters to the app, in this case to advanced features, once the server is run, you can
  # use http://127.0.0.1:7910/?advancedfeatures=TRUE to enable/disable advanced features
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['language']])) {
      cat("query> language ", query[['language']],"\n")
      updateSelectInput(session, "lang", selected = as.character(query[['language']]))
    }
    Sys.sleep(3)
    if (!is.null(query[['advancedfeatures']])) {
      cat("query> advancedfeatures ", query[['advancedfeatures']],"\n")
      updateCheckboxInput(session, "advancedfeatures", value = as.logical(query[['advancedfeatures']]))
    }
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
      if (as.logical(input$advancedfeatures)){
        tabsetPanel(tabPanel(trloc("File"), tableOutput("tbdFile")),
                    tabPanel(trloc("Data"),
                             DT::dataTableOutput("tbdData"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbdData_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbdData_c","csv"))
                             )
                    ),
                    tabPanel(trloc("Seasons"), plotlyOutput("tbdSeasons", width ="100%", height ="100%")),
                    tabPanel(trloc("Series"),plotlyOutput("tbdSeries", width ="100%", height ="100%")),
                    tabPanel(trloc("Timing"),uiOutput("tbdTiming")),
                    tabPanel(trloc("Evolution"),uiOutput("tbdEvolution")),
                    tabPanel(trloc("Stability"),uiOutput("tbdStability")),
                    tabPanel(trloc("Goodness"),uiOutput("tbdGoodness"))
        )
      }else{
        tabsetPanel(tabPanel(trloc("File"), tableOutput("tbdFile")),
                    tabPanel(trloc("Data"),
                             DT::dataTableOutput("tbdData"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbdData_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbdData_c","csv"))
                             )
                    ),
                    tabPanel(trloc("Seasons"), plotlyOutput("tbdSeasons", width ="100%", height ="100%")),
                    tabPanel(trloc("Series"),plotlyOutput("tbdSeries", width ="100%", height ="100%")),
                    tabPanel(trloc("Timing"),uiOutput("tbdTiming")),
                    tabPanel(trloc("Evolution"),uiOutput("tbdEvolution")),
                    tabPanel(trloc("Stability"),uiOutput("tbdStability"))
        )
      }
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
      data.show<-data.frame(var1=trloc(c("File","Dataset")),var2=c(infile$name,indataset))
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
  options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))
  
  output$tbdData_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if(!is.null(datfile)){
        selectedcolumns<-select.columns(i.names=names(datfile), i.from="", i.to="", i.exclude="", i.include="", i.pandemic=T, i.seasons=NA)
        if (length(selectedcolumns)>0) export.mydata(i.data=datfile[selectedcolumns], i.file=file,
                                                     i.sheet=substring(trloc("Data"),1,32), i.rownames=trloc("Week no"), i.format="xlsx")
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
                                                     i.sheet=substring(trloc("Data"),1,32), i.rownames=trloc("Week no"), i.format="csv")
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
                         i.level.curve=as.numeric(input$levelaveragecurve)/100,
                         i.type.other=as.numeric(input$typeother),
                         i.level.other=as.numeric(input$levelaveragecurve)/100,
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
                        i.level.curve=as.numeric(input$levelaveragecurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$levelaveragecurve)/100,
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
      tabsetPanel(tabPanel(trloc("Duration"), plotlyOutput("tbdEduration", width ="100%", height ="100%")),
                  tabPanel(trloc("Start"),plotlyOutput("tbdEstart", width ="100%", height ="100%")),
                  tabPanel(trloc("Epidemic %"), plotlyOutput("tbdEpercentage", width ="100%", height ="100%")),
                  tabPanel(trloc("Thresholds"),plotlyOutput("tbdEthresholds", width ="100%", height ="100%")),
                  tabPanel(trloc("Scheme"), formattable::formattableOutput("tbdEscheme")),
                  tabPanel(trloc("Detailed"),
                           DT::dataTableOutput("tbdEdetailed"),
                           fluidRow(
                             column(8),
                             column(2,
                                    if (zip.present()){
                                      downloadButton("tbdEdetailed_x","xlsx")
                                    }else if (.Platform$OS.type=="windows"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                    }else if (.Platform$OS.type=="unix"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                    }),
                             column(2,downloadButton("tbdEdetailed_c","csv"))
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
      names(datfile.plot)<-trloc(c("Lower limit","Duration","Upper limit"))
      # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
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
      names(datfile.plot)<-trloc(c("Lower limit","Start","Upper limit"))
      # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
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
      names(datfile.plot)<-trloc(c("Lower limit","Epidemic percentage","Upper limit"))
      # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
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
      names(datfile.plot)<-trloc(c("Pre-epidemic thr.","Medium int. thr.","High int. thr.","Very high int. thr.","Post-epidemic thr."))
      colors.palette<-generate_palette(i.number.series=NCOL(datfile.plot),
                                       i.colObservedLines=input$colObservedLines,
                                       i.colObservedPoints=input$colObservedPoints,
                                       i.colEpidemicStart=input$colEpidemicStart,
                                       i.colEpidemicStop=input$colEpidemicStop,
                                       i.colThresholds=input$colThresholds,
                                       i.colSeasons=input$colSeasons,
                                       i.colEpidemic=input$colEpidemic)
      # by inserting \n instead of /, the fixplotly function assign twice the space for the x-axis labs
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
  
  output$tbdEscheme <- formattable::renderFormattable({
    dataevolution <- data_evolution()
    if(is.null(dataevolution)){
      datashow<-NULL
    }else{
      temp1<-dataevolution$evolution.seasons
      if (row.names(temp1)[NROW(temp1)]=="next") row.names(temp1)[NROW(temp1)]<-trloc("next")
      datashow<-formattable::formattable(temp1, apply(temp1, 2,
                                                      function(noxneeded) formattable::formatter("span",
                                                                                                 style = x ~ formattable::style(color = ifelse(x, "green", "red")),
                                                                                                 x ~ formattable::icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))))
      )
    }
    datashow
  })
  
  output$tbdEdetailed <- DT::renderDataTable({
    dataevolution <- data_evolution()
    if(is.null(dataevolution)){
      datashow<-NULL
    }else{
      datashow<-format(round(dataevolution$evolution.data, 2), nsmall=2)
      if (row.names(datashow)[NROW(datashow)]=="next") row.names(datashow)[NROW(datashow)]<-trloc("next")
      names(datashow)<-trloc(c("Seasons", "Duration (lower limit)", "Duration", "Duration (upper limit)", "Start (lower limit)", "Start", "Start (upper limit)", "Epidemic perc. (lower limit)", "Epidemic perc.", "Epidemic perc. (upper limit)", "Epidemic thr.", "Post-epidemic thr.", "Medium thr.", "High thr.", "Very high thr."))
    }
    datashow
  },
  options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

  output$tbdEdetailed_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      dataevolution <- data_evolution()
      datashow<-dataevolution$evolution.data
      if (row.names(datashow)[NROW(datashow)]=="next") row.names(datashow)[NROW(datashow)]<-trloc("next")
      names(datashow)<-trloc(c("Seasons", "Duration (lower limit)", "Duration", "Duration (upper limit)", "Start (lower limit)", "Start", "Start (upper limit)", "Epidemic perc. (lower limit)", "Epidemic perc.", "Epidemic perc. (upper limit)", "Epidemic thr.", "Post-epidemic thr.", "Medium thr.", "High thr.", "Very high thr."))
      if(!is.null(dataevolution)) export.mydata(i.data=datashow, i.file=file,
                                                i.sheet=substring(trloc("Evolution"),1,32), i.rownames=trloc("Season"), i.format="xlsx")
      
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$tbdEdetailed_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      dataevolution <- data_evolution()
      datashow<-dataevolution$evolution.data
      if (row.names(datashow)[NROW(datashow)]=="next") row.names(datashow)[NROW(datashow)]<-trloc("next")
      names(datashow)<-trloc(c("Seasons", "Duration (lower limit)", "Duration", "Duration (upper limit)", "Start (lower limit)", "Start", "Start (upper limit)", "Epidemic perc. (lower limit)", "Epidemic perc.", "Epidemic perc. (upper limit)", "Epidemic thr.", "Post-epidemic thr.", "Medium thr.", "High thr.", "Very high thr."))
      if(!is.null(dataevolution)) export.mydata(i.data=datashow, i.file=file,
                                                i.sheet=substring(trloc("Evolution"),1,32), i.rownames=trloc("Season"), i.format="csv")
    },
    contentType="text/csv"
  )
  
  output$tbdStability <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(is.null(datfile)){
      return(NULL)
    }else{
      tabsetPanel(tabPanel(trloc("Duration"), plotlyOutput("tbdSduration", width ="100%", height ="100%")),
                  tabPanel(trloc("Start"),plotlyOutput("tbdSstart", width ="100%", height ="100%")),
                  tabPanel(trloc("Epidemic %"), plotlyOutput("tbdSpercentage", width ="100%", height ="100%")),
                  tabPanel(trloc("Thresholds"),plotlyOutput("tbdSthresholds", width ="100%", height ="100%")),
                  tabPanel(trloc("Scheme"), formattable::formattableOutput("tbdSscheme")),
                  tabPanel(trloc("Detailed"),
                           DT::dataTableOutput("tbdSdetailed"),
                           fluidRow(
                             column(8),
                             column(2,
                                    if (zip.present()){
                                      downloadButton("tbdSdetailed_x","xlsx")
                                    }else if (.Platform$OS.type=="windows"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                    }else if (.Platform$OS.type=="unix"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                    }),
                             column(2,downloadButton("tbdSdetailed_c","csv"))
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
      names(datfile.plot)<-trloc(c("Lower limit","Duration","Upper limit"))
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
      names(datfile.plot)<-trloc(c("Lower limit","Start","Upper limit"))
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
      names(datfile.plot)<-trloc(c("Lower limit","Epidemic percentage","Upper limit"))
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
      names(datfile.plot)<-trloc(c("Pre-epidemic thr.","Medium int. thr.","High int. thr.","Very high int. thr.","Post-epidemic thr."))
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
  
  output$tbdSscheme <- formattable::renderFormattable({
    datastability <- data_stability()
    if(is.null(datastability)){
      datashow<-NULL
    }else{
      temp1<-datastability$stability.seasons
      datashow<-formattable::formattable(temp1, apply(temp1, 2,
                                                      function(noxneeded) formattable::formatter("span",
                                                                                                 style = x ~ formattable::style(color = ifelse(x, "green", "red")),
                                                                                                 x ~ formattable::icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))))
      )
    }
    datashow
  })
  
  output$tbdSdetailed <- DT::renderDataTable({
    datastability <- data_stability()
    if(is.null(datastability)){
      datashow<-NULL
    }else{
      datashow<-format(round(datastability$stability.data, 2), nsmall=2)
      names(datashow)<-trloc(c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic perc. (lower limit)","Epidemic perc.","Epidemic perc. (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr."))
    }
    datashow
  },
  options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

  output$tbdSdetailed_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      datastability <- data_stability()
      datashow<-datastability$stability.data
      names(datashow)<-trloc(c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic perc. (lower limit)","Epidemic perc.","Epidemic perc. (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr."))
      if(!is.null(datastability)) export.mydata(i.data=datashow, i.file = file,
                                                i.sheet=substring(trloc("Stability"),1,32), i.rownames=trloc("Seasons"), i.format="xlsx")
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$tbdSdetailed_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      datastability <- data_stability()
      datashow<-datastability$stability.data
      names(datashow)<-trloc(c("Duration (lower limit)","Duration","Duration (upper limit)","Start (lower limit)","Start","Start (upper limit)","Epidemic perc. (lower limit)","Epidemic perc.","Epidemic perc. (upper limit)","Epidemic thr.","Post-epidemic thr.","Medium thr.","High thr.","Very high thr."))
      if(!is.null(datastability)) export.mydata(i.data=datashow, i.file = file,
                                                i.sheet=substring(trloc("Stability"),1,32), i.rownames=trloc("Seasons"), i.format="csv")
    },
    contentType="text/csv"
  )
  
  output$tbdGoodness <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(is.null(datfile)){
      return(NULL)
    }
    else
      tabsetPanel(tabPanel(trloc("Indicators"), uiOutput("tbdGoodnessIndicators")),
                  tabPanel(trloc("Summary"),
                           formattable::formattableOutput("tbdGoodnessSummary"),
                           fluidRow(
                             column(8),
                             column(2,
                                    if (zip.present()){
                                      downloadButton("tbdGoodnessSummary_x","xlsx")
                                    }else if (.Platform$OS.type=="windows"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                    }else if (.Platform$OS.type=="unix"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                    }),
                             column(2,downloadButton("tbdGoodnessSummary_c","csv"))
                           )
                  ),
                  tabPanel(trloc("Graphs"), uiOutput("tbdGoodnessGraphs")),
                  tabPanel(trloc("Intensity"), uiOutput("tbdGoodnessIntensity")),
                  tabPanel(trloc("Detailed"),
                           formattable::formattableOutput("tbdGoodnessDetailed"),
                           fluidRow(
                             column(8),
                             column(2,
                                    if (zip.present()){
                                      downloadButton("tbdGoodnessDetailed_x","xlsx")
                                    }else if (.Platform$OS.type=="windows"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                    }else if (.Platform$OS.type=="unix"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                    }),
                             column(2,downloadButton("tbdGoodnessDetailed_c","csv"))
                           )
                  )
                  
      )
  })
  
  output$tbdGoodnessIndicators <- renderUI({
    good <- data_good_global()
    if(is.null(good)){
      return(NULL)
    }else{
      fluidRow(
        valueBox(format(round(good$results["Sensitivity"], 2), nsmall=2), trloc("Sensitivity"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Specificity"], 2), nsmall=2), trloc("Specificity"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Positive predictive value"], 2), nsmall=2), trloc("Positive predictive value"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Negative predictive value"], 2), nsmall=2), trloc("Negative predictive value"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Percent agreement"], 2), nsmall=2), trloc("Percent agreement"), icon = icon("heartbeat"), width=3, color="aqua"),
        valueBox(format(round(good$results["Matthews correlation coefficient"], 2), nsmall=2), trloc("Matthews correlation coefficient"), icon = icon("heartbeat"), width=3, color="aqua"),
        valueBox(format(round(good$results["Youdens Index"], 2), nsmall=2), trloc("Youdens Index"), icon = icon("heartbeat"), width=3, color="aqua")
      )
    }
  })
  
  output$tbdGoodnessSummary <- formattable::renderFormattable({
    good <- data_good_global()
    if(!is.null(good)){
      temp1<-as.data.frame(good$validity.data)
      temp1$Total<-good$results
      temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient", "Youdens Index")]
      good.table<-formattable::formattable(temp1, list(
        "Sensitivity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Specificity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Positive predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Negative predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Percent agreement" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
        "Matthews correlation coefficient" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
        "Youdens Index" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5)
      ), digits = 2, format = "f")
      names(good.table)<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient", "Youdens Index"))
      names(attr(good.table, "formattable")$format[[1]])<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient", "Youdens Index"))
    }else{
      temp1<-data.frame(Error=trloc("Number of columns must be greater than 2"))
      good.table<-formattable::formattable(temp1)
    }
    good.table
  })
  
  output$tbdGoodnessSummary_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      good <- data_good_global()
      if(!is.null(good)){
        temp1<-as.data.frame(good$validity.data)
        temp1$Total<-good$results
        temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")]
        names(temp1)<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Global goodness summary"),1,32), i.rownames=trloc("Season"), i.format="xlsx")
      }
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$tbdGoodnessSummary_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      good <- data_good_global()
      if(!is.null(good)){
        temp1<-as.data.frame(good$validity.data)
        temp1$Total<-good$results
        temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")]
        names(temp1)<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Global goodness summary"),1,32), i.rownames=trloc("Season"), i.format="csv")
      }
    },
    contentType="text/csv"
  )
  
  output$tbdGoodnessIntensity <- renderUI({
    good <- data_good_global()
    peaks <- good$peaks
    if(is.null(good)){
      return(NULL)
    }else{
      if (as.logical(input$advancedfeatures)){
        fluidRow(
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==1]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==1]," ", "level")), icon = icon("heartbeat"), width=2, color="lime"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==2]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==2]," ", "level")), icon = icon("thermometer-1"), width=2, color="green"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==3]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==3]," ", "level")), icon = icon("thermometer-2"), width=2, color="yellow"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==4]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==4]," ", "level")), icon = icon("thermometer-3"), width=2, color="orange"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==5]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==5]," ", "level")), icon = icon("thermometer-4"), width=2, color="red"),
          valueBox(peaks$Count[peaks[,1]==-1], trloc(peaks$Description[peaks[,1]==-1]), icon = icon("heartbeat"), width=3, color="teal"),
          valueBox(peaks$Count[peaks[,1]==0], trloc(peaks$Description[peaks[,1]==0]), icon = icon("heartbeat"), width=3, color="teal")
        )
      }else{
        fluidRow(
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==1]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==1]," ", "level")), icon = icon("heartbeat"), width=2, color="lime"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==2]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==2]," ", "level")), icon = icon("thermometer-1"), width=2, color="green"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==3]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==3]," ", "level")), icon = icon("thermometer-2"), width=2, color="yellow"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==4]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==4]," ", "level")), icon = icon("thermometer-3"), width=2, color="orange"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==5]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==5]," ", "level")), icon = icon("thermometer-4"), width=2, color="red"),
          valueBox(peaks$Count[peaks[,1]==-1], trloc(peaks$Description[peaks[,1]==-1]), icon = icon("heartbeat"), width=3, color="teal")
        )
      }
    }
  })
  
  output$tbdGoodnessDetailed <- formattable::renderFormattable({
    good <- data_good_global()
    if(!is.null(good)){
      temp1 <- good$peaks.data
      temp1$Level<-as.character(temp1$Level)
      temp1$Description<-trloc(temp1$Description)
      currentpalette<-generate_palette(i.colThresholds=input$colThresholds, i.colLevels=input$colLevels)
      thr.c<-currentpalette$colThresholds
      lvl.n<-as.character(c(1:5))
      lvl.t<-trloc(c("Baseline","Low","Medium","High","Very high"))
      lvl.c<-currentpalette$colLevels
      peaks.data<-formattable::formattable(temp1, list(
        "Epidemic threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[1], font.weight = "bold")),
        "Medium threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[2], font.weight = "bold")),
        "High threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[3], font.weight = "bold")),
        "Very high threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[4], font.weight = "bold")),
        "Level" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.n[1], lvl.c[1] , ifelse(x==lvl.n[2], lvl.c[2], ifelse(x==lvl.n[3], lvl.c[3], ifelse(x==lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
        "Description" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.t[1], lvl.c[1] , ifelse(x==lvl.t[2], lvl.c[2], ifelse(x==lvl.t[3], lvl.c[3], ifelse(x==lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold"))
      ), digits = 2, format = "f")
      names(peaks.data)<-trloc(c("Peak","Peak week","Epidemic threshold","Medium threshold","High threshold","Very high threshold","Level","Description"))
      names(attr(peaks.data, "formattable")$format[[1]])<-trloc(c("Epidemic threshold","Medium threshold","High threshold","Very high threshold","Level","Description"))
    }else{
      temp1<-data.frame(Error=trloc("Number of columns must be greater than 2"))
      peaks.data<-formattable::formattable(temp1)
    }
    peaks.data
  })
  
  output$tbdGoodnessDetailed_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      good <- data_good_global()
      if(!is.null(good)){
        temp1 <- good$peaks.data
        temp1$Level<-as.character(temp1$Level)
        temp1$Description<-trloc(temp1$Description)
        names(temp1)<-trloc(names(temp1))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Global goodness intensity"),1,32), i.rownames=trloc("Season"), i.format="xlsx")
      }
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$tbdGoodnessDetailed_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      good <- data_good_global()
      if(!is.null(good)){
        temp1 <- good$peaks.data
        temp1$Level<-as.character(temp1$Level)
        temp1$Description<-trloc(temp1$Description)
        names(temp1)<-trloc(names(temp1))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Global goodness intensity"),1,32), i.rownames=trloc("Season"), i.format="csv")
      }
    },
    contentType="text/csv"
  )
  
  output$tbdGoodnessGraphs = renderUI({
    good <- data_good_global()
    if(is.null(good)) {
      return(NULL)
    }else{
      no.seasons<-NCOL(good$param.data)
      if (good$param.goodness.method=="sequential") se.seasons<-3:no.seasons else se.seasons<-1:no.seasons
      nu.seasons<-(1:no.seasons)[se.seasons]
      na.seasons<-(names(good$param.data))[se.seasons]
      do.call(tabsetPanel,
              lapply(na.seasons,function(s){
                call("tabPanel",s,call('imageOutput',outputId=paste0("tbdGoodnessGraphs_",s), width ="100%", height ="100%"))
              })
      )
    }
  })
  
  #####################################
  ### MODEL TAB
  #####################################
  
  output$tbModel <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(is.null(datfile)){
      return(NULL)
    }else{
      if (as.logical(input$advancedfeatures)){
        tabsetPanel(tabPanel(trloc("Data"),
                             DT::dataTableOutput("tbmData"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbmData_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbmData_c","csv"))
                             )
        ),
        tabPanel(trloc("Seasons"), plotlyOutput("tbmSeasons", width ="100%", height ="100%")),
        tabPanel(trloc("Series"),plotlyOutput("tbmSeries", width ="100%", height ="100%")),
        tabPanel(trloc("Timing"),uiOutput("tbmTiming")),
        tabPanel("MEM", uiOutput("tbmMem")),
        tabPanel(trloc("Goodness"),uiOutput("tbmGoodness")),
        tabPanel(trloc("Optimize"),uiOutput("tbmOptimize"))
        )        
      }else{
        tabsetPanel(tabPanel(trloc("Data"),
                             DT::dataTableOutput("tbmData"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbmData_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbmData_c","csv"))
                             )
        ),
        tabPanel(trloc("Seasons"), plotlyOutput("tbmSeasons", width ="100%", height ="100%")),
        tabPanel(trloc("Series"),plotlyOutput("tbmSeries", width ="100%", height ="100%")),
        tabPanel(trloc("Timing"),uiOutput("tbmTiming")),
        tabPanel("MEM", uiOutput("tbmMem")),
        tabPanel(trloc("Goodness"),uiOutput("tbmGoodness")),
        tabPanel(trloc("Optimize"),uiOutput("tbmOptimizeA"))
        )        
      }
      
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
  options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))

  output$tbmData_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      datamodel<-data_model()
      if(!is.null(datamodel)) export.mydata(i.data=datamodel$param.data, i.file = file,
                                            i.sheet=substring(trloc("Model data"),1,32), i.rownames=trloc("Week no"), i.format="xlsx")
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$tbmData_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      datamodel<-data_model()
      if(!is.null(datamodel)) export.mydata(i.data=datamodel$param.data, i.file = file,
                                            i.sheet=substring(trloc("Model data"),1,32), i.rownames=trloc("Week no"), i.format="csv")
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
                       i.level.curve=as.numeric(input$levelaveragecurve)/100,
                       i.type.other=as.numeric(input$typeother),
                       i.level.other=as.numeric(input$levelaveragecurve)/100,
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
                      i.level.curve=as.numeric(input$levelaveragecurve)/100,
                      i.type.other=as.numeric(input$typeother),
                      i.level.other=as.numeric(input$levelaveragecurve)/100,
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
      tabsetPanel(tabPanel(trloc("Estimators"), uiOutput("tbmMemSummary")),
                  tabPanel(trloc("Detailed"), verbatimTextOutput("tbmMemOutput")),
                  tabPanel(trloc("Graphs"),uiOutput("tbmMemGraph"))
      )
    }
  })
  
  output$tbmMemSummary <- renderUI({
    datamodel<-data_model()
    if(is.null(datamodel)){
      return(NULL)
    }else{
      fluidRow(
        valueBox(datamodel$n.seasons, trloc("Seasons in the model"), icon = icon("heartbeat"), width=3, color="light-blue"),
        valueBox(datamodel$ci.start[2,2], trloc("Average epidemic start week"), icon = icon("heartbeat"), width=3, color="light-blue"),
        valueBox(format(round(datamodel$ci.length[1,2], 2), nsmall=1), trloc("Average epidemic length"), icon = icon("heartbeat"), width=3, color="light-blue"),
        valueBox(paste0(format(round(datamodel$ci.percent[2], 2), nsmall=1), "%"), trloc("Epidemic percentage"), icon = icon("heartbeat"), width=3, color="light-blue"),
        valueBox(format(round(datamodel$pre.post.intervals[1,3], 2), nsmall=1), trloc("Epidemic threshold"), icon = icon("thermometer-1"), width=3, color="green"),
        valueBox(format(round(datamodel$epi.intervals[1,4], 2), nsmall=1), trloc("Medium threshold"), icon = icon("thermometer-2"), width=3, color="yellow"),
        valueBox(format(round(datamodel$epi.intervals[2,4], 2), nsmall=1), trloc("High threshold"), icon = icon("thermometer-3"), width=3, color="orange"),
        valueBox(format(round(datamodel$epi.intervals[3,4], 2), nsmall=1), trloc("Very high threshold"), icon = icon("thermometer-4"), width=3, color="red")
      )
    }
  })
  
  output$tbmMemOutput <- renderPrint({
    datamodel<-data_model()
    if(!is.null(datamodel)){
      summary(datamodel)
    }else{
      war.text <- as.data.frame(error=trloc("MEM needs at least two seasons"))
      names(war.text) <- NULL
      print(noquote(war.text), row.names = FALSE)}
  })
  
  output$tbmMemGraph <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(is.null(datfile)){
      return(NULL)
    }else{
      tabsetPanel(tabPanel(trloc("Moving epidemics"), plotlyOutput("tbmMemGraphMoving", width ="100%", height ="100%")),
                  tabPanel(trloc("Average curve"), plotlyOutput("tbmMemGraphAverage", width ="100%", height ="100%"))
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
      datfile.plot$dummy<-datamodel$typ.curve[,2]
      names(datfile.plot)[names(datfile.plot)=="dummy"]<-trloc("Average curve")
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
                       i.level.curve=as.numeric(input$levelaveragecurve)/100,
                       i.type.other=as.numeric(input$typeother),
                       i.level.other=as.numeric(input$levelaveragecurve)/100,
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
        # Average curve, more width and dot stype
        z$x$data[[NCOL(datfile.plot)]]$line$width<-2*z$x$data[[NCOL(datfile.plot)]]$line$width
        z$x$data[[NCOL(datfile.plot)]]$line$dash<-"dot"
        # Rename name and text for vertical lines I've just added
        z$x$data[[2*length(p$labels)+1]]$name<-trloc("Mean start")
        z$x$data[[2*length(p$labels)+2]]$name<-trloc("Mean end")
        z$x$data[[2*length(p$labels)+1]]$text<-paste(trloc("Mean start"),": ",rownames(datfile.plot)[datamodel$ci.start[1,2]],sep="")
        z$x$data[[2*length(p$labels)+2]]$text<-paste(trloc("Mean end"),": ",rownames(datfile.plot)[datamodel$ci.start[1,2]+datamodel$mean.length-1],sep="")
        # And I need to rearrange the order of the z list for fixplotly to work
        names(z$x$data)<-as.character(1:(2*length(p$labels)+2))
        z$x$data<-z$x$data[as.character(c(1:length(p$labels),2*length(p$labels)+1,2*length(p$labels)+2,(length(p$labels)+1):(2*length(p$labels)),2*length(p$labels)+1,2*length(p$labels)+2))]
        names(z$x$data)<-NULL
        zfix<-fixplotly(z,
                        c(p$labels,trloc(c("Mean start","Mean end"))),
                        c(p$haslines,T,T),
                        c(p$haspoints,F,F),
                        "week","value",p$weeklabels)
      }
    }
    zfix
  })
  
  output$tbmMemGraphAverage <- renderPlotly({
    datamodel<-data_model()
    if(is.null(datamodel)){
      zfix<-NULL
    }else{
      datfile.plot<-data.frame(Average=datamodel$typ.curve[,2],row.names = rownames(datamodel$param.data))
      names(datfile.plot)<-"Average curve"
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
    }else{
      if (as.logical(input$advancedfeatures)){
        tabsetPanel(tabPanel(trloc("Indicators"), uiOutput("tbmGoodnessIndicators")),
                    tabPanel(trloc("Summary"),
                             formattable::formattableOutput("tbmGoodnessSummary"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbmGoodnessSummary_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbmGoodnessSummary_c","csv"))
                             )
                    ),
                    tabPanel(trloc("Graphs"), uiOutput("tbmGoodnessGraphs")),
                    tabPanel(trloc("Intensity"), uiOutput("tbmGoodnessIntensity")),
                    tabPanel(trloc("Detailed"),
                             formattable::formattableOutput("tbmGoodnessDetailed"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbmGoodnessDetailed_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbmGoodnessDetailed_c","csv"))
                             )
                    )
        )
      }else{
        tabsetPanel(tabPanel(trloc("Indicators"), uiOutput("tbmGoodnessIndicators")),
                    tabPanel(trloc("Summary"),
                             formattable::formattableOutput("tbmGoodnessSummary"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbmGoodnessSummary_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbmGoodnessSummary_c","csv"))
                             )
                    ),
                    tabPanel(trloc("Intensity"), uiOutput("tbmGoodnessIntensity")),
                    tabPanel(trloc("Detailed"),
                             formattable::formattableOutput("tbmGoodnessDetailed"),
                             fluidRow(
                               column(8),
                               column(2,
                                      if (zip.present()){
                                        downloadButton("tbmGoodnessDetailed_x","xlsx")
                                      }else if (.Platform$OS.type=="windows"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                      }else if (.Platform$OS.type=="unix"){
                                        shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                      }),
                               column(2,downloadButton("tbmGoodnessDetailed_c","csv"))
                             )
                    )
        )
      }
    }
  })
  
  output$tbmGoodnessIndicators <- renderUI({
    good <- data_good_model()
    if(is.null(good)){
      return(NULL)
    }else{
      fluidRow(
        valueBox(format(round(good$results["Sensitivity"], 2), nsmall=2), trloc("Sensitivity"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Specificity"], 2), nsmall=2), trloc("Specificity"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Positive predictive value"], 2), nsmall=2), trloc("Positive predictive value"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Negative predictive value"], 2), nsmall=2), trloc("Negative predictive value"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(good$results["Percent agreement"], 2), nsmall=2), trloc("Percent agreement"), icon = icon("heartbeat"), width=3, color="aqua"),
        valueBox(format(round(good$results["Matthews correlation coefficient"], 2), nsmall=2), trloc("Matthews correlation coefficient"), icon = icon("heartbeat"), width=3, color="aqua"),
        valueBox(format(round(good$results["Youdens Index"], 2), nsmall=2), trloc("Youdens Index"), icon = icon("heartbeat"), width=3, color="aqua")
      )
    }
  })
  
  output$tbmGoodnessSummary <- formattable::renderFormattable({
    good <- data_good_model()
    if(!is.null(good)){
      temp1<-as.data.frame(good$validity.data)
      temp1$Total<-good$results
      temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")]
      good.table<-formattable::formattable(temp1, list(
        "Sensitivity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Specificity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Positive predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Negative predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Percent agreement" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
        "Matthews correlation coefficient" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
        "Youdens Index" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5)
      ), digits = 2, format = "f")
      names(good.table)<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
      names(attr(good.table, "formattable")$format[[1]])<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
      
    }else{
      temp1<-data.frame(Error=trloc("Number of columns must be greater than 2"))
      good.table<-formattable::formattable(temp1)
    }
    good.table
  })
  
  output$tbmGoodnessSummary_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      good <- data_good_model()
      if(!is.null(good)){
        temp1<-as.data.frame(good$validity.data)
        temp1$Total<-good$results
        temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")]
        names(temp1)<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Model goodness summary"),1,32), i.rownames=trloc("Season"), i.format="xlsx")
      }
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$tbmGoodnessSummary_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      good <- data_good_model()
      if(!is.null(good)){
        temp1<-as.data.frame(good$validity.data)
        temp1$Total<-good$results
        temp1<-as.data.frame(t(temp1))[c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")]
        names(temp1)<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Model goodness summary"),1,32), i.rownames=trloc("Season"), i.format="csv")
      }
    },
    contentType="text/csv"
  )
  
  output$tbmGoodnessGraphs = renderUI({
    good <- data_good_model()
    if(is.null(good)) {
      return(NULL)
    }else{
      no.seasons<-NCOL(good$param.data)
      if (good$param.goodness.method=="sequential") se.seasons<-3:no.seasons else se.seasons<-1:no.seasons
      nu.seasons<-(1:no.seasons)[se.seasons]
      na.seasons<-(names(good$param.data))[se.seasons]
      do.call(tabsetPanel,
              lapply(na.seasons, function(s){
                call("tabPanel",s,call('imageOutput',outputId=paste0("tbmGoodnessGraphs_",s), width ="100%", height ="100%"))
              })
      )
    }
  })
  
  output$tbmGoodnessIntensity <- renderUI({
    good <- data_good_model()
    peaks <- good$peaks
    if(is.null(good)){
      return(NULL)
    }else{
      if (as.logical(input$advancedfeatures)){
        fluidRow(
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==1]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==1]," ", "level")), icon = icon("heartbeat"), width=2, color="lime"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==2]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==2]," ", "level")), icon = icon("thermometer-1"), width=2, color="green"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==3]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==3]," ", "level")), icon = icon("thermometer-2"), width=2, color="yellow"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==4]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==4]," ", "level")), icon = icon("thermometer-3"), width=2, color="orange"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==5]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==5]," ", "level")), icon = icon("thermometer-4"), width=2, color="red"),
          valueBox(peaks$Count[peaks[,1]==-1], trloc(peaks$Description[peaks[,1]==-1]), icon = icon("heartbeat"), width=3, color="teal"),
          valueBox(peaks$Count[peaks[,1]==0], trloc(peaks$Description[peaks[,1]==0]), icon = icon("heartbeat"), width=3, color="teal")
        )
      }else{
        fluidRow(
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==1]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==1]," ", "level")), icon = icon("heartbeat"), width=2, color="lime"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==2]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==2]," ", "level")), icon = icon("thermometer-1"), width=2, color="green"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==3]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==3]," ", "level")), icon = icon("thermometer-2"), width=2, color="yellow"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==4]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==4]," ", "level")), icon = icon("thermometer-3"), width=2, color="orange"),
          valueBox(paste0(format(round(peaks$Percentage[peaks[,1]==5]*100, 2), nsmall=1), "%"), trloc(paste0(peaks$Description[peaks[,1]==5]," ", "level")), icon = icon("thermometer-4"), width=2, color="red"),
          valueBox(peaks$Count[peaks[,1]==-1], trloc(peaks$Description[peaks[,1]==-1]), icon = icon("heartbeat"), width=3, color="teal")
        )
      }
    }
  })
  
  output$tbmGoodnessDetailed <- formattable::renderFormattable({
    good <- data_good_model()
    if(!is.null(good)){
      temp1 <- good$peaks.data
      temp1$Level<-as.character(temp1$Level)
      temp1$Description<-trloc(temp1$Description)
      currentpalette<-generate_palette(i.colThresholds=input$colThresholds, i.colLevels=input$colLevels)
      thr.c<-currentpalette$colThresholds
      lvl.n<-as.character(c(1:5))
      lvl.t<-trloc(c("Baseline","Low","Medium","High","Very high"))
      lvl.c<-currentpalette$colLevels
      peaks.data<-formattable::formattable(temp1, list(
        "Epidemic threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[1], font.weight = "bold")),
        "Medium threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[2], font.weight = "bold")),
        "High threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[3], font.weight = "bold")),
        "Very high threshold"=formattable::formatter("span", style = formattable::style(color = thr.c[4], font.weight = "bold")),
        "Level" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.n[1], lvl.c[1] , ifelse(x==lvl.n[2], lvl.c[2], ifelse(x==lvl.n[3], lvl.c[3], ifelse(x==lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
        "Description" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x),"grey",ifelse(x==lvl.t[1], lvl.c[1] , ifelse(x==lvl.t[2], lvl.c[2], ifelse(x==lvl.t[3], lvl.c[3], ifelse(x==lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold"))
      ), digits = 2, format = "f")
      names(peaks.data)<-trloc(c("Peak","Peak week","Epidemic threshold","Medium threshold","High threshold","Very high threshold","Level","Description"))
      names(attr(peaks.data, "formattable")$format[[1]])<-trloc(c("Epidemic threshold","Medium threshold","High threshold","Very high threshold","Level","Description"))
    }else{
      temp1<-data.frame(Error=trloc("Number of columns must be greater than 2"))
      peaks.data<-formattable::formattable(temp1)
    }
    peaks.data
  })
  
  output$tbmGoodnessDetailed_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      good <- data_good_model()
      if(!is.null(good)){
        temp1 <- good$peaks.data
        temp1$Level<-as.character(temp1$Level)
        temp1$Description<-trloc(temp1$Description)
        names(temp1)<-trloc(names(temp1))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Model goodness intensity"),1,32), i.rownames=trloc("Season"), i.format="xlsx")
      }
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )  
  output$tbmGoodnessDetailed_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      good <- data_good_model()
      if(!is.null(good)){
        temp1 <- good$peaks.data
        temp1$Level<-as.character(temp1$Level)
        temp1$Description<-trloc(temp1$Description)
        names(temp1)<-trloc(names(temp1))
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Model goodness intensity"),1,32), i.rownames=trloc("Season"), i.format="csv")
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
      tabsetPanel(tabPanel(trloc("Manual"), uiOutput("tbmOptimizeM")),
                  tabPanel(trloc("Automatic"), uiOutput("tbmOptimizeA")))
    }
  })
  
  output$tbmOptimizeM = renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(is.null(datfile)) {
      return(NULL)
    }else{
      selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo,
                                      i.exclude=input$SelectExclude, i.include="",
                                      i.pandemic=T,
                                      i.seasons=as.numeric(input$SelectMaximum))
      if (length(selectedcolumns)<2){
        return(NULL)
      }else{
        tabnames<-names(datfile[selectedcolumns])
        do.call(tabsetPanel,
                c(
                  lapply(tabnames,function(s){
                    call("tabPanel",s,call("uiOutput", outputId=paste0("tbmOptimizeM_",as.character(s))))
                  }),
                  list(
                    tabPanel(trloc("Start & end"),tableOutput("tbmOptimizeMstartend")),
                    tabPanel(trloc("Clicks"),tableOutput("tbmOptimizeMclicks")),
                    tabPanel(trloc("Results"),uiOutput("tbmOptimizeMresults"))
                  )
                )
        )
      }
    }
  })
  
  output$tbmOptimizeMstartend<-renderTable({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (NROW(values$clickdata)>0){
      selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo,
                                      i.exclude=input$SelectExclude, i.include="",
                                      i.pandemic=T,
                                      i.seasons=as.numeric(input$SelectMaximum))
      etwo<-extract.two(values$clickdata,"weekno","season")
      etwo<-merge(etwo,data.frame(id.tail=c(1,2),point=trloc(c("Start","End")), stringsAsFactors = F),by="id.tail")
      optr<-subset(etwo,etwo$season %in% names(datfile)[selectedcolumns])[c("season","weekna","point",paste0(names(datfile)[selectedcolumns],"_fixed"))]
      names(optr)[1:3]<-trloc(c("Season","Week","Point"))
      names(optr)[4:(length(selectedcolumns)+3)]<-names(datfile)[selectedcolumns]
    }else{
      optr<-NULL
    }
    optr
  })
  
  output$tbmOptimizeMclicks<-renderTable({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (NROW(values$clickdata)>0){
      selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo,
                                      i.exclude=input$SelectExclude, i.include="",
                                      i.pandemic=T,
                                      i.seasons=as.numeric(input$SelectMaximum))
      clickd<-values$clickdata
      optr<-subset(clickd,clickd$season %in% names(datfile)[selectedcolumns])[c("season","weekna",paste0(names(datfile)[selectedcolumns],"_fixed"))]
      names(optr)[1:2]<-trloc(c("Season","Week"))
      names(optr)[3:(length(selectedcolumns)+2)]<-names(datfile)[selectedcolumns]
    }else{
      optr<-NULL
    }
    optr
  })
  
  output$tbmOptimizeMresults<-renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (NROW(values$clickdata)>0){
      etwo<-extract.two(values$clickdata,"weekno","season")
      etwot<-reshape2::dcast(etwo, season ~  id.tail, value.var="weekno")
      selectedcolumns<-select.columns(i.names=names(datfile), i.from=input$SelectFrom, i.to=input$SelectTo,
                                      i.exclude=input$SelectExclude, i.include="",
                                      i.pandemic=T,
                                      i.seasons=as.numeric(input$SelectMaximum))
      if (length(selectedcolumns)>2){
        if (all(names(datfile)[selectedcolumns] %in% etwo$season) & NCOL(etwot)==3 & sum(is.na(etwot))==0){
          i.data<-values$plotdata[grepl("^.*_fixed$",names(values$plotdata))]
          names(i.data)<-sub("_fixed","",names(i.data),fixed=T)
          i.data<-i.data[names(i.data) %in% names(datfile)[selectedcolumns]]
          row.names(i.data)<-values$plotdata$weekna
          tfile<-tempfile()
          tfile.div<-extract.pfe(tfile)
          
          i.param.values=seq(input$paramrange[1],input$paramrange[2],by=0.1)
          i.graph=T
          i.graph.file=T
          i.graph.file.name= tfile.div$name
          i.graph.title=""
          i.graph.subtitle=""
          i.output = tfile.div$path

          semanas<-dim(i.data)[1]
          anios<-dim(i.data)[2]
          nombre.semana<-rownames(i.data)
          nombre.anios<-colnames(i.data)
          numero.semana<-1:semanas
          n.values<-length(i.param.values)
          
          i.timing.1<-array(dim=c(anios,2))
          resultados.i<-array(dim=c(anios,15,n.values),
                              dimnames=list(year=nombre.anios,indicator=LETTERS[1:15],parameter=i.param.values))
          
          for (i in 1:anios){
            cur<-i.data[i]
            itsnotok<-T
            i.timing.1.1<-etwo$weekno[etwo$season==nombre.anios[i] & etwo$id.tail==1]
            i.timing.1.2<-etwo$weekno[etwo$season==nombre.anios[i] & etwo$id.tail==2]
            i.timing.1.i<-c(i.timing.1.1,i.timing.1.2)
            i.timing.1[i,]<-i.timing.1.i
            curva.map<-mem:::calcular.map(as.vector(as.matrix(cur)))
            for (j in 1:n.values){
              i.param.deteccion<-i.param.values[j]
              i.param.deteccion.label<-format(round(i.param.deteccion,1),digits=3,nsmall=1)
              i.timing.2<-mem:::calcular.optimo(curva.map,2,i.param.deteccion)[4:5]
              resultado.j<-mem:::calcular.indicadores.2.timings(cur,i.timing.1.i,
                                                                i.timing.2,
                                                                i.timing.labels=c("inspection",i.param.deteccion.label),
                                                                i.graph.title="Comparing",
                                                                i.graph.file=F)$indicadores
              resultados.i[i,,j]<-as.numeric(resultado.j)
            }
          }
          resultado<-data.frame(apply(resultados.i,c(3,2),sum,na.rm=T))
          # sensibilidad
          resultado[7]<-resultado[3]/(resultado[3]+resultado[6])
          # especificidad
          resultado[8]<-resultado[5]/(resultado[5]+resultado[4])
          # vpp
          resultado[9]<-resultado[3]/(resultado[3]+resultado[4])
          # vpn
          resultado[10]<-resultado[5]/(resultado[5]+resultado[6])
          # positive likehood ratio
          resultado[11]<-resultado[7]/(1-resultado[8])
          # negative likehood ratio
          resultado[12]<-(1-resultado[7])/resultado[8]
          # percentage agreement/accuracy
          resultado[13]<-(resultado[3]+resultado[5])/(resultado[3]+resultado[4]+resultado[5]+resultado[6])
          # Matthews correlation coefficient
          resultado[14]<-(resultado[3]*resultado[5]-resultado[4]*resultado[6])/sqrt((resultado[3]+resultado[4])*(resultado[3]+resultado[6])*(resultado[5]+resultado[4])*(resultado[5]+resultado[6]))
          # Youdens Index
          resultado[15]<-resultado[7]+resultado[8]-1
          
          resultado[resultado=="NaN"]<-NA
          
          resultados<-data.frame(value=i.param.values,resultado)
          names(resultados)<-c("value",tolower(colnames(resultado.j)))
          
          if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
            rankings.1<-NA
            optimo.1 <- NA
          } else {
            rankings.1 <- rank(-resultados$sensitivity, na.last = T) + rank(-resultados$specificity, na.last = T)
            optimo.1 <- i.param.values[which.min(rankings.1)]
          }
          if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
            rankings.2<-NA
            optimo.2 <- NA
          } else {
            rankings.2 <- rank(-resultados$sensitivity * resultados$specificity, na.last = T)
            optimo.2 <- i.param.values[which.min(rankings.2)]
          }
          if (!any(!is.na(resultados$positive.likehood.ratio))) {
            rankings.3<-NA
            optimo.3 <- NA
          } else {
            rankings.3 <- rank(-resultados$positive.likehood.ratio, na.last = T)
            optimo.3 <- i.param.values[which.min(rankings.3)]
          }
          if (!any(!is.na(resultados$negative.likehood.ratio))) {
            rankings.4<-NA
            optimo.4 <- NA
          } else {
            rankings.4 <- rank(-resultados$negative.likehood.ratio, na.last = T)
            optimo.4 <- i.param.values[which.min(rankings.4)]
          }
          if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
            rankings.5<-NA
            optimo.5 <- NA
          } else {
            qf <- abs(resultados$sensitivity - resultados$specificity)
            qe <- 2 - resultados$sensitivity - resultados$specificity
            qs <- (1 - resultados$sensitivity)^2 + (1 - resultados$specificity)^2
            rankings.5 <- rank(qf) + rank(qe) + rank(qs)
            optimo.5 <- i.param.values[which.min(rankings.5)]
          }
          if (!any(!is.na(resultados$percent.agreement))) {
            rankings.6 <- NA
            optimo.6 <- NA
          } else {
            rankings.6 <- rank(-resultados$percent.agreement, na.last = T)
            optimo.6 <- i.param.values[which.min(rankings.6)]
          }
          if (!any(!is.na(resultados$matthews.correlation.coefficient))) {
            rankings.7 <- NA
            optimo.7 <- NA
          } else {
            rankings.7 <- rank(-resultados$matthews.correlation.coefficient, na.last = T)
            optimo.7 <- i.param.values[which.min(rankings.7)]
          }
          if (!any(!is.na(resultados$youdens.index))) {
            rankings.8 <- NA
            optimo.8 <- NA
          } else {
            rankings.8 <- rank(-resultados$youdens.index, na.last = T)
            optimo.8 <- i.param.values[which.min(rankings.8)]
          }
          
          
          optimum <- data.frame(pos.likehood = optimo.3, neg.likehood = optimo.4, aditive = optimo.1, multiplicative = optimo.2,
                                mixed = optimo.5, percent = optimo.6, matthews=optimo.7, youden=optimo.8)
          
          rankings <- data.frame(pos.likehood = rankings.3, neg.likehood = rankings.4, aditive = rankings.1, multiplicative = rankings.2,
                                 mixed = rankings.5, percent = rankings.6, matthews=rankings.7, youden=rankings.8)
          
          
          optimum.by.inspection.output <- list(optimum = optimum,
                                               rankings = rankings,
                                               insp.data = resultados,
                                               param.data = i.data,
                                               param.param.values = i.param.values,
                                               param.graph=i.graph,
                                               param.graph.file=i.graph.file,
                                               param.graph.file.name=i.graph.file.name,
                                               param.graph.title=i.graph.title,
                                               param.graph.subtitle=i.graph.subtitle,
                                               param.output=i.output)
          
          # Graph all data
          if (i.graph){
            
            if (i.graph.file.name=="") graph.name="inspection analysis" else graph.name<-i.graph.file.name
            
            
            if (i.graph.subtitle!="") graph.title<-paste(i.graph.subtitle," - ",graph.title,sep="")
            if (i.graph.title!="") graph.title<-paste(i.graph.title,"\n",graph.title,sep="")
            
            all.graph.names<-data.frame()
            
            for (i in 1:anios){
              
              graph.title<-nombre.anios[i]
              
              all.graph.names<-rbind(all.graph.names,data.frame(season=graph.title,file=paste0(i.output,"/", graph.name," - ",i,".png"), stringsAsFactors = F))
              
              cur<-i.data[i]
              i.timing.1.i<-i.timing.1[i,]
              curva.map<-mem:::calcular.map(as.vector(as.matrix(cur)))
              i.param.deteccion<-optimum$matthews
              i.param.deteccion.label<-format(round(i.param.deteccion,1),digits=3,nsmall=1)
              i.timing.2<-mem:::calcular.optimo(curva.map,2,i.param.deteccion)[4:5]
              dummmmyyyy<-mem:::calcular.indicadores.2.timings(cur, i.timing.1.i, i.timing.2,
                                                               i.timing.labels=c("inspection",i.param.deteccion.label),
                                                               i.output=i.output,
                                                               i.graph.title=graph.title,
                                                               i.graph.file=i.graph.file,
                                                               i.graph.file.name=paste(graph.name," - ",i,sep=""))
            }
            
            values$optimizegraphs<-all.graph.names
          }
          
          lapply(nombre.anios, function(s){output[[paste0("tbmOptimizeM_",as.character(s),"_image")]] <- renderImage({
            imgfile<-""
            if (NROW(all.graph.names)>0){
              imgtmp<-all.graph.names
              imgtmp2<-subset(imgtmp,imgtmp$season==as.character(s))
              if (NROW(imgtmp2)>0){
                if (file.exists(imgtmp2$file)){
                  imgfile<-imgtmp2$file
                } 
              }
            }          
            gfile<-list(src = imgfile,
                        contentType = 'image/png',
                        width = 800,
                        height = 600,
                        alt = "No image found")
            gfile
          })})
          
          optim<-memgoodness(datfile[selectedcolumns],
                             i.seasons=as.numeric(input$SelectMaximum),
                             i.type.threshold=as.numeric(input$typethreshold),
                             i.tails.threshold=as.numeric(input$ntails),
                             i.type.intensity=as.numeric(input$typeintensity),
                             i.level.intensity=as.numeric(c(input$levelintensitym,input$levelintensityh,input$levelintensityv))/100,
                             i.tails.intensity=as.numeric(input$ntails),
                             i.type.curve=as.numeric(input$typecurve),
                             i.level.curve=as.numeric(input$levelaveragecurve)/100,
                             i.type.other=as.numeric(input$typeother),
                             i.level.other=as.numeric(input$levelaveragecurve)/100,
                             i.method=as.numeric(input$method),
                             i.param=as.numeric(optimum.by.inspection.output$optimum[as.character(input$optimmethod)]),
                             i.n.max=as.numeric(input$nvalues),
                             i.calculation.method = "default",
                             i.goodness.method=as.character(input$validation),
                             i.detection.values = seq(input$paramrange[1],input$paramrange[2],by=0.1),
                             i.weeks.above = 1,
                             i.graph=F,
                             i.min.seasons = 3)$results
          
          fluidRow(
            fluidRow(              
              valueBox(format(round(optim["Sensitivity"], 2), nsmall=2), trloc("Sensitivity"), icon = icon("heartbeat"), width=3, color="yellow"),
              valueBox(format(round(optim["Specificity"], 2), nsmall=2), trloc("Specificity"), icon = icon("heartbeat"), width=3, color="yellow"),
              valueBox(format(round(optim["Positive predictive value"], 2), nsmall=2), trloc("Positive predictive value"), icon = icon("heartbeat"), width=3, color="yellow"),
              valueBox(format(round(optim["Negative predictive value"], 2), nsmall=2), trloc("Negative predictive value"), icon = icon("heartbeat"), width=3, color="yellow")
            ),
            fluidRow(
              valueBox(format(round(optim["Percent agreement"], 2), nsmall=2), trloc("Percent agreement"), icon = icon("heartbeat"), width=3, color="aqua"),
              valueBox(format(round(optim["Matthews correlation coefficient"], 2), nsmall=2), trloc("Matthews correlation coefficient"), icon = icon("heartbeat"), width=3, color="aqua"),
              valueBox(format(round(optim["Youdens Index"], 2), nsmall=2), trloc("Youdens Index"), icon = icon("heartbeat"), width=3, color="aqua"),
              valueBox(format(round(input$param, 2), nsmall=1), trloc("Current parameter"), icon = icon("heartbeat"), width=3, color="red"),
              valueBox(format(round(as.numeric(optimum.by.inspection.output$optimum[as.character(input$optimmethod)]), 2), nsmall=1), trloc("Optimum parameter"), icon = icon("heartbeat"), width=3, color="olive")
              
            ),
            fluidRow(
              
              formattable::renderFormattable({
                if(!is.null(optimum.by.inspection.output$insp.data)){
                  temp1 <- optimum.by.inspection.output$insp.data
                  temp1<-temp1[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient","youdens.index")]
                  names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")
                  rownames(temp1)<-NULL
                  opt.table<-formattable::formattable(temp1, list(
                    "Sensitivity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
                    "Specificity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
                    "Positive predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
                    "Negative predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
                    "Percent agreement" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
                    "Matthews correlation coefficient" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
                    "Youdens Index" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5)
                  ), digits = 2, format = "f")
                  names(opt.table)<-trloc(c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
                  names(attr(opt.table, "formattable")$format[[1]])<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
                }else{
                  temp1<-data.frame(Error=trloc("Number of columns must be greater than 2"),row.names = NULL)
                  opt.table<-formattable::formattable(temp1)
                }
                opt.table
              })
              
              
            )
            
          )
          
        }
      }
      
    }
  })
  
  
  output$tbmOptimizeA <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(is.null(datfile)){
      return(NULL)
    }else{
      tabsetPanel(tabPanel(trloc("Indicators"), uiOutput("tbmOptimizeASummary")),
                  tabPanel(trloc("Detailed"),
                           formattable::formattableOutput("tbmOptimizeADetail"),
                           fluidRow(
                             column(8),
                             column(2,
                                    if (zip.present()){
                                      downloadButton("tbmOptimizeADetail_x","xlsx")
                                    }else if (.Platform$OS.type=="windows"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                    }else if (.Platform$OS.type=="unix"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                    }),
                             column(2,downloadButton("tbmOptimizeADetail_c","csv"))
                           )
                  ),
                  tabPanel(trloc("Graphs"),plotlyOutput("tbmOptimizeAGraph"))
      )
    }
  })
  
  output$tbmOptimizeASummary <- renderUI({
    dataoptim <- data_optim()
    if(is.null(dataoptim)){
      return(NULL)
    }else{
      doptim<-dataoptim$roc.data
      optim<-doptim[doptim$value==as.numeric(dataoptim$optimum[as.character(input$optimmethod)]),]
      fluidRow(
        valueBox(format(round(optim["sensitivity"], 2), nsmall=2), trloc("Sensitivity"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(optim["specificity"], 2), nsmall=2), trloc("Specificity"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(optim["positive.predictive.value"], 2), nsmall=2), trloc("Positive predictive value"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(optim["negative.predictive.value"], 2), nsmall=2), trloc("Negative predictive value"), icon = icon("heartbeat"), width=3, color="yellow"),
        valueBox(format(round(optim["percent.agreement"], 2), nsmall=2), trloc("Percent agreement"), icon = icon("heartbeat"), width=3, color="aqua"),
        valueBox(format(round(optim["matthews.correlation.coefficient"], 2), nsmall=2), trloc("Matthews correlation coefficient"), icon = icon("heartbeat"), width=3, color="aqua"),
        valueBox(format(round(optim["youdens.index"], 2), nsmall=2), trloc("Youdens Index"), icon = icon("heartbeat"), width=3, color="aqua"),
        valueBox(format(round(input$param, 2), nsmall=1), trloc("Current parameter"), icon = icon("heartbeat"), width=3, color="red"),
        valueBox(format(round(as.numeric(dataoptim$optimum[as.character(input$optimmethod)]), 2), nsmall=1), trloc("Optimum parameter"), icon = icon("heartbeat"), width=3, color="olive")
      )
    }
  })
  
  output$tbmOptimizeADetail <- formattable::renderFormattable({
    dataoptim <- data_optim()
    if(!is.null(dataoptim)){
      temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient","youdens.index")]
      names(temp1)<-c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")
      rownames(temp1)<-NULL
      roca.table<-formattable::formattable(temp1, list(
        "Sensitivity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Specificity" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Positive predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Negative predictive value" = fixed_color_bar(color="#FFBBFF",fixedWidth = 100, alpha=0.5),
        "Percent agreement" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
        "Matthews correlation coefficient" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5),
        "Youdens Index" = fixed_color_bar(color="#A5DBEB",fixedWidth = 100, alpha=0.5)
      ), digits = 2, format = "f")
      names(roca.table)<-trloc(c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
      names(attr(roca.table, "formattable")$format[[1]])<-trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
    }else{
      temp1<-data.frame(Error=trloc("Number of columns must be greater than 2"),row.names = NULL)
      roca.table<-formattable::formattable(temp1)
    }
    roca.table
  })
  
  output$tbmOptimizeADetail_x <- downloadHandler(
    filename = function() { paste(input$dataset, '.xlsx', sep='') },
    content = function(file) {
      dataoptim <- data_optim()
      if(!is.null(dataoptim)){
        temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient","youdens.index")]
        names(temp1)<-trloc(c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
        rownames(temp1)<-NULL
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Optimization"),1,32), i.rownames=NA, i.format="xlsx")
      }
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$tbmOptimizeADetail_c <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      dataoptim <- data_optim()
      if(!is.null(dataoptim)){
        temp1<-dataoptim$roc.data[c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient","youdens.index")]
        names(temp1)<-trloc(c("Parameter","Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index"))
        rownames(temp1)<-NULL
        export.mydata(i.data=temp1, i.file = file,
                      i.sheet=substring(trloc("Optimization"),1,32), i.rownames=NA, i.format="csv")
      }
    },
    contentType="text/csv"
  )
  
  output$tbmOptimizeAGraph<- renderPlotly({
    dataoptim <- data_optim()
    if(is.null(dataoptim)){
      z<-NULL
    }else{
      dgraf<-subset(dataoptim$roc.data,select=c("value","sensitivity","specificity","positive.predictive.value","negative.predictive.value","percent.agreement","matthews.correlation.coefficient","youdens.index"))
      names(dgraf)<-c("Parameter", trloc(c("Sensitivity","Specificity","Positive predictive value","Negative predictive value","Percent agreement","Matthews correlation coefficient","Youdens Index")))
      print(dgraf)
      dgrafgg<-melt(dgraf,id="Parameter", value.name = "Value", variable.name = "Indicator")
      print(dgrafgg)
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
        scale_color_manual(values=colors.palette$colSeasons, name=trloc("Indicator")) +
        labs(title = input$textMain, x = input$textX, y = input$textY) +
        ggthemes::theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
      
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
      tabsetPanel(tabPanel(trloc("Data"), DT::dataTableOutput("tbsData")),
                  tabPanel(trloc("Surveillance"),uiOutput("tbsSurveillance"))
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
  options = list(scrollX = TRUE, scrollY = '600px', paging = FALSE, dom = 'Bfrtip', columnDefs=list(list(targets="_all", class="dt-right"))))
  
  output$tbsSurveillance <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if(is.null(datfile)){
      return(NULL)
    }else if(!(input$SelectSurveillance %in% names(datfile))){
      return(NULL)
    }else{
      tabsetPanel(tabPanel(trloc("Week"), plotlyOutput("tbsSurveillanceWeek", width ="100%", height ="100%")),
                  if (animationmethod<4){
                    tabPanel(trloc("Animated"), imageOutput("tbsSurveillanceAnimated"))
                  }else{
                    cat('animation package + GraphicsMagick or ImageMagic or magick package needed for this function to work. Please install it.\n')
                    tabPanel(trloc("Animated"), tableOutput("tbsSurveillanceAnimated_nomagick"))
                  },
                  tabPanel(trloc("Average"),
                           plotlyOutput("tbsSurveillanceAverage", width ="100%", height ="100%")
                           ,
                           fluidRow(
                             column(8),
                             column(2,
                                    if (zip.present()){
                                      downloadButton("tbsSurveillanceAverage_x","xlsx")
                                    }else if (.Platform$OS.type=="windows"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Rtools not found"), icon = icon("file-excel-o"), onclick ="window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                                    }else if (.Platform$OS.type=="unix"){
                                      shiny::actionButton(inputId='noziplink', label=trloc("Zip not found"), icon = icon("file-excel-o"))
                                    }),
                             column(2,downloadButton("tbsSurveillanceAverage_c","csv"))
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
      datfile.plot<-datfile[input$SelectSurveillance]
      max.y<-max(datfile.plot,na.rm=T)
      if (as.logical(input$preepidemicthr)) max.y<-max(max.y,e.thr[1],na.rm=T)
      if (as.logical(input$postepidemicthr)) max.y<-max(max.y,e.thr[2],na.rm=T)
      if (as.logical(input$intensitythr)) max.y<-max(max.y,i.thr,na.rm=T)
      n.surveillance.week<-min((1:(NROW(datfile)))[SurveillanceWeek==rownames(datfile)])
      colors.palette<-generate_palette(i.number.series=NA,
                                       i.colObservedLines=input$colObservedLines,
                                       i.colObservedPoints=input$colObservedPoints,
                                       i.colEpidemicStart=input$colEpidemicStart,
                                       i.colEpidemicStop=input$colEpidemicStop,
                                       i.colThresholds=input$colThresholds,
                                       i.colSeasons=input$colSeasons,
                                       i.colEpidemic=input$colEpidemic)
      cat("animated gif> begin\n")
      cat("animated gif> creating the frames\n")
      plot.list<-list()
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
        plot.list[[i]]<-p$plot
      }
      imgfilegif<-paste0(tempdir(),"/animated.gif")
      if (animationmethod==1){
        cat("animated gif> using animation package with GraphicsMagic\n")
        requireNamespace("animation", quietly = TRUE)
        cat(paste0("animated gif> creating\t\t",imgfilegif,"\n"))
        animation::saveGIF(for (i in 1:n.surveillance.week) print(plot.list[[i]]), movie.name=imgfilegif, interval = 0.5, autobrowse=F, ani.width=640, ani.height=480, loop=T, convert = "gm convert")
        cat(paste0("animated gif> saving\t\t",imgfilegif,"\n"))
        cat("animated gif> end\n")        
      }else if (animationmethod==2){
        cat("animated gif> using animation package with ImageMagic\n")
        requireNamespace("animation", quietly = TRUE)
        cat(paste0("animated gif> creating\t\t",imgfilegif,"\n"))
        animation::saveGIF(for (i in 1:n.surveillance.week) print(plot.list[[i]]), movie.name=imgfilegif, interval = 0.5, autobrowse=F, ani.width=640, ani.height=480, loop=T)
        cat(paste0("animated gif> saving\t\t",imgfilegif,"\n"))
        cat("animated gif> end\n")        
      }else if (animationmethod==3){
        cat("animated gif> using magick package\n")
        requireNamespace("magick", quietly = TRUE)
        for (i in 1:n.surveillance.week){
          imgfile<-paste(tempdir(),"/animatedplot_",i,".png",sep="")
          ggsave(imgfile, plot=plot.list[[i]], width=8, height=6, dpi=150)
          if (i==1) imgfilem<-magick::image_read(imgfile) else imgfilem<-c(imgfilem,magick::image_read(imgfile))
          cat(paste0("animated gif> image\t",i,"/",n.surveillance.week,"\t",imgfile,"\n"))
        }
        cat(paste0("animated gif> creating\t\t",imgfilegif,"\n"))
        anim <- magick::image_animate(imgfilem, fps = 2)
        cat(paste0("animated gif> saving\t\t",imgfilegif,"\n"))
        magick::image_write(anim,path=imgfilegif)
        cat("animated gif> end\n")        
      }
      outdistAnimated<-list(src = imgfilegif,
                            contentType = 'image/gif',
                            width = 800,
                            height = 600,
                            alt = "This is alternate text")
    }
    outdistAnimated
  }, deleteFile = TRUE)
  
  output$tbsSurveillanceAnimated_nomagick <- renderTable({
    data.show<-data.frame(var="magick package needed for this function to work. Please install it.")
    names(data.show)=""
    data.show
  })
  
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
        names(datfile.plot)<-c(input$SelectSurveillance,trloc(c("Lower interval","Average curve","Upper interval")))
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
                         i.level.curve=as.numeric(input$levelaveragecurve)/100,
                         i.type.other=as.numeric(input$typeother),
                         i.level.other=as.numeric(input$levelaveragecurve)/100,
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
            names(datfile.plot)<-c(input$SelectSurveillance,trloc(c("Lower interval","Average curve","Upper interval")))
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
                             i.level.curve=as.numeric(input$levelaveragecurve)/100,
                             i.type.other=as.numeric(input$typeother),
                             i.level.other=as.numeric(input$levelaveragecurve)/100,
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
              names(temp2)<-trloc(names(temp2))
              export.mydata(i.data=temp2, i.file = file,
                            i.sheet=substring(trloc("Average curve"),1,32), i.rownames=trloc("Week no"), i.format="xlsx")
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
            names(datfile.plot)<-c(input$SelectSurveillance,trloc(c("Lower interval","Average curve","Upper interval")))
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
                             i.level.curve=as.numeric(input$levelaveragecurve)/100,
                             i.type.other=as.numeric(input$typeother),
                             i.level.other=as.numeric(input$levelaveragecurve)/100,
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
              names(temp2)<-trloc(names(temp2))
              export.mydata(i.data=temp2, i.file = file,
                            i.sheet=substring(trloc("Average curve"),1,32), i.rownames=trloc("Week no"), i.format="csv")
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
        tabsetPanel(tabPanel(trloc("Data"), DT::dataTableOutput("tbvData")),
                    tabPanel(trloc("Seasons"), plotlyOutput("tbvSeasons", width ="100%", height ="100%")),
                    tabPanel(trloc("Series"),plotlyOutput("tbvSeries", width ="100%", height ="100%")),
                    tabPanel(trloc("Timing"),uiOutput("tbvTiming"))
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
                         i.level.curve=as.numeric(input$levelaveragecurve)/100,
                         i.type.other=as.numeric(input$typeother),
                         i.level.other=as.numeric(input$levelaveragecurve)/100,
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
                        i.level.curve=as.numeric(input$levelaveragecurve)/100,
                        i.type.other=as.numeric(input$typeother),
                        i.level.other=as.numeric(input$levelaveragecurve)/100,
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
  ### UI STRUCTURE
  #####################################
  
  output$uifile = renderUI({
    popify(
      fileInput('file', label=h4(trloc("Load file"), tags$style(type = "text/css", "#q1 {vertical-align: top;}"), bsButton("file_b", label = "", icon = icon("question"), style = "info", size = "extra-small")), accept = c("csv","dat","prn","txt","xls","xlsx","mdb","accdb", "rdata"))
      , title = trloc("Load file"), content = trloc("memapp is able to read text, excel, access and R"), placement = "right", trigger = "hover", options = list(container = "body"))
  })
  
  output$uiDataset = renderUI({
    box(title=trloc("Dataset"), status = "warning", solidHeader = FALSE, width = 12, background = "navy", collapsible = TRUE, collapsed=FALSE,
        uiOutput("uidataset"),
        uiOutput("uifirstWeek"),
        uiOutput("uilastWeek"),
        uiOutput("uitransformation")
    )
  })
  
  output$uidataset = renderUI({
    popify(
      selectInput('dataset', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Dataset")), size=1, selectize = FALSE, choices = getDatasets(), selected = NULL)
      , title = trloc("Dataset"), content = trloc("If the format is able to store different datasets, select the one you want to open"), placement = "right", trigger = "hover", options = list(container = "body"))
  })
  
  output$uifirstWeek = renderUI({
    popify(
      selectInput("firstWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("First Week")), size=1, selectize = FALSE, choices = getWeeksOriginal(), selected = head(getWeeksOriginal(),1))
      , title = trloc("First Week"), content = trloc("First week of the datasets surveillance period"),                                    placement = "right", trigger = "hover", options = list(container = "body"))
  })
  
  output$uilastWeek = renderUI({
    popify(
      selectInput("lastWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Last Week")), size=1, selectize = FALSE, choices = getWeeksOriginal(), selected = tail(getWeeksOriginal(),1))
      , title = trloc("Last Week"), content = trloc("Last week of the datasets surveillance period"),                                     placement = "right", trigger = "hover", options = list(container = "body"))
  })
  
  output$uitransformation = renderUI({
    transformation.list<-list("No transformation"=1, "Odd"=2, "Fill missings"=3, "Loess"=4, "Two waves (observed)"=5, "Two waves (expected)"=6)
    names(transformation.list)<-trloc(c("No transformation", "Odd", "Fill missings", "Loess", "Two waves (observed)", "Two waves (expected)"))
    popify(
      selectInput("transformation", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Transform")), size=1, selectize = FALSE, choices = transformation.list, selected = 1)
      , title = trloc("Transform"), content = trloc("Select the transformation to apply to the original data"),                            placement = "right", trigger = "hover", options = list(container = "body"))
  })
  
  output$uiModel = renderUI({
    box(title=trloc("Model"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
        popify(
          selectInput("SelectFrom", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("From")), size=1, selectize = FALSE, choices = getSeasons(), selected = head(getSeasons(), 1))
          , title = trloc("From"), content = trloc("First season to include in the model selection"), placement = "right", trigger = "hover", options = list(container = "body")),
        popify(
          selectInput("SelectTo", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("To")), size=1, selectize = FALSE, choices = getSeasons(), selected = tail(getSeasons(), 2)[1])
          , title = trloc("To"), content = trloc("Last season to include in the model selection"), placement = "right", trigger = "hover", options = list(container = "body")),
        popify(
          selectInput('SelectExclude', h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Exclude")), multiple = TRUE, choices = getSeasons(), selected=NULL)
          , title = trloc("Exclude"), content = trloc("Select any number of seasons to be excluded from the model"), placement = "right", trigger = "hover", options = list(container = "body")),
        popify(
          numericInput("SelectMaximum", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Maximum seasons")), 10, step=1)
          , title = trloc("Maximum seasons"), content = trloc("Maximum number of seasons to be used in the model.<br>Note that this will probably override the rest options, since it will restrict data to the last number of seasons from the selection already made with From/To/Exclude.<br>For influenza it is not recommended to use more than 10 seasons"), placement = "right", trigger = "hover", options = list(container = "body"))
    )
  })
  
  output$uiSurveillance = renderUI({
    box(title=trloc("Surveillance"), status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed=TRUE,
        popify(
          selectInput("SelectSurveillance", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Season")), size=1, selectize = FALSE, choices = getSeasons(), selected = tail(getSeasons(),1))
          , title = trloc("Season"), content = trloc("Season you want to use for surveillance applying the MEM thresholds.<br>This season can be incomplete.<br> It is recommended not to use the surveillance season in the model selection"), placement = "right", trigger = "hover", options = list(container = "body")),
        popify(
          selectInput("SelectSurveillanceWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Week")), size=1, selectize = FALSE, choices = getWeeksFiltered(), selected = tail(getWeeksFiltered(),1))
          , title = trloc("Week"), content = trloc("Week you want to create the surveillance graph for. It can be any week from the first week of the surveillance season to the last one that have data"), placement = "right", trigger = "hover", options = list(container = "body")),
        popify(
          selectInput("SelectSurveillanceForceEpidemic", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Force epidemic start")), size=1, selectize = FALSE, choices = c("", getWeeksFiltered()), select = "")
          , title = trloc("Force epidemic start"), content = trloc("Chose a week to force the start of the epidemic period.<br>The epidemic will start at the week selected and not at the first week over the epidemic threshold"), placement = "right", trigger = "hover", options = list(container = "body"))
    )
  })
  
  output$uiVisualize = renderUI({
    box(title=trloc("Visualize"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
        popify(
          selectInput('SelectSeasons', h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Seasons")), choices = getSeasons(), multiple = TRUE, selected=NULL)
          , title = trloc("Seasons"), content = trloc("Select any number of seasons to display series, seasons and timing graphs and to apply thresholds from the current model.<br>To delete a season click on it and press delete on your keyboard"), placement = "right", trigger = "hover", options = list(container = "body"))
    )
  })
  
  output$uiThresholds = renderUI({
    box(title=trloc("Thresholds"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
        popify(
          checkboxInput("preepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Pre-epidemic threshold")), value = TRUE)
        , title = trloc("Pre-epidemic threshold"), content = trloc("Check this tickbox if you want to include epidemic thresholds in the graphs.<br>This is a global option that will work on most graphs"), placement = "right", trigger = "hover", options = list(container = "body")),
        popify(
          checkboxInput("postepidemicthr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Post-epidemic threshold")), value = FALSE)
        , title = trloc("Post-epidemic threshold"), content = trloc("Check this tickbox if you want to include post-epidemic thresholds in the graphs.<br>This  is a global option that will work on most graphs"), placement = "right", trigger = "hover", options = list(container = "body")),
        popify(
          checkboxInput("intensitythr", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Intensity thresholds/levels")), value = TRUE)
        , title = trloc("Intensity thresholds/levels"), content = trloc("Check this tickbox if you want to include intensity thresholds in the graphs.<br>This  is a global option that will work on most graphs"), placement = "right", trigger = "hover", options = list(container = "body"))
    )
  })

  output$uiTitle = renderUI({
    fluidPage(
      tagList(
        singleton(tags$head(
          tags$link(rel="stylesheet", type="text/css",href="busyIndicator.css")
        ))
        ,div(class="shinysky-busy-indicator",p(trloc("Calculation in progress. This may take a while...")),img(src="ajaxloaderq.gif"))
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
      titlePanel(h1(trloc("The Moving Epidemic Method Web Application")))
    )
  })
  
  output$uiProcedures = renderUI({
    tabBox(
      title = h3(trloc("Procedures"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), width = 12, height = "800px",
      tabPanel(h4(trloc("Check & describe"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("Check data series, timing and describe the data"), uiOutput("tbData")),
      tabPanel(h4(trloc("Model"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("Summary, graphs, goodness and optimization of the MEM model"), uiOutput("tbModel")),
      tabPanel(h4(trloc("Surveillance"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("Surveillance tools"), uiOutput("tbSurveillance")),
      tabPanel(h4(trloc("Visualize"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("Visualize different sets of data with a MEM model"), uiOutput("tbVisualize"))
    )
  })
  
  output$uiTextoptions = renderUI({
    box(
      title=trloc("Text options"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
      popify(
        textInput("textMain", label = h6(trloc("Main title"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = trloc("Main title"))
        , title = trloc("Main title"), content = trloc("Change the main title in most graphs"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        textInput("textY", label = h6(trloc("Y-axis"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = trloc("Y-axis"))
        , title = trloc("Y-axis"), content = trloc("Change the y-axis label in most graphs"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        textInput("textX", label = h6(trloc("X-axis"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = trloc("X-axis"))
        , title = trloc("X-axis"), content = trloc("Change the x-axis label in most graphs"), placement = "left", trigger = "hover", options = list(container = "body"))
    )
  })
  
  output$uiGraphoptions = renderUI({
    box(
      title=trloc("Graph options"), status = "primary", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
      popify(
        selectInput("colObservedLines", h6(trloc("Observed (line)"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Observed (line)"), content = trloc("Color of the line of observed data"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("colObservedPoints", h6(trloc("Observed (points)"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Observed (points)"), content = trloc("Color of the points of observed data"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("colEpidemicStart", h6(trloc("Epidemic start"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Epidemic start"), content = trloc("Color of the point of the epidemic start marker"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("colEpidemicStop", h6(trloc("Epidemic end"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Epidemic end"), content = trloc("Color of the point of the epidemic end marker"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("colThresholds", h6(trloc("Thresholds palette"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",rownames(brewer.pal.info),colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Thresholds palette"), content = trloc("Palette used to generate color for epidemic and intensity thresholds"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("colLevels", h6(trloc("Levels palette"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",rownames(brewer.pal.info),colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Levels palette"), content = trloc("Palette used to generate color for intensity levels"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("colSeasons", h6(trloc("Seasons palette"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",rownames(brewer.pal.info),colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Seasons palette"), content = trloc("Palette used to generate the colors of the lines of the series graphs and other graphs with multiple lines"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("colEpidemic", h6(trloc("Timing palette"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = c("default",rownames(brewer.pal.info),colors()), size=1, selectize = FALSE, selected = "default")
        , title = trloc("Timing palette"), content = trloc("Palette used to generate the colors of the points of pre, epidemic and post markers in timing graphs"), placement = "left", trigger = "hover", options = list(container = "body"))
    )
  })
  
  output$uiMEMoptions = renderUI({
    method.list<-list("Original method"=1, "Fixed criterium method"=2, "Slope method"=3, "Second derivative method"=4)
    names(method.list)<-trloc(c("Original method", "Fixed criterium method", "Slope method", "Second derivative method"))
    nvalues.list<-list("30 in total"=-1,"All"=0,"1/season"=1,"2/season"=2,"3/season"=3,"4/season"=4,"5/season"=5,"6/season"=6,"7/season"=7,"8/season"=8,"9/season"=9,"10/season"=10)
    names(nvalues.list)<-trloc(c("30 in total","All","1/season","2/season","3/season","4/season","5/season","6/season","7/season","8/season","9/season","10/season"))
    validation.list<-list("Cross"="cross", "Sequential"="sequential")
    names(validation.list)<-trloc(c("Cross", "Sequential"))
    optimmethod.list<-list("Positive likehood"="pos.likehood", "Negative likehood"="neg.likehood", "Aditive"="aditive", "Multiplicative"="multiplicative", "Mixed"="mixed", "Percent agreement"="percent","Matthews Correlation Coefficient"="matthews","Youden's Index"="youden")
    names(optimmethod.list)<-trloc(c("Positive likehood", "Negative likehood", "Aditive", "Multiplicative", "Mixed", "Percent agreement","Matthews Correlation Coefficient","Youden's Index"))
    type.list<-list("Arithmetic mean and mean confidence interval"=1, "Geometric mean and mean confidence interval"=2, "Median and the KC Method to calculate its confidence interval"=3, "Median and bootstrap confidence interval"=4, "Arithmetic mean and point confidence interval"=5, "Geometric mean and point confidence interval"=6)
    names(type.list)<-trloc(c("Arithmetic mean and mean confidence interval", "Geometric mean and mean confidence interval", "Median and the KC Method to calculate its confidence interval", "Median and bootstrap confidence interval", "Arithmetic mean and point confidence interval", "Geometric mean and point confidence interval"))
    
    box(
      title=trloc("MEM options"), status = "danger", solidHeader = FALSE, width = 12,  background = "navy", collapsible = TRUE, collapsed=TRUE,
      h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Timing")),
      popify(
        selectInput("method", h6(trloc("Method for timing"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = method.list, size=1, selectize = FALSE, selected = 2)
        , title = trloc("Method for timing"), content = trloc("<b>Original</b>: uses the process shown in the original paper.<br><b>Fixed criterium</b>: uses the slope of the MAP curve fo find the optimum, which is the point where the slope is lower than a predefined value.<br><b>Slope</b>: calculates the slope of the MAP curve, but the optimum is the one that matches the global mean slope.<br><b>Second derivative</b>: calculates the second derivative and equals to zero to search an inflexion point in the original curve"), placement = "left", trigger = "hover", options = list(container = "body")
      ),
      conditionalPanel(condition = "input.method == 2",
                       popify(
                         numericInput("param", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Slope parameter")), 2.8, step=0.1)
                         , title = trloc("Slope parameter"), content = trloc("Slope parameter used in fixed criterium method"), placement = "left", trigger = "hover", options = list(container = "body"))
      ),
      h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), "Thresholds"),
      fluidRow(
        column(6,
               popify(
                 selectInput("nvalues", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Values per season")), choices = nvalues.list, size=1, selectize = FALSE, selected = -1)
                 , title = trloc("Values per season"), content = trloc("Number of values taken each season for calculate thresholds. If -1, a total of 30 points are used (30/numberofseasons). If 0, all available points are used"), placement = "left", trigger = "hover", options = list(container = "body"))
        ),
        column(6,
               popify(
                 numericInput("ntails", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Tails")), 1, step=1, min = 1, max = 2)
                 , title = trloc("Tails"), content = trloc("Choose if you want to use one-tailed or two-tailed confidence intervals for thresholds"), placement = "left", trigger = "hover", options = list(container = "body"))
        )
      ),
      popify(
        selectInput("typethreshold", h6(trloc("Epidemic threshold"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 5)
        , title = trloc("Epidemic threshold"), content = trloc("Method for calculating the epidemic threshold"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("typeintensity", h6("Intensity thresholds", tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 6)
        , title = trloc("Intensity thresholds"), content = trloc("Method for calculating the intensity threshold"), placement = "left", trigger = "hover", options = list(container = "body")),
      fluidRow(
        column(4,
               popify(
                 numericInput("levelintensitym", h6(trloc("Medium lvl"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 40, step=0.5, min = 0.5, max = 99.5)
                 , title = trloc("Medium lvl"), content = trloc("Level of the confidence interval used to calculate the medium threshold"), placement = "left", trigger = "hover", options = list(container = "body"))
        ),
        column(4,
               popify(
                 numericInput("levelintensityh", h6(trloc("High lvl"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 90, step=0.5, min = 0.5, max = 99.5)
                 , title = trloc("High lvl"), content = trloc("Level of the confidence interval used to calculate the high threshold"), placement = "left", trigger = "hover", options = list(container = "body"))
        ),
        column(4,
               popify(
                 numericInput("levelintensityv", h6(trloc("Very high lvl"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 97.5, step=0.5, min = 0.5, max = 99.5)
                 , title = trloc("Very high lvl"), content = trloc("Level of the confidence interval used to calculate the very high threshold"), placement = "left", trigger = "hover", options = list(container = "body"))
        )
      ),
      h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Goodness & optimize")),
      fluidRow(
        column(6,
               popify(
                 selectInput("validation", h6(trloc("Validation"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = validation.list, size=1, selectize = FALSE, selected = "cross")
                 , title = trloc("Validation"), content = trloc("Cross: Extracts one season and the model is calculated with the remaining seasons.<br>Sequential: Extract a season and the model is calculated with previous seasons only"), placement = "left", trigger = "hover", options = list(container = "body"))
        ),
        column(6,
               popify(
                 selectInput("optimmethod", h6(trloc("Optimization method"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = optimmethod.list, size=1, selectize = FALSE, selected = "matthews")
                 , title = trloc("Optimization method"), content = trloc("Method to choose the optimum parameter"), placement = "left", trigger = "hover", options = list(container = "body"))
        )
      ),
      popify(
        sliderInput("paramrange", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Parameter range")), min = 0.1, max = 10, value = c(2, 4), step=0.1)
        , title = trloc("Parameter range"), content = trloc("Range of possible of values of the slope parameter used by goodness and optimize functions"), placement = "left", trigger = "hover", options = list(container = "body")
      ),
      h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Other")),
      popify(
        selectInput("typecurve", h6(trloc("Average curve CI."), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 2)
        , title = trloc("Average curve CI."), content = trloc("Method for calculating the average curve confidence intervals"), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        selectInput("typeother", h6(trloc("Other CI."), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size=1, selectize = FALSE, selected = 3)
        , title = trloc("Other CI."), content = trloc("Method for calculating other confidence intervals: duration, epidemic percentage, epidemic start, etc."), placement = "left", trigger = "hover", options = list(container = "body")),
      popify(
        numericInput("levelaveragecurve", h6(trloc("Average curve/Other CI. level"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), 95.0, step=0.5, min = 0.5, max = 99.5)
        , title = trloc("Average curve/Other CI. level"), content = trloc("Level of the confidence interval used to calculate the average curve and other intervals"), placement = "left", trigger = "hover", options = list(container = "body"))
    )
  })
  
  output$uiSupport = renderUI({
    box(
      title=trloc("Support"), status = "info", solidHeader = TRUE, width = 12,  background = "black", collapsible = TRUE, collapsed=TRUE,
      h5(a(trloc("Technical manual"), href="https://drive.google.com/file/d/0B0IUo_0NhTOoX29zc2p5RmlBUWc/view?usp=sharing", target="_blank")),
      h5(a(trloc("Submit issues"), href="https://github.com/lozalojo/memapp/issues", target="_blank")),
      popify(
      checkboxInput("advancedfeatures", label = h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("Show advanced features")), value = FALSE)
      , title = trloc("Show advanced features"), content = trloc("Show advanced features of memapp"), placement = "left", trigger = "hover", options = list(container = "body"))
    )
  })
  
  output$uiLanguage = renderUI({
    popify(
    h4(trloc("Language"), tags$style(type = "text/css", "#q1 {vertical-align: top;}"))
    , title = trloc("Language"), content = trloc("Change the language of the application"), placement = "left", trigger = "hover", options = list(container = "body"))
  })

  #####################################
  ### ENDING
  #####################################
  
  session$onSessionEnded(function() {
    stopApp()
  })
})

