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
  if (i.colObservedLines=="default") i.colObservedLines<-params.default$colObservedLines else i.colObservedLines<-rgb(t(col2rgb(i.colObservedLines))/255)
  if (i.colObservedPoints=="default") i.colObservedPoints<-params.default$colObservedPoints else i.colObservedPoints<-rgb(t(col2rgb(i.colObservedPoints))/255)
  if (i.colEpidemicStart=="default") i.colEpidemicStart<-params.default$colEpidemicStart else i.colEpidemicStart<-rgb(t(col2rgb(i.colEpidemicStart))/255)
  if (i.colEpidemicStop=="default") i.colEpidemicStop<-params.default$colEpidemicStop else i.colEpidemicStop<-rgb(t(col2rgb(i.colEpidemicStop))/255)
  # Fifth to Seventh are palettes that I must create
  if(i.colThresholds %in% colors()){
    i.colThresholds<-rep(rgb(t(col2rgb(i.colThresholds))/255),5)
  }else if(i.colThresholds %in% rownames(brewer.pal.info)){
    i.colThresholds<-RColorBrewer::brewer.pal(7,i.colThresholds)[2:6]
  }else{
    i.colThresholds<-params.default$colThresholds
  }
  if(i.colSeasons %in% colors()){
    i.colSeasons<-rep(rgb(t(col2rgb(i.colSeasons))/255),i.number.series)
  }else if(i.colSeasons %in% rownames(brewer.pal.info)){
    i.colSeasons <- colorRampPalette(RColorBrewer::brewer.pal(max(3,min(8,i.number.series)),i.colSeasons))(i.number.series)
  }else{
    i.colSeasons <- colorRampPalette(RColorBrewer::brewer.pal(max(3,min(8,i.number.series)),params.default$colSeasons))(i.number.series)
  }
  if (i.colEpidemic %in% colors()){
    i.colEpidemic<-rep(rgb(t(col2rgb(i.colEpidemic))/255),3)
  }else if(i.colEpidemic %in% rownames(brewer.pal.info)){
    i.colEpidemic<-RColorBrewer::brewer.pal(5,i.colEpidemic)[2:4]
  }else{
    i.colEpidemic<-params.default$colEpidemic
  }
  # Last one is a number between 0 and 1
  # colTransparency<-input$colTransparency
  # if (is.null(colTransparency)) colTransparency<-1 else if (is.na(colTransparency)) colTransparency<-1
  # i.colEpidemicStart<-mem:::add.alpha(i.colEpidemicStart,alpha=0.4)
  # i.colEpidemicStop<-mem:::add.alpha(i.colEpidemicStop,alpha=0.4)
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
      scale_shape_manual(values=i.shapes, name="Legend", labels=labels) +
      scale_color_manual(values=i.colors, name="Legend", labels=labels) +
      scale_fill_manual(values=i.fills, name="Legend", labels=labels) +
      scale_size_manual(values=i.sizes, name="Legend", labels=labels) +
      scale_linetype_manual(values=i.linetypes, name="Legend", labels=labels) +
      scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = i.textMain, x = i.textX, y = i.textY) +
      ggthemes::theme_few() +
      theme(plot.title = element_text(hjust = 0.5))
    p<-list(plot=gplot, gdata=dgrafgg)
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
      temp1<-stringr::str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-stringr::str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
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
    }else if (fileextension %in% c("rda","rdata")){
      temp2<-read.data.rdata(i.file, filenameextension, i.dataset)
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
      seasons<-data.frame(names(datasetread),matrix(stringr::str_match(names(datasetread),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?"),nrow=NCOL(datasetread),byrow=F)[,-1],stringsAsFactors = F)
    }else{
      seasons<-data.frame(t(c(names(datasetread),stringr::str_match(names(datasetread),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[-1])),stringsAsFactors = F)
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
      temp1<-stringr::str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-stringr::str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
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
      temp1<-stringr::str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-stringr::str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    cat("read_data> Excel 97-2003 file detected: ",filenameextension,"\n",sep="")
    #wb <- XLConnect::loadWorkbook(i.file)
    #datasets<-XLConnect::getSheets(wb)
    # readxl needs the extension to be xls, sigh!
    i.file.xls<-tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".xls")
    file.copy(i.file,i.file.xls)
    datasets<-readxl::excel_sheets(i.file.xls)
    rm("i.file.xls")
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
      #temp1<-as.character(XLConnect::readWorksheet(wb, sheet = i.dataset, header=F, colTypes=XLC$DATA_TYPE.STRING, endRow=1))
      #datasetread<-XLConnect::readWorksheet(wb, sheet = i.dataset, rownames=NA, colTypes=XLC$DATA_TYPE.NUMERIC)
      #names(datasetread)<-temp1
      datasetread<-as.data.frame(readxl::read_xls(i.file, sheet = i.dataset, col_types= "numeric"), stringsAsFactors = F)
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

read.data.access<-function(i.file, i.file.name=NA, i.dataset=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-stringr::str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-stringr::str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
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
      if (!mdbtools.present()){
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
          # encodings<-readr::guess_encoding(filecsv, n_max = -1)
          # encodings<-encodings[order(encodings$confidence,decreasing = T),]
          # myencoding<-as.character(encodings$encoding[1])
          lines <- paste(readLines(filecsv, n = -1),collapse="")
          if (stringi::stri_enc_isascii(lines)) {
            myencoding<-"ASCII"
          }else{
            myencoding <- stringi::stri_enc_detect(lines)[[1]]$`Encoding`[1]
          }
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
      temp1<-stringr::str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-stringr::str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    datasets<-filename
    n.datasets<-length(datasets)
    # text files
    # detect encoding
    # temp1<-readr::guess_encoding(i.file, n_max = -1)
    # temp1<-temp1[order(temp1$confidence,decreasing = T),]
    # myencoding<-as.character(temp1$encoding[1])
    lines <- paste(readLines(i.file, n = -1),collapse="")
    if (stringi::stri_enc_isascii(lines)) {
      myencoding<-"ASCII"
    }else{
      myencoding <- stringi::stri_enc_detect(lines)[[1]]$`Encoding`[1]
    }
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
      temp1<-stringr::str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-stringr::str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
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

read.data.rdata<-function(i.file, i.file.name=NA, i.dataset=NA){
  if (!file.exists(i.file)){
    datasets=NULL
    datasetread=NULL
    dataweeks=NULL
    cat("read_data> Warning: file not found\n")
  }else{
    if (is.na(i.file.name)){
      temp1<-stringr::str_match(i.file,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
      temp1[is.na(temp1)]<-""
      filename<-temp1[1,3]
      fileextension<-tolower(temp1[1,5])
    }else{
      temp1<-stringr::str_match(i.file.name,"^(.*)\\.([^\\.]*)$")
      filename<-temp1[1,2]
      fileextension<-tolower(temp1[1,3])
    }
    filenameextension<-paste(filename, fileextension, sep=".")
    cat("read_data> RData file detected: ",filenameextension,"\n",sep="")
    rdaenv = local({load(i.file); environment()})
    datasets<-names(rdaenv)
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
      datasetread<-rdaenv[[i.dataset]]
      dataweeks<-as.numeric(row.names(datasetread))
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
    seasons<-data.frame(i.names,matrix(stringr::str_match(i.names,"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?"),nrow=length(i.names),byrow=F)[,-1],stringsAsFactors = F)
  }else{
    seasons<-data.frame(t(c(i.names,stringr::str_match(i.names,"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[-1])),stringsAsFactors = F)
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

fixed_color_bar <- function (color = "lightgray", fixedWidth=150, alpha=0.5,...){
  formattable::formatter("span", style = function(x) ifelse(is.na(x),
                                                            formattable::style(color = "white"),
                                                            formattable::style(display = "inline-block", 
                                                                               direction = "rtl", 
                                                                               `border-radius` = "4px", 
                                                                               `padding-right` = "2px", 
                                                                               `background-color` = formattable::csscolor(add.alpha.to.color(color, alpha)), 
                                                                               width = paste(fixedWidth*formattable::proportion(x, na.rm = T), "px", sep=""), 
                                                                               ...)
  )
  )
}

add.alpha.to.color <- function(col, alpha=1){
  if(missing(col)) stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))  
}

# export functions

# export.mydata<-function(i.data, i.sheet=NA, i.rownames=NA, i.format="xlsx"){
#   if (is.na(i.sheet)) i.sheet<-"data"
#   if (!is.na(i.rownames)){
#     i.data$dummy<-row.names(i.data)
#     i.data<-i.data[c(NCOL(i.data), 1:(NCOL(i.data)-1))]
#     names(i.data)[1]<-i.rownames
#   }
#   if (i.format=="xlsx"){
#     o.file <- file.selector("xlsx")
#     if (o.file!="") openxlsx::write.xlsx(i.data, rowNames = FALSE, colNames = TRUE, keepNA=FALSE, sheetName=i.sheet, 
#                          asTable = TRUE, file=o.file)
#   }else if (i.format=="csv"){
#     o.file <- file.selector("csv")
#     if (o.file!="") write.table(i.data, row.names = FALSE, col.names = TRUE, sep=",", dec=".", na = "", 
#                 file=o.file)
#   } 
# }

export.mydata<-function(i.data, i.file, i.sheet=NA, i.rownames=NA, i.format="xlsx"){
  if (is.na(i.sheet)) i.sheet<-"data"
  if (!is.na(i.rownames)){
    i.data$dummy<-row.names(i.data)
    i.data<-i.data[c(NCOL(i.data), 1:(NCOL(i.data)-1))]
    names(i.data)[1]<-i.rownames
  }
  if (i.format=="xlsx"){
    if (i.file!="") openxlsx::write.xlsx(i.data, file=i.file, rowNames = FALSE, colNames = TRUE, keepNA=FALSE, sheetName=i.sheet, asTable = TRUE)
  }else if (i.format=="csv"){
    if (i.file!="") write.table(i.data, file=i.file, row.names = FALSE, col.names = TRUE, sep=",", dec=".", na = "")
  } 
}

# impossible to find a solution to the input file problem for all the OS at the same time
# choose.file only works for windows
# file.choose does not force the extension to be of a given type
# tkgetSaveFile goes to the background and stays hidden until you focus it with the mouse

# file.selector<-function(i.format){
#   if (i.format=="xlsx"){
#     if (.Platform$OS.type=="windows"){
#       o.file <- choose.files(caption="Save As...",  filters = c("Excel 2007+ files (.xlsx)","*.xlsx"))
#     }else if (capabilities()["tcltk"]){
#       o.file <- tcltk::tclvalue(tcltk::tkgetSaveFile(initialfile="", title="Save as...", 
#                                                      defaultextension=paste(".", i.format, sep="")))
#     }else{
#       o.file <- file.choose()
#     }
#   }else if (i.format=="csv"){
#     if (.Platform$OS.type=="windows"){
#       o.file<-choose.files(caption="Save As...",  filters = c("Comma Delimited Files (.csv)","*.csv"))
#     }else if (capabilities()["tcltk"]){
#       o.file <- tcltk::tclvalue(tcltk::tkgetSaveFile(initialfile="", title="Save as...", 
#                                                      defaultextension=paste(".", i.format, sep="")))
#     }else{
#       o.file <- file.choose()
#     }
#   }else{
#     o.file<-""
#   }
#   o.file
# }

# Configure a zip extractor in the system, required for openxlsx saving, it is installed with Rtools

set.rzip<-function(){
  if (.Platform$OS.type=="windows"){
    cat("Windows system detected\n")
    if (file.exists("c:\\Rtools\\bin\\zip.exe")){
      ziploc<-"c:\\Rtools\\bin\\zip.exe"
      cat("zip found at default dir ",ziploc,"\n")
    }else{
      temp1<-Sys.getenv("PATH")
      if (grepl("rtools", tolower(temp1))){
        temp2<-as.numeric(gregexpr("rtools", tolower(temp1))[[1]])
        temp3<-c(0,as.numeric(gregexpr(";",temp1)[[1]]),nchar(temp1)+1)
        temp6<-unlist(lapply(temp2,function(x) {
          temp4<-(1:length(temp3))[temp3[temp3>=x][1]==temp3]
          temp5<-substr(temp1,temp3[temp4-1]+1,temp3[temp4]-1)
        }))
        temp7<-unlist(lapply(temp6,function(x){
          file.exists(paste(x,"\\zip.exe",sep=""))
        }))
        if (any(temp7)){
          ziploc<-paste(temp6[temp7][1],"\\zip.exe",sep="")
          cat("zip found at path ",ziploc,"\n")
        }else{
          ziploc<-""
          cat("no zip found\n")
        }
      }else{
        ziploc<-""
        cat("no zip found\n")
      }
    }
  }else if (.Platform$OS.type=="unix"){
    cat("*nix system detected\n")
    if (file.exists("/usr/bin/zip")){
      ziploc<-"/usr/bin/zip"
      cat("zip found at ",ziploc,"\n")
    }else{
      ziploc<-""
      cat("no zip found\n")
    }
  }else{
    cat("No windows or *nix system detected\n")
    ziploc<-""
    cat("no zip found\n")
  }
  Sys.setenv(R_ZIPCMD = ziploc)
}

# extract path, filename and extension

extract.pfe<-function(i.file){
  if (is.na(i.file)){
    extract.pfe.output<-NULL
  }else{
    temp1<-gsub("\\","/",i.file, fixed=T)
    temp2<-stringr::str_match(temp1,"^(?:(.*/))?([^[/\\.]]*)(?:(\\.([^\\.]*)))?$")
    temp2[is.na(temp2)]<-""
    extract.pfe.output<-list()
    extract.pfe.output$param.file<-temp2[1,1]
    if (substring(temp2[1,2],nchar(temp2[1,2]),nchar(temp2[1,2]))=="/") extract.pfe.output$path<-substring(temp2[1,2],1,nchar(temp2[1,2])-1) else extract.pfe.output$path<-temp2[1,2]
    extract.pfe.output$name<-temp2[1,3]
    extract.pfe.output$extension<-temp2[1,5]
  }
  rm("temp1","temp2")
  extract.pfe.output
}

# check if a zip extractor is installed

zip.present<-function() file.exists(Sys.getenv("R_ZIPCMD"))

mdbtools.present<-function() file.exists("/usr/bin/mdb-tables") | file.exists("/usr/local/bin/mdb-tables")

# check what animation method has to be used

animation.method<-function(){
  if (.Platform$OS.type=="windows"){
    cat("Windows system detected\n")
    path.env<-Sys.getenv("PATH")
    if ("animation" %in% rownames(installed.packages()) & grepl("GraphicsMagick", path.env, fixed=T)){
      # GraphicsMagick program + animation package
      cat("GraphicsMagick+animation detected. Using animation package\n")
      animation.method<-1
    }else if ("animation" %in% rownames(installed.packages()) & grepl("ImageMagick", path.env, fixed=T)){
      # ImageMagick program + animation package
      cat("ImageMagick+animation detected. Using animation package\n")
      animation.method<-2
    }else if ("magick" %in% rownames(installed.packages())){
      # magick package
      cat("magick detected. Using magick package\n")
      animation.method<-3
    }else{
      cat("No GraphicsMagick+animation nor ImageMagick+animation nor magick detected. No animation\n")
      animation.method<-4      
    }
  }else if (.Platform$OS.type=="unix"){
    cat("*nix system detected\n")
    if ("animation" %in% rownames(installed.packages()) & file.exists("/usr/bin/gm")){
      # GraphicsMagick program + animation package
      cat("GraphicsMagick+animation detected. Using animation package\n")
      animation.method<-1
    }else if ("animation" %in% rownames(installed.packages()) & file.exists("/usr/bin/convert")){
      # ImageMagick program + animation package
      cat("ImageMagick+animation detected. Using animation package\n")
      animation.method<-2
    }else if("magick" %in% rownames(installed.packages())){
      # magick package
      cat("magick detected. Using magick package\n")
      animation.method<-3
    }else{
      cat("No GraphicsMagick+animation nor ImageMagick+animation nor magick detected. No animation\n")
      animation.method<-4      
    }
  }else{
    cat("No windows or *nix system detected\n")
    animation.method<-4
  }
  # animation.method<-3
  return(animation.method)
}

# functions for the optimize plots

tail.order<-function(i.data, i.n, i.order){
  res<-tail(i.data, n=i.n)
  res<-res[order(res[i.order]),]
  res$id.tail<-1:NROW(res)
  res
}

extract.two<-function(i.data, i.order, i.column){
  # data<-unique(i.data, fromLast=T)
  data<-i.data
  results <- do.call("rbind", by(data, data[i.column], tail.order, i.n=2, i.order=i.order))
  return(results)
}


