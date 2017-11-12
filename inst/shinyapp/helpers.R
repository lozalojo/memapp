#####################################
### CUSTOM FUNCTIONS
#####################################

generate_palette <- function(i.number.series=NA,
                             i.colObservedLines=NULL,
                             i.colObservedPoints=NULL,
                             i.colEpidemicStart=NULL,
                             i.colEpidemicStop=NULL,
                             i.colThresholds=NULL,
                             i.colLevels=NULL,
                             i.colSeasons=NULL,
                             i.colEpidemic=NULL){
  params.default<-list(colObservedLines="#808080",
                       colObservedPoints="#000000",
                       colEpidemicStart="#FF0000",
                       colEpidemicStop="#40FF40",
                       colThresholds=c("#8c6bb1","#88419d","#810f7c","#4d004b","#c0c0ff"),
                       colLevels=c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c"),
                       colSeasons="Accent",
                       colEpidemic=c("#00C000","#800080","#FFB401")
  )
  if (is.na(i.number.series)) i.number.series<-10
  if (is.null(i.colObservedLines)) i.colObservedLines<-"default" else if (is.na(i.colObservedLines)) i.colObservedLines<-"default"
  if (is.null(i.colObservedPoints)) i.colObservedPoints<-"default" else if (is.na(i.colObservedPoints)) i.colObservedPoints<-"default"
  if (is.null(i.colEpidemicStart)) i.colEpidemicStart<-"default" else if (is.na(i.colEpidemicStart)) i.colEpidemicStart<-"default"
  if (is.null(i.colEpidemicStop)) i.colEpidemicStop<-"default" else if (is.na(i.colEpidemicStop)) i.colEpidemicStop<-"default"
  if (is.null(i.colThresholds)) i.colThresholds<-"default" else if (is.na(i.colThresholds)) i.colThresholds<-"default"
  if (is.null(i.colLevels)) i.colLevels<-"default" else if (is.na(i.colLevels)) i.colLevels<-"default"
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
  if(i.colLevels %in% colors()){
    i.colLevels<-rep(rgb(t(col2rgb(i.colLevels))/255),5)
  }else if(i.colLevels %in% rownames(brewer.pal.info)){
    i.colLevels<-RColorBrewer::brewer.pal(7,i.colLevels)[2:6]
  }else{
    i.colLevels<-params.default$colLevels
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
  colors.final<-list(colObservedLines=i.colObservedLines, colObservedPoints=i.colObservedPoints,
                     colEpidemicStart=i.colEpidemicStart, colEpidemicStop=i.colEpidemicStop,
                     colThresholds=i.colThresholds, colLevels=i.colLevels, colSeasons=i.colSeasons,colEpidemic=i.colEpidemic
  )
  colors.final
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
  if (is.null(i.from)) i.from=""
  if (is.null(i.to)) i.to=""
  if (is.na(i.from)) i.from=""
  if (is.na(i.to)) i.to=""
  
  indexes<-1:length(i.names)
  toinclude<-indexes[i.names %in% i.include]
  if (!(i.from=="") & (i.from %in% i.names)) from<-grep(i.from,i.names,fixed=T) else from<-1
  if (!(i.to=="") & (i.to %in% i.names)) to<-grep(i.to,i.names,fixed=T) else to<-length(i.names)
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
      dividetext<-matrix(unlist(strsplit(i.plotly$x$data[[i]]$text,"<br>|<br />")),nrow=length(i.plotly$x$data[[i]]$text), byrow=T)
      i.plotly$x$data[[i]]$text<-paste(i.xname, ": ",i.weeklabels,"<br />",sub(i.yname,i.labels[sequ[i]],dividetext[,2]),sep="")
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
  if (.Platform$OS.type=="windows") i.plotly<-fixlatin(i.plotly)
  return(i.plotly)
}

fixlatin<-function(i.plotly){
  o.plotly<-i.plotly
  for (i in 1:length(i.plotly$x$data)){
    o.plotly$x$data[[i]]$text<-iconv(i.plotly$x$data[[i]]$text, from="UTF-8", to="LATIN1")
  }
  o.plotly
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

export.mydata<-function(i.data, i.file, i.sheet=NA, i.rownames=NA, i.format="xlsx"){
  if (is.na(i.sheet)) i.sheet<-"data"
  if (!is.na(i.rownames)){
    i.data$dummy<-row.names(i.data)
    i.data<-i.data[c(NCOL(i.data), 1:(NCOL(i.data)-1))]
    names(i.data)[1]<-i.rownames
  }
  if (i.file!=""){
    if (i.format=="xlsx"){
      openxlsx::write.xlsx(i.data, file=i.file, rowNames = FALSE, colNames = TRUE, keepNA=FALSE, sheetName=i.sheet, asTable = TRUE)
      cat("export> Exported to ",tools::file_path_as_absolute(i.file)," (",i.sheet,")\n")
    }else if (i.format=="csv"){
      write.table(i.data, file=i.file, row.names = FALSE, col.names = TRUE, sep=",", dec=".", na = "")
      cat("export> Exported to ",tools::file_path_as_absolute(i.file),"\n")
    }     
  }
}

# impossible to find a solution to the input file problem for all the OS at the same time
# choose.file only works for windows
# file.choose does not force the extension to be of a given type
# tkgetSaveFile goes to the background and stays hidden until you focus it with the mouse

# Configure a zip extractor in the system, required for openxlsx saving, it is installed with Rtools

set.rzip<-function(){
  cat("function/setupzip> begin\n")
  if (.Platform$OS.type=="windows"){
    cat("function/setupzip> Windows system detected\n")
    if (file.exists("c:\\Rtools\\bin\\zip.exe")){
      ziploc<-"c:\\Rtools\\bin\\zip.exe"
      cat("function/setupzip> zip found at default dir ",ziploc,"\n")
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
          cat("function/setupzip> zip found at path ",ziploc,"\n")
        }else{
          ziploc<-""
          cat("function/setupzip> no zip found\n")
        }
      }else{
        ziploc<-""
        cat("function/setupzip> no zip found\n")
      }
    }
  }else if (.Platform$OS.type=="unix"){
    cat("function/setupzip> *nix system detected\n")
    if (file.exists("/usr/bin/zip")){
      ziploc<-"/usr/bin/zip"
      cat("function/setupzip> zip found at ",ziploc,"\n")
    }else{
      ziploc<-""
      cat("function/setupzip> no zip found\n")
    }
  }else{
    cat("function/setupzip> No windows or *nix system detected\n")
    ziploc<-""
    cat("function/setupzip> no zip found\n")
  }
  cat("function/setupzip> end\n")  
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
  cat("function/animation.method> begin\n")  
  if (.Platform$OS.type=="windows"){
    cat("function/animation.method> Windows system detected\n")
    path.env<-tolower(Sys.getenv("PATH"))
    if ("animation" %in% rownames(installed.packages()) & grepl("graphicsmagick", path.env, ignore.case = T, fixed=T)){
      # GraphicsMagick program + animation package
      cat("function/animation.method> GraphicsMagick+animation detected. Using animation package\n")
      animation.method<-1
    }else if ("animation" %in% rownames(installed.packages()) & grepl("imagemagick", path.env, ignore.case = T, fixed=T)){
      # ImageMagick program + animation package
      cat("function/animation.method> ImageMagick+animation detected. Using animation package\n")
      animation.method<-2
    }else if ("magick" %in% rownames(installed.packages())){
      # magick package
      cat("function/animation.method> magick detected. Using magick package\n")
      animation.method<-3
    }else{
      cat("function/animation.method> No GraphicsMagick+animation nor ImageMagick+animation nor magick detected. No animation\n")
      animation.method<-4      
    }
  }else if (.Platform$OS.type=="unix"){
    cat("function/animation.method> *nix system detected\n")
    if ("animation" %in% rownames(installed.packages()) & file.exists("/usr/bin/gm")){
      # GraphicsMagick program + animation package
      cat("function/animation.method> GraphicsMagick+animation detected. Using animation package\n")
      animation.method<-1
    }else if ("animation" %in% rownames(installed.packages()) & file.exists("/usr/bin/convert")){
      # ImageMagick program + animation package
      cat("function/animation.method> ImageMagick+animation detected. Using animation package\n")
      animation.method<-2
    }else if("magick" %in% rownames(installed.packages())){
      # magick package
      cat("function/animation.method> magick detected. Using magick package\n")
      animation.method<-3
    }else{
      cat("function/animation.method> No GraphicsMagick+animation nor ImageMagick+animation nor magick detected. No animation\n")
      animation.method<-4      
    }
  }else{
    cat("function/animation.method> No windows or *nix system detected\n")
    animation.method<-4
  }
  cat("function/animation.method> end\n")  
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
  data<-i.data
  results <- do.call("rbind", by(data, data[i.column], tail.order, i.n=2, i.order=i.order))
  return(results)
}

# locale funcions

translation.dir<-function(){
  translation.loc<-c("lang","inst/shinyapp/lang",paste0(.libPaths(),"/memapp/shinyapp/lang"))
  utils::head(translation.loc[dir.exists(translation.loc)],1)
}

get.languages<-function(){
  langfiles<-data.frame(filename=tools::file_path_sans_ext(list.files(translation.dir(), ".*\\.txt")), stringsAsFactors = F)
  locales<-read.locales.table()
  languages<-dplyr::inner_join(locales,langfiles,by="filename")
  # fix for linux locales
  if (.Platform$OS.type=="unix"){
    localesinstalled=
    languages %>%
      select(-localelinux) %>%
      left_join(select(get.linux.locales(), -encoding), by=c('language.iso_639_1','country.iso_3166'))
  }
  languages
}

read.locales.table<-function(){
  locales<-utils::read.delim(paste0(translation.dir(),"/localestable.txt"),header=T,sep=";",row.names=NULL,fill=T,colClasses="character", as.is=T) %>%
    extract(filename, into=c('language.iso_639_1', 'v1', 'country.iso_3166','v2','v3','encoding'), 
            '^([:alpha:]{2})(_([:alpha:]{2}))?(([\\.]+)([^\\.]+))?$', remove=F) %>%
    select(-v1,-v2,-v3) %>%
    filter(!(is.na(language.iso_639_1) & is.na(country.iso_3166))) %>%
    mutate(encoding=if_else(is.na(encoding),"",tolower(encoding)),
           language.iso_639_1=if_else(is.na(language.iso_639_1),"",tolower(language.iso_639_1)),
           country.iso_3166=if_else(is.na(country.iso_3166),"",toupper(country.iso_3166)))
}

get.linux.locales<-function(){
  locales<-data.frame(localelinux=system("locale -a ", intern = TRUE), stringsAsFactors = F) %>%
    extract(localelinux, into=c('language.iso_639_1', 'v1', 'country.iso_3166','v2','v3','encoding'), 
            '^([:alpha:]{2})(_([:alpha:]{2}))?(([\\.]+)([^\\.]+))?$', remove=F) %>%
    select(-v1,-v2,-v3) %>%
    filter(!(is.na(language.iso_639_1) & is.na(country.iso_3166))) %>%
    mutate(encoding=if_else(is.na(encoding),"",tolower(encoding)),
           language.iso_639_1=if_else(is.na(language.iso_639_1),"",tolower(language.iso_639_1)),
           country.iso_3166=if_else(is.na(country.iso_3166),"",toupper(country.iso_3166)))
}


read.language <- function(i.filename){
  langs<-get.languages()
  lfile<-paste0(translation.dir(),"/",i.filename,".txt")
  if (file.exists(lfile)){
    lines <- paste(readLines(lfile, n = -1, warn=F),collapse="")
    if (stringi::stri_enc_isascii(lines)) {
      myencoding<-"ASCII"
    }else{
      myencoding <- stringi::stri_enc_detect(lines)[[1]]$`Encoding`[1]
    }
    translation<-utils::read.delim(lfile,header=T,sep=";",row.names=NULL,fill=T,colClasses="character", as.is=T, encoding = myencoding)
    names(translation)<-c("original","translated")
    translation$filename<-i.filename
  }else{
    translation<-data.frame()
  }
  translation
}

build.languages <- function(){
  cat("function/build.languages> begin\n")
  translation.fil<-paste0(translation.dir(),"/translation.bin")
  langs<-get.languages()
  cat("function/build.languages> List of available languages:\n",paste0(paste0(langs$filename,"\t",langs$lang_name),collapse="\n"),"\n")
  translationContent<-do.call(rbind,lapply(langs$filename,function(x) read.language(x)))
  # To avoid R cmd check as for original, lang, translated as dplyr:select accept verbatim variable as input (not character)
  original=filename=translated=NULL
  translation <- translationContent %>% 
    dplyr::select(original, filename, translated) %>% 
    tidyr::spread(filename, translated, drop = FALSE, fill = NA)
  save(translation, file = translation.fil)
  cat(paste0("function/build.languages> Translation file saved to: ",tools::file_path_as_absolute(translation.fil)," (",NROW(translation)," items)"),"\n")
  cat("function/build.languages> Language file built\n")
  cat("function/build.languages> end\n")
}

get.r.versions <- function(){
  list(
    r=as.character(R.version$version.string),
    platform=as.character(R.version$platform),
    mem=if("mem" %in% rownames(installed.packages())) as.character(packageVersion("mem")) else "not installed",
    memapp=if("memapp" %in% rownames(installed.packages())) as.character(packageVersion("memapp")) else "not installed"
  )
}