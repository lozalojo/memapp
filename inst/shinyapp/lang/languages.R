setwd("H:/R/Moving Epidemic Method/memapp/inst/shinyapp/lang")

library(tidyverse)
library(openxlsx)

ficheros <- data.frame(file=list.files(pattern="^[[:alnum:]]{2}_[[:alnum:]]{2}.txt$"), stringsAsFactors = F) %>%
  mutate(filename=tools::file_path_sans_ext(file))

txttoxlsx <- function(){
  filexlsx <- "languages.xlsx"
  if (file.exists(filexlsx)) wb <- loadWorkbook(filexlsx) else wb <- createWorkbook()
  ficheros <- data.frame(file=list.files(pattern="^[[:alnum:]]{2}_[[:alnum:]]{2}.txt$"), stringsAsFactors = F) %>%
    mutate(filename=tools::file_path_sans_ext(file))
  for (i in 1:NROW(ficheros)){
    temp1<-read.delim(ficheros$file[i], header=T, sep=";", quote="", encoding="UTF-8")
    if (!(ficheros$filename[i] %in% sheets(wb))) addWorksheet(wb, ficheros$filename[i])
    writeData(wb, ficheros$filename[i], temp1)
  }
  saveWorkbook(wb, filexlsx, overwrite = TRUE)
}

writeUtf8 <- function(x, fileo, bom=F) {
  temp1 <- data.frame(line="Original;Translated", stringsAsFactors = F) %>%
    bind_rows(x %>%
                mutate(line=paste(Original,Translated, sep=";"))) %>%
    summarise(lineall=paste(line,collapse="\n")) %>%
    pull(lineall)
  con <- file(fileo, "wb")
  if(bom) writeBin(charToRaw('\xEF\xBB\xBF'), con, endian="little")
  writeBin(charToRaw(temp1), con, endian="little")
  close(con)
}

xlsxtotxt <- function(){
  filexlsx <- "languages.xlsx"
  wb <- loadWorkbook(filexlsx)
  hojas <- data.frame(hoja=sheets(wb), stringsAsFactors = F) %>%
    mutate(hojaok=grepl("^[[:alnum:]]{2}_[[:alnum:]]{2}$", hoja)) %>%
    filter(hojaok)
  for (i in 1:NROW(hojas)){
    x <- read.xlsx(filexlsx, hojas$hoja[i])
    fileo <- file.path("resultados", paste0(hojas$hoja[i],".txt"))
    writeUtf8(x, fileo)
    #write.table(temp1,fileo,sep=";",row.names=F,col.names=T,quote=F,fileEncoding="UTF-8")
  } 
}

txttoxlsx()
xlsxtotxt()




