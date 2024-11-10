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

replace.in.file <- function(input, output, search, replace){
  x <- readLines(input, encoding="UTF-8", warn=FALSE)
  y <- gsub( search, replace, x, fixed = T)
  con <- file(output, encoding="UTF-8")
  writeLines(y, con)
  close(con)
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

# txttoxlsx()
# xlsxtotxt()

# input <- "../server.R"

count.in.file <- function(input, output, search, replace){
  x <- readLines(input, encoding="UTF-8", warn=FALSE)
  y <- str_count(x, "trloc\\(\"[^\"]*\"")
  sum(y)
}

extract.in.file <- function(input, output, search, replace){
  x <- readLines(input, encoding="UTF-8", warn=FALSE)
  re <- "trloc\\(\"([^\"]*)\""
  temp1 <- as.character(str_extract_all(x,re, simplify=T))
  temp2 <- gsub(re,"\\1",temp1[temp1!=""])
  y <- data.frame(clave=temp2, stringsAsFactors = F) %>%
    group_by(clave) %>%
    summarise(veces=n()) %>%
    ungroup() %>%
    arrange(clave)
  y
}

# temp1 <- count.in.file("../server.R")

filexlsx <- "languages v2.xlsx"
wb <- loadWorkbook(filexlsx)
hojas <- data.frame(hoja = sheets(wb), stringsAsFactors = F) %>%
  mutate(hojaok = grepl("^[[:alnum:]]{2}_[[:alnum:]]{2}$", hoja)) %>%
  filter(hojaok)
claves <- read.xlsx(filexlsx, "newkeys") %>%
  pivot_longer(
    c(
      "OriginalNew1",
      "OriginalNew2",
      "OriginalNew3",
      "OriginalNew4",
      "OriginalNew5",
      "OriginalNew6"
    )
  ) %>%
  filter(!is.na(value)) %>%
  group_by(Original) %>%
  arrange(Original, name) %>%
  summarise(newkey = str_to_lower(paste0(value, collapse = ".")))

for (i in 1:NROW(claves)){
  cat(i,"/",NROW(claves),"\t")
  buscar = paste0("trloc(\"",claves$Original[i],"\")")
  reemplazar = paste0("trloc(\"",claves$newkey[i],"\")")
  if (i==1){
    replace.in.file("../ui.R", "resultados/ui.R", buscar, reemplazar)
    replace.in.file("../server.R", "resultados/server.R", buscar, reemplazar)
    replace.in.file("../helpers.R", "resultados/helpers.R", buscar, reemplazar)
  }else{
    replace.in.file("resultados/ui.R", "resultados/ui.R", buscar, reemplazar)
    replace.in.file("resultados/server.R", "resultados/server.R", buscar, reemplazar)
    replace.in.file("resultados/helpers.R", "resultados/helpers.R", buscar, reemplazar)
  }
}


temp1 <- extract.in.file("../server.R")
temp2 <- claves %>%
  select(clave=Original, newkey)
temp3 <- temp1 %>%
  full_join(temp2)
temp4 <- temp3 %>%
  filter(is.na(veces)) %>%
  arrange(newkey)
temp5 <- temp3 %>%
  filter(is.na(newkey))

for (i in 1:NROW(hojas)){
  x <- read.xlsx(filexlsx, hojas$hoja[i]) %>%
    left_join(claves) %>%
    mutate(Original=coalesce(newkey,Original)) %>%
    select(-newkey)
  fileo <- file.path("resultados", paste0(hojas$hoja[i],".txt"))
  writeUtf8(x, fileo)
} 



