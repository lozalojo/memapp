#' reads a translation file
#'
#' @keywords internal
read.language <- function(i.lang){
  libs<-paste0(.libPaths(),"/memapp/shinyapp/lang")
  libs.ok<-utils::head(libs[dir.exists(libs)])
  langs<-get.languages()
  lfile<-paste0(libs.ok,"/",i.lang,".txt")
  if (file.exists(lfile)){
    lines <- paste(readLines(lfile, n = -1, warn=F),collapse="")
    if (stringi::stri_enc_isascii(lines)) {
      myencoding<-"ASCII"
    }else{
      myencoding <- stringi::stri_enc_detect(lines)[[1]]$`Encoding`[1]
    }
    translation<-utils::read.delim(lfile,header=F,sep=";",row.names=NULL,fill=T,colClasses="character", as.is=T, encoding = myencoding)
    names(translation)<-c("original","translated")
    translation$lang<-langs$lcidstring[langs$filename==i.lang]
  }else{
    translation<-data.frame()
  }
  translation
}
