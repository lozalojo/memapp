#' creates the translation file to be used by the app
#'
#' @keywords internal
build.language <- function(i.lang){
  libs<-paste0(.libPaths(),"/memapp/shinyapp/lang")
  libs.ok<-utils::head(libs[dir.exists(libs)])
  # cat(libs.ok,"\n")
  langs<-get.languages(libs.ok)
  cat("List of available languages:\n",paste(langs,collapse="\t"),"\n")
  if (!(i.lang %in% langs)){
    cat("Language not found",i.lang,", setting to default en-GB\n")
    i.lang<-"en-GB"
  }else{
    cat("Setting language to",i.lang,"\n")
  } 
  lfile<-paste0(libs.ok,"/",i.lang,".txt")
  # cat(lfile,"\n")
  lines <- paste(readLines(lfile, n = -1, warn=F),collapse="")
  if (stringi::stri_enc_isascii(lines)) {
    myencoding<-"ASCII"
  }else{
    myencoding <- stringi::stri_enc_detect(lines)[[1]]$`Encoding`[1]
  }
  translation<-utils::read.delim(lfile,header=F,sep=";",row.names=NULL,fill=T,colClasses="character", as.is=T, encoding = myencoding)
  names(translation)<-c("original","translated")
  cat("Language built\n")
  save(translation, file = paste0(libs.ok,"/translation.bin"))
}
