#' reads a translation file
#'
#' @keywords internal
read.language <- function(i.lang){
  translation.loc<-c("lang","inst/shinyapp/lang",paste0(.libPaths(),"/memapp/shinyapp/lang"))
  translation.dir<-utils::head(translation.loc[dir.exists(translation.loc)],1)
  langs<-get.languages()
  lfile<-paste0(translation.dir,"/",i.lang,".txt")
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
