#' creates the translation file to be used by the app
#'
#' @keywords internal
build.languages <- function(){
  libs<-paste0(.libPaths(),"/memapp/shinyapp/lang")
  libs.ok<-utils::head(libs[dir.exists(libs)])
  # cat(libs.ok,"\n")
  langs<-get.languages()
  cat("List of available languages:\n",paste0(paste0(langs$filename,"/",langs$locale),collapse="\n"),"\n")
  translationContent<-do.call(rbind,lapply(langs$lcidstring,function(x) read.language(x)))
  translation<-reshape2::dcast(translationContent, original~lang, fun.aggregate=NULL, value.var="translated")
  # translation$translated<-translation[i.lang]
  cat("Language file built\n")
  save(translation, file = paste0(libs.ok,"/translation.bin"))
}
