#' get a list of available languages
#'
#' @keywords internal
get.languages<-function(){
  libs<-paste0(.libPaths(),"/memapp/shinyapp/lang")
  libs.ok<-utils::head(libs[dir.exists(libs)])
  langfiles<-data.frame(filename=tools::file_path_sans_ext(list.files(libs.ok, ".*\\.txt")), stringsAsFactors = F)
  langfiles$lcidstring<-tolower(langfiles$filename)
  locales<-read.delim(paste0(libs.ok,"/locales.txt"),header=T,sep=";",row.names=NULL,fill=T,colClasses="character", as.is=T)
  languages<-merge(locales,langfiles,by="lcidstring")
  languages
}
