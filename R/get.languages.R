#' get a list of available languages
#'
#' @keywords internal
get.languages<-function(){
  translation.loc<-c("lang","inst/shinyapp/lang",paste0(.libPaths(),"/memapp/shinyapp/lang"))
  translation.dir<-utils::head(translation.loc[dir.exists(translation.loc)],1)
  langfiles<-data.frame(filename=tools::file_path_sans_ext(list.files(translation.dir, ".*\\.txt")), stringsAsFactors = F)
  langfiles$lcidstring<-tolower(langfiles$filename)
  locales<-read.delim(paste0(translation.dir,"/locales.txt"),header=T,sep=";",row.names=NULL,fill=T,colClasses="character", as.is=T)
  languages<-merge(locales,langfiles,by="lcidstring")
  languages
}
