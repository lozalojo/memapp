#' get a list of available languages
#'
#' @keywords internal
get.languages<-function(){
  libs<-paste0(.libPaths(),"/memapp/shinyapp/lang")
  libs.ok<-utils::head(libs[dir.exists(libs)])
  tools::file_path_sans_ext(list.files(libs.ok, ".*\\.txt"))
}
