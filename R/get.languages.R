#' get a list of available languages
#'
#' @keywords internal
get.languages<-function(i.where){
  tools::file_path_sans_ext(list.files(i.where, ".*\\.txt"))
}
