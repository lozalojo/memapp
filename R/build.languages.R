#' creates the translation file to be used by the app
#'
#' @keywords internal
#' 
#' @importFrom tidyr spread
#' @importFrom dplyr select
#' @importFrom magrittr %>%
build.languages <- function(){
  translation.loc<-c("lang","inst/shinyapp/lang",paste0(.libPaths(),"/memapp/shinyapp/lang"))
  translation.dir<-utils::head(translation.loc[dir.exists(translation.loc)],1)
  translation.fil<-paste0(translation.dir,"/translation.bin")
  langs<-get.languages()
  cat("List of available languages:\n",paste0(paste0(langs$filename,"/",langs$locale),collapse="\n"),"\n")
  translationContent<-do.call(rbind,lapply(langs$filename,function(x) read.language(x)))
  # To avoid R cmd check as for original, lang, translated as dplyr:select accept verbatim variable as input (not character)
  original=lang=translated=NULL
  translation <- translationContent %>% 
   dplyr::select(original, lang, translated) %>% 
   tidyr::spread(lang, translated, drop = FALSE, fill = NA)
  save(translation, file = translation.fil)
  cat(paste0("Translation file saved to: ",tools::file_path_as_absolute(translation.fil)," (",NROW(translation)," items)"),"\n")
  cat("Language file built\n")
}
