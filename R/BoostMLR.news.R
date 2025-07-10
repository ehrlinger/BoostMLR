#--------------------------------------------------
# BoostMLR news




#' Show the NEWS file
#' 
#' Show the NEWS file of the BoostMLR package.
#' 
#' 
#' @param ... Further arguments passed to or from other methods.
#' @return None.
#' @author Amol Pande and Hemant Ishwaran
#' @keywords documentation
BoostMLR.news <- function(...) {
  newsfile <- file.path(system.file(package="BoostMLR"), "NEWS")
  file.show(newsfile)
}
