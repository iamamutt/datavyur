# Package info ------------------------------------------------------------

#' datavyur: An R Package for managing exported Datavyu files
#' 
#' A set of R functions to easily manage coded data exported 
#' from the Datavyu software
#' 
#' To see the documentation on how to use this package, run the following
#' code in the console:
#' \cr\cr \code{vignette("tutorial", package="datavyur")}. \cr\cr
#' This will pull up the R vignette "tutorial" and demonstrate 
#' how to use the set of exported functions below.
#' 
#' @section Exported functions:
#' \code{\link{check_codes}} \cr
#' \code{\link{check_timestamps}} \cr
#' \code{\link{datavyu_col_search}} \cr
#' \code{\link{datavyu_dat}} \cr
#' \code{\link{import_column}} \cr
#' \code{\link{merge_nested}} \cr
#' \code{\link{ms2time}} \cr
#' \code{\link{multi_merge}} \cr
#' \code{\link{ordinal_align}} \cr
#' \code{\link{r2datavyu}} \cr
#' \code{\link{temporal_align}} \cr
#' \code{\link{ts2frame}} \cr
#' \cr \cr
#' For more details, see \url{https://github.com/iamamutt/datavyu}
#' @import data.table
#' @docType package
#' @name datavyur
NULL

# Load options ------------------------------------------------------------

.onLoad <- function(libname, pkgname)
{
  
  startupText <- paste0(
    "\nSee tutorial for a brief introduction on how to use this package",
    "by typing the code below into the console:\n",
    'vignette("tutorial", package = "datavyur")'
  )
  
  message(startupText)
  
  op <- options()
  dv.ops <- list(
    datavyur.folder = system.file("extdata", package="datavyur"),
    datavyur.classlist = NULL
  )
  toset <- !(names(dv.ops) %in% names(op))
  if(any(toset)) options(dv.ops[toset])
  return(invisible())
}


