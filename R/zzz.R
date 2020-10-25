# Load options ------------------------------------------------------------
.onLoad <- function(libname, pkgname) {
  op <- options()
  dv.ops <- list(
    datavyur.folder=datavyur_data_folder(),
    datavyur.classlist=list()
  )
  toset <- !(names(dv.ops) %in% names(op))
  if (any(toset)) options(dv.ops[toset])
  return(invisible())
}
