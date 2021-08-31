# These are the default column names that should exist when exporting from datavyu
default_csv_names <- function() {
  c(
    file="character",
    column="character",
    onset="numeric",
    offset="numeric",
    ordinal="numeric"
  )
}

# a placeholder function to use when supplying a data processing function
pass <- function(x, ...) x

# will rename columns numerically if duplicates exist
check_duplicated_code_fields <- function(.data, to_check=NULL) {
  
  if (is.null(to_check)) {
    to_check <- names(default_csv_names())
    to_check <- to_check[to_check != "column"]
  }

  for (col in to_check) {
    dt_names <- names(.data)
    has_file_cols <- dt_names == col
    if (any(has_file_cols) && sum(has_file_cols) > 1L) {
      others <- which(has_file_cols)[-1]
      dt_names[others] <- paste0(col, seq_along(others))
      names(.data) <- dt_names
      warning(col, " code field used in ", stats::na.omit(.data[[col]])[1L], call.=FALSE)
    }
  }
  .data
}


is_file <- function(x) {
  if (is_none(x)) return(FALSE)
  file.exists(x) & !dir.exists(x)
}

# None=NULL or 0-length objects
is_none <- function(x) {
  is.null(x) || length(x) < 1L
}

# return RHS if LHS is None
`%|%` <- function(lhs, rhs) {
  if (is_none(lhs)) {
    return(rhs)
  }
  lhs
}

as_type <- function(x, type) {
  type <- match.arg(type, c("numeric", "double", "integer", "logical", "character"))
  storage.mode(x) <- type
  x
}

non_empty_names <- function(x, na.rm=TRUE) {
  obj_names <- names(x)
  obj_names <- obj_names[nzchar(obj_names)]
  if (!na.rm) {
    return(obj_names)
  }
  Filter(Negate(is.na), obj_names)
}

has_names <- function(x, na.rm=TRUE) {
  !is_none(non_empty_names(x, na.rm))
}

is_real_list <- function(x) {
  inherits(x, c("list", "pairlist"))
}

is_named_list <- function(x) {
  is_real_list(x) && has_names(x)
}

# range and duration subfunctions
range_conditions <- function(x) {
  tmin <- tmax <- NULL
  (x >= tmin & x <= tmax) # & !is.na(x)
}

duration_condition <- function(x, y) {
  tmin <- tmax <- NULL
  z <- y - x
  z >= 0 & abs(z) <= tmax - tmin
}


as_dtbl <- function(.data, copy=FALSE, keys) {
  if (!is.data.table(.data)) {
    d <- data.table::as.data.table(.data)
  } else {
    d <- data.table::copy(.data)
  }

  if (!missing(keys)) {
    if (!is_none(keys) && all(is.na(keys))) {
      data.table::setkey(d)
    } else {
      data.table::setkeyv(d, keys)
    }
  }
  d
}

grab_within <- function(.file, .column, .onset, .offset, .data) {
  onset <- offset <- NULL
  .data <- .data[file == .file & onset >= .onset & offset < .offset,]
  .data[, `:=`(file=NULL)]
  glue_codes_to_names(.data, .column, except="file")
  .data[]
}

#' Multiple data merge
#'
#' Merge data into a single data structure from a list of data.frames/tables
#'
#' @param data_list List of separate data.frames/tables to merge
#' @param ... Additional arguments passed to \code{merge}
#'
#' @return A data.frame/data.table, depending on the input data in the list
#'
#' @examples
#' \dontrun{
#' d1 <- fake_datavyu_data()$parenthands
#' d2 <- as.data.frame(fake_datavyu_data()$childhands)
#' d3 <- fake_datavyu_data(n2=50)$parenthands
#' data_list <- list(d1, d2, d3)
#' merged_data <- multi_merge(data_list, all=TRUE)
#' }
multi_merge <- function(data_list, ...) {
  Reduce(function(x, y) {
    merge(x, y, ...)
  }, data_list)
}

# on:off sequence
on_off_to_seq <- function(onset_frame, offset_frame) {
  if (length(offset_frame) < 1L || length(onset_frame) < 1L || offset_frame < onset_frame) {
    frames <- NA_integer_
  } else {
    frames <- seq(onset_frame, offset_frame, 1L)
  }
  return(as_type(frames, "integer"))
}


# directory="~"; filename="datavyur_test.csv"
generate_ruby_opf_script <- function(directory, filename) {
  script_txt <- 
  "
  #--------------------------------------------------------------
  # This is an automatically generated script that will convert
  # the .csv file generated from datavyur to a Datavyu .opf file
  #--------------------------------------------------------------
  
  require 'Datavyu_API.rb'
  
  begin
  open_c = OpenController.new
  db = open_c.open_data_store(\"%s\")
  db = open_c.get_data_store
  print \"SUCCESSFULLY Opened a project with #{db.get_all_variables.length.to_s} columns!\"
  pj = Project.new()
  pj.setDatabaseFileName(\"dataStore\")
  pj.setProjectName(\"datavyur\")
  save_file = java.io.File.new(\"%s\")
  save_c = SaveController.new
  save_c.save_project(save_file, pj, db)
  end
  "
  
  directory <- normalizePath(directory, mustWork = FALSE, winslash = "/")
  filename <- basename(tools::file_path_sans_ext(filename))
  file_stem <- file.path(directory, "datavyur_export", filename)
  output <- list(
    text="",
    csv=paste0(file_stem, ".csv"),
    opf=paste0(file_stem, ".opf"),
    rb=paste0(file_stem, ".rb")
  )
  
  output$text <- sprintf(script_txt, output$csv, output$opf)
  output
}

verbose_msg <- function(..., .fn=message) {
  if (!getOption("verbose")) return(NULL)
  .fn(...)
}
