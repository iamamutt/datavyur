#' @inherit datavyu-data
get_datavyu_info <- function(folder=NULL,
                             columns=NULL,
                             files=NULL,
                             class_overwrite=NULL,
                             traverse=FALSE) {
  column <- NULL
  if (any(folder %|% "" == datavyur_internal_data())) {
    warning("Using internal example data instead of user data!\n", call. = FALSE)
  }

  filepaths <- grep_file_vec(folder)
  filepaths <- filepaths %|% list_datavyu_files(folder, recursive=traverse)

  common_names <- c("file", "column")

  out_list <- lapply(
    filepaths, function(csv) {
      .data <- suppressWarnings(
        data.table::fread(csv, stringsAsFactors=FALSE, verbose=FALSE, showProgress=FALSE)
      )

      if (is.null(.data) || nrow(.data) < 1L) return(NULL)

      vars <- names(.data)
      fields <- vars[!vars %in% common_names]

      if (!all(common_names %in% vars)) {
        message(".csv at ", csv, " doesn't contain `file` and `column` variables")
        return(NULL)
      }

      opf_name <- .data[, unique(file)]
      dv_column <- .data[, unique(column)]
      field_types <- lapply(.data[, fields, with=FALSE], typeof)

      opf_file_subset(
        data.table::data.table(
          codes=names(field_types),
          classes=unlist(field_types),
          file=opf_name,
          column=dv_column,
          local=csv
        ),
        files
      )
    }
  )

  dv_info <- data.table::rbindlist(out_list)

  if (is_none(dv_info) || nrow(dv_info) < 1L) {
    stop(
      "No .csv files found given `folder`, `files`, or `columns` args.\n",
      "You can find names of files or columns",
      " from the exported `column__file.csv`",
      " files by using the script `datavyu2csv.rb`.",
      call.=FALSE
    )
  }

  dv_info <- unique(dv_info)
  dv_info <- sort_datavyu_colnames(dv_info)
  dv_info <- normalize_code_types(dv_info)
  dv_info <- column_info_and_subset(dv_info, columns, check_single=FALSE)
  dv_info <- type_override_info(dv_info, class_overwrite)
  dv_info[]
}

list_datavyu_files <- function(folder, recursive=FALSE) {
  folder <- folder %|% character()
  folder <- normalizePath(folder, winslash="/", mustWork=TRUE)
  filepaths <- list.files(folder, full.names=TRUE, pattern="\\.csv$", recursive=recursive)
  if (is_none(filepaths)) warning("No .csv files found in folder ", folder, call.=FALSE)
  filepaths
}

# Only returns a vector of files paths
grep_file_vec <- function(files) {
  if (!all(is_file(files))) return(character())
  filepaths <- files[grepl("\\.csv$", files)]
  if (is_none(filepaths)) warning("No .csv files found from file vector", call.=FALSE)
  filepaths
}

opf_file_subset <- function(.data, files) {
  if (missing(files)) return(.data)
  files <- stats::na.omit(unique(files))
  if (is_none(files)) return(.data)
  .data <- as.data.table(.data)
  .data[file %in% files, ]
}

column_info_and_subset <- function(.data, col_subset, check_single=FALSE) {
  column <- NULL
  .data <- as.data.table(.data)
  if (!"column" %in% names(.data)) stop(".data contains no variable `column`")
  if (!missing(col_subset) && !is_none(col_subset)) {
    if (!is.character(col_subset)) stop("`columns` must be a character vector or NULL")
    which_rows <- .data[, column %in% col_subset]
    if (!any(which_rows)) {
      warning("no column to subset for ", col_subset, call.=FALSE)
      return(.data)
    }
    .data <- .data[(which_rows), ]
  }
  if (check_single) {
    unq_cols <- .data[, stats::na.omit(unique(column))]
    if (length(unq_cols) > 1L) {
      stop("`dv_info` must return a single datavyu variable")
    }
  }
  .data
}

valid_types <- function(x) {
  as.character(factor(
    x,
    c("logical", "integer", "numeric", "double", "character", "factor"),
    c("numeric", "numeric", "numeric", "numeric", "character", "character")
  ))
}

normalize_code_types <- function(dv_info) {
  column <- codes <- classes <- N <- combined <- NULL
  type_counts <- dv_info[, .N, by=list(column, codes, classes)]
  type_counts <- type_counts[, list(classes=classes[which.max(N)]), by=list(column, codes)]
  type_counts[, classes := valid_types(classes)]
  type_counts[is.na(classes), classes := "character"]
  dv_info[, classes := NULL]
  dv_info <- merge(dv_info, type_counts, by=c("column", "codes"), all=TRUE)
  dv_info[, combined := paste0(column, ".", codes)]
  dv_info
}

type_override_info <- function(dv_info, class_overwrite) {
  old_classes <- classes <- combined <- NULL
  dv_info <- as.data.table(dv_info)
  dv_info[, old_classes := classes]

  if (missing(class_overwrite) || is_none(class_overwrite)) return(dv_info)
  if (!is_real_list(class_overwrite) || !is_named_list(class_overwrite)) {
    stop(
      "classes must be a named list of typenames, and ",
      "each item in the list a vector of colnames as characters"
    )
  }

  class_names <- valid_types(non_empty_names(class_overwrite))
  if (any(is.na(class_names))) {
    stop('class_overwrite must be only of types "character" or "numeric"')
  }

  for (i in seq_along(class_overwrite)) {
    .column <- class_overwrite[[i]]
    dv_info[combined %in% .column, classes := class_names[i]]
  }

  dv_info
}

dv_info_no_file <- function(dv_info) {
  classes <- column <- codes <- combined <- NULL
  dv <- as.data.table(dv_info)[order(classes, column, codes)]
  unique(dv[, list(combined, column, codes, classes)])
}
