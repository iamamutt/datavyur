
datavyu_csv_fread <- function(.path) {
  # get default and user options as a list
  args <- list(
    file=.path,
    header=TRUE,
    stringsAsFactors=FALSE,
    verbose=FALSE,
    showProgress=FALSE,
    colClasses=default_csv_names()
  )

  # read in data with user options
  do.call(data.table::fread, args)
}

# import datavyur .csv file, check classes and codes
datavyu_csv_column_import <- function(.path, column, dv_info) {
  if (missing(column) || is_none(column)) stop("Need column info to import a column.")

  # read from file and ensure it has only a single dv column in the data
  .data <- datavyu_csv_fread(.path)

  # rename and remove unnecessary columns
  .data <- check_duplicated_code_fields(.data)

  # make sure csv file has only one column
  .data <- column_info_and_subset(.data, column, check_single=TRUE)

  # add other vars if missing from set of files
  column_info <- column_info_and_subset(copy(dv_info), column, check_single=TRUE)

  .data <- append_missing_code_cols(.data, column_info)

  # overwrite classes from estimated classes
  .data <- convert_dtbl_var_types(.data, column_info$codes, column_info$classes)

  sort_datavyu_colnames(.data)
}

# column naming, sorting, append, conversion --------------------------------------


# add column.codes suffixes
glue_codes_to_names <- function(.data, column, except=NA_character_) {
  vars <- names(.data)
  codes <- vars[!vars %in% except]
  data.table::setnames(.data, codes, paste0(column, ".", codes))
  .data
}

sort_datavyu_colnames <- function(.data, dv_info=NULL) {
  vars <- names(.data)

  if (is.null(dv_info)) {
    sort_first <- c(
      "file", "column", "field", "code", "codes",
      "classes", "ordinal", "onset", "offset"
    )
    sort_first <- sort_first[sort_first %in% vars]
    sort_last <- sort(vars[!vars %in% sort_first])
    data.table::setcolorder(.data, c(sort_first, sort_last))
    return(.data)
  }

  dv_copy <- copy(dv_info)
  field_order <- c("ordinal", "onset", "offset")
  dv_copy[, cord := as.integer(factor(codes, field_order))]
  dv_copy[is.na(cord), cord := length(field_order) + 1L]
  dv_copy <- dv_copy[order(tolower(column), cord, tolower(codes)),]
  arg_order <- dv_copy[, paste(column, codes, sep=".")]
  arg_order <- unique(c("file", "ordinal", "frame_number", arg_order))

  if (!all(vars %in% arg_order)) {
    warning(
      'Additional columns exist not found in original file contents:\n  "',
      paste0(vars[!vars %in% arg_order], collapse='", "'), '"',
      call.=FALSE
    )
  }

  var_reorder <- vars[na.omit(match(arg_order, vars))]
  var_reorder <- unique(c(var_reorder, vars[!vars %in% var_reorder]))
  data.table::setcolorder(.data, var_reorder)
  .data
}

convert_dtbl_var_types <- function(.data, fields, types) {
  len <- length(fields)
  if (len != length(types)) stop("column names must be same size as types")
  type_checks <- unique(data.table(fields, types))
  for (i in seq_len(nrow(type_checks))) {
    col <- type_checks$fields[i]
    typ <- type_checks$types[i]

    if (typeof(.data[[col]]) != typ) .data[, eval(col) := as_type(get(col), typ)]

    which_na <- .data[, is.na(get(col))]
    if (any(which_na)) {
      to_val <- ifelse(typ == "character", "", NaN)
      .data[(which_na), (col) := to_val]
    }
  }
  .data
}

get_missing_value <- function(type) {
  type <- match.arg(valid_types(type), c("character", "numeric"))
  ifelse(type == "character", "", NaN)
}

# check for missing code fields from a specific data set of a datavyu column/variable
append_missing_code_cols <- function(.data, dv_info) {
  vnames <- names(.data)

  if ("column" %in% vnames) {
    .data[, column := NULL]
  }

  need_add <- unique(dv_info[!codes %in% vnames, .(column, codes, classes)])

  # add code columns if missing from specific data set
  n_to_add <- nrow(need_add)
  if (n_to_add > 0L) {
    check_multi_cols <- need_add[, .(N=length(unique(column))), .(codes)]
    if (any(check_multi_cols$N > 1L)) {
      stop(
        "Identical codes found for multiple columns. ",
        "Use a subset of `dv_info` with a single column instead."
      )
    }

    for (i in seq_len(n_to_add)) {
      col <- need_add$codes[i]
      val <- as_type(NA, need_add$classes[i])
      .data[, (col) := val]
    }
  }

  .data[]
}

# add whole datavyu "columns" if missing
append_missing_combined_cols <- function(.data_list, dv_info) {
  dv_info <- dv_info_no_file(dv_info)
  necessary_names <- dv_info$combined
  necessary_classes <- dv_info$classes

  .data_list <- lapply(.data_list, function(.data) {
    vars <- names(.data)
    are_missing <- !necessary_names %in% vars

    if (any(are_missing)) {
      new_names <- necessary_names[are_missing]
      new_cls <- necessary_classes[are_missing]
      for (j in seq_along(new_names)) {
        val <- as_type(NA, new_cls[j])
        col <- new_names[j]
        .data[, (col) := val]
      }
    }

    .data
  })

  return(.data_list)
}


# import_datavyu list handling ----------------------------------------------------


stack_data_list <- function(data_list) {
  list_info <- data_list$order
  stacked_list <- unlist(data_list$data, recursive=FALSE, use.names=FALSE)
  names(stacked_list) <- list_info$file_col

  list(
    data=stacked_list,
    names=names(stacked_list),
    columns=unique(list_info$column),
    files=unique(list_info$file)
  )
}

make_list_of_diff_columns <- function(stacked_list, append_column_names) {
  if (!all(c("data", "names", "columns", "files") %in% names(stacked_list))) {
    stop("incorrect list structure for `stacked_list`")
  }

  structure(lapply(stacked_list$columns, function(col) {
    these_names <- stacked_list$names[grepl(paste0("^", col, "__.*$"), stacked_list$names)]
    bind_list_of_same_columns(stacked_list$data[these_names], col, append_column_names)
  }), .Names=stacked_list$columns)
}

bind_list_of_same_columns <- function(col_list, col, append_column_names) {
  check_colnames_match(col_list)
  bounded <- rbindlist(col_list, fill=TRUE)
  bounded <- bounded[order(file, ordinal, onset, offset)]

  if (append_column_names) {
    bounded <- sort_datavyu_colnames(bounded)
    bounded <- glue_codes_to_names(bounded, col, c("file"))
  } else {
    bounded[, column := col]
    bounded <- sort_datavyu_colnames(bounded)
  }

  as.data.frame(bounded)
}


# check a list of dataframes for matching column names
check_colnames_match <- function(data_set_list) {
  namestats <- lapply(data_set_list, function(i) {
    list(cnames=sort(names(i)), file=i$file[1], n=ncol(i))
  })

  Reduce(function(x, y) {
    xcn <- x$cnames
    ycn <- y$cnames
    if ((x$n != y$n) || any(xcn != ycn)) {
      xn <- paste(xcn, collapse=", ")
      yn <- paste(ycn, collapse=", ")
      err <- paste0(
        "codes for ", y$file[1],
        " don't match codes for files(s) ",
        x$file[1], "\n", y$file[1], ": ", yn,
        "\n", x$file[1], ": ", xn
      )
      warning(err, call.=FALSE)
    }
    zcn <- sort(unique(c(xcn, ycn)))
    f <- paste(c(x$file[1], y$file[1]), collapse=", ")
    z <- list(cnames=zcn, file=f, n=length(zcn))
    return(z)
  }, namestats, accumulate=FALSE)

  return(invisible())
}

