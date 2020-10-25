
# datavyu content search ----------------------------------------------------------

#' @inherit datavyu-data
#' @examples
#' # datavyu_col_search(folder="path/to/my/exported/datavyu/data")
#'
#' # using the package's internal data folder as an example
#' datavyu_col_search(folder=datavyur_data_folder())
#' @export
datavyu_col_search <- function(columns=NULL,
                               files=NULL,
                               folder=getOption("datavyur.folder"),
                               traverse=TRUE) {
  message(
    "Searching within each exported .csv file for valid datavyu data...\n",
    '  Using the folder: "', folder, '"'
  )

  dv_info <- get_datavyu_info(
    folder=folder,
    columns=columns,
    files=files,
    traverse=traverse
  )

  as.data.frame(dv_info)
}

# import-contents -----------------------------------------------------------------

#' Imports Datavyu content into R
#'
#' @inherit datavyu-data
#' @section Note: If the same column name was used but has a different number of datavyu
#' codes then you will get an error unless `as_list=TRUE`. This function assumes that
#' the .csv is structured in a way based on how the `datavyu2csv.rb` script exports
#' data.
#' @param as_list a logical value indicating to return a list or data frame (default).
#' @param append_colnames If `TRUE`, the column names for the returned data will be in
#' `column.field` format, e.g., `childhands.ordinal`, instead of having the Datavyu
#' column name in its own variable in the returned data set.
#' @param ... additional arguments passed to [import_content_as_list()].
#' @return See sections `import_datavyu` and `import_content_as_list` below.
#'
#' @section `import_datavyu`: Returns one of the objects below.
#'
#' - a single `data.frame` if a single column was found or specified with
#' `columns="..."`.
#'
#' - a [list] of datasets where each list entry is a datavyu column if different
#' columns are found or if `as_list=FALSE`.
#'
#' @section `import_content_as_list`: Returns a [list] with the names:
#'
#' - `data`: a nested list with files at the top layer and column data within files.
#'
#' - `contents`: a `data.table` with the same contents from [datayu_col_search()].
#'
#' - `order`: The order of files and columns nested within `$data`.
#' @examples
#' import_datavyu(columns="childhands")
#' import_datavyu(columns="childhands", append_colnames=TRUE)
#' import_datavyu(columns=c("childhands", "parenthands"), as_list=TRUE)
#'
#' example_data_info <- datavyu_col_search(folder=datavyur_data_folder())
#' import_datavyu(files=example_data_info$file[1])
#'
#' data_list <- import_content_as_list()
#' str(data_list)
#' @name import-contents
NULL


#' @describeIn import-contents Import content as a `data.frame` or `list` of `data.frame` objects.
#' @export
import_datavyu <- function(...,
                           as_list=FALSE,
                           append_colnames=FALSE) {
  stacked_list <- stack_data_list(import_content_as_list(...))

  n_cols <- length(stacked_list$columns)

  if (!is_none(list(...)$files) && n_cols > 1L) as_list <- TRUE

  if (n_cols > 1L && !as_list) {
    warning(
      "To return a data.frame, only 1 column name is allowed in `import_datavyu`. ",
      "If needing to import more than one column you should set as_list=TRUE ",
      "to return a list with each item in the list as one of the ",
      "columns including all files found for that column."
    )
    as_list <- TRUE
  }

  column_sorted <- make_list_of_diff_columns(stacked_list, append_colnames)

  if (as_list) {
    return(column_sorted)
  }
  column_sorted[[1L]]
}

#' @describeIn import-contents Import content as a nested list.
#' @export
import_content_as_list <- function(folder=getOption("datavyur.folder"),
                                   columns=NULL,
                                   files=NULL,
                                   class_overwrite=getOption("datavyur.classlist"),
                                   traverse=FALSE) {
  message("\nSearching for formatted datavyur data...")
  dv_info <- get_datavyu_info(
    folder=folder,
    columns=columns,
    files=files,
    class_overwrite=class_overwrite,
    traverse=traverse
  )

  message("Importing Datavyu data into R...")
  opf_filenames <- dv_info[, unique(file)]
  data_list_files <- lapply(
    opf_filenames,
    function(opf) {
      # columns to find in current file
      columns <- dv_info[file == opf, unique(column)]

      # read each column data and import as R data
      data_list_cols <- lapply(
        columns, function(col) {
          # path to column .csv for specific .opf file
          csv_file <- dv_info[file == opf & column == col, unique(local)]

          # import .csv
          message("  - reading column `", col, "` from file: ", shQuote(csv_file))
          .data <- datavyu_csv_column_import(csv_file, col, dv_info)

          # get rid of duplicate cells
          unique(.data)
        }
      )
      names(data_list_cols) <- columns
      data_list_cols
    }
  )

  names(data_list_files) <- opf_filenames

  list_info <- rbindlist(
    Map(
      function(dt, n) data.table(file=n, column=names(dt)),
      data_list_files,
      names(data_list_files)
    )
  )

  list_info[, file_col := paste0(column, "__", file)]

  list(data=data_list_files, contents=dv_info, order=list_info)
}



# datavyu-alignment ---------------------------------------------------------------



#' Merge from a list horizontally
#'
#' Merge a list of imported data temporally or ordinally. This aligns data horizontally
#' by appending and merging new columns to a common data set.
#' @param ... additional arguments passed to [import_content_as_list()]
#' @examples
#' # set folder path if needed, otherwise use default path with example data.
#' # options(datavyur.folder="mydatafolder")
#'
#' #-- Align by a common ordinal number
#' ord_aligned <- ordinal_align()
#'
#'
#' # -- Align by a common frame number
#'
#' # example data with no arguments
#' ex_data_aligned <- temporal_align(fps=10)
#'
#' ex_data_aligned_no_frames <- temporal_align(fps=10, keep_frame_num=FALSE)
#'
#' # example data selecting only one of the columns
#' ex_data_aligned2 <- temporal_align(columns="childhands")
#'
#'
#' # -- Align by ordinal or frame but from a list of data objects.
#' .list <- import_content_as_list()
#' ord_aligned <- horz_merge_datavyu_list(.list)
#' @name datavyu-alignment
NULL

#' @describeIn datavyu-alignment Align and merge data by cell number (ordinal
#' alignment). Having a common ordinal number across Datavyu columns may not have much
#' meaning, but this will allow you to append columns within a single data set.
#' @export
ordinal_align <- function(...) {
  data_list <- import_content_as_list(...)
  dat <- horz_merge_datavyu_list(data_list, fps=NULL)
  as.data.frame(dat)
}

#' @describeIn datavyu-alignment This will align several Datavyu columns using a
#' common framerate. You have the option to set the framerate using `fps`. The lower
#' the frame rate, the less likely two events will line up in time. This is because
#' the timestamps are converted to frame numbers based on chunking them into discrete bins. The
#' larger the fps, the larger the bins, and more likely two events will line up in
#' time.
#' @param fps a common framerate to use for alignment. Only set if you want to align
#' temporally. If not set then it will default to ordinal alignment. For
#' `temporal_align` this defaults to 30 FPS.
#' @param keep_frame_num Keep the frame_number column, which will result in a **much** larger
#' data set. If `keep_frame_num=FALSE`, then all you know is which two events overlap
#' to some degree, not knowing for how long. If `keep_frame_num=TRUE` (default), you
#' can calculate the number of frames that overlap between two events, and if fps is
#' know, you can convert this total to milliseconds.
#' video.
#' @export
temporal_align <- function(fps=30, keep_frame_num=TRUE, ...) {
  data_list <- import_content_as_list(...)
  dat <- horz_merge_datavyu_list(data_list, fps=fps)

  if (!keep_frame_num) {
    dat <- as_dtbl(dat)
    dat <- unique(dat, by=which(names(dat) != "frame_number"))
    dat[, frame_number := NULL]
  }

  as.data.frame(dat)
}


#' Merge from a list vertically
#'
#' This will melt all data to have common column names and convert all data to
#' characters except for ordinal, onset, offset.
#'
#' @param .list a nested list returned from [import_content_as_list()]
#' @param .f a function to apply to each data set before anything is done to the data.
#' @param ... additional arguments passed to `.f`
#'
#' @return a data.table with stacked data.
#' @export
#'
#' @examples
#' .list <- import_content_as_list()
#' stacked_data <- vert_merge_datavyu_list(.list)
vert_merge_datavyu_list <- function(.list, .f, ...) {
  if (missing(.f)) .f <- pass

  file_data <- .list$data
  file_list <- lapply(file_data, function(col_data) {
    col_list <- lapply(col_data, function(.data) {
      .dt <- .f(copy(.data), ...)
      .dt[, file := NULL]
      vars <- names(.dt)
      static <- c("ordinal", "onset", "offset")
      fields <- vars[!vars %in% static]

      types <- valid_types(sapply(.dt[, fields, with=FALSE], typeof))
      if (any(is.na(types))) stop("columns must be either double or character")

      strings <- fields[types == "character"]
      numbers <- fields[types %in% c("double", "numeric")]

      dtables <- list(
        data.table(
          ordinal=numeric(),
          onset=numeric(),
          offset=numeric(),
          field=character(),
          code=character()
        )
      )

      if (!is_none(strings)) {
        .dts <- melt(.dt[, c(static, strings), with=FALSE],
          id.vars=static,
          variable.name="field",
          variable.factor=FALSE,
          value.name="code",
          value.factor=FALSE
        )

        dtables <- c(dtables, list(.dts))
      }

      if (!is_none(numbers)) {
        .dtn <- melt(.dt[, c(static, numbers), with=FALSE],
          id.vars=static,
          variable.name="field",
          variable.factor=FALSE,
          value.name="code",
          value.factor=FALSE
        )

        .dtn[, code := as_type(code, "character")]
        .dtn[code == "NaN", code := ""]

        dtables <- c(dtables, list(.dtn))
      }

      rbindlist(dtables)[order(ordinal, onset, offset, field, code)]
    })

    rbindlist(col_list, idcol="column")
  })

  .data <- rbindlist(file_list, idcol="file")
  .data <- sort_datavyu_colnames(.data)
  .data[order(file, column, ordinal, field)]
}


#' @describeIn datavyu-alignment Merge a list of imported data temporally or ordinally and align horizontally by
#' appending new columns to a common data set.
#' @inheritParams temporal_align
#' @inheritParams vert_merge_datavyu_list
#' @return a data.table with aligned data.
#' @export
horz_merge_datavyu_list <- function(.list, .f, ..., fps=NULL) {
  if (missing(.list) || is_none(.list)) {
    warning(".list is missing or no data found in .list argument", call.=FALSE)
    return(invisible())
  }

  if (!is_named_list(.list$data)) {
    stop("each entry in data list must be a named list of .opf filenames")
  }

  if (missing(.f)) .f <- pass

  dv_info <- .list$contents
  key_cols <- "file"
  by_ord <- is_none(fps) || any(is.na(fps))
  if (by_ord) {
    key_cols <- c(key_cols, "ordinal")
  } else {
    key_cols <- c(key_cols, "frame_number")
  }

  file_data <- .list$data
  file_list <- lapply(file_data, function(col_data) {
    if (is_none(col_data)) {
      return(NULL)
    }
    if (!is_named_list(col_data)) {
      stop("each sub entry in each .opf file must be a named list of column names")
    }

    to_merge <- Map(
      function(.data, col) {
        dt_copy <- .f(copy(.data), ...)
        if (!by_ord) {
          # expand time to frame counts
          dt_copy <- as.data.table(framerate_expand(dt_copy, fps))
        }

        # add column prefix to argument names
        dt_copy <- glue_codes_to_names(dt_copy, col, key_cols)

        # get rid of duplicates (possibly due to same files in folder)
        unique(dt_copy)
      },
      col_data,
      names(col_data)
    )

    # begin file merge
    if (length(to_merge) > 1L) {
      merged <- multi_merge(to_merge, by=key_cols, all=TRUE, allow.cartesian=TRUE)
    } else {
      merged <- to_merge[[1L]]
    }

    # set key for larger merge later
    data.table::setkey(merged)
    unique(merged)
  })

  message("\nMerging all .opf files")

  # some files may not have all the necessary codes. add them if necessary
  if (by_ord) {
    # ordinal used as id_col
    dv_info <- dv_info[codes != "ordinal",]
  }
  file_list <- append_missing_combined_cols(file_list, dv_info)

  # names of columns for all list items
  all_names <- unique(unlist(lapply(file_list, function(i) names(i))))

  # begin merging all files into one large data set
  opf_merged <- multi_merge(file_list, by=all_names, all=TRUE)

  # some cleanup
  opf_merged <- unique(opf_merged)

  na_rows <- apply(
    opf_merged[, !names(opf_merged) %in% key_cols, with=FALSE],
    1L,
    function(i) all(is.na(i))
  )

  opf_merged <- opf_merged[(!na_rows),]
  opf_merged <- opf_merged[order(get(key_cols)),]
  opf_merged <- sort_datavyu_colnames(opf_merged, dv_info)
  message("Merge successful!")
  as.data.frame(opf_merged)
}


#' Merge nested data
#'
#' Merges two Datavyu columns by onset/offset timestamps
#'
#' Since data is nested, this will repeat rows from the higher level data. If any cell
#' in the lower level is not within a cell in the higher level, it will not be in the
#' final data set. This function is to be used only with truly nested data. For finer
#' control over potentially overlapping cells see [temporal_align()] with the
#' option `keep.frames=FALSE` and a high value for `fps` for better precision
#' in aligning timestamps.
#'
#' @param outer_col top level column name as a character, (e.g., trial or block)
#' @param inner_col bottom level column name as a character (e.g., eye gazes within
#' trial)
#' @param ... additional parameters passed to [import_datavyu()].
#' @examples
#' # merge nested data, throwing away lower level cells that
#' # are not within higher level cells
#' merged <- merge_nested("childhands", "parenthands")
#'
#' # another way to merge nested, but contains NAs for cells that are not
#' # fully within one another since these data are not nested, you can see how much
#' # is being thrown away compared to x
#' y <- temporal_align(
#'   columns=c("childhands", "parenthands"),
#'   fps=30,
#'   keep.frames=FALSE
#' )
#' @export
merge_nested <- function(outer_col, inner_col, ...) {
  # grab upper and lower level data
  dat_list <- import_datavyu(
    columns=c(outer_col, inner_col),
    append_colnames=FALSE,
    as_list=TRUE,
    ...
  )

  lhs <- data.table::as.data.table(dat_list[[outer_col]])
  rhs <- data.table::as.data.table(dat_list[[inner_col]])

  # start merge
  merged <- lhs[, grab_within(file, inner_col, onset, offset, rhs), by=names(lhs)]

  # add rest of col prefixes
  non_append <- c("file", names(merged)[grepl(paste0(inner_col, "."), names(merged))])
  glue_codes_to_names(merged, outer_col, except=non_append)

  return(as.data.frame(merged))
}

# misc. exported ------------------------------------------------------------------


#' R data to Datavyu .csv file
#'
#' Process to transfer R data to Datavyu
#'
#' Exports R data (as a list of lists or data frames corresponding to Datavyu columns)
#' to a `.csv` file. This can then be used by Datavyu for saving as a `.opf` and
#' importing R data into Datavyu. Each list item is a different column in the final
#' Datavyu file. \cr **NOTE**: Datavyu cannot currently import the .csv files this
#' function creates, so a Ruby script is generated to convert the `.csv` file to `.opf`,
#' which can be loaded into Datavyu.
#'
#' @param rlist List of lists or `data.frames`. These will be the columns used in the
#' final Datavyu file.
#' @param filename File name of the `.csv` file to be created. Any extension in the file
#' name will be removed.
#' @param directory Path to a directory that will store the newly created folder
#' "datavyur_export" containing the `.csv` file and the `.rb` script used within
#' Datavyu, which is needed to convert the `.csv` to `.opf`.
#' @examples
#' # First get example data to use
#' example_data <- fake_datavyu_data()
#'
#' # See how the example data is structured, as a list with another list and data.frame
#' # Both the list and data.frame are named. If not named, one will be assigned.
#' str(example_data)
#'
#' # Export R list to a .csv file for importing into Datavyu
#' r2datavyu(example_data, "~")
#' @export
r2datavyu <- function(rlist, directory=".", filename="datavyur_export") {
  warn_txt <- paste0(
    "\nNote: At the moment there doesn't seem to be a way for datavyu to import a .csv",
    " even though you can export to one. This requires you to do an additional step. ",
    "To get it back to .opf, use the generated ruby script in the output folder.\n"
  )

  message(warn_txt)

  if (!inherits(rlist, "list")) {
    if (inherits(rlist, "data.frame")) {
      rlist <- list(rlist)
    } else {
      stop("Input must be a list of lists, ",
        "list of data.frames, or a single data.frame",
        call.=FALSE
      )
    }
  }

  na2val <- function(x, v="") ifelse(is.na(x), v, x)

  top_digit <- "#4"

  n_col <- length(rlist)
  col_names <- names(rlist)

  if (n_col < 1L) stop("no columns found in r list object", call.=FALSE)

  # check for named rlist, add new names if necessary
  if (is_none(col_names)) {
    new_names <- paste0("datavyur", 1L:n_col)
    names(rlist) <- new_names
  } else {
    no_names <- col_names == ""
    if (any(no_names)) {
      new_names <- paste0("datavyur", 1L:n_col)[no_names]
      names(rlist)[no_names] <- new_names
    }
  }

  col_names <- names(rlist)

  # go through each column structured as an r list
  each_col <- lapply(
    1L:n_col,
    function(col) {

      # get names of codes
      codes <- rlist[[col]]
      col_name <- col_names[col]
      code_names <- names(codes)

      if (is.null(code_names)) {
        stop(paste0("ordinal, onset, offset not found for: ", col_name), call.=FALSE)
      }

      # check if codes have these common arguments
      common_code_names <- c("ordinal", "onset", "offset")
      common_codes_l <- common_code_names %in% code_names
      custom_code_names <- code_names[!code_names %in% common_code_names]

      if (!all(common_codes_l)) {
        stop(paste0("ordinal, onset, offset not found for: ", col_name), call.=FALSE)
      }

      ts_ord <- codes$ordinal
      ts_on <- as.character(ms2time(codes$onset)[ts_ord])
      ts_off <- as.character(ms2time(codes$offset)[ts_ord])

      if (length(custom_code_names) == 0) {
        codes$code1 <- rep(NA, length(ts_ord))
        custom_code_names <- "code1"
      }

      code_str <- paste0(custom_code_names, "|NOMINAL", collapse=",")
      col_str <- paste0(col_name, " (MATRIX,true,)-", code_str)

      code_mat <- lapply(
        custom_code_names, function(cn) {
          na2val(as.character(codes[[cn]])[ts_ord])
        }
      )

      code_mat <- cbind(ts_on, ts_off, do.call(cbind, code_mat))

      col_dat <- apply(
        code_mat, 1, function(s) {
          code_text <- paste0("(", paste0(s[-c(1, 2)], collapse=","),
            ")",
            collapse=""
          )
          paste0(s[1], ",", s[2], ",", code_text)
        }
      )

      return(c(col_str, col_dat))
    }
  )

  text_lines <- c(top_digit, c(each_col, recursive=TRUE))
  text_data <- generate_ruby_opf_script(directory, filename)
  if (!dir.exists(dirname(text_data$csv))) {
    dir.create(dirname(text_data$csv), recursive=TRUE)
  }
  writeLines(text_lines, text_data$csv)
  writeLines(text_data$text, text_data$rb)
  NULL
}

#' Check for `NA`'s but not `NaN`'s
#'
#' `NaN` values are used to denote missing values from the original Datavyu data.
#'
#' @param x atomic numeric vector
#'
#' @return logical vector
#' @export
#'
#' @examples
#' is_NA(c(0, NaN, NA, 1.0))
is_NA <- function(x) {
  if (!typeof(x) %in% c("double", "numeric")) stop("only checks numeric types.")
  is.na(x) & !is.nan(x)
}

#' Return the data folder for the `datavyur` package
#'
#' This is the default folder if none is specified in the R option "datavyur.folder"
#'
#' @return path string
#' @export
#'
#' @examples
#' datavyur_data_folder()
datavyur_data_folder <- function() {
  system.file("extdata", package="datavyur", mustWork=TRUE)
}

#' Return the path to the ruby scripts used within the Datavyu software
#'
#' @return path string
#' @export
#'
#' @examples
#' ruby_script_folder()
ruby_script_folder <- function() {
  system.file("ruby", package="datavyur", mustWork=TRUE)
}

#' Fake Datavyu data
#'
#' This function will create fake data in the R format needed to import back into the
#' Datavyu software
#'
#' The function shows how you can have either a list of columns or an actual data frame.
#' Either way will work.
#'
#' @param n1 Sample size for variable 1
#' @param n2 Sample size for variable 2
#' @return List of datavyu formated data
#' @examples
#' my_data <- fake_datavyu_data()
#' @export
fake_datavyu_data <- function(n1=10L, n2=15L) {
  n1 <- as.integer(max(1L, n1))
  n2 <- as.integer(max(1L, n2))

  max_ts <- 3600000L
  pr_on <- sort(round(runif(n2, 0, max_ts)))
  pr_off <- abs(round(runif(n2, pr_on + 1, c(pr_on[2:n2] - 1, max_ts))))

  ch_on <- sort(round(runif(n1, 0, max_ts)))
  ch_off <- abs(round(runif(n1, ch_on + 1, c(ch_on[2:n1] - 1, max_ts))))

  hand_char <- c("left", "right", "both", "")
  look_val <- c("0", "1", "")

  dat <- list(
    childhands=list(
      ordinal=seq_len(n1),
      onset=as.integer(ch_on),
      offset=as.integer(ch_off),
      hand=sample(hand_char, n1, replace=TRUE),
      look=sample(look_val, n1, replace=TRUE)
    ),
    parenthands=data.frame(
      ordinal=seq_len(n2),
      onset=as.integer(pr_on),
      offset=as.integer(pr_off),
      hand=sample(hand_char, n2, replace=TRUE),
      look=sample(look_val, n2, replace=TRUE)
    )
  )
  return(dat)
}

#' Convert milliseconds to a time string
#'
#' This will take a duration of time in milliseconds and convert it to the time string
#' format used by Datavyu.
#'
#' @param timestamp A numeric time duration in **milliseconds**, such as `6000`ms
#' representing six seconds.
#' @examples
#' # 1102013 = 18 minutes and 22 seconds and 13 milliseconds
#' ms2time(1102013)
#'
#' timestamps <- c(0, 300000, 86399999)
#' ms2time(timestamps)
#' @export
ms2time <- function(timestamps) {
  # max timestamp for Datavyu is 24 * 60 * 60 * 1000
  long_ts <- any(timestamps >= 86400000L)
  if (long_ts) {
    warning("Timestamp can't be more than 24 hours. ",
      "Datavyu doesn't do days.",
      "Converting these to NA's",
      call.=FALSE
    )
    timestamps[long_ts] <- NA
  }
  seconds <- timestamps / 1000
  ms <- formatC(
    x=round((seconds - trunc(seconds)) * 1000, digits=3),
    digits=3, width=3, format="d", flag="0"
  )
  seconds <- trunc(seconds)
  start <- as.POSIXct(Sys.time())
  dt <- difftime(start + seconds, start, units="secs")
  time_char <- paste0(format(.POSIXct(dt, tz="GMT"), "%H:%M:%S"), ":", ms)
  return(time_char)
}

#' Convert timestamps to frame numbers
#'
#' This is typically used for aligning cells by a common time.
#'
#' Frame numbers may be repeated if several timestamps are close in duration within the
#' specified frames per second.
#'
#' @param x Vector of timestamps in **milliseconds**
#' @param fps Frames per second of the video source. Defaults to 30 fps. The smaller the
#' value, the more likely two events will be binned together and marked as the same
#' frame number.
#' @param tstart Start timestamp in milliseconds (optional). Any timestamp before
#' `start` start will be `NA`, and the frame number will start at `1` where above
#' `tstart`. A warning will be triggered if anything outside of `tstart` and `tend` is
#' found, unless `warn=FALSE`.
#' @param tend Longest possible timestamp in milliseconds (optional). Any timestamp past
#' `tend` will be `NA`. Defaults to max of `x` if not used.
#' @param as_ts If set to `TRUE`, will return a timestamp representation of the frame
#' number, adjusted for `tstart` and `tend`.
#' @param warn Turn on/off warnings if any `NA`s were found.
#' @return A numeric vector of the same size as `x`.
#' @export
#' @examples
#' t <- c(
#'   -1000, 100, 4047, 7451, 14347, -1,
#'   27920, 30669, 39798, 42504, 42510, 51451, 56034
#' )
#' ts2frame(t, tstart=1000, warn=FALSE)
ts2frame <- function(x, fps=30, tstart=0L, tend=NULL, as_ts=FALSE, warn=TRUE) {
  ms_dur <- 1000 / fps
  if (is_none(tend)) tend <- max(x)
  time_breaks <- seq(tstart, tend + ms_dur - ((tend - tstart) %% ms_dur), by=ms_dur)
  frame <- findInterval(x, time_breaks)
  frame[x < tstart | x > tend] <- NA
  if (warn && any(is.na(frame))) warning("Found NA's for some frames", call.=FALSE)
  if (as_ts) {
    return(time_breaks[frame])
  }
  frame
}

#' Expand a data set containing onset/offset columns to reflect their frame numbers.
#'
#' @param .data A `data.frame` with `onset` and `offset` columns.
#' @param fps The frames per second corresponding to the coded video.
#' @param keep_ts Whether to keep the `onset` and `offset` columns in the data.
#'
#' @return A `data.table` with the added column `frame_number`
#' @export
#'
#' @examples
#' my_data <- data.frame(onset=1000, offset=1999, code="hello")
#' framerate_expand(my_data, 30)
#'
#' my_data2 <- data.frame(onset=c(-1, 0), offset=c(100, 125), x=c(".", "-"))
#' framerate_expand(my_data2, 30)
framerate_expand <- function(.data, fps, keep_ts=TRUE) {
  if (missing(fps) || is_none(fps)) {
    stop("Need frames per second argument `fps` to expand onset/offset.")
  }

  dtbl <- as_dtbl(.data, copy=TRUE)

  # convert timestamps to frame counts
  dtbl[, `:=`(
    ._onset_f=ts2frame(onset, fps=fps, warn=FALSE),
    ._offset_f=ts2frame(offset, fps=fps, warn=FALSE)
  )]

  # index column for merging
  dtbl[, ._row_index := .GRP, .(._onset_f, ._offset_f)]

  # do the frame expansion
  frame_data <- dtbl[
    !(is.na(._onset_f) | is.na(._offset_f)),
    .(frame_number=on_off_to_seq(min(._onset_f), max(._offset_f))), .(._row_index)
  ]

  dtbl <- merge(dtbl, frame_data, by="._row_index", all.x=TRUE)
  dtbl[, `:=`(._onset_f=NULL, ._offset_f=NULL, ._row_index=NULL)]
  if (!keep_ts) dtbl[, `:=`(onset=NULL, offset=NULL)]
  as.data.frame(dtbl)
}
