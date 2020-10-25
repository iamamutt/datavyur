# TODO test the functions for new update

# Code checking -------------------------------------------------------------------


#' Check for invalid timestamps
#'
#' @param ts_list A list with each item corresponding to either a pair of timestamp
#' columns \code{c(onset, offset)}, or just a single column name. If a single name,
#' durations will not be checked, and only out of range will be checked.
#' @param dat A data.frame/data.table that contains the columns with the onset/offset
#' timestamps
#' @param tmin Minimum allowed timestamp. Defaults to 0 (milliseconds)
#' @param tmax Maximum allowed timestamp. Defaults to one day in milliseconds.
#' @param as.na Set bad timestamps to \code{NA}
#'
#' @return Returns a list with 3 items, \code{$data, $ranges, $durations}. \code{$data}
#' is new data with bad timestamps as \code{NA}. \code{$ranges} is a list of column
#' names, with a vector of indices corresponding to timestamps out of range. If all
#' okay, the vector is empty. \code{$durations} is a list of bad timestamp durations.
#' If \code{as.na=TRUE}, the offset timestamp is set to \code{NA}.
#' @export
#' @examples
#' # Use example data
#' dat <- ordinal_align()
#'
#' # Make the list of timestamps to check in the example data
#' # These check durations as well as out of range
#' ts_list <- list(
#'   c(on="parenthands.onset", off="parenthands.offset"), # explicit
#'   child=c("childhands.onset", "childhands.offset") # infer from order which is on/off
#' )
#' ts_checked <- check_timestamps(ts_list, dat)
#'
#' # This only checks for bad ranges since its only one column name.
#' # ts_list can be mixed with 2 or 1 items each.
#' ts_list <- list("parenthands.offset")
#' ts_checked2 <- check_timestamps(ts_list, dat)
check_timestamps <- function(ts_list, dat, tmin=0, tmax=864e5, as.na=TRUE) {
  
  # check if list
  if (!is_real_list(ts_list)) {
    stop("`ts_list` must be a list object. see ?check_timestamps")
  }
  
  # make as data.table
  d <- as_dtbl(dat, copy=TRUE)
  
  # assess common column names
  cnames <- unique(unlist(ts_list))
  dnames <- names(d)
  valid_names <- cnames[cnames %in% dnames]
  invalid_names <- cnames[!cnames %in% dnames]
  
  if (length(invalid_names) != 0) {
    stop(
      paste0(invalid_names, collapse=", "),
      paste0("\ncolumns not found in data: ", w1)
    )
  }
  
  # find bad ranges
  bad_rng <- lapply(valid_names, function(i) {
    which(!range_conditions(d[[i]]))
  })
  names(bad_rng) <- valid_names
  
  # find bad durations
  bad_dur <- lapply(
    ts_list,
    function(i) {
      l <- length(i)
      if (l == 1) {
        bad_dur <- integer()
      } else if (l == 2) {
        if (is.null(names(i))) {
          on <- i[1]
          off <- i[2]
        } else {
          on <- i["on"]
          off <- i["off"]
        }
        bad_dur <- which(!duration_condition(d[[on]], d[[off]]))
        if (length(bad_dur) > 0 & as.na) d[bad_dur, eval(off) := NA]
      } else {
        stop("each item in ts_list must be of length 1 or 2")
      }
      return(bad_dur)
    }
  )
  
  # check for named ts_list, add new names if necessary
  tnames <- names(ts_list)
  if (is.null(tnames)) {
    dur_names <- paste0("dur", 1:length(ts_list))
    names(bad_dur) <- dur_names
  } else {
    no_names <- tnames == ""
    dur_names <- paste0("dur", 1:length(tnames))[no_names]
    names(bad_dur)[no_names] <- dur_names
  }
  
  ## Overwrite data or set data to null
  if (as.na) {
    for (i in names(bad_rng)) {
      d[bad_rng[[i]], eval(i) := NA]
    }
    new_dat <- as.data.frame(d)
  } else {
    new_dat <- NULL
  }
  
  return(list(data=new_dat, ranges=bad_rng, durations=bad_dur))
}

#' Check columns for bad codes
#'
#' Check for invalid codes used for each column and argument specified
#'
#' This takes an already existing data.frame/data.table and checks specific columns to
#' see if they contain only the codes listed in \code{code_list}. Each item in the list
#' must be named according to the column name in the data.frame, and each item must
#' contain a vector of 1 or more of valid codes to check. Codes can be numeric or
#' characters.
#'
#' @param code_list A list of column names with each name having a vector of codes to
#' check.
#' @param dat The data.frame/data.table to check
#' @param as.na If \code{TRUE}, will return a new data set with \code{NA} instead of the
#' bad code.
#'
#' @return A list containing two items. \code{$data} is the new data with NAs,
#' \code{$bad_codes} is another list, each item corresponding to the input list. In
#' each there is a data.frame that is either empty (no bad codes found), or contains
#' indices for the row numbers with bad codes and the column name and type of bad code
#' found. If \code{as.na=FALSE}, the new data will be \code{NULL}, and will only
#' return \code{$bad_codes}, if any.
#' @export
#' @examples
#' # Use example data
#' dat <- ordinal_align()
#'
#' # Make the list of valid codes, names corresponding to columns in the data
#' code_list <- list(
#'   childhands.hand=c("left", "right", "both"),
#'   childhands.look=c(0, 1),
#'   parenthands.hand=c("left", "right", "both"),
#'   parenthands.look=c(0, 1)
#' )
#'
#' # check for bad codes, returning new data with bad codes as NAs
#' codes_checked <- check_codes(code_list, dat)
check_codes <- function(code_list, dat, as.na=TRUE) {
  # check if list
  if (class(code_list) != "list") {
    stop(simpleError("code_list must be a list of column names and valid codes"))
  }
  
  # make as data.table
  d <- as_dtbl(dat, copy=TRUE)
  
  # assess common column names
  cnames <- names(code_list)
  dnames <- names(d)
  valid_names <- cnames[cnames %in% dnames]
  invalid_names <- cnames[!cnames %in% dnames]
  
  bad_list <- code_list[valid_names]
  
  # check codes here, make table of bad codes
  for (n in valid_names) {
    valid_codes <- code_list[[n]]
    not_ok <- which(!(d[[n]] %in% valid_codes | is.na(d[[n]])))
    bad_list[[n]] <- d[not_ok, .(V1=not_ok, V2=get(n))]
    data.table::setnames(bad_list[[n]], c("V1", "V2"), c("index", n))
    if (as.na) d[not_ok, eval(n) := NA]
  }
  
  # convert bad codes to old data.frame
  bad_list <- lapply(bad_list, as.data.frame)
  
  # overwritten data to data.frame or null
  if (as.na) {
    new_dat <- as.data.frame(d)
  } else {
    new_dat <- NULL
  }
  
  # warn about invalid names used in arg list
  if (length(invalid_names) != 0) {
    w1 <- paste0(invalid_names, collapse=", ")
    w2 <- paste0("columns not found in data: ", w1)
    warning(simpleWarning(w2))
  }
  
  return(list(data=new_dat, bad_codes=bad_list))
}


#' Copies of rows with invalid codes
#'
#' @param dat Original data object used on the function \code{check_codes}
#' @param checked_list The list returned after using \code{check_codes}
#'
#' @return a list with subsets of data corresponding to rows with invalid codes
#' @export
#'
#' @examples
#' # see check_codes first, after that do:
#' bad_copies <- bad_code_copies(dat, codes_checked)
bad_code_copies <- function(dat, checked_list) {
  bad_codes <- checked_list$bad_codes
  
  if (is.null(bad_codes)) {
    stop("Could not find bad_codes list item. Did you use check_codes function?")
  }
  
  copies <- lapply(bad_codes, function(i) {
    if (nrow(i) > 0) {
      d <- dat[i$index,]
      return(d)
    }
  })
  return(copies)
}

#' Copies of rows with invalid timestamps
#'
#' @param dat Original data object used on the function \code{check_timestamps}
#' @param checked_list The list returned after using \code{check_timestamps}
#'
#' @return a list with subsets of data corresponding to rows with invalid timestamps
#' @export
#'
#' @examples
#' # see check_timestamps first, after that do:
#' bad_timestamps <- bad_ts_copies(dat, ts_checked)
bad_ts_copies <- function(dat, checked_list) {
  bad_ranges <- checked_list$ranges
  bad_durations <- checked_list$durations
  
  if (!is.null(bad_ranges)) {
    ranges <- lapply(bad_ranges, function(i) {
      if (length(i) > 0) {
        d <- dat[i,]
        return(d)
      }
    })
  } else {
    stop("Could not find ranges list item. Did you use check_timestamps function?")
  }
  
  if (!is.null(bad_durations)) {
    durations <- lapply(bad_durations, function(i) {
      if (length(i) > 0) {
        d <- dat[i,]
        return(d)
      }
    })
  } else {
    durations <- NULL
  }
  
  return(list(ranges=ranges, durations=durations))
}
