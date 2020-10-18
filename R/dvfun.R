# Main Datavyu Functions -------------------------------------------------------

#' Scan .csv files for data
#' 
#' Scans data exported as .csv from Datavyu and returns a list of .opf column names among other attributes
#' 
#' The function will read all .csv files found in \code{folder}.
#' All valid .opfs will be found based on if they have the columns \code{file, column, onset, offset, ordinal}.
#' 
#' @param folder Character string of the name of the folder to be scanned. Defaults to option \code{datavyur.folder}.
#' @param unq Return only unique column names
#' @param cnames Name of columns to return if \code{unq=TRUE}.
#' @return data.frame with .csv info. The variable \code{file} is the original .opf file name used.
#' \code{column} is the name of the column within the .opf file. \code{codes} are the argument names used.
#' \code{classes} are the guessed classes for each argument based on R import functions.
#' \code{local} is the .csv file name and location for a particular column and .opf file.
#' @examples
#' datavyu_col_search("myfolder")
#' @export
datavyu_col_search <- function(folder=getOption("datavyur.folder"), 
                               unq=FALSE, 
                               cnames=c("file", "column", "codes", "classes", "local"))
{
    message("Searching through .csv files for valid .opf data...")
    dvcols <- check_opf_data(folder=folder)
    if (unq) {
        dvcols <- unique(dvcols[, cnames, with=FALSE], by=cnames)
    }
    return(as.data.frame(dvcols))
}


#' Import Datavyu column into R
#' 
#' Imports a Datavyu column to R when using the datavyu2csv.rb script
#' 
#' This function only works if you had previously used the \code{datavyu2csv.rb} script to export a Datavyu file to .csv
#' This can be obtained from \url{http://github.com/iamamutt/datavyu/general}.
#' \cr
#' Note: If the same column name was used but has different number of arguments then you will get an error unless \code{asList=TRUE}.
#' This function assumes that the .csv is structured in a way based on how the \code{datavyu2csv.rb} script exports data.
#' 
#' @param column The name of the column to import as used in the Datavyu .opf file
#' @param folder Character string corresponding to the folder path to be scanned. Defaults to option \code{datavyur.folder}.
#' @param asList Logical value indicating to return a list or data frame (default).
#' @param append.colnames If \code{true}, add column name to each argument, 
#' e.g., \code{column.arg}, instead of having column as a variable in the data. 
#' @param classes List of new classes to override guessed classes when reading in .csv data. see \code{data.table::fread}
#' @param ... Additional options passed to the function \code{data.table::fread}
#'
#' @examples
#' import_column("childhands")
#' import_column("childhands", append.colnames = TRUE)
#' @export
import_column <- function(column,
                          folder = getOption("datavyur.folder"),
                          asList = FALSE,
                          append.colnames = TRUE,
                          classes = getOption("datavyur.classlist"),
                          ...)
{
    # check length of columns to import
    if (length(column) > 1 & asList == FALSE) {
        stop(simpleError(
            paste0("Only 1 column name allowed in import_column. ",
                   "If needing to import more than one column ",
                   "set asList=TRUE to return a list object with each ",
                   "item in the list as one of the columns")
        ))
    }
    
    # get opf info based on columns
    opf_info <- opf_and_col_selector(all.opf = TRUE, 
                                     all.cols = column, 
                                     folder = folder)
    
    # override classes
    est_classes <- override_typeofs(opf_info, classes)
    
    # for each file in each column, read in data
    # append col names if necessary
    col_dat <- lapply(column, function(i) {
        
        fpaths <- unique(opf_info[column == i, .(local, file)], by="local")
        
        dat <- lapply(fpaths$local, function(fin) {
            DT <- opf_col_import(fin, i, est_classes, ...)
            return(data.table::copy(DT))
        })
        
        names(dat) <- fpaths$file
        
        if (append.colnames) {
            lapply(dat, function(j) {
                append_colname(j, i, c("file", "column"))
            })
        }
        return(dat)
    })
    names(col_dat) <- column
    
    # combine separate files into one data.frame
    dat <- lapply(col_dat, function(i) {
        # check that the same columns exist between data frames
        check_colnames_match(i)
        # combine each .opf of the same column into one dataframe
        as.data.frame(do.call(rbind, i))
    })
    
    if (!asList) {
        return(dat[[1]])
    } else {
        return(dat)
    }
}

#' Import datavyu data from file name(s)
#'
#' @inheritParams import_column
#' @param file A character vector of file names
#'
#' @return A list or data.frame
#' @export
import_file <- function(file,
                        folder = getOption("datavyur.folder"),
                        asList = FALSE,
                        append.colnames = TRUE,
                        classes = getOption("datavyur.classlist"),
                        ...) {
    
    # check length of columns to import
    if (!asList) append.colnames <- TRUE
    
    # get opf info based on columns
    opf_info <- opf_and_col_selector(all.opf = file, 
                                     all.cols = TRUE, 
                                     folder = folder)
    
    # override classes
    est_classes <- override_typeofs(opf_info, classes)
    
    # for each file in each column, read in data
    # append col names if necessary
    file_dat <- lapply(file, function(i) {
        
        fpaths <- unique(opf_info[file == i, .(local, column)], by="local")
        
        nc <- nrow(fpaths)
        
        if (nc > 0) {
            
        }
        
        dat <- lapply(1:nc, function(j) {
            fpath <- fpaths$local[j]
            cname <- fpaths$column[j]
            DT <- opf_col_import(fpath, cname, est_classes, ...)
            return(DT)
        })
        
        names(dat) <- fpaths$column
        
        if (append.colnames) {
            lapply(1:nc, function(j) {
                append_colname(dat[[j]], names(dat)[j], except=c("file", "column"))
            })
        }
        return(dat)
    })
    names(file_dat) <- file
    
    if (!asList) {
        unq_names <- unique(unlist(lapply(file_dat, names)))
        DTl <- lapply(file_dat, function(i) {
            DT <- data.table::rbindlist(i, use.names = TRUE, fill = TRUE)
        })
        
        file_dat <- data.table::rbindlist(DTl, use.names = TRUE, fill = TRUE)
        file_dat <- as.data.frame(file_dat)
    }
    return(file_dat)
}

#' Combine multiple files into a single list
#'
#' @inheritParams import_file
#' @param merge.cells If the same column names exist, you can merge cells or set as two different cols.
#'
#' @return A list for export
#' @export
#' @examples 
#' files_to_merge <- c("dyad1", "dyad2", "dyad3")
#' export_list <- merge_opf_files_for_export(files_to_merge)
#' r2datavyu(export_list, "merged_file_name")
merge_opf_files_for_export <- function(file,
                                       merge.cells = TRUE,
                                       folder = getOption("datavyur.folder"),
                                       classes = getOption("datavyur.classlist"),
                                       ...) {
    
    file_list <- import_file(file, asList = TRUE, folder = folder, 
                             append.colnames = FALSE, classes = classes, ...)
    
    file_cols <- unlist(lapply(file_list, names))
    same_cols <- unique(file_cols[duplicated(file_cols)])
    diff_cols <- file_cols[!file_cols %in% same_cols]
    
    merge_list <- function(file_list, list_cols, append_names, append_data) {
        
        col_collect <- lapply(file_list, function(i) {
            cols <- names(i)
            list_sub <- cols[cols %in% list_cols]
            if (length(list_sub) > 0) {
                return(i[list_sub])
            }
            return(invisible(NULL))
        })
        
        unq_col_names <- unlist(lapply(col_collect, names))
        col_collect <- do.call(c, col_collect)
        names(col_collect) <- unq_col_names
        
        if (append_names) {
            
            col_collect_names <- names(col_collect)
            for (i in 1:length(list_cols)) {
                scol <- list_cols[i]
                which_same <- scol == col_collect_names
                col_collect_names[which_same] <- paste0(scol, 1:sum(which_same))
            }   
            names(col_collect) <- col_collect_names
        }
        
        if (append_data) {
            col_collect_names <- names(col_collect)
            col_collect <- lapply(list_cols, function(i) {
                which_same <- i == col_collect_names
                if (any(which_same)) {
                    data.table::rbindlist(col_collect[which(which_same)], use.names = TRUE, fill = TRUE)
                }
            })
            names(col_collect) <- list_cols
        }
        
        return(col_collect)
    }
    
    diff_list <- merge_list(file_list, diff_cols, FALSE, FALSE)
    same_list <- merge_list(file_list, same_cols, !merge.cells, merge.cells)
    merged_lists <- c(same_list, diff_list)
    
    merged_lists <- lapply(merged_lists, function(i) {
        i <- i[order(onset),]
        i[, ordinal := 1:.N]
        i[, file := NULL]
        return(as.data.frame(i))
    })
    
    return(merged_lists)
}

#' Merge nested data
#' 
#' Merges two Datavyu columns by onset/offset timestamps
#' 
#' Since data is nested, this will repeat rows from the higher level data.
#' If any cell in the lower level is not within a cell in the higher level, it
#' will not be in the final data set. This function is to be used only with truly
#' nested data. For finer control over potentially overlapping cells see 
#' \code{\link{temporal_align}} with the option \code{keep.frames=FALSE} and a high
#' value for \code{fps} for better precision in aligning timestamps.
#' 
#' @param c1 top level column name as a character, (e.g., trial or block)
#' @param c2 bottom level column name as a character (e.g., eye gazes within trial)
#' @param folder Character string of the name of the folder to be scanned. Defaults to option \code{datavyur.folder}.
#' @param ... Additional options passed to \code{data.table::fread}
#' @examples
#' # merge nested data, throwing away lower level cells that are not within higher level cells
#' merged <- merged_nested("childhands", "parenthands")
#' 
#' # another way to merge nested, but contains NAs for cells that are not fully within one another
#' # since these data are not nested, you can see how much is being thrown away compared to x
#' y <- temporal_align(all.cols=c("childhands", "parenthands"), fps=60, keep.frames=FALSE)
#' @export
merge_nested <- function(c1, 
                         c2, 
                         folder = getOption("datavyur.folder"),
                         classes = getOption("datavyur.classlist"),
                         ...)
{
    
    # grab upper and lower level data
    dat_list <- import_column(c(c1, c2), 
                              folder = folder,
                              append.colnames = FALSE, 
                              asList = TRUE, ...)
    
    l1 <- data.table::as.data.table(dat_list[[1]])
    l2 <- data.table::as.data.table(dat_list[[2]])
    
    # ADD timestamps checks here
    
    # start merge
    grab_within <- function(fname, cell, on, off, d2) {
        d2_sub <- d2[file==fname & onset >= on & offset < off, ]
        d2_sub[, `:=` (file=NULL)]
        append_colname(d2_sub, c2, except="file")
        return(d2_sub)
    }
    merged <- l1[, grab_within(file, ordinal, onset, offset, l2), by=names(l1)]
    
    # add rest of col prefixes
    non_append <- c("file", names(merged)[grepl(paste0(c2,"."), names(merged))])
    append_colname(merged, c1, except = non_append)
    
    return(as.data.frame(merged))
}


#' Temporally align data by frame
#' 
#' This will align several Datavyu columns by common frame number
#' 
#' You have the option to set the framerate using \code{fps}. The lower the frame rate, the less likely
#' two events will line up in time. This is because the timestamps are converted to frame numbers based on chunking into bins.
#' The larger the fps, the larger the bins, and more likely two events will line up in time.
#' Note: Sometimes R doesn't get the right class of the imported column argument. 
#' This can happen if you have weird characters in your data. Use \code{classes} to override this.
#' 
#' @param all.opf Use all found .opf files from \code{folder} or specify .opf file names to use. Must 
#' be a character vector with exact file name matches as seen inside one of the exported .csv files, which are 
#' just the original file names with .opf removed. If you choose to provide full path and file names, 
#' you MUST set \code{folder=NULL}. 
#' @param all.cols Use all columns found in \code{folder} or only use specified column names entered
#' as a character vector. 
#' @param fps Common framerate to use for alignment. Defaults to 30 frames per second video.
#' @param keep.frames Keep the frame_number column, which will result in a much larger dataset.
#' If \code{keep.frames=FALSE}, then all you know is which two events overlap to some degree, not knowing for how long.
#' If \code{keep.frames=TRUE} (default), you can calculate the number of frames that overlap between two events, and if fps is know,
#' you can convert this total to milliseconds.
#' @param classes List of new classes to override guessed classes when reading in .csv data. see \code{data.table::fread}
#' @param folder Defaults to option \code{datavyur.folder}.
#' @param ... Additional arguments passed to \code{data.table::fread}, 
#' except \code{stringsAsFactors, colClasses, verbose, showProgress}
#' @return data.frame with aligned data. This will be very large!
#' @export
#' @examples
#' # set folder path if needed, otherwise use default path with example data.
#' # options(datavyur.folder="mydatafolder")
#' 
#' # example data with no arguments
#' ex_data_aligned <- temporal_align()
#' 
#' # example data selecting only one of the columns
#' ex_data_aligned2 <- temporal_align(all.cols="childhands")
#' 
#' # example data using additional arguments
#' newClasses <- list(integer=c("childhands.look", "parenthands.look"), 
#'                    character=c("childhands.hand", "parenthands.hand"))
#' ex_data_aligned3 <- temporal_align(fps=10, classes=newClasses)
temporal_align <- function(all.opf = TRUE,
                           all.cols = TRUE,
                           folder = getOption("datavyur.folder"),
                           fps = 30,
                           keep.frames = TRUE,
                           classes = getOption("datavyur.classlist"),
                           ...)
{
    
    dat <- align_routine(
        ordinal = FALSE,
        all.opf = all.opf,
        all.cols = all.cols,
        folder = folder,
        fps = fps,
        class_overwrite = classes,
        ...
    )
    
    if (!keep.frames) {
        dat[, frame_number := NULL]
        dat <- unique(dat)
    }
    
    return(as.data.frame(dat))
}


#' Ordinal alignment
#' 
#' Align and merge data by cell number (ordinal)
#'
#' See \code{\link{temporal_align}} for more details on function usage. 
#' 
#' @inheritParams temporal_align
#' @return A data.frame with merged data aligned by cell number (ordinal value)
#' @export
#' @seealso \code{\link{temporal_align}}
#' @examples
#' ordinal_align()
ordinal_align <- function(all.opf = TRUE,
                          all.cols = TRUE,
                          folder = getOption("datavyur.folder"),
                          classes = getOption("datavyur.classlist"),
                          ...)
{
    dat <- align_routine(
        ordinal = TRUE,
        all.opf = all.opf,
        all.cols = all.cols,
        folder = folder,
        fps = NA,
        class_overwrite = classes,
        ...
    )
    return(as.data.frame(dat))
}


#' Check columns for bad codes
#' 
#' Check for invalid codes used for each column and argument specified
#' 
#' This takes an already existing data.frame/data.table and checks specific columns 
#' to see if they contain only the codes listed in \code{code_list}. Each item in the 
#' list must be named according to the column name in the data.frame, and each item
#' must contain a vector of 1 or more of valid codes to check. Codes can be numeric or
#' characters.
#'
#' @param code_list A list of column names with each name having a vector of codes to check.
#' @param dat The data.frame/data.table to check
#' @param as.na If \code{TRUE}, will return a new data set with \code{NA} instead of the bad code.
#'
#' @return A list containing two items. \code{$data} is the new data with NAs, 
#' \code{$bad_codes} is another list, each item corresponding to the input list. In 
#' each there is a data.frame that is either empty (no bad codes found), or contains indices
#' for the row numbers with bad codes and the column name and type of bad code found. 
#' If \code{as.na=FALSE}, the new data will be \code{NULL}, and will only return
#' \code{$bad_codes}, if any.
#' @export
#' @examples
#' # Use example data
#' dat <- ordinal_align()
#' 
#' # Make the list of valid codes, names corresponding to columns in the data
#' code_list <- list(
#'     childhands.hand = c("left", "right", "both"),
#'     childhands.look = c(0,1),
#'     parenthands.hand = c("left", "right", "both"),
#'     parenthands.look = c(0,1)
#' )
#' 
#' # check for bad codes, returning new data with bad codes as NAs
#' codes_checked <- check_codes(code_list, dat)
check_codes <- function(code_list, 
                        dat, 
                        as.na=TRUE)
{
    # check if list
    if (class(code_list) != "list") {
        stop(simpleError("code_list must be a list of column names and valid codes"))
    }
    
    # make as data.table
    if (!any(class(dat) == "data.table")) {
        d <- data.table::as.data.table(dat)
    } else {
        d <- data.table::copy(dat)
    }
    data.table::setkey(d)
    
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
        bad_list[[n]] <- d[not_ok, .(V1 = not_ok, V2 = get(n))]
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


#' Check for invalid timestamps
#'
#' @param ts_list A list with each item corresponding to either a pair of timestamp columns
#' \code{c(onset, offset)}, or just a single column name. If a single name, durations will not be checked,
#' and only out of range will be checked. 
#' @param dat A data.frame/data.table that contains the columns with the onset/offset timestamps
#' @param tmin Minimum allowed timestamp. Defaults to 0 (milliseconds)
#' @param tmax Maximum allowed timestamp. Defaults to one day in milliseconds.
#' @param as.na Set bad timestamps to \code{NA}
#'
#' @return Returns a list with 3 items, \code{$data, $ranges, $durations}. 
#' \code{$data} is new data with bad timestamps as \code{NA}. \code{$ranges} is
#' a list of column names, with a vector of indices corresponding to timestamps
#' out of range. If all okay, the vector is empty. \code{$durations} is a list
#' of bad timestamp durations. If \code{as.na=TRUE}, the offset timestamp is 
#' set to \code{NA}.
#' @export
#' @examples
#' # Use example data
#' dat <- ordinal_align()
#' 
#' # Make the list of timestamps to check in the example data
#' # These check durations as well as out of range
#' ts_list <- list(
#'     c(on="parenthands.onset", off="parenthands.offset"), # explicit
#'     child = c("childhands.onset", "childhands.offset") # infer from order which is on/off
#' )
#' ts_checked <- check_timestamps(ts_list, dat)
#' 
#' # This only checks for bad ranges since its only one column name.
#' # ts_list can be mixed with 2 or 1 items each.
#' ts_list <- list("parenthands.offset")
#' ts_checked2 <- check_timestamps(ts_list, dat)
check_timestamps <- function(ts_list, 
                             dat, 
                             tmin=0, 
                             tmax=864e5, 
                             as.na=TRUE)
{
    
    # check if list
    if (class(ts_list) != "list") {
        stop(simpleError(
            "ts_list must be a list object. see ?check_timestamps"
        ))
    }
    
    # make as data.table
    if (!any(class(dat) == "data.table")) {
        d <- data.table::as.data.table(dat)
    } else {
        d <- data.table::copy(dat)
    }
    data.table::setkey(d)
    
    # range and duration subfunctions
    range_conditions <- function(x) {
        (x >= tmin & x <= tmax) #& !is.na(x)
    }
    
    duration_condition <- function(x, y) {
        z <- y - x
        z >= 0 & abs(z) <= tmax - tmin
    }
    
    # assess common column names
    cnames <- unique(unlist(ts_list))
    dnames <- names(d)
    valid_names <- cnames[cnames %in% dnames]
    invalid_names <- cnames[!cnames %in% dnames]
    
    if (length(invalid_names) != 0) {
        w1 <- paste0(invalid_names, collapse=", ")
        w2 <- paste0("columns not found in data: ", w1)
        stop(simpleError(w2))
    }
    
    # find bad ranges
    bad_rng <- lapply(valid_names, function(i) {
        which(!range_conditions(d[[i]]))
    })
    names(bad_rng) <- valid_names
    
    # find bad durations
    bad_dur <- lapply(ts_list, function(i) {
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
            bad_dur <-  which(!duration_condition(d[[on]], d[[off]]))
            if (length(bad_dur) > 0 & as.na) d[bad_dur, eval(off) := NA]
        } else {
            stop(simpleError(
                "each item in ts_list must be of length 1 or 2"
            ))
        }
        return(bad_dur)
    })
    
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

#' R data to Datavyu .csv file
#' 
#' Process to transfer R data to Datavyu
#' 
#' Exports R data (as a list of lists or dataframes corresponding to Datavyu columns) to a .csv file. 
#' This can then be used by Datavyu for saving as .opf and importing R data Datvyu. 
#' Each list item is a different column in the final Datavyu file.
#' NOTE: Datavyu cannot currently import the .csv files this function creates.
#' To get the .csv file back into Datavyu, use the \code{csv2opf.rb} file found here: 
#' \url{http://github.com/iamamutt/datavyu/general}.
#' 
#' @param rlist List of lists or data.frames. These are the columns to be used in the final Datavyu file.
#' @param filename Filename of the .csv file to be created. Leave off extension. May specify path as prefix.
#' @examples
#' # First get example data to use
#' example_data <- datavyu_dat()
#' 
#' # See how the example data is structured, as a list with another list and data.frame
#' # Both the list and data.frame are named. If not named, one will be assigned.
#' str(example_data)
#' 
#' # Export R list to a .csv file for importing into Datavyu
#' r2datavyu(example_data, "example_file")
#' @export
r2datavyu <- function(rlist, filename="datavyur_export") {
    
    warnText <- paste0(
        "\nNote: At the moment there doesn't seem to be a way for datavyu to import", 
        " a .csv even though you can export to one. To get it back to .opf, use", 
        " the csv2opf.rb ruby script in the general folder.\n"
    )
    
    warning(simpleWarning(warnText))
    
    if (any(class(rlist) != "list")) {
        if (any(class(rlist) == "data.frame")) {
            rlist <- list(rlist)
        } else {
            stop('Input must be a list of lists, list of data.frames, or a single data.frame')
        }
    }
    
    
    na2val <- function(x, v="") ifelse(is.na(x), v, x)
    
    top_digit <- "#4"
    
    n_col <- length(rlist)
    col_names <- names(rlist)
    
    if (n_col < 1) stop(simpleError("no columns found in r list object"))
    
    # check for named rlist, add new names if necessary
    if (is.null(col_names)) {
        new_names <- paste0("datavyur", 1:n_col)
        names(rlist) <- new_names
    } else {
        no_names <- col_names == ""
        if (any(no_names)) {
            new_names <- paste0("datavyur", 1:n_col)[no_names]
            names(rlist)[no_names] <- new_names
        }
    }
    
    col_names <- names(rlist)
    
    # go through each column structured as an r list
    each_col <- lapply(1:n_col, function(col) {
        
        # get names of codes
        codes <- rlist[[col]]
        col_name <- col_names[col]
        code_names <- names(codes)
        
        if (is.null(code_names)) {
            stop(simpleError(
                paste0("ordinal, onset, offset not found for: ", col_name)
            ))
        }
        
        # check if codes have these common arguments
        common_code_names <- c("ordinal", "onset", "offset")
        common_codes_l <- common_code_names %in% code_names
        custom_code_names <- code_names[!code_names %in% common_code_names]
        
        if (!all(common_codes_l)) {
            stop(simpleError(
                paste0("ordinal, onset, offset not found for: ", col_name)
            ))
        }
        
        ts_ord <- codes$ordinal
        ts_on <- ms2time(codes$onset)[ts_ord]
        ts_off <- ms2time(codes$offset)[ts_ord]
        
        if (length(custom_code_names) == 0) {
            codes$code1 <- rep(NA, length(ts_ord))
            custom_code_names <- "code1"
        }
        
        code_str <- paste0(custom_code_names, "|NOMINAL", collapse=",")
        col_str <- paste0(col_name, " (MATRIX,true,)-", code_str)
        
        code_mat <- lapply(custom_code_names, function(cn) {
            na2val(as.character(codes[[cn]])[ts_ord])
        })
        
        code_mat <- cbind(ts_on, ts_off, do.call(cbind, code_mat))
        
        col_dat <- apply(code_mat, 1, function(s) {
            code_text <- paste0("(", paste0(s[-c(1,2)], collapse=","), ")", collapse="")
            paste0(s[1], ",", s[2], ",", code_text)
        })
        
        return(c(col_str, col_dat))
    })
    
    text_lines <- c(top_digit, c(each_col, recursive=TRUE))
    out_file <- file(paste0(tools::file_path_sans_ext(filename), ".csv"), "w")
    writeLines(text_lines, out_file)
    close(out_file)
}
