# Subfunctions ------------------------------------------------------------

.dv_test <- function() {
    glob <- list(
        all.opf=TRUE,
        all.cols=TRUE,
        folder=getOption("datavyur.folder"),
        class_overwrite=NULL,
        column="childhands",
        fps=30
    )
    for (i in 1:length(glob)) {
        assign(names(glob)[i], glob[[i]], .GlobalEnv)
    }
}

# on:off sequence
frame_expand <- function(onset, offset) {
    if (offset < onset) {
        frames <- NA
    } else {
        frames <- seq(onset, offset, 1)
    }
    return(as.integer(frames))
}

# read all .csv data from folder and check for valid format
check_opf_data <- function(folder = getOption("datavyur.folder"), files = FALSE)
{
    if (is.null(folder) && !is.logical(files)) {
        filepaths <- files
    } else {
        folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)
        filepaths <- list.files(folder, full.names=TRUE, pattern="\\.csv$")
        if (length(filepaths) == 0) {
            stop(simpleError(
                paste0("No .csv files found in ", folder)
            ))
        }
    }

    static_names <- c("file", "column")
    out_list <- lapply(filepaths, function(x) {
        DT <- suppressWarnings(
            data.table::fread(x, 
                              stringsAsFactors=FALSE, 
                              verbose=FALSE, 
                              showProgress=FALSE)
        )
        all_names <- names(DT)
        args_in_col <- all_names[!all_names %in% static_names]
        if (all(static_names %in% all_names)) {
            d_file <- DT[, unique(file)]
            c_type <- DT[, unique(column)]
            dat <- data.table::data.table(
                codes=args_in_col,
                classes = sapply(DT[, args_in_col, with=FALSE], typeof),
                file=d_file,
                column=c_type,
                local=x
            )
            return(dat)
        } else {
            return(NULL)  
        }
    })
    dat <- do.call(rbind, out_list)
    
    if (is.null(dat)) {
        stop(simpleError(
            paste0("No .csv files found from using datavyu2csv.rb in folder: ", folder)
        ))
    }
    
    data.table::setkey(dat)
    dat <- unique(dat)
    data.table::setcolorder(dat, c("file", "column", "codes", "classes", "local"))
    
    return(dat)
}

# subset read-in data, estimate classes
opf_and_col_selector <- function(all.opf = TRUE, 
                                 all.cols = TRUE, 
                                 folder = getOption("datavyur.folder")
                                 )
{
    if (is.logical(all.opf) && isTRUE(all.opf)) {
        fdat <- check_opf_data(folder = folder)
        fnames <- unique(fdat$file)
        if (length(fnames) == 0) {
            stop(simpleError("Could not find any files from all.opf input"))
        }
    } else if (is.character(all.opf)) {
        fdat <- check_opf_data(folder = folder, files = all.opf)
        fnames <- fdat$file
    } else {
        fErr <- paste0("Set all.opf to TRUE or use a character vector",
                       " of opf file names. You can find names of files", 
                       " from the exported .csv files using the script", 
                       " datavyu2csv.rb")
        stop(simpleError(fErr))
    }

    est_classes <- fdat[, .N, by=list(column, codes, classes)][order(codes, N, classes), ]
    est_classes <- est_classes[, .(classes=classes[which.max(N)]), by=list(column, codes)]
    est_classes[classes == "logical", classes := "character"]
    
    fdat[, classes := NULL]
    fdat <- merge(fdat, est_classes, by = c("column", "codes"), all=TRUE)
    
    if (is.logical(all.cols) && isTRUE(all.cols)) {
        cnames <- unique(fdat$column)
        if (length(cnames) == 0) {
            stop(simpleError("Could not find `column` in .csv file"))
        }
    } else if (is.character(all.cols)) {
        cnames <- fdat[column %in% all.cols, unique(column)]
        if (length(cnames) != length(all.cols)) {
            errm <- paste0(c("Could not find columns: ", 
                             paste0(all.cols[!all.cols %in% cnames], 
                                    collapse = ", ")), 
                           collapse = " ")
            stop(simpleError(errm))
        }
    } else {
        stop(simpleError(
            paste0("Set all.cols to TRUE or use a character vector of column",
                   "names exactly as in the opf file")
        ))
    }
    
    fdat <- fdat[file %in% fnames & column %in% cnames, ]
    
    return(fdat)
}

# override some of the classes if necessary
override_typeofs <- function(opf_info, class_overwrite) {
    
    # classes for each argument, add ordinal
    est_classes <- unique(opf_info[, c("column", "codes", "classes"), with=FALSE])
    est_classes[, both := paste0(column, ".", codes)]
    
    if (!is.null(class_overwrite)) {
        if (class(class_overwrite) != "list") {
            stop(simpleError(
                paste("classes must be a  named list of typenames, and",
                "each item in the list a vector of colnames as characters")
            ))
        }
        class_names <- names(class_overwrite)
        for (i in 1:length(class_overwrite)) {
            est_classes[both %in% class_overwrite[[i]], classes := class_names[i]]
        }
    }
    
    return(est_classes)
}

# import datavyur .csv file, check classes and codes
opf_col_import <- function(fpath, cname, est_classes, ...) {
    
    # class defaults
    static_classes <- c(file="character",
                        column="character",
                        onset="integer",
                        offset="integer",
                        ordinal="integer")
    
    # read default options
    fread_opts <- list(stringsAsFactors=FALSE,
                       verbose=FALSE,
                       showProgress=FALSE,
                       colClasses=static_classes)
    
    # overwrite default fread options
    ops <- list(...)
    ops_names <- names(ops)
    if (any(ops_names == "colClasses")) {
        warning(simpleWarning(
            paste("Don't use colClasses option for data.table::fread",
                  "To overwrite classes, use classes argument as a list")
        ))
        ops[names(ops) == "colClasses"] <- NULL
    }
    toset <- !(names(fread_opts) %in% names(ops))
    fread_opts <- c(list(input=fpath), c(fread_opts[toset], ops))
    
    # read in data with options
    DT <- suppressWarnings(do.call(data.table::fread, fread_opts))
    
    # remove unecessary column id
    DT[, column := NULL]
    
    # check existing columns and codes
    current_cols <- est_classes[column == cname, ]
    need_add <- current_cols[!codes %in% names(DT), .(codes, classes)]
    
    # add arguments if missing
    if (nrow(need_add) > 0) {
        for (i in 1:nrow(need_add)) {
            DT[, eval(need_add[i, codes]) := NA]
        }
    }
    
    # sort names
    sort_first <- c("file", "ordinal", "onset", "offset")
    sort_last <- sort(names(DT)[!names(DT) %in% sort_first])
    data.table::setcolorder(DT, c(sort_first, sort_last))
    
    # overwrite classes from estimated classes
    check_args <- current_cols[, codes]
    check_class <- current_cols[, classes]
    for (i in 1:length(check_args)) {
        if (typeof(DT[[check_args[i]]]) != check_class[i]) {
            suppressWarnings(
                class(DT[[check_args[i]]]) <- check_class[i]
            )
        }
    }
    
    return(DT)
}


# main routine for temporal, ordinal alignment
align_routine <- function(ordinal,
                          all.opf = TRUE,
                          all.cols = TRUE,
                          folder = getOption("datavyur.folder"),
                          fps = 30,
                          class_overwrite,
                          ...)
{
    if (ordinal) {
        id_cols <- c("file", "ordinal")
    } else {
        id_cols <- c("file", "frame_number")
    }
    
    # get list of opf files, columns, codes, locations
    message("Searching through .csv files for valid .opf data...")
    opf_info <- opf_and_col_selector(all.opf = all.opf, 
                                     all.cols = all.cols, 
                                     folder = folder)
    
    # override classes
    est_classes <- override_typeofs(opf_info, class_overwrite)
    
    message("Importing all located Datavyu data into R...")
    
    opf_list <- lapply(unique(opf_info$file), function(opf) {
        # DEBUG: opf <- opf_info$file[1]
        
        # columns to find in current file
        col_names <- opf_info[file==opf, unique(column)]
        
        # cycle through each column and import data
        to_merge <- lapply(col_names, function(clm) {
            # DEBUG: clm <- opf_info[file==opf, unique(column)][1]
            
            # path to column .csv for specific .opf file
            fpath <- opf_info[file==opf & column==clm, unique(local)]
            
            # import .csv
            DT <- opf_col_import(fpath, clm, est_classes, ...)
            
            if (!ordinal) {
                # expand time to frame counts
                DT <- reshape_by_frame(DT, fps)
            }
            
            # add column prefix to argument names
            DT <- append_colname(DT, clm, id_cols)
            
            # get rid of duplicates (possibly due to same files in folder)
            DT <- unique(DT)
            
            return(data.table::copy(DT))
        })
        
        # begin file merge
        if (length(to_merge) > 1) {
            merged <- multi_merge(to_merge, 
                                  by = id_cols, 
                                  all = TRUE, 
                                  allow.cartesian = TRUE)
        } else {
            merged <- to_merge[[1]]
        }
        
        # set key for larger merge later
        data.table::setkey(merged)
        merged_opf <- data.table::copy(unique(merged))
        
        return(merged_opf)
    })
    
    message("Merging all .opf files...")
    
    # some files may not have all the necessary arguments
    # add them if necessary
    if (ordinal) { # ordinal used as id_col
        est_classes <- est_classes[codes !=  "ordinal", ]
    }
    opf_list <- add_columns(opf_list, est_classes)
    
    # names of columns for all list items
    all_names <- unique(unlist(lapply(opf_list, function(i) names(i))))
    
    # begin merging all files into one large dataset
    opf_merged <- multi_merge(opf_list, by=all_names, all=TRUE)
    
    # some cleanup
    opf_merged <- opf_merged[order(get(id_cols)), ]
    data.table::setkey(opf_merged)
    opf_merged <- unique(opf_merged)
    
    rowNAs <- apply(opf_merged[, !names(opf_merged) %in% id_cols, 
                               with=FALSE], 1, function(i) {
                                   all(is.na(i))  
                               })
    
    opf_merged <- opf_merged[rowNAs==FALSE, ]
    
    message("Merge successful!")
    
    data.table::setcolorder(opf_merged, name_sort(names(opf_merged), est_classes))
    
    return(opf_merged)
}

# get names but with custom ordering
name_sort <- function(n, est_classes) {
    est_classes[, cord := as.integer(NA)]
    morder <- c("ordinal", "onset", "offset")
    for (i in 1:length(morder)) {
        est_classes[codes == morder[i], cord := i]
    }
    est_classes[is.na(cord), cord := length(morder)+1]
    arg_order <- est_classes[order(column, cord, codes), both]
    arg_order <- c("file", "ordinal", "frame_number", arg_order)
    
    if (all(n %in% arg_order)) {
        new_n <- n[na.omit(match(arg_order, n))]
    } else {
        stop(simpleError(
            "important columns missing during column arrange"
        ))    
    }
    
    return(new_n)
}

# used with temporal alignment
reshape_by_frame <- function(DT, fps) {
    DT <- data.table::copy(DT)
    # convert timestamps to frame counts
    DT[, `:=`(onset.f=ts2frame(onset, fps=fps, warn=FALSE), 
              offset.f=ts2frame(offset, fps=fps, warn=FALSE))]
    # remove bad
    DT <- DT[!(is.na(onset.f) | is.na(offset.f)), ]
    # frame expansion
    DT <- DT[, .(frame_number=as.integer(frame_expand(onset.f, offset.f))), 
             by=names(DT)]
    DT[, `:=` (onset.f=NULL, offset.f=NULL)]
    return(DT)
}

# add column.codes suffixes
append_colname <- function(DT, clm, except=NA) {
    new_suffixes <- names(DT)[!names(DT) %in% except]
    data.table::setnames(DT, new_suffixes, paste0(clm, ".", new_suffixes))
    return(DT)
}

# add whole datavyu columns if missing
add_columns <- function(opf_list, est_classes) {
    necessary_names <- est_classes$both
    necessary_classes <- est_classes$classes
    
    opf_list <- lapply(opf_list, function(i) {
        lnames <- names(i)
        need_idx <- !necessary_names %in% lnames
        if (any(need_idx)) {
            new_names <- necessary_names[need_idx]
            new_cls <- necessary_classes[need_idx]
            for (j in 1:length(new_names)) {
                i[[new_names[j]]] <- NA
                class(i[[new_names[j]]]) <- new_cls[j]
            }
        }
        return(i)
    })
    
    return(opf_list)
}

# check a list of dataframes for matching column names
check_colnames_match <- function(clist) {
    namestats <- lapply(clist, function(i) {
        list(cnames = sort(names(i)),
             file=i$file[1],
             n=ncol(i))
    })
    temp <- Reduce(function(x, y) {
        xcn <- x$cnames
        ycn <- y$cnames
        if ((x$n != y$n) || any(xcn != ycn)) {
            xn <- paste(xcn, collapse=", ")
            yn <- paste(ycn, collapse=", ")
            err <- paste0("codes for ", y$file[1],
                          " don't match codes for files(s) ",
                          x$file[1],
                          "\n", y$file[1], ": ", yn,
                          "\n", x$file[1], ": ", xn
            )
            stop(simpleError(err))
        }
        zcn <- sort(unique(c(xcn, ycn)))
        f <- paste(c(x$file[1], y$file[1]), collapse=", ")
        z <- list(cnames=zcn, file=f, n=length(zcn))
        return(z)
    }, namestats, accumulate = FALSE)
    return(invisible())
}
