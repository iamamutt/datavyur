# Other exported functions -----------------------------------------------------


#' Fake Datavyu data
#' 
#' This function will create fake data in the R format needed to import back into the Datavyu software
#'
#' The function shows how you can have either a list of columns or an actual data frame.
#' Either way will work.
#' 
#' @param n1 Sample size for variable 1
#' @param n2 Sample size for variable 2
#' @return List of datavyu formated data
#' @examples
#' my_data <- datavyu_dat()
#' @export
datavyu_dat <- function(n1 = 10, n2 = 15) {

    n1 <- max(c(1, n1))
    n2 <- max(c(1, n2))

    ch_on <- sort(round(runif(n1, 0, 3600000)))
    pr_on <- sort(round(runif(n2, 0, 3600000)))
    
    
    ch_off <- abs(round(runif(n1, ch_on+1, c(ch_on[2:n1]-1, 3600000))))
    pr_off <- abs(round(runif(n2, pr_on+1, c(pr_on[2:n2]-1, 3600000))))
    
    hand_char <- c("left", "right", "both", "")
    look_val <- c("0", "1", "")
    
    dat <- list(
        childhands = list(
            ordinal=1:n1,
            onset=ch_on,
            offset=ch_off,
            hand=sample(hand_char, n1, replace=TRUE),
            look=sample(look_val, n1, replace=TRUE)
        ),
        parenthands = data.frame(
            ordinal=1:n2,
            onset=pr_on,
            offset=pr_off,
            hand=sample(hand_char, n2, replace=TRUE),
            look=sample(look_val, n2, replace=TRUE)
        )
    )
    return(dat)
}


#' Multiple data merge
#' 
#' Merge data into a single data structure from a list of data.frames/tables
#'
#' @param data_list List of separate data.frames/tables to merge
#' @param ... Additional arguments passed to \code{merge}
#'
#' @return A data.frame/data.table, depending on the input data in the list
#' @export
#'
#' @examples
#' d1 <- datavyu_dat()$parenthands
#' d2 <- as.data.frame(datavyu_dat()$childhands)
#' d3 <- datavyu_dat(n2=50)$parenthands
#' data_list <- list(d1, d2, d3)
#' merged_data <- multi_merge(data_list, all=TRUE)
multi_merge <- function(data_list, ...) {
    Reduce(function(x, y) {
        #if (data.table::is.data.table(x)) data.table::setkey(x)
        #if (data.table::is.data.table(y)) data.table::setkey(y)
        merge(x, y, ...)
    }, data_list)
}


#' Convert milliseconds to a time string
#' 
#' This will take a duration of time in milliseconds and convert it to a time string format
#' 
#' @param timestamp A numeric time duration, such as \code{1102013} ms
#' @param unit How the string will be contructed. Default is Hours:Minutes:Seconds.MS
#' @param msSep How the separator between seconds and ms will look like
#' @examples
#' # 18 minutes and 22 seconds and 13 milliseconds
#' ms2time(1102013)
#' @export
ms2time <- function(timestamp, 
                    unit="%H:%M:%S", 
                    msSep=":") 
{
    # 60*60*1000*23.99999972209
    if (any(timestamp >= 24*60*60*1000)) stop(simpleError("More than 24 hours. Can't do days"))
    sec <- timestamp/1000
    ms <- formatC(x=round((sec-trunc(sec))*1000, digits=3), 
                  digits=3, width=3, format="d", flag="0")
    sec <- trunc(sec)
    start <- as.POSIXct(Sys.time())
    dt <- difftime(start+sec, start, units="secs")
    time_char <- paste0(format(.POSIXct(dt, tz="GMT"), unit), msSep, ms)
    return(time_char)
}


#' Convert timestamps to frame numbers
#'
#' @param x Vector of timestamps
#' @param fps Frames per second of the video source. 
#' Defaults to 30 Frames Per Second. The smaller the value, the more likely 
#' two events will be chunked in the same frame.
#' @param tstart Start timestamp. Anything below start will not be converted. 
#' @param tend End timestamp. Anything above will be NA. Defaults to max of x if not set. 
#' @param chunked If set to TRUE, will return a time back to you instead of frame number, 
#' but the chunked/cut value corresponding to that frame. 
#' @param warn Turn on/off warnings for NAs
#' @return numeric vector same size as x
#' @export
#'
#' @examples
#' t <- c(-1000,4047,7451,14347,17424,21673,27920,30669,39798,42504,49995,51451,56034)
#' ts2frame(t)
ts2frame <- function(x, 
                     fps=30, 
                     tstart=0, 
                     tend, 
                     chunked=FALSE, 
                     warn=TRUE)
{
    foa <- 1000 / fps
    if (missing(tend)) tend <- max(x)
    tinterval <- seq(tstart, tend + foa - ((tend-tstart) %% foa), foa)
    f <- findInterval(x, tinterval, rightmost.closed=FALSE, all.inside=FALSE)
    f[x < tstart | x > tend] <- NA
    if (any(is.na(f)) && warn) warning(simpleWarning("Found NAs for some frames"))
    
    if (chunked) {
        return(tinterval[f])
    } else {
        return(f)  
    }
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
            d <- dat[i$index, ]
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
                d <- dat[i, ]
                return(d)
            }
        })
    } else {
        stop("Could not find ranges list item. Did you use check_timestamps function?")
    }
    
    if (!is.null(bad_durations)) {
        durations <- lapply(bad_durations, function(i) {
            if (length(i) > 0) {
                d <- dat[i, ]
                return(d)
            }
        })
    } else {
        durations <- NULL
    }
    
    return(list(ranges=ranges, durations=durations))
}

