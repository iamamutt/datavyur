## ---- echo=FALSE, results='hide', message=FALSE--------------------------
knitr::opts_chunk$set(eval=FALSE, comment = ">", message = FALSE)
library(datavyur)

## ---- eval=FALSE---------------------------------------------------------
#  vignette("data_manipulation", "datavyur")

## ------------------------------------------------------------------------
#  # Load the datavyur library to use the functions below
#  library(datavyur)

## ------------------------------------------------------------------------
#  # Replace the string path/to/data with the location to where your data is located, relative to working directory.
#  options(datavyur.folder="path/to/data")

## ------------------------------------------------------------------------
#  getOption("datavyur.folder")

## ---- echo=FALSE, eval=TRUE----------------------------------------------
list.files(getOption("datavyur.folder"), pattern = "\\.csv$")

## ------------------------------------------------------------------------
#  # Find the full path to where the .csv files have been saved
#  # Replace path/to/data with the correct path
#  data_path <- normalizePath("path/to/data")

## ------------------------------------------------------------------------
#  # see help documentation
#  ?import_column

## ------------------------------------------------------------------------
#  # see help documentation
#  ?datavyur

## ------------------------------------------------------------------------
#  # if datavyur.folder option has been set
#  datavyu_col_search()
#  
#  # otherwise use explicit folder path stored in data_path
#  datavyu_col_search(data_path)

## ---- echo=FALSE, eval=TRUE, results='asis'------------------------------
# if datavyur.folder option has been set
fdat <- datavyu_col_search()
fdat$local <- "./*.csv"
knitr::kable(head(fdat, 11))

## ---- eval=TRUE----------------------------------------------------------
# if datavyur.folder option has been set
datavyu_col_search(unq=TRUE, cnames="column")

## ------------------------------------------------------------------------
#  # if datavyur.folder option has been set
#  datavyu_col_search(unq=TRUE, cnames=c("column", "codes"))

## ---- echo=FALSE, eval=TRUE, results='asis'------------------------------
knitr::kable(datavyu_col_search(unq=TRUE, cnames=c("column", "codes")))

## ---- eval=TRUE----------------------------------------------------------
child_hands <- import_column("childhands") # or import_column("childhands", data_path)

## ---- echo=FALSE, eval=TRUE, results='asis'------------------------------
knitr::kable(head(child_hands))

## ---- eval=TRUE----------------------------------------------------------
parent_hands <- import_column("parenthands", append.colnames = TRUE)

## ---- echo=FALSE, eval=TRUE, results='asis'------------------------------
knitr::kable(head(parent_hands))

## ---- eval=TRUE----------------------------------------------------------
# load column "childhands" and combine across separate .opf files
child_hands <- import_column("childhands", append.colnames = TRUE)

## ---- eval=TRUE----------------------------------------------------------
# named list of valid codes, where names are extact matches of real column names
child_hands_codes <- list(
    childhands.hand = c("left", "right", "both"),
    childhands.look = c(1, 0)
    )

# pass the list to the function check_codes
checked_list <- check_codes(child_hands_codes, child_hands)

## ---- eval=TRUE----------------------------------------------------------
checked_list$bad_codes$childhands.hand

## ---- eval=TRUE----------------------------------------------------------
checked_list$bad_codes$childhands.look

## ---- eval=TRUE----------------------------------------------------------
child_hands <- checked_list$data

## ---- eval=TRUE----------------------------------------------------------
timestamps_to_check <- list(
    c(on="childhands.onset", off="childhands.offset")
)

checked_ts <- check_timestamps(timestamps_to_check, child_hands)

## ---- eval=TRUE----------------------------------------------------------
# overwrite old data with new data and bad timestamps as NA
child_hands <- checked_ts$data

# see bad timestamp ranges, no min and max was specified so nothing was bad
checked_ts$ranges

# see row indices with bad durations
checked_ts$durations

## ------------------------------------------------------------------------
#  write.csv(child_hands, file="child_cleaned.csv", row.names=FALSE, na="")

## ---- eval=TRUE----------------------------------------------------------
# merges childhands columns within parenthands columns
my_nested_data <- merge_nested("parenthands", "childhands")

## ---- eval=TRUE, echo=FALSE, results='asis'------------------------------
knitr::kable(head(my_nested_data, 15))

## ------------------------------------------------------------------------
#  t_aligned <- temporal_align()

## ------------------------------------------------------------------------
#  # merge columns and sort by cell number
#  o_aligned <- ordinal_align()

## ------------------------------------------------------------------------
#  # provide a list of data to convert
#  fake_data <- datavyu_dat()
#  r2datavyu(fake_data, "myexport")

## ------------------------------------------------------------------------
#  x <- as.data.frame(datavyu_dat(n1=25, n2=2)[[1]])
#  y <- datavyu_dat(n1=2, n2=100)[[2]]

## ---- eval=TRUE----------------------------------------------------------
# print milliseconds to time string (one hour)
ms2time(3600000)

## ---- eval=TRUE----------------------------------------------------------
ms <- 999
ts2frame(ms, fps = 30)
ts2frame(ms, fps = 60)

