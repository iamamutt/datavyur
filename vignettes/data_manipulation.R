## ---- echo=FALSE, results='hide', message=FALSE, warning = FALSE---------
knitr::opts_chunk$set(eval = TRUE, comment = ">", 
                      message = FALSE, 
                      warning = FALSE)
library(datavyur)
library(dplyr)

## ------------------------------------------------------------------------
# load the packages we are going to be using, must be installed first
library(datavyur)
library(dplyr)

# align data from chilhands and parenthands columns by file and frame number
mydata <- temporal_align()

## ------------------------------------------------------------------------
# create list of valid codes
codes_to_check  <- list(
        childhands.hand = c("left", "right", "both"),
        childhands.look = c(1, 0),
        parenthands.hand = c("left", "right", "both"),
        parenthands.look = c(1, 0)
    )

# create list of timestamp columns
timestamps_to_check <- list(
    child = c(on="childhands.onset", off="childhands.offset"),
    parent = c(on="parenthands.onset", off="parenthands.offset")
)

# check and overwrite existing data
mydata <- check_codes(codes_to_check, mydata)$data
mydata <- check_timestamps(timestamps_to_check, mydata)$data

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(rbind(head(mydata), tail(mydata)))

## ---- eval=FALSE---------------------------------------------------------
#  vignette("introduction", "dplyr")

## ---- eval=FALSE---------------------------------------------------------
#  # get help on dplyr::filter
#  ?filter

## ---- eval=FALSE---------------------------------------------------------
#  # 1. from mydata, select all id and parenthands.x columns
#  parent <- select(mydata, file, frame_number, parenthands.ordinal, parenthands.onset, parenthands.offset, parenthands.hand, parenthands.look)
#  
#  # 2. use ranges if the column order is known
#  parent <- select(mydata, file, frame_number, parenthands.ordinal:parenthands.look)
#  
#  # 3. by omitting the childhands columns using the - sign
#  parent <- select(mydata, -(childhands.ordinal:childhands.look))

## ---- echo=FALSE---------------------------------------------------------
parent <- select(mydata, -(childhands.ordinal:childhands.look))

## ------------------------------------------------------------------------
# in parent data, rename frame_number to frame, overwrite same data
parent <- rename(parent, frame = frame_number)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(head(filter(parent, !is.na(parenthands.hand))))

## ------------------------------------------------------------------------
# get rid of frame column from parent data
parent <- select(parent, -(frame))

# obtain distinct rows, checking all columns in the parent data
parent <- distinct(parent)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(head(filter(parent, file == "dyad1", parenthands.ordinal >= 2)))

## ------------------------------------------------------------------------
# make new column called parenthands.duration
parent <- mutate(parent, parenthands.duration = parenthands.offset - parenthands.onset)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(head(parent))

## ---- eval=FALSE---------------------------------------------------------
#  # transmute does the same but keeps only the columns that are specified
#  parent <- transmute(parent, file, parenthands.duration = parenthands.offset - parenthands.onset)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(head(transmute(parent, file, parenthands.duration = parenthands.offset - parenthands.onset)))

## ------------------------------------------------------------------------
# each condition separated by a comman
# filter(data, something, AND something else, AND this too)
both_hand_look <- filter(parent, parenthands.hand == "both", parenthands.look == 1)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(both_hand_look)

## ------------------------------------------------------------------------
# hands == left OR right, AND look == 1
lr_hand_look <- filter(parent, parenthands.hand == "left" | parenthands.hand == "right", parenthands.look == 1)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(lr_hand_look)

## ---- eval=FALSE---------------------------------------------------------
#  # select rows 1 through 10 from the parent data set
#  slice(parent, 1:10)
#  
#  # select rows 1,5,10,100
#  slice(parent, c(1, 5, 10, 100))

## ------------------------------------------------------------------------
# arrange data from shortest to longest duration
lr_hand_look <- arrange(lr_hand_look, parenthands.duration)

## ------------------------------------------------------------------------
# arrange data from shortest to longest duration
lr_hand_look <- arrange(lr_hand_look, desc(parenthands.duration))

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(lr_hand_look)

## ------------------------------------------------------------------------
# mean of time spent fixated on left or right hands in seconds across all dyads
seconds_stat <- summarise(lr_hand_look, avg_sec = mean(parenthands.duration) / 1000)

## ---- echo=FALSE---------------------------------------------------------
seconds_stat

## ------------------------------------------------------------------------
# group parent data by file, save as new data set
parent_grouped <- group_by(parent, file)

## ------------------------------------------------------------------------
# same operation, but on grouped data, and removing any NAs
summarise(parent_grouped, avg_sec = mean(parenthands.duration, na.rm = TRUE) / 1000)

## ------------------------------------------------------------------------
# remove NAs from parenthands.hand first
# use is.na to check NA values within parenthands.hand, return the opposite using ! (NOT)
parent <- filter(parent, !is.na(parenthands.hand))

# group parent data by file and hand, save as new data set
parent_grouped2 <- group_by(parent, file, parenthands.hand)

summarise(parent_grouped2, avg_sec = mean(parenthands.duration, na.rm = TRUE) / 1000)

## ------------------------------------------------------------------------
final_result <-                                                             # 0
    mydata %>%                                                              # 1
    select(file, childhands.ordinal:childhands.look) %>%                    # 2
    distinct() %>%                                                          # 3
    mutate(childhands.duration = childhands.offset - childhands.onset) %>%  # 4
    filter(childhands.hand == "both", childhands.look == 1) %>%             # 5
    group_by(file) %>%                                                      # 6
    summarise(avg_dur = mean(childhands.duration, na.rm = TRUE))            # 7

## ---- echo=FALSE---------------------------------------------------------
final_result

## ---- eval=FALSE---------------------------------------------------------
#  # export final_result data to a file called child_data_summary in your current working directory
#  write.csv(final_result, "child_data_summary.csv")

