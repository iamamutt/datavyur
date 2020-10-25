
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datavyur

<!-- badges: start -->
<!-- badges: end -->

The goal of datavyur is to …

## Installation

You can install the released version of datavyur from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("datavyur")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iamamutt/datavyur")
```

## Datavyu data structure

It is important to understand the parts of one or more Datavyu files to
understand how to use this R package. The structure is basically
organized like so:

-   datavyu files
    -   column names
        -   cells
            -   standard fields <value>
            -   custom fields <code>

For example, you may have several Datavyu files with multiple “columns”
in each file, and in each column exists the standard `onset`, `offset`,
and `ordinal` fields and their values, along with custom fields that
hold their “codes.”

    |datavyu_file1.opf
    |- column_name1
    |  |- cell
    |     |- onset <ts>
    |     |- offset <ts>
    |     |- ordinal <int>
    |     |- custom_code_id1 <code>
    |     |- custom_code_id2 <code>
    |  |- cell
    |     |- onset <ts>
    |     |- offset <ts>
    |     |- ordinal <int>
    |     |- custom_code_id1 <code>
    |     |- custom_code_id2 <code>
    |  |- ...
    |- column_name2
    |  |- cell
    |     |- onset <ts>
    |     |- offset <ts>
    |     |- ordinal <int>
    |     |- custom_code_id1 <code>
    |     |- custom_code_id2 <code>
    |  |- ...
    |datavyu_file2.opf
    |- column_name1
    |  |- ...
    |- column_name2
    |  |- ...
    |...

## Example

This is a basic example which shows you how to import Datavyu data into
R. First, load the package.

``` r
# Load the datavyur library to use the functions below
library(datavyur)
```

Next, point to a directory containing `.csv` data that has been
previously exported using the script `datavyu2csv.rb`. The example below
uses the internal package data directory for demonstration purposes.

``` r
ex_data_dir <- datavyur_internal_data()
```

The folder `ex_data_dir` originally contained 3 `.opf` files that have
been split into 6 separate `.csv` files, one `.csv` file for each
Datavyu file (`.opf`) and column combination.

Names of `.csv` files after using the Ruby script `datavyu2csv.rb`:

    #> [1] "childhands__dyad1.csv"  "childhands__dyad2.csv"  "childhands__dyad3.csv" 
    #> [4] "parenthands__dyad1.csv" "parenthands__dyad2.csv" "parenthands__dyad3.csv"

Names of the original Datavyu `.opf` files:

    #> [1] "dyad1" "dyad2" "dyad3"

Names of the Datavyu columns found inside the set of exported `.csv`
files:

    #> [1] "childhands"  "parenthands"

This information can be obtained by using the function `datavyu_search`.

``` r
datavyu_search(folder=ex_data_dir)
```

Use the `import_datavyu` function to import Datavyu columns into R as
separate `data.frames`. Since there are multiple Datavyu columns that
need to be imported from `ex_data_dir`, set `as_list=TRUE` to import a
`data.frame` for each Datavyu column.

``` r
dv_columns <- import_datavyu(folder=ex_data_dir, as_list=TRUE)
```

You can access each `data.frame` like so:

``` r
child_data <- dv_columns$childhands
```

The first few rows are shown below.

| file  | column     | ordinal |  onset | offset | hand  | look |
|:------|:-----------|--------:|-------:|-------:|:------|-----:|
| dyad1 | childhands |       1 |  28846 | 157944 | left  |  NaN |
| dyad1 | childhands |       2 | 177790 | 184132 |       |    1 |
| dyad1 | childhands |       3 | 199152 | 201028 |       |    1 |
| dyad1 | childhands |       4 | 204120 | 226493 | left  |  NaN |
| dyad1 | childhands |       5 | 236796 | 251476 | right |  NaN |
| dyad1 | childhands |       6 | 274095 | 315907 | both  |    0 |

## Horizontal alignment of Datavyu data

``` r
main_list <- import_datavyu_to_list()
```

### Aligning data by timestamp

To align data found in the folder `ex_data_dir` use the function
`temporal_align`.

``` r
time_aligned1 <- temporal_align(folder=ex_data_dir, fps=30, columns="childhands")
```

| file  | frame\_number | childhands.ordinal | childhands.onset | childhands.offset | childhands.hand | childhands.look |
|:------|--------------:|-------------------:|-----------------:|------------------:|:----------------|----------------:|
| dyad1 |           866 |                  1 |            28846 |            157944 | left            |             NaN |
| dyad1 |           867 |                  1 |            28846 |            157944 | left            |             NaN |
| dyad1 |           868 |                  1 |            28846 |            157944 | left            |             NaN |
| dyad1 |           869 |                  1 |            28846 |            157944 | left            |             NaN |
| dyad1 |           870 |                  1 |            28846 |            157944 | left            |             NaN |
| dyad1 |           871 |                  1 |            28846 |            157944 | left            |             NaN |

## Vertical alignment of Datavyu data

To stack several Datavyu columns’ data vertically, use the function
`vert_merge_datavyu_list`. This will convert all codes to characters so
that they’re able to exist in a single column.

``` r
vert_data <- vert_merge_datavyu_list(main_list)
```

| file  | column      | field | code | ordinal |   onset |  offset |
|:------|:------------|:------|:-----|--------:|--------:|--------:|
| dyad1 | childhands  | hand  | left |       1 |   28846 |  157944 |
| dyad1 | childhands  | look  |      |       1 |   28846 |  157944 |
| dyad1 | childhands  | hand  |      |       2 |  177790 |  184132 |
| dyad1 | childhands  | look  | 1    |       2 |  177790 |  184132 |
| dyad1 | childhands  | hand  |      |       3 |  199152 |  201028 |
| dyad1 | childhands  | look  | 1    |       3 |  199152 |  201028 |
| dyad3 | parenthands | hand  |      |       3 | 1411097 | 1541540 |
| dyad3 | parenthands | look  | 0    |       3 | 1411097 | 1541540 |
| dyad3 | parenthands | hand  |      |       4 | 1912060 | 2240150 |
| dyad3 | parenthands | look  |      |       4 | 1912060 | 2240150 |
| dyad3 | parenthands | hand  |      |       5 | 2885547 | 3046254 |
| dyad3 | parenthands | look  | 0    |       5 | 2885547 | 3046254 |

## Exporting Datavyu `.opf` files

## Importing R data into Datavyu
