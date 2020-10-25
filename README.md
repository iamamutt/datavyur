
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
            -   standard fields
            -   custom codes

For example, you may have several files with multiple “columns” in each
file, and in each column have the standard `onset`, `offset`, and
`ordinal` fields, along with custom codes that hold values.

    |datavyu_file1.opf
    |- column_name1
    |  |- cell
    |     |- onset <value>
    |     |- offset <value>
    |     |- ordinal <value>
    |     |- custom_code_name1 <value>
    |     |- custom_code_name2 <value>
    |  |- cell
    |     |- onset <value>
    |     |- offset <value>
    |     |- ordinal <value>
    |     |- custom_code_name1 <value>
    |     |- custom_code_name2 <value>
    |  |- ...
    |- column_name2
    |  |- cell
    |     |- onset <value>
    |     |- offset <value>
    |     |- ordinal <value>
    |     |- custom_code_name1 <value>
    |     |- custom_code_name2 <value>
    |  |- ...
    |datavyu_file2.opf
    |- column_name1
    |  |- ...
    |- column_name2
    |  |- ...

## Example

This is a basic example which shows you how to import Datavyu data into
R.

``` r
# Load the datavyur library to use the functions below
library(datavyur)
```

## Exporting Datavyu `.opf` files

## Importing R data into Datavyu
