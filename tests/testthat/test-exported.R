context("test-exported")

options(datavyur.folder=datavyur_internal_data())

file_names <- c("childhands__dyad1.csv", "parenthands__dyad1.csv")

info_cols <- c(
  "column",
  "codes",
  "file",
  "local",
  "classes",
  "combined",
  "old_classes"
)

data_cols <- c(
  "file",
  "column",
  "ordinal",
  "onset",
  "offset",
  "hand",
  "look"
)

vert_cols <- c(
  "file",
  "column",
  "field",
  "code",
  "ordinal",
  "onset",
  "offset"
)

data_frame_tests <- function(.data, class="data.frame", nrows=NULL, colnames=NULL) {
  expect_is(.data, class)
  if (!is.null(nrows)) expect_equal(nrow(.data), nrows)
  if (!is.null(colnames)) expect_identical(names(.data), colnames)
}

test_that("column search works", {
  data_frame_tests(
    datavyu_search(),
    nrows=29L,
    colnames=info_cols
  )

  data_frame_tests(
    datavyu_search(files="dyad2"),
    nrows=10L,
    colnames=info_cols
  )
})

test_that("main datavyu import function works", {
  data_frame_tests(
    import_datavyu(columns="childhands"),
    nrows=86L,
    colnames=data_cols
  )
})

test_that("import datavyu with appended column names works", {
  data_frame_tests(
    import_datavyu(columns="parenthands", append_colnames=TRUE),
    nrows=155L,
    colnames=c(
      "file", "parenthands.ordinal", "parenthands.onset",
      "parenthands.offset", "parenthands.hand", "parenthands.look"
    )
  )
})

test_that("import datavyu with file specified works", {
  .data <- import_datavyu(files="dyad1")
  expect_is(.data, "list")
  data_frame_tests(
    .data$childhands,
    nrows=50L,
    colnames=data_cols
  )
})

test_that("import datavyu with multiple files specified works", {
  .data <- import_datavyu(files=c("dyad1", "dyad2"))
  expect_is(.data, "list")
  expect_equal(length(.data), 2L)
  data_frame_tests(
    .data$childhands,
    nrows=83L,
    colnames=data_cols
  )
})

test_that("import datavyu with multiple folder csv files works", {
  .data <- import_datavyu(
    folder=file.path(getOption("datavyur.folder"), file_names),
    as_list=TRUE
  )
  expect_is(.data, "list")
  expect_equal(length(.data), 2L)
  data_frame_tests(
    .data$parenthands,
    nrows=50L,
    colnames=data_cols
  )
})

test_that("import datavyu as list works", {
  .data <- import_datavyu(columns=c("childhands", "parenthands"), as_list=TRUE)
  expect_is(.data, "list")
  expect_equal(length(.data), 2L)
  data_frame_tests(
    .data$childhands,
    nrows=86L,
    colnames=data_cols
  )
})

.list <- import_datavyu_to_list()

test_that("vert concat works from list", {
  data_frame_tests(
    vert_merge_datavyu_list(.list),
    nrows=482L,
    colnames=vert_cols
  )
})

test_that("horz concat is ordinal aligned and identical to ordinal import", {
  .data <- horz_merge_datavyu_list(.list)
  data_frame_tests(
    .data,
    nrows=155L,
    colnames=c(
      "file", "ordinal", "childhands.onset",
      "childhands.offset", "childhands.hand", "childhands.look",
      "parenthands.onset",
      "parenthands.offset", "parenthands.hand", "parenthands.look"
    )
  )

  expect_identical(.data, ordinal_align())
})

# test_that("temporal alignment works", {
#   data_frame_tests(
#     temporal_align(fps=10),
#     nrows=68839L
#   )
# 
#   data_frame_tests(
#     temporal_align(columns="childhands", fps=1),
#     nrows=4312L,
#     colnames=c(
#       "file", "frame_number",
#       "childhands.ordinal", "childhands.onset", "childhands.offset",
#       "childhands.hand", "childhands.look"
#     )
#   )
# })

test_that("nested merge works", {
  data_frame_tests(
    merge_nested("parenthands", "childhands"),
    nrows=19L
  )
})
