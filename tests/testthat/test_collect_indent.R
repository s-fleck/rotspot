context("analyse_indent")


test_that("collect_indent works as expected", {
  tf <- rprojroot::find_testthat_root_file("testdata", "indent_testdata.R")
  td <- readLines(tf)

  expect_identical(
    count_indent(td),
    c(0L, 2L, 4L, 6L, 4L, 6L, 4L, 2L, 0L, 0L, 2L, 4L, 2L, 0L)
  )

  expect_identical(collect_indent(tf)[[1]], count_indent(td))
  expect_identical(names(collect_indent(tf)), tf)

})
