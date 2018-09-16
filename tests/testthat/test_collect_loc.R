context("lines of code analysis")


test_that("is_loc, keep_loc, count_loc, collect_loc work", {
  tfile <- rprojroot::find_testthat_root_file("testdata/loc_testdata.R")
  tdat  <- readLines(tfile)

  expect_identical(
    is_loc(tdat),
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_identical(keep_loc(tdat), tdat[c(2, 5)])
  expect_identical(count_loc(tdat), 2L)

  tres <- collect_loc(tfile)
  expect_identical(unname(tres), count_loc(tdat))
  expect_identical(names(tres), tfile)
})
