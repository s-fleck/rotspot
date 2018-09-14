context("calc_loc")


test_that("calc_loc works as expected", {
  tfile <- rprojroot::find_testthat_root_file("testdata/loc_testdata.R")

  expect_identical(
    calc_loc(tfile),
    2L
  )

})
