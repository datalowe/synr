context("Plotting related functions")
library(synr)

test_that("Grapheme.get_plot_data_list() returns correctly formatted data when all response colors are valid", {
  g <- synr::Grapheme$new(symbol='Monday')
  g$set_colors(c("#00DE00", "#00FF00", "#390000"), "Luv")
  data_ls <- g$get_plot_data_list()
  expect_equal(data_ls[[1]], 'Mon')
  expect_gt(data_ls[[2]], 200)
  expect_equal(data_ls[[3]], "#00DE00")
  expect_equal(data_ls[[4]], "#00FF00")
  expect_equal(data_ls[[5]], "#390000")
})

test_that("Grapheme.get_plot_data_list() returns correctly formatted data when one response color is invalid", {
  g <- synr::Grapheme$new(symbol='Monday')
  g$set_colors(c("#00DE00", NA, "#390000"), "Luv")
  data_ls <- g$get_plot_data_list()
  expect_equal(data_ls[[1]], 'Mon')
  expect_true(is.na(data_ls[[2]]) )
  expect_equal(data_ls[[3]], "#00DE00")
  expect_true(is.na(data_ls[[4]]) )
  expect_equal(data_ls[[5]], "#390000")
})
