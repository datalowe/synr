context("Functions related to checking if color is in color range")
library(synr)


test_that("is_in_color_range using black color_label", {
  test_color_black <- c(0.03, 0.03, 0.05)
  expect_equal(is_in_color_range(test_color_black,
                                 color_label="black", color_space="sRGB"),
               TRUE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 color_label="black", color_space="sRGB"),
               FALSE)
})

test_that("is_in_color_range using r/g/b arguments for black", {
  test_color_black <- c(0.03, 0.03, 0.05)
  expect_equal(is_in_color_range(test_color_black,
                                 r=c(0.0, 0.3),
                                 g=c(0.0, 0.3),
                                 b=c(0.0, 0.3),
                                 color_space="sRGB"),
               TRUE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 r=c(0.0, 0.3),
                                 g=c(0.0, 0.3),
                                 b=c(0.0, 0.3),
                                 color_space="sRGB"),
               FALSE)
})

test_that("is_in_color_range using white color_label", {
  test_color_black <- c(0.03, 0.03, 0.05)
  expect_equal(is_in_color_range(test_color_black,
                                 color_label="white", color_space="sRGB"),
               FALSE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 color_label="white", color_space="sRGB"),
               TRUE)
})


test_that("is_in_color_range using blue color_label", {
  test_color_blue <- c(0.03, 0.03, 0.98)
  expect_equal(is_in_color_range(test_color_blue,
                                 color_label="blue", color_space="sRGB"),
               TRUE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 color_label="blue", color_space="sRGB"),
               FALSE)
})
