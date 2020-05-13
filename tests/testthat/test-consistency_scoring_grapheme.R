context("Consistency scoring")
library(synr)

test_that("Grapheme All black color responses leads to a consistency score near 0",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            expect_lt(a$get_consistency_score(),
                         1)
          })

test_that("Grapheme All perfect blue color responses leads to a consistency score near 0",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000FF", "#0000FF", "#0000FF", "#0000FF"), "Luv")
            expect_lt(a$get_consistency_score(),
                         1)
          })

test_that("Grapheme 3 very different color responses leads to a consistency score above 135",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000FF", "#00FF00", "#FF0000"), "Luv")
            expect_gt(a$get_consistency_score(),
                         135)
          })

test_that("Grapheme All NA color responses leads to a NA consistency score",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c(NA, NA, NA), "Luv")
            is_consistency_na <- is.na(a$get_consistency_score())
            expect_true(is_consistency_na)
          })

test_that("Grapheme Fetching consistency score before inserting responses
          produces error",
          {
            a <- synr::Grapheme$new(symbol='a')
            expect_error(a$get_consistency_score(),
                         paste0("This grapheme, with symbol ", a$symbol,
                                ' has no registered response colors. Please register responses before calling .get_consistency_score().')
                         )
          })

test_that("Participant get_consistency_scores() returns correct list for valid response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  cons_list <- p$get_consistency_scores()
  expect_lt(cons_list[['b']], 0.01)
  expect_lt(cons_list[[2]], 0.01)
  expect_gt(cons_list[['a']], 100)
})

test_that("Participant get_consistency_scores() returns correct list for nonvalid response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c(NA, NA, NA, NA), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c(NA, NA, NA, NA), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c(NA, NA, NA, NA), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  cons_list <- p$get_consistency_scores()
  expect_true(is.na(cons_list[['b']]))
  expect_true(is.na(cons_list[[2]]))
  expect_true(is.na(cons_list[['a']]))
})

test_that("Participant get_mean_consistency_score() returns value close to 0 for all black response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_lt(p$get_mean_consistency_score(), 0.01)
})

test_that("Participant get_mean_consistency_score() returns higher than 200 for very mixed response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#FFFFFF", "#000000", "#FFFFFF"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#0000FF", "#000000", "#FF0000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_gt(p$get_mean_consistency_score(), 200)
})

test_that("Participant get_mean_consistency_score() returns NA value for no valid responses graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c(NA, NA, NA, NA), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c(NA, NA, NA, NA), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c(NA, NA, NA, NA), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_true(is.na(p$get_mean_consistency_score()))
})
