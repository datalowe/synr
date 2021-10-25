# helper function for testing
get_random_color <- function() {
  r_val <- runif(1, 0, 1)
  g_val <- runif(1, 0, 1)
  b_val <- runif(1, 0, 1)
  alpha_val <- runif(1, 0, 1)
  hex_val <- rgb(r_val, g_val, b_val, alpha_val)
  return(hex_val)
}

test_that("A participant with no graphemes is counted as invalid.", {
  p <- Participant$new()
  res <- p$check_valid_get_twcv()
  expect_false(res$valid)
  expect_equal(res$reason_invalid, "no_color_responses")
  expect_equal(res$twcv, NA)
})


test_that("A participant with just one grapheme is counted as invalid.", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#0000FF", "#0000FF", "#00FF00", "#00FF00"), "Luv")
  g_list <- list(g1)
  p$add_graphemes(g_list)
  res <- p$check_valid_get_twcv()
  expect_false(res$valid)
  expect_equal(res$reason_invalid, "too_few_graphemes_with_complete_responses")
  expect_equal(res$twcv, NA)
})

test_that(
  "A participant with 20 graphemes of same color is counted as invalid."
  , {
  p <- Participant$new()
  for (l in LETTERS[1:20]) {
    g <- Grapheme$new(symbol=l)
    g$set_colors(c("#0000FF", "#0000FF", "#00FF00", "#00FF00"), "Luv")
    p$add_grapheme(g)
  }

  res <- p$check_valid_get_twcv()
  expect_false(res$valid)
  expect_equal(res$reason_invalid, "few_clusters_low_twcv")
  expect_equal(res$twcv, 0)
  }
)


test_that(
  "A participant with 20 graphemes, with 3 responses each,
  of wildly varying (randomly generated) colors is counted as valid."
  , {
    p <- Participant$new()
    for (l in LETTERS[1:20]) {
      g <- Grapheme$new(symbol=l)
      g$set_colors(c(get_random_color(), get_random_color(), get_random_color()), "Luv")
      p$add_grapheme(g)
    }
    res <- p$check_valid_get_twcv()
    expect_true(res$valid)
    expect_equal(res$reason_invalid, "")
    expect_gt(res$twcv, 500)
  }
)
