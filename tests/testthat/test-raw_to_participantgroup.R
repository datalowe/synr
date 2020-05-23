context("Converting raw consistency test data to a participant group with all the data bundled up")
library(synr)

test_that("a single row of data (split into symbol/color/time vectors)
          is correctly converted using create_participant()", {
  participant_id <- "1"
  target_symbols_vec <- c("A", "D", "7")
  symbol_vec <- c("A", "D", "7",
                  "D", "A", "7",
                  "7", "A", "D")
  times_vec <- c(1.1, 0.4, 5,
                 0.3, 2.4, 7.3,
                 1, 10.2, 8.4)
  color_vec <- c("98FF22", "138831", "791322",
                 "8952FE", "DC8481", "7D89B0",
                 "001100", "887755", "FF0033")
  p <- create_participant(participant_id=participant_id,
                          grapheme_symbols=target_symbols_vec,
                          n_trials_per_grapheme=3,
                          trial_symbols=symbol_vec,
                          response_times=times_vec,
                          response_colors=color_vec)
  expect_equal(p$get_symbols()[1], "A")
  expect_gt(p$get_mean_consistency_score(), 100)
})
