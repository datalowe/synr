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


test_that("Proportion of color responses that are
          blue is 1 when all registered responses are blue", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000DE", "#0000FF", "#0000FF", "#3333DD"), "Luv")
            expect_equal(a$get_prop_color(color_label="blue"), 1)
          })


test_that("Proportion of color responses that are
          blue is 0 when no registered responses are blue", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#000000", "#AA0000", "#00BB00", "#000077", "#AADD11"), "Luv")
            expect_equal(a$get_prop_color(color_label="blue"), 0)
          })


test_that("Proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            expect_equal(a$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                         0.5)
          })


test_that("Participant.get_prop_color(): proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            b <- synr::Grapheme$new(symbol='b')
            b$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            p <- Participant$new(id='1')
            p$add_grapheme(a)
            p$add_grapheme(b)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                         0.5)
          })


test_that("Participant.get_prop_color(): proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications, even if there's a non-valid response", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000", NA), "Luv")

            p <- Participant$new(id='1')
            p$add_grapheme(a)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                         0.5)
          })


test_that("Participant.get_prop_color(): proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications, even if there's a grapheme with all non-valid
          responses", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000", NA), "Luv")
            b <- synr::Grapheme$new(symbol='b')
            b$set_colors(c(NA, NA, NA, NA, NA), "Luv")

            p <- Participant$new(id='1')
            p$add_graphemes(list(a, b))
            expect_equal(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                         0.5)
          })


test_that("Participant.get_prop_color(): returns NA if there are no valid response colors", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c(NA, NA, NA, NA, NA), "Luv")
            b <- synr::Grapheme$new(symbol='b')
            b$set_colors(c(NA, NA, NA, NA, NA), "Luv")

            p <- Participant$new(id='1')
            p$add_graphemes(list(a, b))
            expect_true(is.na(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1))))
          })


test_that("Participant.get_prop_color(): proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications, with filter on", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            b <- synr::Grapheme$new(symbol='b')
            b$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            p <- Participant$new(id='1')
            p$add_grapheme(a)
            p$add_grapheme(b)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3),
                                          g=c(0.0, 0.3),
                                          b=c(0.7, 1),
                                          symbol_filter=c("a")),
                         0.5)
          })

test_that("Participant.get_prop_color(): proportion of color being
          blue is correctly changed by symbol filter", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            b <- synr::Grapheme$new(symbol='b')
            b$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            p <- Participant$new(id='1')
            p$add_grapheme(a)
            p$add_grapheme(b)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3),
                                          g=c(0.0, 0.3),
                                          b=c(0.7, 1),
                                          symbol_filter=c("a")),
                         0.5)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3),
                                          g=c(0.0, 0.3),
                                          b=c(0.7, 1),
                                          symbol_filter=c("a", "b")),
                         0.25)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3),
                                          g=c(0.0, 0.3),
                                          b=c(0.7, 1),
                                          symbol_filter=c("b")),
                         0)
          })

test_that("ParticipantGroup.get_prop_color(): proportion of color being
          blue/green is correctly changed by symbol filter", {
            a1 <- synr::Grapheme$new(symbol='a')
            a1$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            b1 <- synr::Grapheme$new(symbol='b')
            b1$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            p1 <- Participant$new(id='1')
            p1$add_grapheme(a1)
            p1$add_grapheme(b1)

            a2 <- synr::Grapheme$new(symbol='a')
            a2$set_colors(c("#FF0000", "#FF0000", "#000000", "#000000"), "Luv")
            b2 <- synr::Grapheme$new(symbol='b')
            b2$set_colors(c("#000000", "#000000", "#00FF00", "#000000"), "Luv")
            p2 <- Participant$new(id='2')
            p2$add_grapheme(a2)
            p2$add_grapheme(b2)


            pg <- ParticipantGroup$new()
            pg$add_participant(p1)
            pg$add_participant(p2)

            prop_col_vals_a <- pg$get_prop_color_values(r=c(0.0, 0.3),
                                                     g=c(0.0, 0.3),
                                                     b=c(0.7, 1),
                                                     symbol_filter=c("a"))

            prop_col_vals_b <- pg$get_prop_color_values(r=c(0.0, 0.3),
                                                        g=c(0.7, 1),
                                                        b=c(0.0, 0.3),
                                                        symbol_filter=c("b"))

            expect_equal(prop_col_vals_a[1],
                         0.5)
            expect_lt(prop_col_vals_a[2],
                         0.01)

            expect_lt(prop_col_vals_b[1],
                         0.01)
            expect_equal(prop_col_vals_b[2],
                      0.25)

          })
