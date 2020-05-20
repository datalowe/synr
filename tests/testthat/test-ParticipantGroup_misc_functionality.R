context("Grapheme misc. functionality")
library(synr)

test_that("has_participants() returns TRUE if there is one participant with an id, added with add_participant", {
  p <- Participant$new(id="1")
  pl <- ParticipantGroup$new()
  pl$add_participant(p)
  expect_true(pl$has_participants())
})


test_that("add_participant() throws error if trying to add participant with no id", {
  p <- Participant$new()
  pl <- ParticipantGroup$new()
  expect_error(pl$add_participant(p))
})


test_that("has_participants() returns FALSE if there are no participants in the group", {
  pl <- ParticipantGroup$new()
  expect_false(pl$has_participants())
})


test_that("has_participants() returns TRUE if there is one participant with an id, added as a list
          with add_participants", {
  p <- Participant$new(id="1")
  pl <- ParticipantGroup$new()
  pl$add_participants(list(p))
  expect_true(pl$has_participants())
})


test_that("get_ids() returns correct ids for two participants", {
            p1 <- Participant$new(id="1")
            p2 <- Participant$new(id="2")
            pl <- ParticipantGroup$new()
            pl$add_participants(list(p1, p2))
            out_ids <- pl$get_ids()
            expect_equal(out_ids[1], '1')
            expect_equal(out_ids[2], '2')
          })


test_that("get_mean_response_times() returns correct mean response time for
          1 participant with 2 graphemes", {
  g1 <- Grapheme$new(symbol="a")
  g2 <- Grapheme$new(symbol="b")
  g1$set_times(c(3, 8, 5))
  g2$set_times(c(20, 0.3, 4))

  p1 <- Participant$new(id="1")
  p1$add_graphemes(list(g1, g2))

  pl <- ParticipantGroup$new()
  pl$add_participants(list(p1))

  expect_lt(abs(pl$get_mean_response_times()[1] - 6.716667), 0.0001)
})


test_that("get_mean_response_times() returns correct mean response times for
          2 participants", {
  g1 <- Grapheme$new(symbol="a")
  g2 <- Grapheme$new(symbol="b")
  g1$set_times(c(3, 8, 5))
  g2$set_times(c(20, 0.3, 4))

  g3 <- Grapheme$new(symbol="a")
  g4 <- Grapheme$new(symbol="b")
  g3$set_times(c(10, 0.3, 0.02))
  g4$set_times(c(9, 0.3, 7))

  p1 <- Participant$new(id="1")
  p1$add_graphemes(list(g1, g2))

  p2 <- Participant$new(id="2")
  p2$add_graphemes(list(g3, g4))

  pl <- ParticipantGroup$new()
  pl$add_participants(list(p1, p2))

  expect_lt(abs(pl$get_mean_response_times()[1] - 6.716667), 0.0001)
  expect_lt(abs(pl$get_mean_response_times()[2] - 4.436667), 0.0001)
})


test_that("get_numbers_all_colored_graphemes() returns correct number of graphemes
          with all colored graphemes for 1 participant", {
  g1 <- Grapheme$new(symbol="a")
  g2 <- Grapheme$new(symbol="b")
  g1$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
  g2$set_colors(c(NA, "#F08000", "#993322"), "Luv")

  p1 <- Participant$new(id="1")
  p1$add_graphemes(list(g1, g2))

  pl <- ParticipantGroup$new()
  pl$add_participants(list(p1))

  expect_equal(pl$get_numbers_all_colored_graphemes()[1], 1)
})


test_that("get_numbers_all_colored_graphemes() returns correct number of graphemes
          with all colored graphemes for 2 participants", {
  g1 <- Grapheme$new(symbol="a")
  g2 <- Grapheme$new(symbol="b")
  g1$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
  g2$set_colors(c(NA, "#F08000", "#993322"), "Luv")
  g3 <- Grapheme$new(symbol="a")
  g4 <- Grapheme$new(symbol="b")
  g3$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
  g4$set_colors(c(NA, NA, NA), "Luv")

  p1 <- Participant$new(id="1")
  p1$add_graphemes(list(g1, g2))
  p2 <- Participant$new(id="2")
  p2$add_graphemes(list(g3, g4))

  pl <- ParticipantGroup$new()
  pl$add_participants(list(p1, p2))

  expect_equal(pl$get_numbers_all_colored_graphemes()[1], 1)
  expect_equal(pl$get_numbers_all_colored_graphemes()[2], 1)
})



