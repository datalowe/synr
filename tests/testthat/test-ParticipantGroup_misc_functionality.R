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


# test_that("get_mean_response_times() returns correct mean response time for
#           one participant") {
#   g1 <- Grapheme$new(symbol="a")
#   expect_equal(g1$symbol, "a")
#   g2 <- Grapheme$new()
#   g2$set_symbol('b')
#
#   p1 <- Participant$new(id="1")
#   p2 <- Participant$new(id="2")
#
#   pl <- ParticipantGroup$new()
#   pl$add_participants(list(p1, p2))
#
#   expect_equal(out_ids[1], '1')
#   expect_equal(out_ids[2], '2')
# }
