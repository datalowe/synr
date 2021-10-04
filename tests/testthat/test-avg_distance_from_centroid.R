test_that("avg_distance_from_centroid correctly calculates average distance for simple input", {
  point_matrix <- matrix(
    c(1, 0, 0, -1, 0, 0),
    ncol=3,
    byrow=TRUE
  )
  expect_equal(
    avg_distance_from_centroid(point_matrix),
    1
  )
})


test_that("avg_distance_from_centroid correctly calculates average distance for input based on multiple colors", {
  test_hexes <- c(
    "#FAFAFE",
    "#000000",
    "#ABDC07",
    "#0088BB",
    "#FFFFFF"
  )
  test_color_matrix <- t(sapply(
    test_hexes, function(x) col2rgb(x)
  ))
  expect_lt(
    abs(avg_distance_from_centroid(test_color_matrix)-35449),
    0.4
  )
})
