#' @title Calculate average distance from centroid
#'
#' @description Calculates average distance from centroid
#' of passed 3D points.
#'
#' @param point_matrix An n-by-3 numeric matrix where each
#' row corresponds to a single point in 3D space.
#'
#' @seealso \code{\link[grDevices]{convertColor}}

avg_distance_from_centroid <- function(
  point_matrix
) {
  # if there is only one row in the matrix of points,
  # return 0 (since the single point
  # is then itself the centroid, and at distance 0 from itself)
  if (nrow(point_matrix) == 1) {
    return(0)
  }
  # calculate mean coordinates to form a 1-by-3 matrix
  # (a row vector with 3 entries), corresponding to the
  # centroid ('mean point')
  mean_point <- apply(point_matrix, 2, mean)
  # (using vector operation `apply` function)
  # for each row of actual coordinate points:
  # 1. subtract the `mean_point` row vector
  # 2. square each of the resulting vector elements
  # 3. sum the resulting vector elements
  point_distances <- apply(
    point_matrix,
    1,
    function(row_point) {
      sum((row_point - mean_point) ** 2)
    }
  )
  avg_distance <- sum(point_distances) / nrow(point_matrix)
  return(avg_distance)
}
