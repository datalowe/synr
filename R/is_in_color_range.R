#' @title Check if specified color falls within specified color range
#'
#' @description Checks if specified color coordinates represent a color
#' within a specified color range. Color range is specified
#' using _either_ color_label _or_ r/g/b arguments. Possible
#' color_label specifications are: "blue", "red", "green",
#' "white", "black" or "hazy". r/g/b each take a two-value
#' numeric vector providing specifications of minimum/maximum
#' red/green/blue values within the range 0-1 (e.g. c(0.2, 0.8) ).
#'
#' Supported color spaces are: "XYZ", "sRGB", "Apple RGB", "Lab",
#' and "Luv". Note that RGB values must be specified using the
#' range 0-1, not 0-255.
#'
#' @param color_to_check A three-element numeric vector of color coordinates
#' @param color_label color specification (possible choices are: "blue", "red",
#' "green", "white", "black", "hazy")
#' @param r a two-value vector
#' @param g a two-value vector
#' @param b a two-value vector
#' @param color_space A one-element character vector describing the color space
#' that the passed color coordinates are in.
#'
#' @seealso \code{\link[grDevices]{convertColor}}
#' @keywords internal

is_in_color_range <- function(color_to_check, color_label = NULL,
                              r = NULL, g = NULL, b = NULL,
                              color_space) {
  if (is.null(color_label) & (is.null(r) | is.null(g) | is.null(b))) {
    cat("You must specify either color_label, or all of r, g and b")
    return(NA)
  }
  if (any(is.na(color_to_check))) {
    return(NA)
  }
  if (!is.null(color_label)) {
    if (color_label == "blue") {
      comp_vals <- c(0, 0.314, 0, 0.314, 0.745, 1)
    }
    else if (color_label == "red") {
      comp_vals <- c(0.745, 1, 0, 0.314, 0, 0.314)
    }
    else if (color_label == "green") {
      comp_vals <- c(0, 0.314, 0.745, 1, 0, 0.314)
    }
    else if (color_label == "black") {
      comp_vals <- c(0, 0.129, 0, 0.129, 0, 0.129)
    }
    else if (color_label == "white") {
      comp_vals <- c(0.871, 1, 0.871, 1, 0.871, 1)
    }
    else if (color_label == "hazy") {
      comp_vals <- c(0.392, 0.588, 0.392, 0.588, 0.392, 0.588)
    }
  } else {
    comp_vals <- c(r, g, b)
  }
  pri_rgb <- grDevices::convertColor(
    color_to_check,
    from = color_space,
    to = "sRGB"
  )
  if (pri_rgb[1] >= comp_vals[1] && pri_rgb[1] <= comp_vals[2] &&
      pri_rgb[2] >= comp_vals[3] && pri_rgb[2] <= comp_vals[4] &&
      pri_rgb[3] >= comp_vals[5] && pri_rgb[3] <= comp_vals[6]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
