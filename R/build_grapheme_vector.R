#' @title Build a vector of graphemes
#' @description Uses internal data constants to build a character vector
#' containing graphemes in accordance with specifications passed as
#' function arguments. Supports English and Swedish language specifications. Note that
#' all "use_" parameters default to FALSE, meaning you only need to pass the value TRUE
#' for grapheme kinds that you want to use.
#' @param language A character element. What language should be used for the vector?
#' @param use_digits A logical scalar. Should digits be included in the vector?
#' @param use_letters A logical scalar. Should letters be included in the vector?
#' @param use_weekdays A logical scalar. Should weekdays be included in the vector?
#' @param uppercase A logical scalar. Should graphemes be in uppercase? (otherwise uses all lowercase)
#' @examples
#' build_grapheme_vector(language="english", use_digits=TRUE, use_letters=FALSE,
#' use_weekdays=TRUE, uppercase=TRUE)
#' @export
build_grapheme_vector <- function(language,
                                  use_digits=FALSE,
                                  use_letters=FALSE,
                                  use_weekdays=FALSE,
                                  uppercase=TRUE) {
  language = tolower(language)
  if (!(language %in% c("english", "swedish"))) {
    stop(sprintf("Invalid language specificiation to build_grapheme_vector,
         I was passed this value: %s", language))
  }
  if (!use_digits && !use_letters && !use_weekdays) {
    warning("For build_grapheme_vector, no kind of grapheme was specified.
                   You probably want to specify at least one.")
  }
  grapheme_vector <- character()
  if (use_digits) {
    grapheme_vector <- c(grapheme_vector, DIGITS)
  }
  if (language == "english") {
    if (uppercase) {
      if (use_letters) {
        grapheme_vector <- c(grapheme_vector, ENGLISH_LETTERS_UPPERCASE)
      }
      if (use_weekdays) {
        grapheme_vector <- c(grapheme_vector, ENGLISH_WEEKDAYS_UPPERCASE)
      }
    }
    else {
      if (use_letters) {
        grapheme_vector <- c(grapheme_vector, ENGLISH_LETTERS_LOWERCASE)
      }
      if (use_weekdays) {
        grapheme_vector <- c(grapheme_vector, ENGLISH_WEEKDAYS_LOWERCASE)
      }
    }
  }
  else if (language == "swedish") {
    if (uppercase) {
      if (use_letters) {
        grapheme_vector <- c(grapheme_vector, SWEDISH_LETTERS_UPPERCASE)
      }
      if (use_weekdays) {
        grapheme_vector <- c(grapheme_vector, SWEDISH_WEEKDAYS_UPPERCASE)
      }
    }
    else {
      if (use_letters) {
        grapheme_vector <- c(grapheme_vector, SWEDISH_LETTERS_LOWERCASE)
      }
      if (use_weekdays) {
        grapheme_vector <- c(grapheme_vector, SWEDISH_WEEKDAYS_LOWERCASE)
      }
    }
  }
  return(grapheme_vector)
}
