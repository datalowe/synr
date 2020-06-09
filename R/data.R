#' Raw consistency test data example (small)
#'
#' A data frame with an example of raw consistency test data that are
#' compatible with the synr package, with data for three participants
#' from a test that only used three graphemes ("A", "D", 7) and two responses
#' per grapheme, to show the expected data frame format. More graphemes and/or
#' responses per grapheme can be handled by the package (though participant
#' plots do not function correctly if there are more than three responses
#' per grapheme)
#'
#' @format A data frame with 3 rows and 8 columns:
#' \describe{
#'   \item{participant_id}{Participant ID}
#'   \item{symbol_1}{Column with symbol/grapheme connected to first response}
#'   \item{response_color_1}{Column with color of first response}
#'   \item{response_time_1}{(optional) Column with time from
#'   presentation to response, for first response}
#'   \item{symbol_2}{Column with symbol/grapheme connected to second response}
#'   \item{response_color_2}{Column with color of second response}
#'   \item{response_time_2}{(optional) Column with time from
#'   presentation to response, for second response}
#'   \item{symbol_3}{Column with symbol/grapheme connected to third response}
#'   \item{response_color_3}{Column with color of third response}
#'   \item{response_time_3}{(optional) Column with time from
#'   presentation to response, for third response}
#'   \item{symbol_4}{Column with symbol/grapheme connected to fourth response}
#'   \item{response_color_4}{Column with color of fourth response}
#'   \item{response_time_4}{(optional) Column with time from
#'   presentation to response, for fourth response}
#'   \item{symbol_5}{Column with symbol/grapheme connected to fifth response}
#'   \item{response_color_5}{Column with color of fifth response}
#'   \item{response_time_5}{(optional) Column with time from
#'   presentation to response, for fifth response}
#'   \item{symbol_6}{Column with symbol/grapheme connected to sixth response}
#'   \item{response_color_6}{Column with color of sixth response}
#'   \item{response_time_6}{(optional) Column with time from
#'   presentation to response, for sixth response}
#' }
"example_raw_df"



#' Raw consistency test data example (full)
#'
#' A data frame with an example of raw consistency test data that are
#' compatible with the synr package, with data for ten participants
#' from a test that used English letters and weekdays, and digits. Has three responses
#' per grapheme. More graphemes and/or
#' responses per grapheme can be handled by the package (though participant
#' plots do not function correctly if there are more than three responses
#' per grapheme, and too many graphemes might make the plots look odd)
#'
#' @format A data frame with 10 rows and 389 columns. The
#' first two columns are participant id and time/date of testing.
#' The remaining columns describe trial data, with each trial
#' represented by a set of 3 columns. The 3 columsn represent
#' the symbol (grapheme) used, response time, and the color code that
#' the participant responded with.
#'
"example_full_raw_df"
