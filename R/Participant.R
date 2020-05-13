#' A Reference Class to represent consistency test participants.
#'
#'
#' @field id A one-element character vector containing the participant's ID.
#' Set at class new() call.
#' @field test_date A one-element Date vector which specifies the date
#' on which the participant did the consistency test.
#' @field graphemes A list of \code{\link[synr]{Grapheme}} class instances.
#' @importFrom methods new
#' @export Participant
#' @exportClass Participant

# TO DO add examples above
Participant <- setRefClass("Participant",
                       fields = list(id= "character",
                                     test_date = "Date",
                                     graphemes = "list"),
                       methods = list(
                         add_grapheme = function(grapheme) {
                           "Add a passed grapheme to the participant's list
                           of graphemes. The grapheme's entry in
                           the list is named based on the grapheme's
                           symbol. Note that if you try to add
                           a grapheme with a symbol that's identical
                           to one of the graphemes already in the
                           participant's list of graphemes, the
                           already existing same-symbol grapheme
                           is overwritten."
                           if (!length(grapheme$symbol)) {
                             stop("I was passed a grapheme without a symbol. You must assign the grapheme a symbol before using <participant>.add_grapheme().")
                           }
                           gs <- grapheme$symbol
                           graphemes[[gs]] <<- grapheme
                         },

                         add_graphemes = function(grapheme_vector) {
                           "Go through a passed list of Grapheme instances
                           and add each one using the add_grapheme() method."
                           for (i in grapheme_vector) {
                             add_grapheme(i)
                           }
                           rm(i)
                         },

                         set_date = function(in_date) {
                           "Takes in a one-element character vector with a date
                           in the format 'YYYY-MM-DD' and sets the participant's
                           test_date to the specified date."
                           test_date <<- as.Date(in_date)
                         },

                         has_graphemes = function() {
                           "Returns TRUE if there is at least one
                           grapheme in the participant's graphemes list,
                           otherwise returns FALSE"
                           return(length(graphemes) > 0)
                         },

                         get_symbols = function() {
                           "Returns a character vector with all symbols for
                           graphemes associated with the participant."
                           return(names(graphemes))
                         },

                         get_mean_response_time = function(na.rm=FALSE) {
                           "Returns the mean response time, with respect to all
                           Grapheme instances associated with the participant.
                           Weights response times based on number of valid responses
                           that each grapheme has."
                           if (!has_graphemes()) {
                             stop("Tried to fetch mean response time for participant without graphemes. Please add graphemes before calling get_mean_response_time().")
                           }
                           grapheme_level_response_times <- numeric()
                           for (g in graphemes) {
                             weight <- length(g$response_times)
                             g_time <- g$get_mean_response_time()
                             grapheme_level_response_times <- c(grapheme_level_response_times, rep(g_time, weight))
                           }
                           return(mean(grapheme_level_response_times, na.rm=na.rm))
                         },

                         get_number_all_colored_graphemes = function() {
                           "Returns the number of graphemes for which all
                           responses have an associated non-na color"
                           if (!has_graphemes()) {
                             return(0)
                           }
                           num_all_colored_response <- 0
                           for (g in graphemes) {
                             if (g$has_only_non_na_colors()) {
                               num_all_colored_response <- num_all_colored_response + 1
                             }
                           }
                           return(num_all_colored_response)
                         },

                         get_consistency_scores = function(na.rm=FALSE) {
                           "Returns a list of grapheme symbols with associated consistency scores"
                           if (!has_graphemes()) {
                             stop("Tried to fetch mean consistency score for participant without graphemes. Please add graphemes before calling get_mean_consistency_score().")
                           }
                           grapheme_level_consistency_scores <- list()
                           for (g in graphemes) {
                             grapheme_level_consistency_scores[[g$symbol]] <- g$get_consistency_score(na.rm=na.rm)
                           }
                           return(grapheme_level_consistency_scores)
                         },

                         get_mean_consistency_score = function(na.rm=FALSE) {
                           "Returns the mean consistency score, with respect to
                           Grapheme instances, that only have valid response colors,
                           associated with the participant."
                           cons_vec <- unlist(get_consistency_scores(na.rm=na.rm))
                           return(mean(cons_vec, na.rm=na.rm))
                         },

                         get_plot_data = function() {
                           "Returns a data frame with the following columns:
                           1. Character: grapheme (grapheme names)
                           2. Numeric: consistency_score
                           3... Character columns: color_resp<x> hold response colors (number of columns depends on number of response colors associated with each grapheme).

                           The data frame is intended to be used for plotting participant data,
                           using .get_consistency_plot(). The call will end with an error
                           if not all of the participant's graphemes have the same number
                           of color responses. This is intended."
                           if (!has_graphemes()) {
                             stop("Tried to fetch plot data for participant without graphemes. Please add graphemes before calling get_plot_data().")
                           }
                           num_responses <- length(graphemes[[1]]$response_colors)
                           col_names <- c("symbol", "consistency_score",
                                          paste0("color_resp_", 1:num_responses))
                           plot_mat <- matrix(vector(), nrow=0, ncol=num_responses+2,
                                              dimnames=list(c(), col_names))
                           plot_df <- data.table::data.table(plot_mat, stringsAsFactors = FALSE)
                           for (g in graphemes) {
                             plot_df <- data.table::rbindlist(plot_df, g$get_plot_data_list())
                           }
                           return(plot_df)
                         }
                       )
)
