#' A Reference Class for representing consistency test participants
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

                         add_graphemes = function(grapheme_list) {
                           "Go through a passed list of Grapheme instances
                           and add each one using the add_grapheme() method."
                           for (g in grapheme_list) {
                             add_grapheme(g)
                           }
                           rm(g)
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

                         get_mean_response_time = function(na.rm=FALSE,
                                                           symbol_filter=NULL) {
                           "Returns the mean response time, with respect to all
                           Grapheme instances associated with the participant.
                           Weights response times based on number of valid responses
                           that each grapheme has. If na.rm=TRUE, returns mean response
                           time even if there are missing response times. If na.rm=FALSE,
                           returns mean response time if there is at least one response time
                           value for at least one of the participants' graphemes. If a
                           character vector is passed to symbol_filter, only data from
                           graphemes with symbols in the passed vector are used when
                           calculating the mean response time."
                           if (!has_graphemes()) {
                             stop("Tried to fetch mean response time for participant without graphemes. Please add graphemes before calling get_mean_response_time().")
                           }
                           grapheme_level_response_times <- numeric()
                           for (grapheme in graphemes) {
                             if ( (!is.null(symbol_filter)) && (!grapheme$symbol %in% symbol_filter)) {
                               next
                             }
                             weight <- length(grapheme$response_times)
                             g_time <- grapheme$get_mean_response_time(na.rm=na.rm)
                             grapheme_level_response_times <- c(grapheme_level_response_times, rep(g_time, weight))
                           }
                           return(mean(grapheme_level_response_times, na.rm=na.rm))
                         },

                         get_number_all_colored_graphemes = function(symbol_filter=NULL) {
                           "Returns the number of graphemes for which all
                           responses have an associated non-na color. If a
                           character vector is passed to symbol_filter, only data from
                           graphemes with symbols in the passed vector are used when
                           calculating the mean response time."
                           if (!has_graphemes()) {
                             return(0)
                           }
                           num_all_colored_response <- 0
                           for (grapheme in graphemes) {
                             if ( (!is.null(symbol_filter)) && (!grapheme$symbol %in% symbol_filter)) {
                               next
                             }
                             if (grapheme$has_only_non_na_colors()) {
                               num_all_colored_response <- num_all_colored_response + 1
                             }
                           }
                           return(num_all_colored_response)
                         },

                         get_consistency_scores = function(na.rm=FALSE,
                                                           symbol_filter=NULL,
                                                           method="euclidean") {
                           "Returns a list of grapheme symbols with associated consistency scores.
                           If na.rm=TRUE, for each grapheme a consistency score calculation is
                           forced (except if ALL response colors associated with the grapheme
                           are NA). That probably isn't what you want, because it leads to things
                           like a perfect consistency score if all except one response color are
                           NA. Defaults to na.rm=FALSE.

                           If a character vector is passed to
                           symbol_filter, only consistency scores for graphemes with symbols
                           in the passed vector are returned.

                           Use the method argument to specify what kind of color space
                           distances should be used when calculating consistency score
                           (usually 'manhattan' or 'euclidean' - see documentation for
                           the base R dist function for all options)"
                           if (!has_graphemes()) {
                             stop("Tried to fetch mean consistency score for participant without graphemes. Please add graphemes before calling get_mean_consistency_score().")
                           }
                           grapheme_level_consistency_scores <- list()
                           if (!is.null(symbol_filter)) {
                             for (g in graphemes) {
                               if (g$symbol %in% symbol_filter) {
                               grapheme_level_consistency_scores[[g$symbol]] <- g$get_consistency_score(na.rm=na.rm,
                                                                                                        method=method)
                               }
                             }
                           } else {
                             for (g in graphemes) {
                               grapheme_level_consistency_scores[[g$symbol]] <- g$get_consistency_score(na.rm=na.rm,
                                                                                                        method=method)
                             }
                           }
                           return(grapheme_level_consistency_scores)
                         },

                         get_mean_consistency_score = function(na.rm=FALSE,
                                                               symbol_filter=NULL,
                                                               method='euclidean') {
                           "Returns the mean consistency score with respect to
                           Grapheme instances associated with the participant.

                           If na.rm=FALSE, calculates the mean consistency score if
                           all of the participants' graphemes only have response
                           colors that are non-NA, otherwise returns NA.
                           If na.rm=TRUE, returns the mean consistency score for
                           all of the participant's graphemes that only have
                           non-NA response colors, while ignoring graphemes
                           that have at least one NA response color value. Note that
                           NA is returned in either case, if ALL of the participants'
                           graphemes have at least one NA response color value.

                           If a character vector is passed to
                           symbol_filter, only data from graphemes with symbols
                           in the passed vector are used when calculating the
                           mean score.

                           Use the method argument to specify what kind of color space
                           distances should be used when calculating consistency score
                           (usually 'manhattan' or 'euclidean' - see documentation for
                           the base R dist function for all options)"
                           cons_vec <- unlist(get_consistency_scores(symbol_filter=symbol_filter,
                                                                     method=method))
                           return(mean(cons_vec, na.rm=na.rm))
                         },

                         get_prop_color = function(color_label=NULL,
                                                   r=NULL, g=NULL, b=NULL,
                                                   symbol_filter=NULL) {
                           "Get the proportion of participant's response colors that
                           are within a specified color range. The range is specified using
                           color_label or the r/g/b arguments. Possible color_label
                           specifications are: \"blue\", \"red\", \"green\", \"white\",
                           \"black\" or \"hazy\". For r/g/b arguments, value ranges are specified
                           using two-element numeric vectors, with rgb values on a 0-1 scale.
                           E. g. r=c(0, 0.3), g=c(0, 0.3), b=c(0, 0.3) would code for a dark color range.
                           If a character vector is passed to symbol_filter, only data for graphemes
                           with symbols in the passed vector are used."
                           if (!has_graphemes()) {
                             stop("Tried to fetch proportion of responses within a color range for participant without graphemes. Please add graphemes before calling get_prop_color().")
                           }
                           grapheme_level_color_props <- numeric()
                           for (grapheme in graphemes) {
                             if ( (!is.null(symbol_filter)) && (!grapheme$symbol %in% symbol_filter)) {
                               next
                             }
                             weight <- nrow(grapheme$response_colors)
                             g_prop_col <- grapheme$get_prop_color(color_label=color_label, r=r, g=g, b=b)
                             grapheme_level_color_props <- c(grapheme_level_color_props, rep(g_prop_col, weight))
                           }
                           return(mean(grapheme_level_color_props, na.rm=TRUE))
                         },

                         get_plot_data = function(symbol_filter=NULL) {
                           "Returns a data frame with the following columns:\n
                           1. grapheme (grapheme names - of type character)\n
                           2. consistency_score (of type numeric)\n
                           3... color_resp<x>, where x is a digit: hold response hex color codes (number of columns depends on number of response colors associated with each grapheme).

                           The data frame is intended to be used for plotting participant data,
                           using .get_plot(). The call will end with an error
                           if not all of the participant's graphemes have the same number
                           of color responses. This is intended.

                           If a character vector is passed to symbol_filter, only data for graphemes
                           with symbols in the passed vector are used."
                           if (!has_graphemes()) {
                             stop("Tried to fetch plot data for participant without graphemes. Please add graphemes before calling get_plot_data().")
                           }
                           num_responses <- nrow(graphemes[[1]]$response_colors)
                           col_names <- c("symbol", "consistency_score",
                                          paste0("color_resp_", 1:num_responses))
                           plot_mat <- matrix(vector(), nrow=0, ncol=num_responses+2,
                                              dimnames=list(c(), col_names))
                           plot_df <- data.table::data.table(plot_mat, stringsAsFactors = FALSE)
                           for (grapheme in graphemes) {
                             if ( (!is.null(symbol_filter)) && (!grapheme$symbol %in% symbol_filter)) {
                               next
                             }
                             plot_df <- data.table::rbindlist(list(plot_df, grapheme$get_plot_data_list() ))
                           }
                           plot_df <- plot_df[order(nchar(plot_df$symbol), plot_df$symbol), ]
                           plot_df$symbol <- factor(plot_df$symbol, levels=plot_df$symbol)
                           return(plot_df)
                         },

                         get_plot = function(cutoff_line=FALSE, mean_line=FALSE,
                                             grapheme_size=2, grapheme_angle=0,
                                             symbol_filter=NULL) {
                           # TO DO change the plotting functionality so that it can handle
                           # all-black graphemes (this leads to clustering of symbols at
                           # one position atm)
                           # TO DO add support for more than three graphemes (this leads
                           # to one symbol ending up below the plot area and cut off atm)
                           "Returns a ggplot2 plot that describes this participant's
                           grapheme color responses and per-grapheme consistency scores.

                           If cutoff_line=TRUE, the plot will include a blue line that
                           indicates the value 135.30, which is the synesthesia cut-off score recommended
                           by Rothen, Seth, Witzel & Ward (2013) for the L*u*v color space.
                           If mean_line=TRUE, the plot will include a green line that indicates
                           the participant's mean consistency score for graphemes with all-valid
                           response colors (if the participant has any such graphemes). If a vector
                           is passed to symbol_filter, this green line represents the mean score
                           for ONLY the symbols included in the filter.

                           Pass a value to grapheme_size to adjust the size of graphemes
                           shown at the bottom of the plot, e. g. increasing the size if
                           there's a lot of empty space otherwise, or decreasing the size if the
                           graphemes don't fit. Similarly, you can use the grapheme_angle
                           argument to rotate the graphemes, which might help them fit better.

                           If a character vector is passed to symbol_filter, only data for graphemes
                           with symbols in the passed vector are used.

                           Graphemes are sorted left-to-right by 1. length and
                           2. unicode value (this means among other things that digits
                           come before letters)."
                           plot_df <- get_plot_data(symbol_filter=symbol_filter)
                           y_upper_limit <- ifelse(all(is.na(plot_df$consistency_score)),
                                                   5,
                                                   max(plot_df$consistency_score, na.rm=TRUE))
                           y_breaks <- round(seq(0, y_upper_limit, length.out = 10), -floor(log10(y_upper_limit)) )

                           consistency_plot <- ggplot2::ggplot(data=plot_df, ggplot2::aes(x=symbol, y=consistency_score)) +
                             ggplot2::geom_col(fill="black", color="black", width=0.5) +
                             ggplot2::scale_y_continuous(breaks= y_breaks) +
                             ggplot2::labs(x="Grapheme", y="Sum distance between responses") +
                             ggplot2::scale_x_discrete(labels=NULL) +
                             ggplot2::theme(axis.ticks.x=ggplot2::element_blank(),
                                            panel.grid.major.x=ggplot2::element_blank(),
                                            panel.grid.minor.y=ggplot2::element_blank()) +
                             ggplot2::coord_cartesian(y = c(-y_upper_limit / 10,
                                                            y_upper_limit))

                           pos_factor <- 0.04
                           for (color_column in colnames(plot_df)[3:ncol(plot_df)]) {
                             consistency_plot <- consistency_plot +
                               ggplot2::geom_text(y=-y_upper_limit * pos_factor,
                                                  label=plot_df[['symbol']],
                                                  size=grapheme_size,
                                                  angle=grapheme_angle,
                                                  color=plot_df[[color_column]])
                             pos_factor <- pos_factor + 0.04
                           }
                           if (cutoff_line) {
                             consistency_plot <- consistency_plot + ggplot2::geom_hline(yintercept=135.30, color="blue")
                           }
                           if (mean_line && get_number_all_colored_graphemes() > 0) {
                             mean_cs <- mean(plot_df$consistency_score, na.rm=TRUE)
                             consistency_plot <- consistency_plot + ggplot2::geom_hline(yintercept=mean_cs, color="green")
                           }
                           return(consistency_plot)
                         },

                         save_plot = function(path=NULL, file_format='png', dpi=300,
                                              cutoff_line=FALSE, mean_line=FALSE,
                                              grapheme_size=2, grapheme_angle=0,
                                              symbol_filter=NULL, ...) {
                           "Saves a ggplot2 plot that describes this participant's
                           grapheme color responses and per-grapheme consistency scores,
                           using the ggsave function.

                           If a character vector is passed to symbol_filter, only data for graphemes
                           with symbols in the passed vector are used.

                           If save_dir is not specified, the plot is saved to the current
                           working directory. Otherwise, the plot is saved to the specified
                           directory. The file is saved using the specified file_format,
                           e. g. JPG (see ggplot2::ggsave documentation for list of
                           supported formats), and the resolution specified with
                           the dpi argument.

                           If cutoff_line=TRUE, the plot will include a blue line that
                           indicates the value 135.30, which is the synesthesia cut-off score recommended
                           by Rothen, Seth, Witzel & Ward (2013) for the L*u*v color space.
                           If mean_line=TRUE, the plot will include a green line that indicates
                           the participant's mean consistency score for graphemes with all-valid
                           response colors (if the participant has any such graphemes). If a vector
                           is passed to symbol_filter, this green line represents the mean score
                           for ONLY the symbols included in the filter.

                           Pass a value to grapheme_size to adjust the size of graphemes
                           shown at the bottom of the plot, e. g. increasing the size if
                           there's empty space otherwise, or decreasing the size if the
                           graphemes don't fit. Similarly, you can use the grapheme_angle
                           argument to rotate the graphemes, which might help them fit better.

                           Apart from these, all other arguments
                           that ggsave accepts (e. g. 'scale') also work with this function, since
                           all arguments are passed on to ggsave."
                           consistency_plot <- get_plot(cutoff_line=cutoff_line,
                                                        mean_line=mean_line,
                                                        grapheme_size=grapheme_size,
                                                        grapheme_angle=grapheme_angle,
                                                        symbol_filter=symbol_filter)
                           plot_file_name <- paste0(id, '_consistency_plot.', file_format)
                           suppressWarnings(
                             ggplot2::ggsave(filename=plot_file_name,
                                           plot=consistency_plot,
                                           path=path,
                                           dpi=dpi,
                                           ...=...)
                           )
                         }
                       )
)
