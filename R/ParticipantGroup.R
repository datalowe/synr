#' A Reference Class for representing a group of consistency test participants
#' @field participants A list of \code{\link[synr]{Participant}} class instances.
#' @importFrom methods new
#' @export ParticipantGroup
#' @exportClass ParticipantGroup

# TO DO add examples above
ParticipantGroup <- setRefClass("ParticipantGroup",
                           fields = list(participants = "list"),
                           methods = list(
                             add_participant = function(participant) {
                               "Add a passed participant to the participantgroup's list
                           of participants. The participant's entry in
                           the list is named based on the participant's
                           id. Note that if you try to add
                           a participant with an id that's identical
                           to one of the participants already in the
                           participantgroup's list of participants, the
                           already existing same-id participant
                           is overwritten."
                               if (!length(participant$id)) {
                                 stop("I was passed a participant without an id. You must assign the participant an id before using <participantgroup>.add_participant().")
                               }
                               p_id <- participant$id
                               participants[[p_id]] <<- participant
                             },

                             add_participants = function(participant_list) {
                               "Go through a passed list of Participant instances
                           and add each one using the add_participant() method."
                               for (p in participant_list) {
                                 add_participant(p)
                               }
                               rm(p)
                             },

                             has_participants = function() {
                               "Returns TRUE if there is at least one
                           participant in the participantgroup's participants list,
                           otherwise returns FALSE"
                               return(length(participants) > 0)
                             },

                             get_ids = function() {
                               "Returns a character vector with all ids for
                           participants associated with the participantgroup."
                               return(names(participants))
                             },

                             get_mean_response_times = function(na.rm=FALSE,
                                                                symbol_filter=NULL) {
                               "Returns the mean response times, with respect to
                               Grapheme instances associated with each participant.
                               If na.rm=TRUE, for each participant returns mean response time even
                               if there are missing response times. If na.rm=FALSE, returns
                               mean response time if there is at least one response time
                               value for at least one of the participants' graphemes. If a
                               character vector is passed to symbol_filter, only data from
                               graphemes with symbols in the passed vector are used when
                               calculating each participant's mean response time."
                               if (!has_participants()) {
                                 stop("Tried to fetch mean response times for participantgroup without participants. Please add participants before calling get_mean_response_times().")
                               }
                               participant_level_response_times <- numeric(length(participants))
                               loop_index <- 1
                               for (p in participants) {
                                 p_time <- p$get_mean_response_time(na.rm=na.rm,
                                                                    symbol_filter=symbol_filter)
                                 participant_level_response_times[loop_index] <- p_time
                                 loop_index <- loop_index + 1
                               }
                               return(participant_level_response_times)
                             },

                             get_prop_color_values = function(color_label=NULL,
                                                              r=NULL, g=NULL, b=NULL,
                                                              symbol_filter=NULL) {
                               "For each participant, get the proportion of its response colors that
                             are within a specified color range. The range is specified using
                             color_label or the r/g/b arguments.

                             Possible color_label specifications are: \"blue\", \"red\", \"green\", \"white\",
                             \"black\" or \"hazy\". Note that the ranges associated with each color
                             label are extremely arbitrary and cannot be relied on - use them only,
                             if at all, to get a very rough idea of sample characteristics.

                             For r/g/b arguments, value ranges are specified
                             using two-element numeric vectors, with rgb values on a 0-1 scale.
                             E. g. r=c(0, 0.3), g=c(0, 0.3), b=c(0, 0.3) would code for a dark color range.

                             If a character vector is passed to symbol_filter, only data for graphemes
                             with symbols in the passed vector are used."
                               if (!has_participants()) {
                                 stop("Tried to get 'proportion of specified color' values for participantgroup without participants. Please add participants before calling get_prop_color_values().")
                               }
                               participant_level_prop_cols <- numeric(length(participants))
                               loop_index <- 1
                               for (p in participants) {
                                 p_prop_col <- p$get_prop_color(color_label=color_label,
                                                                r=r, g=g, b=b,
                                                                symbol_filter=symbol_filter)
                                 participant_level_prop_cols[loop_index] <- p_prop_col
                                 loop_index <- loop_index + 1
                               }
                               return(participant_level_prop_cols)
                             },

                             get_numbers_all_colored_graphemes = function(symbol_filter=NULL) {
                               "Returns a vector with numbers representing how many
                               graphemes with all-valid (non-na) response colors that each
                               participant has.  If a character vector is passed to symbol_filter,
                               only data from graphemes with symbols in the passed vector are
                               used when calculating the mean response time."
                               if (!has_participants()) {
                                 stop("Tried to fetch mean numbers of all colored graphemes for participantgroup without participants. Please add participants before calling get_numbers_all_colored_graphemes().")
                               }
                               participant_level_number_all_colored_graphemes <- numeric(length(participants))
                               loop_index <- 1
                               for (p in participants) {
                                 p_num_all_colored <- p$get_number_all_colored_graphemes(symbol_filter=symbol_filter)
                                 participant_level_number_all_colored_graphemes[loop_index] <- p_num_all_colored
                                 loop_index <- loop_index + 1
                               }
                               return(participant_level_number_all_colored_graphemes)
                             },

                             get_mean_consistency_scores = function(na.rm=FALSE,
                                                                    symbol_filter=NULL,
                                                                    method="euclidean") {
                               "Returns a vector of mean consistency scores for
                               participants in the group. If na.rm=FALSE, for each
                               participant calculates the mean consistency score if
                               all of the participants' graphemes only have response
                               colors that are non-NA, otherwise puts an NA value
                               for that participant in returned vector. If na.rm=TRUE,
                               for each participant calculates the mean consistency score for
                               all of the participant's graphemes that only have
                               non-NA response colors, while ignoring graphemes
                               that have at least one NA response color value. Note that
                               for participants whose graphemes ALL have at least one NA
                               response color value, an NA is put in the returned vector for
                               that participant, regardless of what na.rm is set to.

                               If a character vector is passed to symbol_filter, only
                               data from graphemes with symbols in the passed vector
                               are used when calculating each participant's mean score.

                               Use the method argument to specify what kind of color space
                               distances should be used when calculating consistency scores
                               (usually 'manhattan' or 'euclidean' - see documentation for
                               the base R dist function for all options)"
                               if (!has_participants()) {
                                 stop("Tried to fetch mean numbers of all colored graphemes for participantgroup without participants. Please add participants before calling get_numbers_all_colored_graphemes().")
                               }
                               participant_level_mean_consistency_scores <- numeric(length(participants))
                               loop_index <- 1
                               for (p in participants) {
                                 p_mean_c_score <- p$get_mean_consistency_score(na.rm=na.rm,
                                                                                symbol_filter=symbol_filter,
                                                                                method=method)
                                 participant_level_mean_consistency_scores[loop_index] <- p_mean_c_score
                                 loop_index <- loop_index + 1
                               }
                               return(participant_level_mean_consistency_scores)
                             },

                             save_plots = function(path=NULL, file_format='png', dpi=300,
                                                   cutoff_line=FALSE, mean_line=FALSE,
                                                   grapheme_size=2, grapheme_angle=0,
                                                   symbol_filter=NULL,
                                                   ...) {
                             "Goes through all participants and for each one produces and saves
                             a ggplot2 plot that describes the participant's
                             grapheme color responses and per-grapheme consistency scores,
                             using the ggsave function.

                             If a character vector is passed to symbol_filter, only data for graphemes
                             with symbols in the passed vector are used.

                             If path is not specified, plots are saved to the current
                             working directory. Otherwise, plots are saved to the specified
                             directory. The file is saved using the specified file_format,
                             e. g. JPG (see ggplot2::ggsave documentation for list of
                             supported formats), and the resolution specified with
                             the dpi argument.

                             If cutoff_line=TRUE, each plot will include a blue line that
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

                             Apart from the ones above, all other arguments
                             that ggsave accepts (e. g. 'scale') also work with this function, since
                             all arguments are passed on to ggsave. "
                               if (!has_participants()) {
                                 stop("Tried to fetch mean numbers of all colored graphemes for participantgroup without participants. Please add participants before calling get_numbers_all_colored_graphemes().")
                               }
                               for (p in participants) {
                                 p$save_plot(path=path, file_format=file_format, dpi=dpi,
                                             cutoff_line=cutoff_line, mean_line=mean_line, grapheme_size=grapheme_size,
                                             grapheme_angle=grapheme_angle, symbol_filter=symbol_filter,
                                             ...)
                               }
                             }
                           )
)
