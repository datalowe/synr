#' A Reference Class to represent a group of consistency test participants.
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
                           of participant The participant's entry in
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

                             get_mean_response_times = function(na.rm=FALSE) {
                               "Returns the mean response times, with respect to
                           Grapheme instances associated with each participant. See
                               the documentation for the Participant class for more
                               information."
                               if (!has_participants()) {
                                 stop("Tried to fetch mean response times for participantgroup without participants. Please add participants before calling get_mean_response_times().")
                               }
                               participant_level_response_times <- numeric(length(participants))
                               loop_index <- 1
                               for (p in participants) {
                                 p_time <- p$get_mean_response_time()
                                 participant_level_response_times[loop_index] <- p_time
                                 loop_index <- loop_index + 1
                               }
                               return(participant_level_response_times)
                             },

                             get_numbers_all_colored_graphemes = function() {
                               "Returns the a list with number representing how many
                               graphemes with all-valid (non-na) response colors that each
                               participant has."
                               if (!has_participants()) {
                                 stop("Tried to fetch mean numbers of all colored graphemes for participantgroup without participants. Please add participants before calling get_numbers_all_colored_graphemes().")
                               }
                               participant_level_number_all_colored_graphemes <- numeric(length(participants))
                               loop_index <- 1
                               for (p in participants) {
                                 p_num_all_colored <- p$get_number_all_colored_graphemes()
                                 participant_level_number_all_colored_graphemes[loop_index] <- p_num_all_colored
                                 loop_index <- loop_index + 1
                               }
                               return(participant_level_number_all_colored_graphemes)
                             },

                             get_mean_consistency_scores = function(na.rm=FALSE) {
                               "Returns a vector mean consistency scores for
                               participants in the group"
                               if (!has_participants()) {
                                 stop("Tried to fetch mean numbers of all colored graphemes for participantgroup without participants. Please add participants before calling get_numbers_all_colored_graphemes().")
                               }
                               participant_level_mean_consistency_scores <- numeric(length(participants))
                               loop_index <- 1
                               for (p in participants) {
                                 p_mean_c_score <- p$get_mean_consistency_score()
                                 participant_level_mean_consistency_scores[loop_index] <- p_mean_c_score
                                 loop_index <- loop_index + 1
                               }
                               return(participant_level_mean_consistency_scores)
                             },

                             save_plots = function(path=NULL, file_format='png', dpi=300, ...) {
                               "Goes through all participants and for each one produces and saves
                               a ggplot2 plot that describes the participant's
                           grapheme color responses and per-grapheme consistency scores,
                           using the ggsave function.
                           If save_dir is not specified, plots are saved to the current
                           working directory. Otherwise, plots are saved to the specified
                           directory. The file is saved using the specified file_format,
                           e. g. JPG (see ggplot2::ggsave documentation for list of
                           supported formats), and the resolution specified with
                           the dpi argument. Apart from these, all other arguments
                           that ggsave accepts (e. g. 'scale') also work with this function, since
                           all arguments are passed on to ggsave."
                               if (!has_participants()) {
                                 stop("Tried to fetch mean numbers of all colored graphemes for participantgroup without participants. Please add participants before calling get_numbers_all_colored_graphemes().")
                               }
                               for (p in participants) {
                                 p$save_plot(path=path, file_format=file_format, dpi=dpi)
                               }
                             }
                           )
)
