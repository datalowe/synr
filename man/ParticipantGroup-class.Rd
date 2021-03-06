% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParticipantGroup.R
\docType{class}
\name{ParticipantGroup-class}
\alias{ParticipantGroup-class}
\alias{ParticipantGroup}
\title{A Reference Class for representing a group of consistency test participants}
\description{
A Reference Class for representing a group of consistency test participants
}
\section{Fields}{

\describe{
\item{\code{participants}}{A list of \code{\link[synr]{Participant}} class instances.}
}}

\section{Methods}{

\describe{
\item{\code{add_participant(participant)}}{Add a passed participant to the participantgroup's list
of participants. The participant's entry in
the list is named based on the participant's
id. Note that if you try to add
a participant with an id that's identical
to one of the participants already in the
participantgroup's list of participants, the
already existing same-id participant
is overwritten.}

\item{\code{add_participants(participant_list)}}{Go through a passed list of Participant instances
and add each one using the add_participant() method.}

\item{\code{get_ids()}}{Returns a character vector with all ids for
participants associated with the participantgroup.}

\item{\code{get_mean_consistency_scores(
  na.rm = FALSE,
  symbol_filter = NULL,
  method = "euclidean"
)}}{Returns a vector of mean consistency scores for
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
                               the base R dist function for all options)}

\item{\code{get_mean_response_times(na.rm = FALSE, symbol_filter = NULL)}}{Returns the mean response times, with respect to
Grapheme instances associated with each participant.
If na.rm=TRUE, for each participant returns mean response time even
if there are missing response times. If na.rm=FALSE, returns
mean response time if there is at least one response time
value for at least one of the participants' graphemes. If a
character vector is passed to symbol_filter, only data from
graphemes with symbols in the passed vector are used when
calculating each participant's mean response time.}

\item{\code{get_numbers_all_colored_graphemes(symbol_filter = NULL)}}{Returns a vector with numbers representing how many
graphemes with all-valid (non-na) response colors that each
participant has.  If a character vector is passed to symbol_filter,
only data from graphemes with symbols in the passed vector are
used when calculating the mean response time.}

\item{\code{get_prop_color_values(
  color_label = NULL,
  r = NULL,
  g = NULL,
  b = NULL,
  symbol_filter = NULL
)}}{For each participant, get the proportion of its response colors that
                             are within a specified color range. The range is specified using
                             color_label or the r/g/b arguments.

                             Possible color_label specifications are: "blue", "red", "green", "white",
                             "black" or "hazy". Note that the ranges associated with each color
                             label are extremely arbitrary and cannot be relied on - use them only,
                             if at all, to get a very rough idea of sample characteristics.

                             For r/g/b arguments, value ranges are specified
                             using two-element numeric vectors, with rgb values on a 0-1 scale.
                             E. g. r=c(0, 0.3), g=c(0, 0.3), b=c(0, 0.3) would code for a dark color range.

                             If a character vector is passed to symbol_filter, only data for graphemes
                             with symbols in the passed vector are used.}

\item{\code{has_participants()}}{Returns TRUE if there is at least one
participant in the participantgroup's participants list,
otherwise returns FALSE}

\item{\code{save_plots(
  path = NULL,
  file_format = "png",
  dpi = 300,
  cutoff_line = FALSE,
  mean_line = FALSE,
  grapheme_size = 2,
  grapheme_angle = 0,
  symbol_filter = NULL,
  ...
)}}{Goes through all participants and for each one produces and saves
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
                             all arguments are passed on to ggsave. }
}}

