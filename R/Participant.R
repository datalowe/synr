#' A Reference Class for representing consistency test participants
#' 
#' @field id A one-element character vector containing the participant's ID.
#' Set at class new() call.
#' @field test_date A one-element Date vector which specifies the date
#' on which the participant did the consistency test.
#' @field graphemes A list of \code{\link{Grapheme}} class instances.
#' @importFrom methods new
#' @export Participant
#' @exportClass Participant

Participant <- setRefClass(
  "Participant",
  fields = list(id = "character",
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
      stop(paste0(
        "I was passed a grapheme without a symbol. ",
        "You must assign the grapheme a symbol before ",
        "using <participant>.add_grapheme()."
      ))
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

    get_all_colored_symbols = function(symbol_filter = NULL) {
      "Returns a character vector of symbols corresponding to graphemes for
      which all responses have an associated non-NA color. If a
      character vector is passed to symbol_filter, only
      symbols in the passed vector are returned."
      allcolor_symbols <- character()
      if (!has_graphemes()) {
        return(allcolor_symbols)
      }
      filtered_graphemes <- filter_graphemes(
        graphemes,
        symbol_filter
      )
      for (grapheme in filtered_graphemes) {
        if (grapheme$has_only_non_na_colors()) {
          allcolor_symbols <- c(allcolor_symbols, grapheme$symbol)
        }
      }
      return(allcolor_symbols)
    },

    get_symbols = function() {
        "Returns a character vector with all symbols for
        graphemes associated with the participant."
        return(names(graphemes))
    },

    get_mean_response_time = function(
        symbol_filter = NULL,
        na.rm = FALSE
    ) {
      "Returns the mean response time, with respect to all
      Grapheme instances associated with the participant.
      Weights response times based on number of valid responses
      that each grapheme has. If na.rm = TRUE, returns mean response
      time even if there are missing response times. If na.rm = FALSE,
      returns mean response time if there is at least one response time
      value for at least one of the participants' graphemes. If a
      character vector is passed to symbol_filter, only data from
      graphemes with symbols in the passed vector are used when
      calculating the mean response time."
      if (!has_graphemes()) {
        stop(paste0(
          "Tried to fetch mean response time for ",
          "participant without graphemes. Please add ",
          "graphemes before calling get_mean_response_time()."
        ))
      }
      grapheme_level_response_times <- numeric()
      filtered_graphemes <- filter_graphemes(
        graphemes,
        symbol_filter
      )
      for (grapheme in filtered_graphemes) {
        weight <- length(grapheme$response_times)
        g_time <- grapheme$get_mean_response_time(na.rm = na.rm)
        grapheme_level_response_times <- c(
          grapheme_level_response_times,
          rep(g_time, weight)
        )
      }
      return(mean(grapheme_level_response_times, na.rm = na.rm))
    },

    get_nonna_color_resp_mat = function(symbol_filter = NULL) {
      "Returns an n-by-3 matrix of all non-NA color responses' data,
      where each column represents a color axis and each row a response
      color. If a character vector is passed to symbol_filter,
      only data from responses associated with graphemes with corresponding 
      symbols are included."
      if (!has_graphemes()) {
        return(matrix(nrow = 0, ncol = 3))
      }
      filtered_graphemes <- filter_graphemes(
        graphemes,
        symbol_filter
      )
      color_matrix <- matrix(nrow = 0, ncol = 3)
      for (grapheme in filtered_graphemes) {
        color_matrix <- rbind(color_matrix, grapheme$response_colors)
      }
      # remove NA color responses
      color_matrix <- na.omit(color_matrix)
      return(color_matrix)
    },

    get_number_all_colored_graphemes = function(symbol_filter = NULL) {
      "Returns the number of graphemes for which all
      responses have an associated non-NA color. If a
      character vector is passed to symbol_filter, only
      graphemes with symbols in the passed vector are counted."
      num_all_colored <- length(get_all_colored_symbols(symbol_filter = symbol_filter))
      return(num_all_colored)
    },

    get_grapheme_mean_colors = function(
      symbol_filter = NULL,
      na.rm = FALSE
    ) {
      "Returns a list of grapheme symbols with associated mean colors,
      using the color space set at participant creation. Colors are represented
      by 3-element vectors. 
      
      Example: if color space is RGB, vector element 1 represents
      grapheme mean R value, element 2 mean G value, element 3
      B value.

      If na.rm = TRUE, for each grapheme a mean color is calculated even
      if one its associated response colors is missing. Defaults to
      na.rm = FALSE.

      If a character vector is passed to symbol_filter, only
      mean colors for graphemes with symbols
      in the passed vector are returned."
      if (!has_graphemes()) {
        stop(paste0(
          "Tried to fetch mean colors for participant ",
          "without graphemes. Please add graphemes before calling ",
          "get_grapheme_mean_colors()."
        ))
      }
      grapheme_mean_colors <- list()
      filtered_graphemes <- filter_graphemes(
        graphemes,
        symbol_filter
      )
      for (grapheme in filtered_graphemes) {
        g_m_color <- grapheme$get_mean_color(
          na.rm = na.rm
        )
        grapheme_mean_colors[[grapheme$symbol]] <- g_m_color
      }
      return(grapheme_mean_colors)
    },

    get_participant_mean_color = function(
      symbol_filter = NULL,
      na.rm = FALSE
    ) {
      "Returns average of all of participants' registered
      response colors (based on the color space
      set at participant initialization) as a 3-element vector.
      Example: if color space is RGB, element 1 represents
      mean R value, element 2 mean G value, element 3
      B value.

      If a character vector is passed to
      symbol_filter, only data from graphemes with symbols
      in the passed vector are used when calculating the
      mean color.

      If na.rm = FALSE, calculates the mean response color if
      all of the participants' graphemes only have response
      colors that are non-NA, otherwise returns NA.
      If na.rm = TRUE, returns the mean response color based on
      all non-NA response colors."

      color_matrix <- do.call(
        rbind,
        get_grapheme_mean_colors(
          symbol_filter = symbol_filter,
          na.rm = na.rm
        )
      )
      return(colMeans(color_matrix, na.rm = na.rm)) 
    },

    get_consistency_scores = function(
      method = "euclidean",
      symbol_filter = NULL,
      na.rm = FALSE
    ) {
      "Returns a list of grapheme symbols with associated consistency scores.
      If na.rm = TRUE, for each grapheme a consistency score calculation is
      forced (except if ALL response colors associated with the grapheme
      are NA). That probably isn't what you want, because it leads to things
      like a perfect consistency score if all except one response color are
      NA. Defaults to na.rm = FALSE.

      If a character vector is passed to
      symbol_filter, only consistency scores for graphemes with symbols
      in the passed vector are returned.

      Use the method argument to specify what kind of color space
      distances should be used when calculating consistency score
      (usually 'manhattan' or 'euclidean' - see documentation for
      the base R dist function for all options)"
      if (!has_graphemes()) {
        stop(paste0(
          "Tried to fetch consistency scores for participant ",
          "without graphemes. Please add graphemes before calling ",
          "get_consistency_scores()."
        ))
      }
      grapheme_consistency_scores <- list()
      filtered_graphemes <- filter_graphemes(
        graphemes,
        symbol_filter
      )
      for (grapheme in filtered_graphemes) {
        g_c_score <- grapheme$get_consistency_score(
          method = method,
          na.rm = na.rm
        )
        grapheme_consistency_scores[[grapheme$symbol]] <- g_c_score
      }
      return(grapheme_consistency_scores)
    },

    get_mean_consistency_score = function(
      symbol_filter = NULL,
      method="euclidean",
      na.rm = FALSE
    ) {
      "Returns the mean consistency score with respect to
      Grapheme instances associated with the participant.

      If na.rm = FALSE, calculates the mean consistency score if
      all of the participants' graphemes only have response
      colors that are non-NA, otherwise returns NA.
      If na.rm = TRUE, returns the mean consistency score for
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
      cons_vec <- unlist(
        get_consistency_scores(
          method = method,
          symbol_filter = symbol_filter
        )
      )
      return(mean(cons_vec, na.rm = na.rm))
    },

    get_plot_data = function(symbol_filter = NULL) {
      "Returns a data frame with the following columns:\n
      1. grapheme (grapheme names - of type character)\n
      2. consistency_score (of type numeric)\n
      3... color_resp<x>, where x is a digit: hold response hex color codes
      (number of columns depends on number of response colors
      associated with each grapheme).

      The data frame is intended to be used for plotting participant data,
      using .get_plot(). The call will end with an error
      if not all of the participant's graphemes have the same number
      of color responses. This is intended.

      If a character vector is passed to symbol_filter, only data for graphemes
      with symbols in the passed vector are used."
      if (!has_graphemes()) {
        stop(paste0(
          "Tried to fetch plot data for participant without graphemes. ",
          "Please add graphemes before calling get_plot_data()."
        ))
      }
      num_responses <- nrow(graphemes[[1]]$response_colors)
      col_names <- c("symbol", "consistency_score",
                    paste0("color_resp_", 1:num_responses))
      plot_mat <- matrix(vector(), nrow = 0, ncol = num_responses + 2,
                        dimnames = list(c(), col_names))
      plot_df <- data.table::data.table(plot_mat, stringsAsFactors = FALSE)
      filtered_graphemes <- filter_graphemes(
        graphemes,
        symbol_filter
      )
      for (grapheme in filtered_graphemes) {
        plot_df <- data.table::rbindlist(
          list(
            plot_df,
            grapheme$get_plot_data_list()
          )
        )
      }
      row_order <- order(
        nchar(plot_df$symbol),
        plot_df$symbol,
        decreasing = TRUE
      )
      plot_df <- plot_df[row_order, ]
      plot_df$symbol <- factor(plot_df$symbol, levels = plot_df$symbol)
      return(plot_df)
    },

    get_plot = function(
      cutoff_line = FALSE,
      mean_line = FALSE,
      grapheme_size = 2,
      grapheme_angle = 0,
      grapheme_spacing = 0.25,
      foreground_color = "black",
      background_color = "white",
      symbol_filter = NULL
    ) {
      "Returns a ggplot2 plot that describes this participant's
      grapheme color responses and per-grapheme consistency scores.

      If cutoff_line = TRUE, the plot will include a blue line that
      indicates the value 135.30, which is the synesthesia
      cut-off score recommended by Rothen, Seth, Witzel & Ward (2013)
      for the L*u*v color space. If mean_line = TRUE, the plot will
      include a green line that indicates the participant's mean
      consistency score for graphemes with all-valid
      response colors (if the participant has any such graphemes). If a vector
      is passed to symbol_filter, this green line represents the mean score
      for ONLY the symbols included in the filter.

      Pass a value to grapheme_size to adjust the size of graphemes
      shown at the bottom of the plot, e. g. increasing the size if
      there's a lot of empty space otherwise, or decreasing the size if the
      graphemes don't fit. The grapheme_angle
      argument allows rotating graphemes. grapheme_spacing is for adjusting
      how far grapheme symbols are spaced from each other.

      If a character vector is passed to symbol_filter, only data for graphemes
      with symbols in the passed vector are used.

      Graphemes are sorted left-to-right by 1. length and
      2. unicode value (this means among other things that digits
      come before letters)."
      plot_df <- get_plot_data(symbol_filter = symbol_filter)
      # if all values are NA, set upper limit to 5, to enable proper
      # display of graphemes
      y_upper_limit <- ifelse(
        all(is.na(plot_df$consistency_score)),
        5,
        max(plot_df$consistency_score, na.rm = TRUE)
      )
      # if the maximum consistency score is 0 (ie perfect consistency for all
      # graphemes), set upper limit to 5,
      # to enable proper display of graphemes
      y_upper_limit <- ifelse(y_upper_limit > 0, y_upper_limit, 5)
      y_breaks <- round(
        seq(0, y_upper_limit, length.out = 10),
        -floor(log10(y_upper_limit))
      )

      consistency_plot <- ggplot2::ggplot(
        data = plot_df,
        ggplot2::aes(x = symbol, y = consistency_score)
      ) +
        ggplot2::geom_col(fill = foreground_color, color = foreground_color, width = 0.5) +
        ggplot2::scale_y_continuous(breaks = y_breaks) +
        ggplot2::labs(x = "Grapheme", y = "Sum distance between responses") +
        ggplot2::scale_x_discrete(labels = NULL) +
        ggplot2::theme(
          axis.title = ggplot2::element_text(colour = foreground_color),
          axis.text = ggplot2::element_text(colour = foreground_color),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(color = "#ADD8E6"),
          panel.background = ggplot2::element_rect(fill = background_color),
          panel.border = ggplot2::element_rect(
            fill = "transparent",
            color = "#ADD8E6",
            size = 0.4
          ),
          plot.background = ggplot2::element_rect(fill = background_color)
        ) +
        ggplot2::coord_flip(y = c(-y_upper_limit * 0.7,
                                      y_upper_limit))

      pos_factor <- grapheme_spacing / 2
      for (color_column in colnames(plot_df)[3:ncol(plot_df)]) {

        consistency_plot <- consistency_plot +
          ggplot2::geom_text(
            y = -y_upper_limit * pos_factor,
            label = plot_df[["symbol"]],
            size = grapheme_size,
            angle = grapheme_angle,
            color = plot_df[[color_column]]
          )
        pos_factor <- pos_factor + grapheme_spacing
      }
      if (cutoff_line) {
        consistency_plot <- consistency_plot +
          ggplot2::geom_hline(yintercept = 135.30, color = "blue")
      }
      if (mean_line && get_number_all_colored_graphemes() > 0) {
        mean_cs <- mean(plot_df$consistency_score, na.rm = TRUE)
        consistency_plot <- consistency_plot +
          ggplot2::geom_hline(yintercept = mean_cs, color = "green")
      }
      return(consistency_plot)
    },

    save_plot = function(
      save_dir = NULL,
      file_format="png",
      dpi = 300,
      cutoff_line = FALSE,
      mean_line = FALSE,
      grapheme_size = 2,
      grapheme_angle = 0,
      foreground_color = "black",
      background_color = "white",
      symbol_filter = NULL,
      ...
    ) {
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

      If cutoff_line = TRUE, the plot will include a blue line that
      indicates the value 135.30, which is the synesthesia
      cut-off score recommended by Rothen, Seth, Witzel & Ward (2013)
      for the L*u*v color space. If mean_line = TRUE, the plot will
      include a green line that indicates the participant's mean
      consistency score for graphemes with all-valid response colors
      (if the participant has any such graphemes). If a vector
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
      consistency_plot <- get_plot(
        cutoff_line = cutoff_line,
        mean_line = mean_line,
        grapheme_size = grapheme_size,
        grapheme_angle = grapheme_angle,
        foreground_color = foreground_color,
        background_color = background_color,
        symbol_filter = symbol_filter
      )
      plot_file_name <- paste0(id, "_consistency_plot.", file_format)
      suppressWarnings(
        ggplot2::ggsave(
          filename = plot_file_name,
          plot = consistency_plot,
          path = save_dir,
          dpi = dpi,
          ... = ...
        )
      )
    },

    check_valid_get_twcv = function(
      min_complete_graphemes = 5,
      dbscan_eps = 20,
      dbscan_min_pts = 4,
      max_var_tight_cluster = 150,
      max_prop_single_tight_cluster = 0.6,
      safe_num_clusters = 3,
      safe_twcv = 250,
      complete_graphemes_only = TRUE,
      symbol_filter = NULL
    ) {
    "
    Checks if this participant's data are valid based on passed arguments.
    This method aims to identify participants who had too few responses or
    varied their response colors too little, by marking them as invalid.
    Note that there are no absolutely correct values, as what is 'too little
    variation' is highly subjective. You might need to tweak parameters to be
    in line with your project's criteria, especially if you use another color
    space than CIELUV, since the default values are based on what seems
    to make sense in a CIELUV context. If you use the results in a
    research article, make sure to reference synr and specify what parameter
    values you passed to the function.

    This method relies heavily on the DBSCAN algorithm and the package
    'dbscan', and involves calculating a synr-specific 'Total Within-Cluster
    Variance' (TWCV) score. You can find more information, and
    what the parameters here mean, in
    the documentation for the function \\code{validate_get_twcv}.
    \\subsection{Parameters}{
      \\itemize{
        \\item{\\code{min_complete_graphemes} The minimum number of graphemes
          with complete (all non-NA color) responses that the participant data
          must have for them to not be categorized as invalid based on this
          criterion. Defaults to 5.
        }
        \\item{\\code{dbscan_eps} Radius of 'epsilon neighborhood' when applying
          DBSCAN clustering. Defaults to 20.
        }
        \\item{\\code{dbscan_min_pts} Minimum number of points required in the
          epsilon neighborhood for core points (including the core point
          itself). Defaults to 4.
        }
        \\item{\\code{max_var_tight_cluster} Maximum variance for an identified
          DBSCAN cluster to be considered 'tight-knit'. Defaults to 150.
        }
        \\item{\\code{max_prop_single_tight_cluster} Maximum proportion of
          points allowed to be within a single 'tight-knit' cluster (exceeding
          this leads to classification as invalid). Defaults to 0.6.
        }
        \\item{\\code{safe_num_clusters} Minimum number of identified DBSCAN
          clusters (including 'noise' cluster only if it consists of at least
          'dbscan_min_pts' points) that guarantees validity if
          points are 'non-tight-knit'. Defaults to 3.
        }
        \\item{\\code{safe_twcv} Minimum total within-cluster variance (TWCV)
          score that guarantees validity if points are 'non-tight-knit'.
          Defaults to 250.
        }
        \\item{\\code{complete_graphemes_only} A logical vector. If TRUE, 
          only data from graphemes that have all non-NA color responses
          are used; if FALSE, even data from graphemes with some NA color
          responses are used. Defaults to TRUE.
        }
        \\item{\\code{symbol_filter} A character vector (or NULL) that specifies
          which graphemes' data to use. Defaults to NULL, meaning data from
          all of the participant's graphemes will be used.
        }
      }
    }

    \\subsection{Returns}{
      A list with components
      \\itemize{
        \\item{\\code{valid} TRUE if categorized as valid, otherwise FALSE.}
        \\item{\\code{reason_invalid} One-element character vector describing
          why participant's data were deemed invalid, or empty string if
          valid is TRUE.
        }
        \\item{\\code{twcv} One-element numeric (or NA if there are no/too few
          graphemes with complete responses) vector indicating participant's
          calculated TWCV.
        }
        \\item{\\code{num_clusters} One-element numeric (or NA if there are no/too few
          graphemes with complete responses) vector indicating
          the number of identified clusters counting toward the
          tally compared with 'safe_num_clusters'.
        }
      }
    }
    "
      if (!has_graphemes()) {
        return(list(
          valid = FALSE,
          reason_invalid = "no_color_responses",
          twcv = NA,
          num_clusters = NA
        ))
      }

      num_allcolored <- get_number_all_colored_graphemes(
        symbol_filter = symbol_filter
      )
      if (num_allcolored < min_complete_graphemes) {
        return(list(
          valid = FALSE,
          reason_invalid = "too_few_graphemes_with_complete_responses",
          twcv = NA,
          num_clusters = NA
        ))
      }

      if(complete_graphemes_only) {
        symbol_filter <- get_all_colored_symbols(symbol_filter = symbol_filter)
      }

      color_matrix <- get_nonna_color_resp_mat(
        symbol_filter = symbol_filter
      )
      res_val_list <- validate_get_twcv(
        color_matrix = color_matrix,
        dbscan_eps = dbscan_eps,
        dbscan_min_pts = dbscan_min_pts,
        max_var_tight_cluster = max_var_tight_cluster,
        max_prop_single_tight_cluster = max_prop_single_tight_cluster,
        safe_num_clusters = safe_num_clusters,
        safe_twcv = safe_twcv
      )
      return(res_val_list)
    }
  )
)
