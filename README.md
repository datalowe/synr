# synr
[![Build Status](https://app.travis-ci.com/datalowe/synr.svg?branch=master)](https://app.travis-ci.com/datalowe/synr)

This is an [R](https://www.r-project.org/) package for working with data resulting from [grapheme-color synesthesia](https://en.wikipedia.org/wiki/Grapheme%E2%80%93color_synesthesia)-related [consistency tests](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4118597/#S4title). synr provides tools for exploring test data, including visualizing single participant's data, and applying summarizing functions such as calculating [color variation/consistency scores](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4118597/#S4title) or classifying participant data as valid or invalid.

## Installation
TODO: This section will be updated once it's been decided where to make synr binaries available. 

## Usage
Once data are in an appropriately formatted data frame/tibble, everything starts with rolling up participant data into a 'ParticipantGroup' object with `create_participantgroup`:

```r
library(synr)

pgroup <- create_participantgroup(
    formatted_df, # data frame/tibble to use
    n_trials_per_grapheme=3, # number of trials that grapheme was used for
    participant_col_name="participant_id", # name of column which holds participant ID's
    symbol_col_regex='symbol', # regex pattern matching columns which hold grapheme symbol strings
    color_col_regex='color', # regex pattern matching columns which hold response color HEX codes
    color_space_spec = "Luv" # color space to use for all calculations with participant group
)
```
Using the resulting object (`pgroup`), you can call various methods. A couple of examples follow. 

### Example group-level method: get_mean_consistency_scores
`pgroup$get_mean_consistency_scores(symbol_filter=LETTERS)` would return a vector of [CIELUV](https://en.wikipedia.org/wiki/CIELUV)-based consistency scores, using only data from trials involving capital letters.

### Example participant-level method: get_mean_consistency_scores
`pgroup$participants[[1]]$get_plot(symbol_filter=LETTERS)` would produce a bar plot of per-grapheme consistency scores for a single participant, using only data from trials involving capital letters. You can see an example below.

<img src="man/figures/example_consistency_plot.png" width="400" alt="Example bar plot of grapheme-level consistency scores">

### Detailed usage information
More details on required data format and how to use synr itself can be found in the [tutorial vignette](https://datalowe.github.io/synr/articles/synr-tutorial.html), which is also included in the package itself (run `help(synr)` to find it).
