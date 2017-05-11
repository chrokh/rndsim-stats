library(data.table)
library(plyr)
library('optparse')



# ==================
# Data frame transformations
# ==================


groupBy <- function(df, cols) {
  print('Adding column(s)')
  df$proj_is_at_poi_n <- ifelse(df$proj_is_at_poi == 'true', 1, 0)

  print('Grouping')
  return(ddply(df, cols, summarise,
              mean_tick                     = mean(TICK),
              mean_interventions_tot_size   = mean(interventions_tot_size),
              tot_pois                      = sum(proj_is_at_poi_n),
              max_pois                      = max(proj_is_at_poi_n),
              mean_pois                     = mean(proj_is_at_poi_n),
              min_pois                      = min(proj_is_at_poi_n),
              tot_proj_capital_accumulated  = sum(proj_capital_accumulated),
              max_proj_capital_accumulated  = max(proj_capital_accumulated),
              mean_proj_capital_accumulated = mean(proj_capital_accumulated),
              min_proj_capital_accumulated  = min(proj_capital_accumulated)
              ))
}


only_reached_market <- function(d) {
  projs <- subset(d, d$proj_is_at_poi == 'true')
  projs <- c(projs$PROJ)
  return(subset(d, d$PROJ %in% projs))
}


binInterventions <- function(df, bins) {
  # Drop if outside of bin edges
  df <- subset(df, df$interventions_tot_size >= min(intervention_bins) & df$interventions_tot_size <= max(intervention_bins))

  # Bin
  smallNumber  <- 0.00001
  expandedBins <- bins
  expandedBins[1] <- expandedBins[1] + smallNumber
  expandedBins[length(expandedBins)] <- expandedBins[length(expandedBins)] + smallNumber
  df$intervention_bin      <- findInterval(df$interventions_tot_size, expandedBins)
  df$intervention_bin_top  <- bins[df$intervention_bin + 1]

  return(df)
}


dropPartialRuns <- function(df) {
  # TODO: Actually drops last, but should drop partials
  top <- max(df$RUN)
  return(subset(df, df$RUN != top))
}




# ==================
# Script helpers
# ==================

parseArgs <- function(options, validate) {
  parser = OptionParser(option_list = options)
  args   = parse_args(parser)

  if (!validate(args)) {
    print_help(parser)
    stop('Missing or bad arguments')
  }

  return(args)
}


load <- function(path, cols = NULL) {
  if (is.null(cols)) {
    df <- fread(path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  } else {
    df <- fread(path, sep = ",", header = TRUE, stringsAsFactors = FALSE, select = cols)
  }

  return(df)
}

getSet <- function(input, cache_folder, cache_name, fun, cols = NULL) {
  # Set max memory limit
  memory.size(max=TRUE)
  memory.limit(size=17000000000000)

  path <- file.path(cache_folder, cache_name)
  if (file.exists(path)) {
    print('Reading cache')
    df <- load(path)
  } else {
    print('Reading file')
    df <- load(input, cols)

    print('Cleaning file')
    df <- dropPartialRuns(df)

    print('Applying pre-processing')
    df <- fun(df)

    print('Writing to cache')
    write.csv(file = path, x = df)
  }
  print('Got data')
  return(df)
}

plotToFile <- function(path) {
  pdf(path, width = 9, height = 7)
}

buildLim <- function(lo, hi) {
  if (is.null(hi)) {
    return(NULL)
  } else {
    return(c(0, round(as.numeric(hi))))
  }
}
