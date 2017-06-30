library(data.table)
library(plyr)
library('optparse')




# ==================
# Data frame transformations
# ==================

# Can be used as function in ddply
countCompletes <- function(state) {
  sum(ifelse(state == 'COMPLETED', 1, 0))
}
meanCompletes <- function(state) {
  mean(ifelse(state == 'COMPLETED', 1, 0))
}
countEntries <- function(proj_is_at_poi) {
  sum(ifelse(proj_is_at_poi == 'true', 1, 0))
}

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


only_no_grants <- function(df) {
  print(nrow(df))
  df <- subset(df, df$grants_preclinical_frac == 0 | df$grants_preclinical_size == 0)
  print(nrow(df))
  df <- subset(df, df$grants_phase1_frac == 0 | df$grants_phase1_size == 0)
  print(nrow(df))
  df <- subset(df, df$grants_phase2_frac == 0 | df$grants_phase2_size == 0)
  print(nrow(df))
  df <- subset(df, df$grants_phase3_frac == 0 | df$grants_phase3_size == 0)
  print(nrow(df))
  df <- subset(df, df$grants_approval_frac == 0 | df$grants_approval_size == 0)
  print(nrow(df))
  return(df)
}

only_reached_market <- function(d) {
  projs <- subset(d, d$proj_is_at_poi == 'true')
  projs <- unique(projs$PROJ)
  return(subset(d, d$PROJ %in% projs))
}

only_did_not_reach_market <- function(d) {
  projs <- subset(d, d$proj_is_at_poi == 'true')
  projs <- unique(projs$PROJ)
  return(subset(d, (!d$PROJ %in% projs)))
}

only_before_market <- function(df) {
  return(subset(df, !grepl('Market', df$proj_stage_group)))
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
  if('RUN' %in% colnames(df)) {
    print('WARN: I don\'t know how to drop partial runs... Dropping last run at least.')
    top <- max(df$RUN)
    return(subset(df, df$RUN != top))
  } else {
    print('Did not load column RUN. Cannot drop partial runs.')
    return(df)
  }
}


factorizeStages <- function(stages) {
  return(factor(stages, levels = c(
    'PreClinical',
    'Phase1',
    'Phase2',
    'Phase3',
    'Approval',
    'MarketEntry',
    'MarketUptake',
    'MarketPeak'
  )))
}

simplifyStages <- function(stages) {
  return(revalue(stages,
                 c(
                   'PreClinical'  = 'PC',
                   'Phase1'       = 'P1',
                   'Phase2'       = 'P2',
                   'Phase3'       = 'P3',
                   'Approval'     = 'Ap',
                   'MarketEntry'  = 'M1',
                   'MarketUptake' = 'M2-10',
                   'MarketPeak'   = 'M10+'
                   )))
}

factorizeDevStages <- function(stages) {
  return(factor(stages, levels = c(
    'PreClinical',
    'Phase1',
    'Phase2',
    'Phase3',
    'Approval'
  )))
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
  print(paste('Reading file', path))
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
  if (is.null(cache_folder)) {
    path <- cache_name
  }

  if (file.exists(path)) {
    print(paste('Reading cache'))
    df <- load(path)
  } else {
    df <- load(input, cols)

    print('Dropping partial runs')
    df <- dropPartialRuns(df)

    print('Applying pre-processing')
    df <- fun(df)

    print('Writing to cache')
    ensurePathExists(path)
    write.csv(file = path, x = df)
  }
  print('Got data')
  return(df)
}

ensurePathExists <- function(path) {
  if (dir.exists(dirname(path))) {
    print(paste('Directory', dirname(path), 'exists'))
  } else {
    print(paste('Creating Directory', dirname(path)))
    dir.create(dirname(path))
  }
}

plotToFile <- function(path, a4like = FALSE) {
  ensurePathExists(path)
  print(paste('Will plot to', path))
  if (grepl('\\.pdf', path)) {
    if (a4like) {
      pdf(path, width = 9, height = 12)
    } else {
      pdf(path, width = 9, height = 7)
    }
  } else {
    if (a4like) {
      png(path, width = 2480,
          height = 3508,
          units = 'px',
          res = 300
          )
    } else {
      png(path, width = 2400,
          height = 1800,
          units = 'px',
          res = 220
          )
    }
  }
}

plotToPortrait <- function(path) {
  ensurePathExists(path)
  print(paste('Will plot to', path))
  if (grepl('\\.pdf', path)) {
    pdf(path, width = 9, height = 12)
  } else {
    png(path,
        width = 2480,
        height = 3508,
        units = 'px',
        res = 300
        )
  }
}

plotToLandscape <- function(path) {
  ensurePathExists(path)
  print(paste('Will plot to', path))
  if (grepl('\\.pdf', path)) {
    pdf(path, height = 9, width = 12)
  } else {
    png(path,
        height = 2480,
        width = 3508,
        units = 'px',
        res = 300
        )
  }
}

buildLim <- function(lo, hi) {
  if (is.null(hi)) {
    return(NULL)
  } else {
    return(c(0, round(as.numeric(hi))))
  }
}

roundToNearest <- function(x, base){
  return(base * round(x / base))
}
