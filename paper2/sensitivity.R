source('shared.R')


# ARGUMENTS
# ==================
args <-
  parseArgs(list(
                 make_option(
                             c('-i', '--input'),
                             default =NULL,
                             help    ='dataset file path',
                             metavar ='file'),
                 make_option(
                             c('-o', '--output'),
                             default ='.',
                             help    ='output directory [default= %default]',
                             metavar ='folder'),
                 make_option(
                             c('-f', '--format'),
                             default ='pdf',
                             help    ='output file format [default= %default]',
                             metavar ='string'),
                 make_option(
                             c('-c', '--cache'),
                             default = '.',
                             help    = 'cache folder [default= %default]',
                             metavar = 'folder'),
                 make_option(
                             c('-s', '--stream'),
                             default = TRUE,
                             help    = 'read input line by line (only suitable for large files) [default= %default]',
                             metavar = 'boolean')
                 ),
            function(args) !is.null(args$input))






# BIN CONFIG
# ============================================
binSize.cash <- 500
binSize.cost <- 10
binSize.prob <- 0.015
binSize.rate <- 0.02




# ============================================
# ALT 1: DDPLY FILE
# ============================================
cached <- NULL
ddplyFile <- function(col) {
  if (is.null(cached)) {
    cached <- load(args$input, c(
                                'PROJ',
                                'proj_state',
                                'intervention',
                                'interventions_tot_size',
                                'grants_avg_frac',
                                'proj_tot_cash',
                                'proj_tot_cost',
                                'proj_tot_prob',
                                'inv_rate',
                                'orgs_infcap_thresh'
                                ))

    print('Rename columns')
    names(cached)[names(cached) == 'interventions_tot_size'] <- 'intervention_size'
    names(cached)[names(cached) == 'grants_avg_frac']        <- 'grants'

    print('Bin interaction parameters')
    cached$proj_tot_cash <- roundToNearest(cached$proj_tot_cash, binSize.cash)
    cached$proj_tot_cost <- roundToNearest(cached$proj_tot_cost, binSize.cost)
    cached$proj_tot_prob <- roundToNearest(cached$proj_tot_prob, binSize.prob)
    cached$inv_rate      <- roundToNearest(cached$inv_rate,      binSize.rate)

    cached <<- cached
  }


  print('Compute likelihood')
  df <- cached
  df <- ddply(df, c('intervention', 'intervention_size', 'grants', col), summarise,
              pois   = countCompletes(proj_state),
              projs  = length(unique(PROJ)))


  print(paste('Rename column', col, 'to x'))
  names(df)[names(df) == col] <- 'x'

  df
}


# ============================================
# ALT 2: STREAM FILE
# ============================================
numLines <- NULL
streamFile <- function(col, binSize) {

  if (is.null(numLines)) {
    print('Counting number of lines of input file')
    numLines <<- nrow(fread(args$input, select = 1L))
  }

  print('Begin streaming file...')
  stop = FALSE
  f = file(args$input, "r")
  lineNum = 1
  header = c()

  projsDone <- c()
  projsAll  <- c()


  resultColumns = c(
                  'RUN',
                  'PROJ',
                  'proj_state',
                  'intervention',
                  'interventions_tot_size',
                  'grants_avg_frac',
                  'x',
                  'pois',
                  'projs'
                  )
  result        <- data.frame(matrix(ncol=length(resultColumns), nrow=0))
  names(result) <- resultColumns

  while(!stop) {

    if (lineNum == 1) {

      # Save header if first line
      header = unlist(strsplit(readLines(f, n = 1), ','))

    } else {

      # Read next line
      next_line = unlist(strsplit(readLines(f, n = 1), ','))

      # Convert row to df
      df <- data.frame(matrix(ncol=length(header), nrow=0))
      names(df) <- header
      df[1,] <- next_line

      # Rename col in question to x
      names(df)[names(df) == col] <- 'x'

      # Keep only relevant columns
      df <- df[c(
                 'PROJ',
                 'proj_state',
                 'intervention',
                 'interventions_tot_size',
                 'grants_avg_frac',
                 'x'
                 )]

      # Extract info about row and then drop columns
      isDone = df$proj_state == 'COMPLETED'
      projID = df$PROJ
      df <- df[, !(names(df) %in% c('PROJ', 'proj_state'))]

      # Bin and coerce datatype
      if (is.null(binSize)) {
        df$x <- as.numeric(df$x)
      } else {
        df$x <- roundToNearest(as.numeric(df$x), binSize)
      }

      # If the project is completed and we have not seen it before as completed
      if (isDone & !(projID %in% projsDone)) {

        # Add it to the list of seen projects
        projsDone <- c(projsDone, projID)

        # Check if there's an entry matching the parameter combination
        if(nrow(subset(result,
                       (result$intervention == df$intervention &
                        result$interventions_tot_size == df$interventions_tot_size &
                        result$grants_avg_frac == df$grants_avg_frac &
                        result$x == df$x))) > 0) {
          # If so then edit
          result$pois <- ifelse(
                                (result$intervention == df$intervention &
                                 result$interventions_tot_size == df$interventions_tot_size &
                                 result$grants_avg_frac == df$grants_avg_frac &
                                 result$x == df$x),
                                result$pois + 1,
                                result$pois)
        } else {
          # Else add
          df$pois     <- 1
          df$projs    <- 0
          result <- rbind(result, df)
        }

      }

      # If the project is not completed and we have not seen it before
      else if (!(projID %in% projsAll)) {

        # Add it to the list of seen projects
        projsAll <- c(projsAll, projID)

        # Check if there's an entry matching the parameter combination
        if(nrow(subset(result, (result$intervention == df$intervention &
                                result$interventions_tot_size == df$interventions_tot_size &
                                result$grants_avg_frac == df$grants_avg_frac &
                                result$x == df$x))) > 0) {
          result$projs <- ifelse((result$intervention == df$intervention &
                                  result$interventions_tot_size == df$interventions_tot_size &
                                  result$grants_avg_frac == df$grants_avg_frac &
                                  result$x == df$x),
                                 result$projs + 1,
                                 result$projs)
        } else {
          df$pois     <- 0
          df$projs    <- 1
          result <- rbind(result, df)
        }

      }

      # Stop reading when there are no more lines
      if(length(next_line) == 0) {
        stop = TRUE
        close(f)
      }
    }

    # Increment number of lines read
    lineNum = lineNum + 1

    # Print % read every now and then
    if (lineNum %% 10000 == 0) {
      gc()
      print(paste(round(lineNum / numLines, 2),
                  '% streamed and converted',
                  ' (', lineNum, '/', numLines, ')',
                  ' (N=', nrow(result), ')',
                  ' (', Sys.time(), ')',
                  sep=''))
    }
  }
  print('Completed streaming file')

  print('Renaming columns')
  names(result)[names(result) == 'interventions_tot_size'] <- 'intervention_size'
  names(result)[names(result) == 'grants_avg_frac']        <- 'grants'

  return(result)
}









# ============================================
# PREPARE
# ============================================
prepare <- function(col, binSize=NULL) {
  if (args$stream) {
    function() {
      streamFile(col, binSize)
    }
  } else {
    function() {
      ddplyFile(col)
    }
  }
}



df.cash   <- simpleGetSet(args$cache, 'sensitivity-cash.csv',   prepare('proj_tot_cash', binSize.cash))
df.cost   <- simpleGetSet(args$cache, 'sensitivity-cost.csv',   prepare('proj_tot_cost', binSize.cost))
df.prob   <- simpleGetSet(args$cache, 'sensitivity-prob.csv',   prepare('proj_tot_prob', binSize.prob))
df.rate   <- simpleGetSet(args$cache, 'sensitivity-rate.csv',   prepare('inv_rate',      binSize.rate))
df.thresh <- simpleGetSet(args$cache, 'sensitivity-thresh.csv', prepare('orgs_infcap_thresh'))












# OUTPUT PATH MAKER
# ============================================
makePath <- function(name) {
  paste(args$output, '/', name, '.', args$format, sep='')
}


# MEAN FINDER (PLOT PREPARER)
# ============================================
buildFindMeans <- function(interaction, f=NULL) {
  function(df) {
    print('Recomputing means')
    cols <- c('x') # default in case of no interaction

    if (!is.null(f)) {
      print('Applying function')
      print(f)
      df <- f(df)
    }

    if (!is.null(interaction)) {
      names(df)[names(df) == interaction] <- 'interaction'
      cols <- c('x', 'interaction')
    }
    df <- ddply(df, cols, summarise,
                pois  = sum(pois),
                projs = sum(projs))
    df$pois <- df$pois / df$projs * 100
    df
  }
}


# PLOTTING CALLER
# ============================================
doPlot <- function(name, df, context, binSize, xlab, xIsPercentage=FALSE, palette=NULL, legend=NULL) {
  plotToFile(makePath(name))
  plotSensitivity(df=df, context=df, binSize=binSize, xlab=xlab, xIsPercentage=xIsPercentage, palette=palette, legend=legend)

  plotToFile(makePath(paste('ylim-', name, sep='')))
  plotSensitivity(df=df, context=context, binSize=binSize, xlab=xlab, xIsPercentage=xIsPercentage, palette=palette, legend=legend)
}


# PLOTTING FUNCTION
# ============================================
plotSensitivity <- function(
                            df      = NULL,
                            x       = NULL,
                            context = NULL,
                            xlab    = NULL,
                            binSize = NULL,
                            xIsPercentage = FALSE,
                            palette = NULL,
                            legend  = NULL
                            ) {
  # Convinience
  interventions  <- unique(df$interaction)
  hasInteraction <- 'interaction' %in% names(df)

  # Prepare colors
  if (hasInteraction) {
    colors   <- colorRampPalette(c('red', 'orange', 'green'))(length(interventions))
    if (!is.null(palette)) {
      colors <- colorRampPalette(palette)(length(interventions))
    }
    df$color <- colors[as.numeric(as.factor(df$interaction))]
  }

  if (xIsPercentage) {
    df$x <- df$x * 100
    binSize = binSize * 100
  }

  if (!is.null(binSize)) {
    xlab = paste(xlab, '( \u00B1', binSize / 2, ')')
  }

  # Plot
  plot(
       df$pois ~ df$x,
       col  = df$color,
       #col  = 'white',
       pch  = 19,
       yaxt = 'n',
       xaxt = 'n',
       xlab = xlab,
       ylab = 'Likelihood of Market Entry (%)',
       ylim = c(min(context$pois), max(context$pois))
       )

  # Axes and tick marks
  if (!is.null(binSize)) {
    xticks <- seq(round(min(df$x)), round(max(df$x)), by = binSize)
  } else {
    xticks <- sort(unique(df$x))
  }

  # Brute force find a good resolution for the y axis
  yticks <- seq(round(min(context$pois)), round(max(context$pois)), by = 1)
  minAmountOfYTicks = 6
  if (length(yticks) < minAmountOfYTicks) {
    yticks <- seq(round(min(context$pois)), round(max(context$pois)), by = 0.5)
  }
  if (length(yticks) < minAmountOfYTicks) {
    yticks <- seq(round(min(context$pois)), round(max(context$pois)), by = 0.25)
  }
  if (length(yticks) < minAmountOfYTicks) {
    yticks <- seq(round(min(context$pois)), round(max(context$pois)), by = 0.1)
  }

  # Print tick marks
  axis(side=1, col='black', las = 2, at = xticks)
  axis(side=2, col='black', las = 2, at = yticks)

  # Grid
  abline(h=yticks, v=xticks, col="gray", lty=3)

  # Lines
  if (hasInteraction) {
    for (bin in interventions) {
      sub <- subset(df, df$interaction == bin)
      lines(sub$pois ~ sub$x, col = sub$color)
    }
  } else {
    lines(df$pois ~ df$x)
  }

  # Legend
  if (hasInteraction) {
    pos = if(!is.null(legend)) legend else 'bottomright'
    legend(pos, levels(factor(df$interaction)),
           pch = 19,
           col = colors,
           bty = 'n',
           cex = 1)
  }
}







# ============================================
# PLOTS
# ============================================
gc()







print('Subsetting away extreme values')
df.cash   <- subset(df.cash, df.cash$x <= 4000)
df.cost   <- subset(df.cost, df.cost$x >= 190 & df.cost$x <= 340)
df.prob   <- subset(df.prob, df.prob$x <= 0.15)





#
# PULL
# =========================================================
print('### PULL ###')
findMeans <- buildFindMeans('intervention_size')

print('Prepare FD+PD subsets for plotting')
df.both.cash   <- findMeans(df.cash)
df.both.cost   <- findMeans(df.cost)
df.both.prob   <- findMeans(df.prob)
df.both.rate   <- findMeans(df.rate)
df.both.thresh <- findMeans(df.thresh)

print('Prepare FD subsets for plotting')
df.fd.cash   <- findMeans(subset(df.cash,   df.cash$intervention == 'FDMER'))
df.fd.cost   <- findMeans(subset(df.cost,   df.cost$intervention == 'FDMER'))
df.fd.prob   <- findMeans(subset(df.prob,   df.prob$intervention == 'FDMER'))
df.fd.rate   <- findMeans(subset(df.rate,   df.rate$intervention == 'FDMER'))
df.fd.thresh <- findMeans(subset(df.thresh, df.thresh$intervention == 'FDMER'))

print('Prepare PD subsets for plotting')
df.pd.cash   <- findMeans(subset(df.cash,   df.cash$intervention == 'PDMER'))
df.pd.cost   <- findMeans(subset(df.cost,   df.cost$intervention == 'PDMER'))
df.pd.prob   <- findMeans(subset(df.prob,   df.prob$intervention == 'PDMER'))
df.pd.rate   <- findMeans(subset(df.rate,   df.rate$intervention == 'PDMER'))
df.pd.thresh <- findMeans(subset(df.thresh, df.thresh$intervention == 'PDMER'))

print('Collate subsets for plotting context')
df.all <- rbind(
                df.both.cash,
                df.both.cost,
                df.both.prob,
                df.both.rate,
                df.both.thresh,

                df.fd.cash,
                df.fd.cost,
                df.fd.prob,
                df.fd.rate,
                df.fd.thresh,

                df.pd.cash,
                df.pd.cost,
                df.pd.prob,
                df.pd.rate,
                df.pd.thresh
                )


# Plot pull interaction
doPlot('pull-cash',         df.both.cash,    df.all,  binSize.cash,  'Total Projected Revenues (mUSD)')
doPlot('pull-cost',         df.both.cost,    df.all,  binSize.cost,  'Total Projected Costs (mUSD)')
doPlot('pull-prob',         df.both.prob,    df.all,  binSize.prob,  'Total Projected Probability of Success (%)',   TRUE)
doPlot('pull-rate',         df.both.rate,    df.all,  binSize.rate,  'Venture Capital Discount Rate (%)',            TRUE)
doPlot('pull-thresh',       df.both.thresh,  df.all,  NULL,          'Big Pharma Threshold (mUSD)')
doPlot('pull-fd-cash',      df.fd.cash,      df.all,  binSize.cash,  'Total Projected Revenues (mUSD)')
doPlot('pull-pd-cash',      df.pd.cash,      df.all,  binSize.cash,  'Total Projected Revenues (mUSD)')
doPlot('pull-fd-cost',      df.fd.cost,      df.all,  binSize.cost,  'Total Projected Costs (mUSD)')
doPlot('pull-pd-cost',      df.pd.cost,      df.all,  binSize.cost,  'Total Projected Costs (mUSD)')
doPlot('pull-fd-prob',      df.fd.prob,      df.all,  binSize.prob,  'Total Projected Probability of Successs (%)',  TRUE)
doPlot('pull-pd-prob',      df.pd.prob,      df.all,  binSize.prob,  'Total Projected Probability of Successs (%)',  TRUE)
doPlot('pull-fd-rate',      df.fd.rate,      df.all,  binSize.rate,  'Venture Capital Discount Rate (%)',            TRUE)
doPlot('pull-pd-rate',      df.pd.rate,      df.all,  binSize.rate,  'Venture Capital Discount Rate (%)',            TRUE)
doPlot('pull-fd-thresh',    df.fd.thresh,    df.all,  NULL,          'Big Pharma Threshold (mUSD)')
doPlot('pull-pd-thresh',    df.pd.thresh,    df.all,  NULL,          'Big Pharma Threshold (mUSD)')






#
# PUSH
# =========================================================
print('### PUSH ###')
findMeans <- buildFindMeans('grants', function(df) { subset(df, df$intervention_size == 0) })

print('Prepare push subsets for plotting')
df.both.cash   <- findMeans(df.cash)
df.both.cost   <- findMeans(df.cost)
df.both.prob   <- findMeans(df.prob)
df.both.rate   <- findMeans(df.rate)
df.both.thresh <- findMeans(df.thresh)

df.all <- rbind(
                df.both.cash,
                df.both.cost,
                df.both.prob,
                df.both.rate,
                df.both.thresh
                )

# Plot push interaction
doPlot('push-cash',    df.both.cash,    df.all,  binSize.cash,  'Total Projected Revenues (mUSD)')
doPlot('push-cost',    df.both.cost,    df.all,  binSize.cost,  'Total Projected Costs (mUSD)')
doPlot('push-prob',    df.both.prob,    df.all,  binSize.prob,  'Total Projected Probability of Success (%)', TRUE)
doPlot('push-rate',    df.both.rate,    df.all,  binSize.rate,  'Venture Capital Discount Rate (%)', TRUE)
doPlot('push-thresh',  df.both.thresh,  df.all,  NULL,          'Big Pharma Threshold (mUSD)')




#
# NO INTERACTION
# =========================================================
print('### NO INTERACTION ###')
findMeans <- buildFindMeans(NULL, function(df) { subset(df, df$intervention_size == 0) })

print('Prepare push subsets for plotting')
df.both.cash   <- findMeans(df.cash)
df.both.cost   <- findMeans(df.cost)
df.both.prob   <- findMeans(df.prob)
df.both.rate   <- findMeans(df.rate)
df.both.thresh <- findMeans(df.thresh)

df.all <- rbind(
                df.both.cash,
                df.both.cost,
                df.both.prob,
                df.both.rate,
                df.both.thresh
                )

# Plot push interaction
doPlot('all-cash',   df.both.cash,   df.all, binSize.cash, 'Total Projected Revenues (mUSD)')
doPlot('all-cost',   df.both.cost,   df.all, binSize.cost, 'Total Projected Costs (mUSD)')
doPlot('all-prob',   df.both.prob,   df.all, binSize.prob, 'Total Projected Probability of Success (%)', TRUE)
doPlot('all-rate',   df.both.rate,   df.all, binSize.rate, 'Venture Capital Discount Rate (%)', TRUE)
doPlot('all-thresh', df.both.thresh, df.all, NULL,         'Big Pharma Threshold (mUSD)')




#
# NO INTERACTION ALL AT ONCE
# =========================================================
print('### PARAMETER UNDER CONSIDERATION AS INTERACTION, ALL AT ONCE ###')
findMeans <- buildFindMeans('interaction', function(df) { subset(df, df$intervention_size == 0) })

normalize <- function(df, group) {
  mi = min(df$x)
  ma = max(df$x)
  df$x <- (df$x - mi) / (ma - mi)
  df$interaction <- group
  df
}

print('Prepare push subsets for plotting')
df.normalized <- rbind(
                       findMeans(normalize(df.prob,   'Probability')),
                       findMeans(normalize(df.cash,   'Revenue')),
                       findMeans(normalize(df.cost,   'Cost')),
                       findMeans(normalize(df.rate,   'Discount Rate')),
                       findMeans(normalize(df.thresh, 'Threshold'))
                       )

# Plot push interaction
clrs = c(
         'chartreuse2',
         'gold',
         'coral1',
         'bisque4',
         'cornflowerblue'
         )
doPlot('all', df.normalized, df.normalized, 0.05, 'Input Parameter (%)', TRUE, palette=clrs, legend='topleft')
