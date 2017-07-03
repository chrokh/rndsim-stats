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
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))






# BIN CONFIG
# ============================================
binSize.cash <- 500
binSize.cost <- 10
binSize.prob <- 0.015
binSize.rate <- 0.02





# ONLY READ FILE ONCE BY CALLING THIS FUNCTION
# ============================================
loadIfNull <- function(df) {
  if (is.null(df)) {
    print('Does not have file in memory. Reading.')
    df <- load(args$input,
               c('RUN',
                 'PROJ',
                 'intervention',
                 'interventions_tot_size',
                 'proj_state',
                 'proj_tot_cash',
                 'proj_tot_cost',
                 'proj_tot_prob',
                 'inv_rate',
                 'orgs_infcap_thresh'
                 ))

    print('Rename parameters')
    names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'

    df
  } else {
    print('Has file in memory. Returning')
    df
  }
}
df <- NULL






# PLOTTING FUNCTION
# ============================================
plotSensitivity <- function(
                            df      = NULL,
                            x       = NULL,
                            context = NULL,
                            xlab    = NULL,
                            binSize = NULL,
                            xIsPercentage = FALSE
                            ) {

  # Convinience
  interventions  <- sort(unique(df$intervention_size))

  # Prepare colors
  colors   <- colorRampPalette(c('red', 'orange', 'green'))(length(interventions))
  df$color <- colors[as.numeric(as.factor(df$intervention_size))]

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
       ylab = 'Mean Likelihood of Market Entry (%)',
       ylim = c(min(context$pois), max(context$pois))
       )

  # Axes and tick marks
  if (!is.null(binSize)) {
    xticks <- seq(round(min(df$x)), round(max(df$x)), by = binSize)
  } else {
    xticks <- sort(unique(df$x))
  }
  yticks <- seq(round(min(context$pois)), round(max(context$pois)), by = 1)
  axis(side=1, col='black', las = 2, at = xticks)
  axis(side=2, col='black', las = 2, at = yticks)

  # Grid
  abline(h=yticks, v=xticks, col="gray", lty=3)

  # Lines
  for (bin in interventions) {
    sub <- subset(df, df$intervention_size == bin)
    lines(sub$pois ~ sub$x, col = sub$color)
  }

  # Legend
  legend('bottomright', levels(factor(df$intervention_size)),
         pch = 19, col = df$color, bty = 'n', title='MER',
         cex = 1)
}







# ============================================
# READ OR WRITE RESTRUCTURED DATA FROM CACHE
# ============================================
prepare <- function(col, binSize=NULL) {
  function() {
    print(paste('Preparing', col))
    gc()
    df <<- loadIfNull(df)
    gc()

    print('Round x variable')
    if (!is.null(binSize)) {
      df$x <- roundToNearest(df[[col]], binSize)
    } else {
      df$x <- df[[col]]
    }
    gc()

    print('Counting entries per group')
    ddply(df, c('intervention', 'intervention_size', 'x'), summarise,
          pois   = countCompletes(proj_state),
          projs  = length(unique(PROJ)))
  }
}

df.cash   <- simpleGetSet(args$cache, 'sensitivity-cash.csv',   prepare('proj_tot_cash',      binSize.cash))
df.cost   <- simpleGetSet(args$cache, 'sensitivity-cost.csv',   prepare('proj_tot_cost',      binSize.cost))
df.prob   <- simpleGetSet(args$cache, 'sensitivity-prob.csv',   prepare('proj_tot_prob',      binSize.prob))
df.rate   <- simpleGetSet(args$cache, 'sensitivity-rate.csv',   prepare('inv_rate',           binSize.rate))
df.thresh <- simpleGetSet(args$cache, 'sensitivity-thresh.csv', prepare('orgs_infcap_thresh', NULL))





# ============================================
# PLOTS
# ============================================
gc()

# Output path making function
makePath <- function(name) { paste(args$output, '/', name, '.', args$format, sep='') }

# Mean finding function
findMeans <- function(df) {
  print('Recomputing means')
  df <- ddply(df, c('x', 'intervention_size'), summarise,
               pois  = sum(pois),
               projs = sum(projs))
  df$pois <- df$pois / df$projs * 100
  df
}


print('Subsetting away extreme values')
df.cash   <- subset(df.cash, df.cash$x <= 4000)
df.cost   <- subset(df.cost, df.cost$x >= 190 & df.cost$x <= 340)
df.prob   <- subset(df.prob, df.prob$x <= 0.15)


print('Prepare subsets for plotting')
df.both.cash   <- findMeans(df.cash)
df.both.cost   <- findMeans(df.cost)
df.both.prob   <- findMeans(df.prob)
df.both.rate   <- findMeans(df.rate)
df.both.thresh <- findMeans(df.thresh)

df.fd.cash   <- findMeans(subset(df.cash,   df.cash$intervention == 'FDMER'))
df.fd.cost   <- findMeans(subset(df.cost,   df.cost$intervention == 'FDMER'))
df.fd.prob   <- findMeans(subset(df.prob,   df.prob$intervention == 'FDMER'))
df.fd.rate   <- findMeans(subset(df.rate,   df.rate$intervention == 'FDMER'))
df.fd.thresh <- findMeans(subset(df.thresh, df.thresh$intervention == 'FDMER'))

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


# Plotting function
doPlot <- function(name, df, context, binSize, xlab, xIsPercentage=FALSE) {
  plotToFile(makePath(name))
  plotSensitivity(df=df, context=df, binSize=binSize, xlab=xlab, xIsPercentage=xIsPercentage)

  plotToFile(makePath(paste('ylim-', name, sep='')))
  plotSensitivity(df=df, context=df.all, binSize=binSize, xlab=xlab, xIsPercentage=xIsPercentage)
}


# Plot
doPlot('all-cash',    df.both.cash,    df.all,  binSize.cash,  'Total Projected Revenues (mUSD)')
doPlot('all-cost',    df.both.cost,    df.all,  binSize.cost,  'Total Projected Costs (mUSD)')
doPlot('all-prob',    df.both.prob,    df.all,  binSize.prob,  'Total Projected Probability of Success (%)')
doPlot('all-rate',    df.both.rate,    df.all,  binSize.rate,  'Venture Capital Discount Rate (%)')
doPlot('all-thresh',  df.both.thresh,  df.all,  NULL,          'Big Pharma Threshold (mUSD)')

doPlot('fd-cash',    df.fd.cash,    df.all,  binSize.cash,  'Total Projected Revenues (mUSD)')
doPlot('pd-cash',    df.pd.cash,    df.all,  binSize.cash,  'Total Projected Revenues (mUSD)')

doPlot('fd-cost',    df.fd.cost,    df.all,  binSize.cost,  'Total Projected Costs (mUSD)')
doPlot('pd-cost',    df.pd.cost,    df.all,  binSize.cost,  'Total Projected Costs (mUSD)')

doPlot('fd-prob',    df.fd.prob,    df.all,  binSize.prob,  'Total Projected Probability of Successs (%)')
doPlot('pd-prob',    df.pd.prob,    df.all,  binSize.prob,  'Total Projected Probability of Successs (%)')

doPlot('fd-rate',    df.fd.rate,    df.all,  binSize.rate,  'Venture Capital Discount Rate (%)')
doPlot('pd-rate',    df.pd.rate,    df.all,  binSize.rate,  'Venture Capital Discount Rate (%)')

doPlot('fd-thresh',  df.fd.thresh,  df.all,  NULL,          'Big Pharma Threshold (mUSD)')
doPlot('pd-thresh',  df.pd.thresh,  df.all,  NULL,          'Big Pharma Threshold (mUSD)')

