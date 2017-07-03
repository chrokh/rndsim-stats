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






# PREPARE
# ============================================
binSize.cash <- 500
binSize.cost <- 10
binSize.prob <- 1.5
binSize.rate <- 2
prepare <- function(df) {

  print('Rename parameters')
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'
  names(df)[names(df) == 'proj_tot_cash']          <- 'cash'
  names(df)[names(df) == 'proj_tot_cost']          <- 'cost'
  names(df)[names(df) == 'proj_tot_prob']          <- 'prob'
  names(df)[names(df) == 'orgs_infcap_thresh']     <- 'thresh'
  names(df)[names(df) == 'inv_rate']               <- 'rate'

  print('Round project revenues')
  df$cash <- roundToNearest(df$cash, binSize.cash)

  print('Round project cost')
  df$cost <- roundToNearest(df$cost, binSize.cost)

  print('Round project probability')
  df$prob <- roundToNearest(df$prob * 100, binSize.prob)

  print('Round investor rates')
  df$rate <- roundToNearest(df$rate * 100, binSize.rate)

  print('Counting entries per group')
  df <- ddply(df,
              c(
                'intervention',
                'intervention_size',
                'cash',
                'cost',
                'prob',
                'rate',
                'thresh'
                ),
              summarise,
              pois   = countCompletes(proj_state),
              projs  = length(unique(PROJ)))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'sensitivity.csv',
             prepare,
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








# REVENUE
# ============================================

findMeans <- function(df = NULL, x = NULL) {

  df <- ddply(df, c(x, 'intervention_size'), summarise,
               pois  = sum(pois),
               projs = sum(projs))
  df$pois <- df$pois / df$projs * 100

  print(paste('Finding means for', x))
  names(df)[names(df) == x] <- 'x'

  return(df)
}

plotSensitivity <- function(
                            df      = NULL,
                            x       = NULL,
                            context = NULL,
                            xlab    = NULL,
                            binSize = NULL
                            ) {

  # Convinience
  interventions  <- sort(unique(df$intervention_size))

  # Prepare colors
  colors   <- colorRampPalette(c('red', 'orange', 'green'))(length(interventions))
  df$color <- colors[as.numeric(as.factor(df$intervention_size))]

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
# PLOTS
# ============================================
makePath <- function(name) { paste(args$output, '/', name, '.', args$format, sep='') }


# Subset away extreme values
df <- subset(df, df$cash <= 4000)
df <- subset(df, df$cost >= 190 & df$cost <= 340)
df <- subset(df, df$prob <= 15)

# Prepare subsets for plotting
df.cash   <- findMeans(df, 'cash')
df.cost   <- findMeans(df, 'cost')
df.prob   <- findMeans(df, 'prob')
df.rate   <- findMeans(df, 'rate')
df.thresh <- findMeans(df, 'thresh')
# Prepare fd subsets
df.fd <- subset(df, df$intervention == 'FDMER')
df.fd.cash   <- findMeans(df.fd, 'cash')
df.fd.cost   <- findMeans(df.fd, 'cost')
df.fd.prob   <- findMeans(df.fd, 'prob')
df.fd.rate   <- findMeans(df.fd, 'rate')
df.fd.thresh <- findMeans(df.fd, 'thresh')
# Prepare pd subsets
df.pd <- subset(df, df$intervention == 'PDMER')
df.pd.cash   <- findMeans(df.pd, 'cash')
df.pd.cost   <- findMeans(df.pd, 'cost')
df.pd.prob   <- findMeans(df.pd, 'prob')
df.pd.rate   <- findMeans(df.pd, 'rate')
df.pd.thresh <- findMeans(df.pd, 'thresh')

# Collate subsets for plotting context
df.all <- rbind(
                df.cash,
                df.cost,
                df.prob,
                df.rate,
                df.thresh,

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


doPlot <- function(name, df, context, binSize, xlab) {
  plotToFile(makePath(name))
  plotSensitivity(df=df, context=df, binSize=binSize, xlab=xlab)

  plotToFile(makePath(paste('ylim-', name, sep='')))
  plotSensitivity(df=df, context=df.all, binSize=binSize, xlab=xlab)
}

# Plot
doPlot('all-cash', df.cash, df.all, binSize.cash, 'Total Projected Revenues (mUSD)')
doPlot('all-cost', df.cost, df.all, binSize.cost, 'Total Projected Costs (mUSD)')
doPlot('all-prob', df.prob, df.all, binSize.prob, 'Total Projected Probability of Success (%)')
doPlot('all-rate', df.rate, df.all, binSize.rate, 'Venture Capital Discount Rate (%)')
doPlot('all-thresh', df.thresh, df.all, NULL, 'Big Pharma Threshold (mUSD)')

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
