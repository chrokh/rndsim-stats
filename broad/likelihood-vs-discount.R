source('shared.R')
library(RColorBrewer)


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
                             default ='plot.png',
                             help    ='output file name [default= %default]',
                             metavar ='file'),
                 make_option(
                             c('-c', '--cache'),
                             default = '.',
                             help    = 'cache folder [default= %default]',
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))






# PREPARE
# ============================================
binSize <- 2
prepare <- function(df) {

  print('Round investor rates')
  df$rate <- roundToNearest(df$inv_rate * 100, binSize)

  print('Counting entries')
  df <- ddply(df,
              c('intervention', 'rate', 'interventions_tot_size', 'grants_tot_size'),
              summarise,
              tot_pois  = countCompletes(proj_state),
              projs     = length(unique(PROJ)))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'likelihood-vs-discount.csv',
             prepare,
             c('RUN',
               'PROJ',
               'interventions_tot_size',
               'proj_state',
               'inv_rate',
               'grants_tot_size',
               'intervention'
               ))





# PLOT
# ============================================
findMeans <- function(df) {
  df <- ddply(df, c('rate', 'interventions_tot_size'), summarise,
               tot_pois = sum(tot_pois),
               projs    = sum(projs))
  df$mean_pois <- df$tot_pois / df$projs * 100
  return(df)
}

doPlot <- function(df, name) {

  generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))
  interventions    <- sort(unique(df$interventions_tot_size))
  numInterventions <- length(interventions)
  colorscale       <- generateColorscale(numInterventions)
  df$color  <- colorscale[findInterval(df$interventions_tot_size, interventions)]

  plot(
       df$mean_pois ~ df$rate,
       col  = df$color,
       #col  = 'white',
       pch  = 19,
       xlab = paste('Investor Discount Rate (%) ( \u00B1', binSize / 2, ')'),
       ylab = 'Mean Likelihood of Entry (%)',
       ylim = c(bot, top)
       )

  for (bin in interventions) {
    sub <- subset(df, df$interventions_tot_size == bin)
    lines(sub$mean_pois ~ sub$rate, col = sub$color)
  }

  legend('topright', levels(factor(df$interventions_tot_size)),
         pch = 19, col = df$color, bty = 'n', title="Intervention",
         cex = 1)

  mtext(name)
}


plotToPortrait(args$output)
layout(matrix(c(1,2,3,4,5,6), nrow = 3, byrow = TRUE))

fd <- subset(df, df$intervention == 'FDMER')
pd <- subset(df, df$intervention == 'PDMER')

fdGrants <- subset(fd, fd$grants_tot_size > 0)
pdGrants <- subset(pd, pd$grants_tot_size > 0)

fdNoGrants <- subset(fd, fd$grants_tot_size == 0)
pdNoGrants <- subset(pd, pd$grants_tot_size == 0)

fd         <- findMeans(fd)
pd         <- findMeans(pd)
fdNoGrants <- findMeans(fdNoGrants)
pdNoGrants <- findMeans(pdNoGrants)
fdGrants   <- findMeans(fdGrants)
pdGrants   <- findMeans(pdGrants)

all <- rbind(fd, pd, fdGrants, pdGrants, fdNoGrants, pdNoGrants)

top <- max(all$mean_pois)
bot <- min(all$mean_pois)

doPlot(fd, 'Full Delinkage')
doPlot(pd, 'Partial Delinkage')
doPlot(fdGrants, 'Full Delinkage + Grants')
doPlot(pdGrants, 'Partial Delinkage + Grants')
doPlot(fdNoGrants, 'Full Delinkage + No Grants')
doPlot(pdNoGrants, 'Partial Delinkage + No Grants')

mtext('Likelihood of Entry Per Investor Discount Rate', outer=TRUE,  cex=1, line=-2)
