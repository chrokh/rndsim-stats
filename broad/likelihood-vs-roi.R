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

  print('Calculate ROI')
  df$roi <- df$proj_tot_cash / df$proj_tot_cost

  print('Round project ROI')
  #df$roi <- round(df$roi)
  df$roi <- roundToNearest(df$roi, binSize)

  print('Counting entries per group')
  df <- ddply(df,
              c('intervention', 'roi', 'interventions_tot_size', 'grants_tot_size'),
              summarise,
              tot_pois  = countCompletes(proj_state),
              projs     = length(unique(PROJ)))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'likelihood-vs-roi.csv',
             prepare,
             c('RUN',
               'PROJ',
               'interventions_tot_size',
               'proj_state',
               'proj_tot_cost',
               'proj_tot_cash',
               'grants_tot_size',
               'intervention'
               ))





# PLOT
# ============================================
plotToPortrait(args$output)

findMeans <- function(df) {
  df <- ddply(df, c('roi', 'interventions_tot_size'), summarise,
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
       df$mean_pois ~ df$roi,
       #col  = df$color,
       col  = 'white',
       pch  = 19,
       xlab = paste('ROI ( \u00B1', binSize / 2, ')'),
       ylab = 'Mean Likelihood of Entry (%)',
       ylim = c(bot, top)
       )

  for (bin in interventions) {
    sub <- subset(df, df$interventions_tot_size == bin)
    lines(sub$mean_pois ~ sub$roi, col = sub$color)
  }

  legend('topright', levels(factor(df$interventions_tot_size)),
         pch = 19, col = df$color, bty = 'n', title="Intervention",
         cex = 1)

  mtext(name)
}


layout(matrix(c(1,2,3,4,5,6), nrow = 3, byrow = TRUE))

print('Subset on below mid')
mid <- (max(df$roi) - min(df$roi)) / 2 + min(df$roi)
df <- subset(df, df$roi <= mid)

print('Subset on ROI > 0')
df <- subset(df, df$roi > 0)

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

mtext('Likelihood of Entry Per Projected ROI', outer=TRUE,  cex=1, line=-2)
mtext('0 < X <= Lower half', outer=TRUE,  cex=1, line=-2, side=1)
