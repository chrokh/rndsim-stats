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
prepare <- function(df) {

  print('Subset away extremely high peaks')
  df <- subset(df, df$proj_peak_cash <= 85)

  print('Round project costs')
  df$cost <- roundToNearest(df$proj_tot_cost, 25)

  print('Round project times')
  df$time <- roundToNearest(df$proj_tot_time, 50)

  print('Round project peak year sales')
  df$peak <- roundToNearest(df$proj_peak_cash, 5)

  print('Multiply peak by 12 so we display peak year rather than peak month')
  df$peak <- df$peak * 12

  print('Counting entries intervention size and peak')
  df <- ddply(df,
              c('cost', 'time', 'intervention', 'peak', 'interventions_tot_size'),
              summarise,
              tot_pois  = countCompletes(proj_state),
              projs     = length(unique(PROJ)))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'broad-mean-entries-by-peak-time-cost.csv',
             prepare,
             c('RUN',
               'PROJ',
               'proj_peak_cash',
               'interventions_tot_size',
               'proj_state',
               'proj_tot_cost',
               'proj_tot_time',
               'intervention'
               ))





# PLOT
# ============================================
plotToFile(args$output, TRUE)

findMeans <- function(df) {
  df <- ddply(df, c('peak', 'interventions_tot_size'), summarise,
               tot_pois = sum(tot_pois),
               projs    = sum(projs))
  df$mean_pois <- df$tot_pois / df$projs
  return(df)
}

doPlot <- function(df, name) {

  generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))
  interventions    <- unique(df$interventions_tot_size)
  numInterventions <- length(interventions)
  colorscale       <- generateColorscale(numInterventions)
  df$color  <- colorscale[findInterval(df$interventions_tot_size, interventions)]

  plot(
       df$mean_pois ~ df$peak,
       #col  = df$color,
       col  = 'white',
       pch  = 19,
       xlab = 'Peak Year Sales',
       ylab = 'Mean Likelihood of Entry',
       ylim = c(bot, top)
       )

  for (bin in interventions) {
    sub <- subset(df, df$interventions_tot_size == bin)
    lines(sub$mean_pois ~ sub$peak, col = sub$color)
  }

  legend('topright', levels(factor(df$interventions_tot_size)),
         pch = 19, col = df$color, bty = 'n', title="Intervention",
         cex = 1)

  mtext(name)
}


layout(matrix(c(1,2,3,4,5,6), nrow = 3, byrow = TRUE))




# Full delinkage
fd1 <- findMeans(subset(df, df$intervention == 'FDMER'))

# Partial delinkage
pd1 <- findMeans(subset(df, df$intervention == 'PDMER'))

# Full top cost
fd2 <- findMeans(subset(df, df$intervention == 'FDMER' & df$cost > mean(df$cost)))

# Partial top cost
pd2 <- findMeans(subset(df, df$intervention == 'PDMER' & df$cost > mean(df$cost)))

# Full top time
fd3 <- findMeans(subset(df, df$intervention == 'FDMER' & df$time > mean(df$time)))

# Partial top time
pd3 <- findMeans(subset(df, df$intervention == 'PDMER' & df$time > mean(df$time)))



all <- rbind(fd1, pd1, fd2, pd2)
top <- max(all$mean_pois)
bot <- min(all$mean_pois)


doPlot(fd1, 'Full Delinkage')
doPlot(pd1, 'Partial Delinkage')
doPlot(fd2, 'Full Delinkage, Above Mean Total Cost')
doPlot(pd2, 'Partial Delinkage, Above Mean Total Cost')
doPlot(fd3, 'Full Delinkage, Above Mean Total Time')
doPlot(pd3, 'Partial Delinkage, Above Mean Total Time')


mtext('Likelihood of Entry Per Peak Year Sales', outer=TRUE,  cex=1, line=-2)
