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

  setnames(df, 'orgs_infcap_thresh', 'thresh')
  setnames(df, 'proj_grants_accumulated', 'grants')

  print('Round project peak year sales')
  df$peak <- roundToNearest(df$proj_peak_cash, 5)

  print('Counting entries intervention size and peak')
  df <- ddply(df,
              c('grants', 'thresh', 'peak', 'intervention', 'interventions_tot_size'),
              summarise,
              tot_pois  = countCompletes(proj_state),
              projs     = length(unique(PROJ)))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'broad-mean-entries-no-grants-hi-thresh.csv',
             prepare,
             c('RUN',
               'PROJ',
               'proj_peak_cash',
               'interventions_tot_size',
               'intervention',
               'proj_state',
               'proj_grants_accumulated',
               'orgs_infcap_thresh'
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





df1 <- findMeans(subset(df, df$intervention == 'FDMER'))

df2 <- findMeans(subset(df, df$intervention == 'PDMER'))

df3 <- findMeans(subset(df, df$intervention == 'FDMER' &
                        df$grants == 0))

df4 <- findMeans(subset(df, df$intervention == 'PDMER' &
                        df$grants == 0))

threshStop = 200
df5 <- findMeans(subset(df, df$intervention == 'FDMER' &
                        df$thresh > threshStop))

df6 <- findMeans(subset(df, df$intervention == 'PDMER' &
                        df$thresh > threshStop))


all <- rbind(df1, df2, df3, df4, df5, df6)
top <- max(all$mean_pois)
bot <- min(all$mean_pois)


doPlot(df1, 'Full Delinkage')
doPlot(df2, 'Partial Delinkage')
doPlot(df3, 'Full Delinkage + No Grants')
doPlot(df4, 'Partial Delinkage + No Grants')
doPlot(df5, paste('Full Delinkage + Thresholds Above', threshStop))
doPlot(df6, paste('Partial Delinkage + Thresholds Above', threshStop))


mtext('Likelihood of Entry Per Peak Year Sales', outer=TRUE,  cex=1, line=-2)
