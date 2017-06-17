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
                             default ='plot',
                             help    ='output file base [default= %default]',
                             metavar ='file'),
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
prepare <- function(df) {

  names(df)[names(df) == 'orgs_infcap_thresh'] <- 'x'

  print('Counting entries per group')
  df <- ddply(df,
              c('intervention', 'x', 'interventions_tot_size', 'grants_tot_size'),
              summarise,
              tot_pois  = countCompletes(proj_state),
              projs     = length(unique(PROJ)))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'likelihood-vs-thresh.csv',
             prepare,
             c('RUN',
               'PROJ',
               'interventions_tot_size',
               'proj_state',
               'orgs_infcap_thresh',
               'grants_tot_size',
               'intervention'
               ))





# PLOT
# ============================================

findMeans <- function(df) {
  df <- ddply(df, c('x', 'interventions_tot_size'), summarise,
               tot_pois = sum(tot_pois),
               projs    = sum(projs))
  df$mean_pois <- df$tot_pois / df$projs * 100
  return(df)
}

doBarPlot <- function(df, name) {
  generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))
  interventions    <- sort(unique(df$interventions_tot_size))
  numInterventions <- length(interventions)
  colorscale       <- generateColorscale(numInterventions)
  df$color  <- colorscale[findInterval(df$interventions_tot_size, interventions)]

  tbl <- xtabs(df$mean_pois ~ df$interventions_tot_size + df$x)
  barplot(tbl, beside = TRUE,
          xlab = 'Threshold of Organizations With \'Infinite\' Capital',
          ylab = 'Mean Likelihood of Entry (%)',
          ylim = c(0, top),
          col = colorscale
          )

  mtext(name)
}

doPlot <- function(df, name) {

  generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))
  interventions    <- sort(unique(df$interventions_tot_size))
  numInterventions <- length(interventions)
  colorscale       <- generateColorscale(numInterventions)
  df$color  <- colorscale[findInterval(df$interventions_tot_size, interventions)]

  plot(
       df$mean_pois ~ df$x,
       col  = df$color,
       #col  = 'white',
       pch  = 19,
       xlab = 'Threshold of Organizations With \'Infinite\' Capital',
       ylab = 'Mean Likelihood of Entry (%)',
       ylim = c(bot, top)
       )

  for (bin in interventions) {
    sub <- subset(df, df$interventions_tot_size == bin)
    lines(sub$mean_pois ~ sub$x, col = sub$color)
  }

  legend('topright', levels(factor(df$interventions_tot_size)),
         pch = 19, col = df$color, bty = 'n', title="Intervention",
         cex = 1)

  mtext(name)
}


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



# Line plots
plotToPortrait(paste(args$output, '1.', args$format, sep = ''))
layout(matrix(c(1,2,3,4,5,6), nrow = 3, byrow = TRUE))

doPlot(fd, 'Full Delinkage')
doPlot(pd, 'Partial Delinkage')
doPlot(fdGrants, 'Full Delinkage + Grants')
doPlot(pdGrants, 'Partial Delinkage + Grants')
doPlot(fdNoGrants, 'Full Delinkage + No Grants')
doPlot(pdNoGrants, 'Partial Delinkage + No Grants')

mtext('Likelihood of Entry Per Threshold of Larger Organizations', outer=TRUE,  cex=1, line=-2)



# Bar plots
plotToPortrait(paste(args$output, '2.', args$format, sep = ''))
layout(matrix(c(1,2,3,4,5,6), nrow = 3, byrow = TRUE))

doBarPlot(fd, 'Full Delinkage')
doBarPlot(pd, 'Partial Delinkage')
doBarPlot(fdGrants, 'Full Delinkage + Grants')
doBarPlot(pdGrants, 'Partial Delinkage + Grants')
doBarPlot(fdNoGrants, 'Full Delinkage + No Grants')
doBarPlot(pdNoGrants, 'Partial Delinkage + No Grants')

mtext('Likelihood of Entry Per Threshold of Larger Organizations', outer=TRUE,  cex=1, line=-2)
