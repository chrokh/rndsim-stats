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
revenueBinSize <- 500
discountRateBinSize <- 1
prepare <- function(df) {

  print('Rename parameters')
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'
  names(df)[names(df) == 'proj_tot_cash'] <- 'cash'
  names(df)[names(df) == 'inv_rate'] <- 'rate'
  names(df)[names(df) == 'orgs_infcap_thresh'] <- 'thresh'

  print('Round project revenues')
  df$cash <- roundToNearest(df$cash, revenueBinSize)

  print('Round investor rates')
  df$rate <- roundToNearest(df$rate * 100, discountRateBinSize)

  print('Counting entries')
  df <- ddply(df,
              c('intervention',
                'rate',
                'intervention_size',
                'cash',
                'thresh'),
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
               'proj_tot_cash',
               'orgs_infcap_thresh',
               'intervention'
               ))








# PLOT FUNCTIONS
# ============================================
findMeans <- function(df) {
  df <- ddply(df, c('rate', 'intervention_size'), summarise,
               tot_pois = sum(tot_pois),
               projs    = sum(projs))
  df$mean_pois <- df$tot_pois / df$projs * 100
  return(df)
}

doPlot <- function(df, context, name) {
  generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))
  interventions    <- sort(unique(df$intervention_size))
  numInterventions <- length(interventions)
  colorscale       <- generateColorscale(numInterventions)
  df$color  <- colorscale[findInterval(df$intervention_size, interventions)]

  plot(
       df$mean_pois ~ df$rate,
       #col  = df$color,
       col  = 'white',
       pch  = 19,
       yaxt = 'n',
       xaxt = 'n',
       xlab = paste('VC Discount Rate (%) ( \u00B1', discountRateBinSize / 2, ')'),
       ylab = 'Mean Likelihood of Market Entry (%)',
       ylim = c(min(context$mean_pois), max(context$mean_pois))
       )

  xticks <- seq(round(min(context$rate)), round(max(context$rate)), by = 1)
  yticks <- seq(round(min(context$mean_pois)), round(max(context$mean_pois)), by = 0.5)
  axis(side=1, col='black', las = 2, at = xticks)
  axis(side=2, col='black', las = 2, at = yticks)

  abline(h=yticks, v=xticks, col="gray", lty=3)

  for (bin in interventions) {
    sub <- subset(df, df$intervention_size == bin)
    lines(sub$mean_pois ~ sub$rate, col = sub$color)
  }

  legend('topright', levels(factor(df$intervention_size)),
         pch = 19, col = df$color, bty = 'n', title='Intervention',
         cex = 1)

  mtext(name)
}






# PLOT
# ============================================

plotToLandscape(args$output)
layout(matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))

scenario <- function(df) {
  return(subset(df,
                df$thresh >= 200 &
                  df$thresh <= 500 &
                  df$cash <= 1500))
}

df <- subset(df, df$rate >= 9)

sub1 <- subset(df, df$intervention == 'FDMER')
sub2 <- subset(df, df$intervention == 'PDMER')
sub3 <- scenario(sub1)
sub4 <- scenario(sub2)

sub1 <- findMeans(sub1)
sub2 <- findMeans(sub2)
sub3 <- findMeans(sub3)
sub4 <- findMeans(sub4)

all <- rbind(sub1, sub2, sub3, sub4)

doPlot(sub1, all, 'Full Delinkage (FD)')
doPlot(sub2, all, 'Partial Delinkage (PD)')
doPlot(sub3, all, 'FD (BP Threshold 200-500 + Projected Revenues <= 1.5B )')
doPlot(sub4, all, 'PD (BP Threshold 200-500 + Projected Revenues <= 1.5B )')

mtext('Likelihood of Market Entry vs VC Discount Rate', outer=TRUE,  cex=1, line=-2, font=2)
mtext('Likelihood of Market Entry vs VC Discount Rate For Suggested Scenario', outer=TRUE,  cex=1, line=-28.7, font=2)
