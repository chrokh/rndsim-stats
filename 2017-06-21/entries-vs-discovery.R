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
                             c('-c', '--cache'), default = '.',
                             help    = 'cache folder [default= %default]',
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))






# PREPARE
# ============================================
binSize <- 0.5
prepare <- function(df) {

  print('Rename columns')
  names(df)[names(df) == 'proj_type_discovery_rate'] <- 'discovery'
  names(df)[names(df) == 'orgs_infcap_thresh'] <- 'thresh'
  names(df)[names(df) == 'inv_rate'] <- 'rate'
  names(df)[names(df) == 'proj_tot_cash'] <- 'cash'
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'

  print('Removing initial discoveries')
  df <- subset(df, df$discovery != 0)
  print(head(df))

  print('Subsetting on scenario')
  df <- subset(df,
               df$thresh >= 200 &
                 df$thresh <= 500 &
                 df$rate >= 0.2 &
                 df$cash <= 1500)

  print('Round discovery rates')
  df$discovery <- roundToNearest(df$discovery, binSize)

  print('Counting entries')
  df <- ddply(df, c(
                    'RUN',
                    'discovery',
                    'intervention',
                    'intervention_size'
                    ), summarise,
              tot_pois  = countCompletes(proj_state),
              projs     = length(unique(PROJ))
              )
  return(df)
}

df <- getSet(args$input,
             args$cache,
             'entries-vs-discovery.csv',
             prepare,
             c('RUN',
               'proj_type_discovery_rate',
               'interventions_tot_size',
               'intervention',
               'proj_state',
               'inv_rate',
               'proj_tot_cash',
               'orgs_infcap_thresh',
               'PROJ'
               ))



# PLOTTING PREPARATION FUNCTION
# ============================================

prepareSet <- function(df) {

  print('Estimate number of market entries')
  df$poi_estimate <- df$tot_pois / df$projs * df$discovery * 29 * 12

  print('Group by discovery rate')
  df <- ddply(df, c(
                    'discovery',
                    'intervention_size'
                    ), summarise,
              pois  = mean(poi_estimate))
}






# PLOTTING FUNCTION
# ============================================

plotSet <- function(df, context) {
  interventions      <- sort(unique(df$intervention_size))
  generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))
  colorscale         <- generateColorscale(length(interventions))
  df$color           <- colorscale[findInterval(df$intervention_size, interventions)]

  plot(df$pois ~ df$discovery,
       col     = df$color,
       ylab    = 'Derived Mean Number of Market Entries Per Run',
       xlab    = paste('Monthly Entry Rate ( \u00B1', binSize / 2, ')'),
       xaxt    = 'n',
       yaxt    = 'n',
       main    = 'Derived Mean Market Entries Per Run vs Entry Rate',
       ylim    = c(min(context$pois), max(context$pois)),
       pch     = 19
       )

  for (bin in unique(df$intervention_size)) {
    sub <- subset(df, df$intervention_size == bin)
    lines(sub$pois ~ sub$discovery,
          col = sub$color)
  }

  xticks <- seq(round(min(context$discovery)), round(max(context$discovery)), by = 1)
  yticks <- seq(round(min(context$pois)), round(max(context$pois)), by = 10)
  axis(side=1, col='black',  at = xticks)
  axis(side=2, col='black', las = 2, at = yticks)

  abline(h=yticks, v=xticks, col="gray", lty=3)

  legend('topleft', levels(factor(df$intervention_size)),
         pch = 19, col = df$color, bty = 'n', title='Intervention',
         cex = 1)
}





# PLOT
# ============================================

plotToPortrait(args$output)
#layout(matrix(c(
#                1,1,1,1,1,1,1,1,1,1,1,1,
#                2,2,2,2,2,2,2,2,2,2,2,2,
#                3), nrow = 25, byrow = TRUE))
layout(matrix(c(1,2), nrow=2, byrow = TRUE))

sub1 <- prepareSet(subset(df, df$intervention == 'FDMER'))
sub2 <- prepareSet(subset(df, df$intervention == 'PDMER'))
all <- rbind(sub1, sub2)

plotSet(sub1, all)
mtext('Full Delinkage (BP Threshold 200-500 + VC DR 20-30% + Revenues <= 1.5B)')
plotSet(sub2, all)
mtext('Partial Delinkage (BP Threshold 200-500 + VC DR 20-30% + Revenues <= 1.5B)')

#mtext('DERIVATION = entry_rate * 30 years * likelihood_of_reaching_market', outer=TRUE, cex=0.8, side=1, line=-2)
