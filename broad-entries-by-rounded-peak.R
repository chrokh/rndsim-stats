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
                             metavar = 'folder'),
                 make_option(
                             c('-l', '--label'),
                             default = '',
                             help    = 'dataset identifier [default= %default]',
                             metavar = 'character'),
                 make_option(
                             c('--ylimit'),
                             default = NULL,
                             help    = 'ylim max of plot [default= %default]',
                             metavar = 'float')
                 ),
            function(args) !is.null(args$input))






# PREPARE
# ============================================
prepare <- function(df) {

  print('Subset on only full delinkage')
  df <- subset(df, df$intervention == 'FDMER')

  print('Subset on top quartile of entry rates')
  mi <- min(df$proj_type_discovery_rate)
  ma <- max(df$proj_type_discovery_rate)
  top <- (mi - ma) / 4 * 3 + mi
  df <- subset(df, df$proj_type_discovery_rate >= top)

  base <- 5
  print(paste('Round peak year sales to nearest', base))
  df$peak <- roundToNearest(df$proj_peak_cash, base)

  print('Counting entries intervention size and peak')
  df <- ddply(df,
              c('RUN', 'peak', 'interventions_tot_size'),
              summarise,
              tot_pois = countCompletes(proj_state))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'broad-entries-by-rounded-peak.csv',
             prepare,
             c('RUN', 'proj_peak_cash', 'interventions_tot_size', 'proj_state', 'proj_type_discovery_rate', 'intervention'))





# PLOT
# ============================================
plotToFile(args$output)
colorscale <- function(df, n) {
  rev(brewer.pal(n, 'Set1'))
}

generateColorscale <- colorRampPalette(c('green', 'yellow', 'red'))
categories <- length(unique(df$interventions_tot_size))
boxcolors  <- rep(generateColorscale(categories), categories)
plot(df$tot_pois  ~
     interaction(
                 df$interventions_tot_size,
                 df$peak
                 ),
     boxfill = boxcolors,
     main    = paste('Total Entries Per Rounded Peak Year Sales And Intervention Size', args$label),
     ylab    = 'Number of entries',
     xlab    = '',
     las     = 2,
     ylim    = buildLim(NULL, args$ylim)
     )

pos <- seq(from = categories + 0.5,
           to = categories * length(unique(df$peak)),
           by = categories)
abline(h = NULL, v = pos, col = 'darkgray', lty = 'solid')

mtext('Full Delinkage + Top Quartile Entry Rates')

legend('topleft', levels(factor(df$interventions_tot_size)), pch = 15, col = 'red', bty = 'n', title="Type")
