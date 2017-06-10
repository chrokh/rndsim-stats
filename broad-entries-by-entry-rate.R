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

  print('Bin by entry rates')
  # Source: https://stackoverflow.com/questions/4126326/how-to-quickly-form-groups-quartiles-deciles-etc-by-ordering-columns-in-a
  df$rate <-
    with(df,
         factor(
                findInterval(proj_type_discovery_rate, c(-Inf, quantile(proj_type_discovery_rate, probs=c(0.25, .5, .75)), Inf)),
                labels=c('Q1','Q2','Q3','Q4')
                ))

  print('Counting entries intervention size')
  df <- ddply(df,
              c('RUN', 'rate', 'interventions_tot_size'),
              summarise,
              tot_pois = countCompletes(proj_state))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'broad-entries-by-entry-rate.csv',
             prepare,
             c('RUN', 'proj_type_discovery_rate', 'interventions_tot_size', 'proj_state'))





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
                 df$rate
                 ),
     boxfill = boxcolors,
     main    = paste('Total entries per entry rate quartile and intervention size', args$label),
     ylab    = 'Number of entries',
     xlab    = '',
     las     = 2,
     ylim    = buildLim(NULL, args$ylim)
     )

pos <- seq(from = (categories + 0.5),
           to = (categories * length(unique(df$rate))),
           by = categories)
abline(h = NULL, v = pos, col = 'darkgray', lty = 'solid')

legend('topleft', levels(factor(df$interventions_tot_size)), pch = 15, col = 'red', bty = 'n', title="Type")
