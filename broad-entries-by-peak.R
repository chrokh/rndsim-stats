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

  print('Bin by peak year sales')
  # Source: https://stackoverflow.com/questions/4126326/how-to-quickly-form-groups-quartiles-deciles-etc-by-ordering-columns-in-a
  df$peak <-
    with(df,
         factor(
                findInterval(proj_peak_cash,
                             c(-Inf,
                               quantile(proj_peak_cash, probs=seq(0.125, 0.875, by = 0.125)),
                               Inf)),
                labels=c('O1','O2','O3','O4', 'O5', 'O6', 'O7', 'O8')
                ))

  print('Counting entries intervention size')
  df <- ddply(df,
              c('RUN', 'peak', 'interventions_tot_size'),
              summarise,
              tot_pois = countCompletes(proj_state))

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'broad-entries-by-peak.csv',
             prepare,
             c('RUN', 'proj_peak_cash', 'intervention', 'interventions_tot_size', 'proj_state'))





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
     main    = paste('Total Entries Per Peak Year Sales Octile, And Intervention Size', args$label),
     ylab    = 'Number of Entries per Run',
     xlab    = '',
     las     = 2,
     ylim    = buildLim(NULL, args$ylim)
     )

pos <- seq(from = categories + 0.5,
           to = categories * length(unique(df$peak)),
           by = categories)
abline(h = NULL, v = pos, col = 'darkgray', lty = 'solid')

mtext('Full Delinkage')

legend('topleft', levels(factor(df$interventions_tot_size)), pch = 15, col = 'red', bty = 'n', title="Type")
