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

  print('Counting entries by intervention size and threshold')
  df <- ddply(df,
              c('RUN', 'orgs_infcap_thresh', 'interventions_tot_size'),
              summarise,
              tot_pois = countCompletes(proj_state))

  return(df)
}

df <- getSet(args$input,
             args$cache,
             'broad-entries-by-threshold.csv',
             prepare,
             c('RUN',
               'orgs_infcap_thresh',
               'interventions_tot_size',
               'proj_state'
               ))





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
                 df$orgs_infcap_thresh
                 ),
     boxfill = boxcolors,
     main    = paste('Total Entries per Buyer Threshold and Intervention', args$label),
     ylab    = 'Number of entries per run',
     xlab    = '',
     las     = 2,
     ylim    = buildLim(NULL, args$ylim)
     )

pos <- seq(from = categories + 0.5,
                       to = categories * length(unique(df$orgs_infcap_thresh)),
                       by = categories)
abline(h = NULL, v = pos, col = 'darkgray', lty = 'solid')

grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)

legend('topleft', levels(factor(df$interventions_tot_size)), pch = 15, col = 'red', bty = 'n', title="Type")
