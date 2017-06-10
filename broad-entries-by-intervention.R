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




# CONFIG
# ============================================
colorscale <- function(df) {
  rev(brewer.pal(nlevels(factor(df$proj_group)), "Set1"))
}




# PREPARE
# ============================================
prepare <- function(df) {
  df <- ddply(df, c('RUN', 'interventions_tot_size'), summarise,
              tot_pois = countCompletes(proj_state))
  return(df)
}
df <- getSet(args$input,
             args$cache,
             'broad-entries-by-intervention.csv',
             prepare,
             c('RUN', 'interventions_tot_size', 'proj_state'))


head(df)



# PLOT
# ============================================
plotToFile(args$output)
plot(df$tot_pois  ~ as.factor(df$interventions_tot_size),
     main    = paste('Total entries per intervention size', args$label),
     ylab    = 'Number of entries',
     xlab    = '',
     las     = 2,
     ylim    = buildLim(NULL, args$ylim)
     )
