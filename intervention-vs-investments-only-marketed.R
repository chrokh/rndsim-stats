source('shared.R')


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
  df <- subset(only_reached_market(df))
  print('(grouping 1)')
  df <- ddply(df, c('RUN', 'PROJ', 'interventions_tot_size'), summarise, proj_investments = max(proj_capital_accumulated))
  print('(grouping 2)')
  df <- ddply(df, c('RUN', 'interventions_tot_size'), summarise, mean_investments = mean(proj_investments))
  return(df)
}
df <- getSet(args$input, args$cache, 'intervention-vs-investments-only-marketed.csv', prepare)





# PLOT
# ============================================
plotToFile(args$output)
plot(df$interventions_tot_size, df$mean_investments,
     main = 'Do private investments increase with intervention size?',
     sub  = '(Only projects that reached market)',
     xlab = 'Intervention Size',
     ylab = 'Private Investment',
     ylim = buildLim(NULL, args$ylim)
     )
