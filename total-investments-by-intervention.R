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
                             metavar = 'float'),
                 make_option(
                             c('--proj_group'),
                             default = NULL,
                             help    = 'subset on project group [default= %default]',
                             metavar = 'string')
                 ),
            function(args) !is.null(args$input))





# PREPARE
# ============================================

prepare <- function(df) {
  print('(Subsetting on only PreClinical, Phase1, Phase2, Phase3)')
  df <- subset(df, df$proj_stage_group %in% c('PreClinical', 'Phase1', 'Phase2', 'Phase3'))
  if (!is.null(args$proj_group)) {
    print('(Subsetting by proj group passed as argument)')
    df <- subset(df, df$proj_group == args$proj_group)
    print('WARN: Subsetted data set will be written to cache!')
  }
  print('(Subsetting on only projects that received investors)')
  df <- subset(df, df$proj_capital_accumulated > 0)
  print('(Grouping by project)')
  df <- ddply(df, c('RUN', 'PROJ', 'interventions_tot_size'), summarise,
              proj_investments = max(proj_capital_accumulated))
  print('(Grouping by run and intervention size)')
  df <- ddply(df, c('RUN', 'interventions_tot_size'), summarise,
              mean_investments = sum(proj_investments))
  return(df)
}
colsToRead <- c('RUN',
                'PROJ',
                'interventions_tot_size',
                'proj_capital_accumulated',
                'proj_stage_group',
                'proj_group')
df <- getSet(args$input, args$cache, 'total-investments-by-intervention.csv', prepare, colsToRead)





# PLOT
# ============================================
plotToFile(args$output)
plot(as.factor(df$interventions_tot_size), df$mean_investments,
     main  = paste('Do private investments increase with intervention size?', args$label),
     xlab = 'Intervention Size',
     ylab = 'Total Private Investments per Run',
     ylim = buildLim(NULL, args$ylim)
     )
