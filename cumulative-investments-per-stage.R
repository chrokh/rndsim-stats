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
colsToRead <- c('PROJ', 'proj_stage_group', 'proj_investors')
df <- getSet(args$input, args$cache, 'cumulative-investments-per-stage.csv',
             function(df) {
               return(ddply(df, c('PROJ', 'proj_stage_group'), summarise,
                           max_investors = max(proj_investors)))
             }, colsToRead)




# PLOT
# ============================================
plotToFile(args$output)
plot(factorizeStages(df$proj_stage_group), df$max_investors,
     main  = paste('Cumulative number of investors per stage', args$label),
     ylab  = 'Cumulative Number of VC Investors Per Project',
     xlab  = '',
     ylim  = buildLim(NULL, args$ylim),
     las   = 2)
