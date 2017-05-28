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
                             help    = 'subset on project group [default= %default] [example= --proj_group A]',
                             metavar = 'string')
                 ),
            function(args) !is.null(args$input))




# PREPARE
# ============================================
colsToRead <- c('PROJ', 'proj_group', 'proj_stage_group', 'proj_composite_valuation')
df <- getSet(args$input, args$cache, 'valuation-per-stage.csv',
             function(df) {
               return(ddply(df, c('PROJ', 'proj_group', 'proj_stage_group'), summarise,
                           min_valuation = min(proj_composite_valuation)))
             }, colsToRead)




# PLOT
# ============================================
plotToFile(args$output)

# Subset to single group if given as arg
if (!is.null(args$proj_group)) {
  df <- subset(df, df$proj_group == args$proj_group)
}

plot(factorizeStages(df$proj_stage_group), df$min_valuation,
     main  = paste('Min valuation per project per stage', args$label),
     ylab  = 'Min valuation per project',
     xlab  = '',
     ylim  = buildLim(NULL, args$ylim),
     las   = 2)
