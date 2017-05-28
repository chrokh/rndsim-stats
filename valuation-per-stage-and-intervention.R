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





# CONFIG
# ============================================
generateColorscale <- colorRampPalette(c('green', 'orange', 'red'))




# PREPARE
# ============================================
colsToRead <- c('PROJ',
                'interventions_tot_size',
                'proj_group',
                'proj_stage_group',
                'proj_composite_valuation')
df <- getSet(args$input, args$cache, 'valuation-per-stage-and-intervention.csv',
             function(df) {
               df <- only_before_market(df)
               return(ddply(df, c('PROJ', 'interventions_tot_size', 'proj_group', 'proj_stage_group'), summarise,
                           min_valuation = min(proj_composite_valuation)))
             }, colsToRead)




# PLOT
# ============================================
plotToFile(args$output)

# Subset to single group if given as arg
if (!is.null(args$proj_group)) {
  df <- subset(df, df$proj_group == args$proj_group)
}

# Subset to single intervention if given as input
if (!is.null(args$min_intervention)) {
  min <- args$min_intervention
} else {
  min <- min(df$interventions_tot_size)
}

if (!is.null(args$max_intervention)) {
  max <- args$max_intervention
} else {
  max <- max(df$interventions_tot_size)
}

if (!is.null(args$min_intervention) || !is.null(args$max_intervention)) {
  df <- subset(df,
               df$interventions_tot_size <= max
               & df$interventions_tot_size >= min)
}

# Prepare intervention groups
interventions    <- sort(unique(df$interventions_tot_size))
numInterventions <- length(interventions)

# Prepare colors
colorscale <- generateColorscale(numInterventions)
df$color   <- colorscale[findInterval(df$interventions_tot_size, interventions)]
boxcolors  <- rep(head(colorscale, numInterventions), numInterventions)

# Plot
plot(interaction(df$interventions_tot_size, factorizeDevStages(df$proj_stage_group)), df$min_valuation,
     main    = paste('Min valuation of project within stage, per project', args$label),
     ylab    = 'Project Valuation',
     xlab    = '',
     ylim    = buildLim(NULL, args$ylim),
     col     = df$interventions_tot_size,
     boxfill = boxcolors,
     las     = 2)

# Legend
legend('topleft', levels(factor(df$interventions_tot_size)), pch = 15, col = colorscale, bty = 'n', title='Amount')

