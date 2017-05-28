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
df <- getSet(args$input, args$cache, 'valuation-per-stage-and-intervention-per-run.csv',
             function(df) {
               df <- ddply(df, c('PROJ', 'interventions_tot_size', 'proj_group', 'proj_stage_group'), summarise,
                           min_valuation = min(proj_composite_valuation))
               return(df)
             }, colsToRead)




# PLOT
# ============================================
plotToFile(args$output)

# Subset to single group if given as arg
if (!is.null(args$proj_group)) {
  df <- subset(df, df$proj_group == args$proj_group)
}

# Group and calculate mean values
df <- ddply(df, c('interventions_tot_size', 'proj_stage_group'), summarise,
            mean_valuation = mean(min_valuation))

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

# Order data based on factor to improve printing
df$proj_stage_group <- factorizeStages(df$proj_stage_group)
df <- df[order(df$proj_stage_group),]

# Plot
plot(as.numeric(df$proj_stage_group), df$mean_valuation,
     main    = paste('Mean project valuation per run per stage', args$label),
     ylab    = 'Min Project Valuation (within stage)',
     xlab    = '',
     ylim    = buildLim(NULL, args$ylim),
     xaxt    = 'n',
     col     = df$color,
     pch     = 19)

axis(side=1, seq(min(as.numeric(df$proj_stage_group)), max(as.numeric(df$proj_stage_group))))

# Legend
legend('topleft', levels(factor(df$interventions_tot_size)), pch = 15, col = colorscale, bty = 'n', title='Amount')

# Lines
for (bin in interventions) {
  sub <- subset(df, df$interventions_tot_size == bin)
  lines(sub$proj_stage_group, sub$mean_valuation, col = sub$color)
}
