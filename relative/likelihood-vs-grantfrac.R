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
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))






# PREPARE
# ============================================
prepare <- function(df) {

  print('Rename columns')
  names(df)[names(df) == 'grants_avg_frac'] <- 'frac'
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'

  print('Counting entries')
  df <- ddply(df, c(
                    'frac',
                    'intervention',
                    'intervention_size'
                    ), summarise,
              pois  = countCompletes(proj_state),
              projs = length(unique(PROJ))
              )

  return(df)
}

df <- getSet(args$input,
             args$cache,
             'likelihood-vs-grantfrac.csv',
             prepare,
             c(
               'PROJ',
               'grants_avg_frac',
               'intervention',
               'interventions_tot_size',
               'proj_state'
               ))




# PREPARE PLOT
# ============================================
print('Recompputing market entries to %')
df$pois <- df$pois / df$projs * 100

print('Recompputing fraction to %')
df$frac <- df$frac * 100




# PLOT
# ============================================

plotToLandscape(args$output)

# Prepare counts
nSizes <- length(unique(df$intervention_size))
nTypes <- length(unique(df$intervention))

# Prepare colors and linetypes
palette    <- colorRampPalette(c('red', 'orange', 'green'))
linetypes  <- c(1:nTypes)
colors     <- palette(nSizes)
df$color   <- colors[as.numeric(as.factor(df$intervention_size))]

# Plot
plot(df$pois ~ df$frac,
     col     = df$color,
     main    = 'Likelihood of Market Approval vs Fraction Potentially Funded by Grants (in PC - P2)',
     pch     = 16,
     ylab    = 'Likelihood of Market Approval (%)',
     xlab    = 'Fraction (%) of Stage Costs offered as Grant'
     )

mtext('By MER size (red to green) and level of delinkage (solid or dashed).')

# Convert factors to numeric for convenience
df$intervention_size <- as.numeric(as.factor(df$intervention_size))
df$intervention <- as.numeric(as.factor(df$intervention))

# Print lines
for (type in c(1:nTypes)) {
  for (size in c(1:nSizes)) {
    sub <- subset(df,
                  df$intervention_size == size &
                    df$intervention == type)
    lines(sub$pois ~ sub$frac,
          col = colors[size],
          lty = linetypes[type])
  }
}

