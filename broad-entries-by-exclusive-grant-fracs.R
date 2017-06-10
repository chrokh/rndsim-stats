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

  do <- function(df, column, nicename) {
    grp <- df
    print(paste('Restructuring column ', column))

    # Prepare for subsetting
    cols <- c('grants_preclinical_frac',
            'grants_phase1_frac',
            'grants_phase2_frac',
            'grants_phase3_frac',
            'grants_approval_frac')

    # Remove the column we're extracting
    cols <- cols[cols != column]

    # Subset on all other cols than the one in question being 0
    for (col in cols) {
      grp <- grp[ grp[[col]] == 0 , ]
    }

    grp <- ddply(grp,
                  c('RUN', column),
                  summarise,
                  entries = countCompletes(proj_state))

    # Rename col in question
    names(grp)[names(grp) == column] <- 'frac'
    grp$stage <- nicename

    return(grp)
  }

  df <- rbind(
              do(df, 'grants_preclinical_frac', 'PC'),
              do(df, 'grants_phase1_frac',      'P1'),
              do(df, 'grants_phase2_frac',      'P2'),
              do(df, 'grants_phase3_frac',      'P3'),
              do(df, 'grants_approval_frac',    'Ap')
              )

  return(df)
}

df <- getSet(args$input,
             args$cache,
             'broad-entries-by-exclusive-grant-fracs.csv',
             prepare,
             c('RUN',
               'proj_state',
               'grants_preclinical_frac',
               'grants_phase1_frac',
               'grants_phase2_frac',
               'grants_phase3_frac',
               'grants_approval_frac'
               ))





# PLOT
# ============================================
plotToFile(args$output)

# Reorder stages
df$stage <- factor(df$stage, levels = c('PC', 'P1', 'P2', 'P3', 'Ap'))

# Display fraction as percentage
df$frac <- df$frac * 100

generateColorscale <- colorRampPalette(c('lightgray', 'red'))
categories <- length(unique(df$frac))
boxcolors  <- rep(generateColorscale(categories), categories)

plot(df$entries ~ interaction(df$frac, df$stage),
     boxfill = boxcolors,
     las     = 2,
     xlab    = '',
     main    = paste('Total entries per Stage Specific Grant Fraction', args$label),
     ylab    = 'Entries per run',
     ylim    = buildLim(NULL, args$ylim)
     )

mtext('In cases where grants in other stages fund 0%.')

group <- seq(from = categories + 0.5,
             to = categories * length(unique(df$stage)),
             by = categories)
abline(h = NULL, v = group, col = 'darkgray', lty = 'solid', lwd = '2')
