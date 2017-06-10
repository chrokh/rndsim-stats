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
    print(paste('Restructuring column ', column))
    grp <- df
    names(grp)[names(grp) == column] <- 'rate'
    grp$rate <- roundToNearest(grp$rate, 0.05)
    grp <- ddply(grp,
                  c('RUN', 'rate'),
                  summarise,
                  entries = countCompletes(proj_state))
    grp$stage <- nicename
    return(grp)
  }

  df <- rbind(
              do(df, 'inv_preclinical_rate', 'PC'),
              do(df, 'inv_phase1_rate',      'P1'),
              do(df, 'inv_phase2_rate',      'P2'),
              do(df, 'inv_phase3_rate',      'P3'),
              do(df, 'inv_approval_rate',    'Ap')
              )

  return(df)
}

df <- getSet(args$input,
             args$cache,
             'broad-entries-by-investment-rates.csv',
             prepare,
             c('RUN',
               'proj_state',
               'inv_preclinical_rate',
               'inv_phase1_rate',
               'inv_phase2_rate',
               'inv_phase3_rate',
               'inv_approval_rate'
               ))





# PLOT
# ============================================
plotToFile(args$output)

df$stage <- factor(df$stage, levels = c('PC', 'P1', 'P2', 'P3', 'Ap'))

generateColorscale <- colorRampPalette(c('lightgray', 'red'))
categories <- length(unique(df$rate))
boxcolors  <- rep(generateColorscale(categories), categories)

plot(df$entries ~ interaction(df$rate, df$stage),
     boxfill = boxcolors,
     las     = 2,
     xlab    = '',
     main    = paste('Total entries per Stage Specific VC\'s Discount Rates', args$label),
     ylab    = 'Entries per run',
     ylim    = buildLim(NULL, args$ylim)
     )

mtext('Not controlled for VC discount rates in other stages.')

group <- seq(from = categories + 0.5,
             to = categories * length(unique(df$stage)),
             by = categories)
abline(h = NULL, v = group, col = 'darkgray', lty = 'solid', lwd = '2')
