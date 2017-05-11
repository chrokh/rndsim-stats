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
intervention_bins <- c(0, 100, 200, 500, 700, 1000, 1200, 1500, 1700, 2000, 2500, 3000, 4000)
colorscale <- function(df) {
  rev(brewer.pal(nlevels(factor(df$proj_group)), "Set1"))
}




# PREPARE
# ============================================
df <- getSet(args$input, args$cache, 'entries-by-intervention-and-group.csv',
             function(df) {
               df <- binInterventions(df, intervention_bins)
               df <- groupBy(df, c('RUN', 'proj_group', 'intervention_bin_top'))
               return(df)
             })





# PLOT
# ============================================
plotToFile(args$output)
df$color   <- colorscale(df)[factor(df$proj_group)]
categories <- length(unique(df$proj_group))
boxcolors  <- rep(head(colorscale(df), categories), categories)
plot(df$tot_pois  ~ interaction(df$proj_group, df$intervention_bin_top),
     boxfill = boxcolors,
     main    = paste('Total entries per group and intervention size', args$label),
     ylab    = 'Number of entries',
     xlab    = '',
     las     = 2,
     ylim    = buildLim(NULL, args$ylim)
     )

pos_after_group <- seq(from = nlevels(factor(df$proj_group)) + 0.5,
                       to = (nlevels(factor(df$proj_group)) * length(unique(df$intervention_bin_top))),
                       by = nlevels(factor(df$proj_group)))
abline(h = NULL, v = pos_after_group, col = 'darkgray', lty = 'solid')

grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)

legend('topleft', levels(factor(df$proj_group)), pch = 15, col = df$color, bty = 'n', title="Type")