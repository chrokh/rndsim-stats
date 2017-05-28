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
colorscale <- function(df) {
  rev(brewer.pal(nlevels(factor(df$proj_group)), "Set1"))
}




# PREPARE
# ============================================
cols <- c('RUN', 'proj_group', 'interventions_tot_size', 'proj_is_at_poi')
df <- getSet(args$input, args$cache, 'entries-by-intervention-and-group.csv',
             function(df) {
               df <- ddply(df, c('RUN', 'proj_group', 'interventions_tot_size'), summarise,
                           tot_pois = countEntries(proj_is_at_poi))
               return(df)
             }, cols)





# PLOT
# ============================================
plotToFile(args$output)
df$color   <- colorscale(df)[factor(df$proj_group)]
categories <- length(unique(df$proj_group))
boxcolors  <- rep(head(colorscale(df), categories), categories)
plot(df$tot_pois  ~ interaction(df$proj_group, df$interventions_tot_size),
     boxfill = boxcolors,
     main    = paste('Total entries per group and intervention size', args$label),
     ylab    = 'Number of entries',
     xlab    = '',
     las     = 2,
     ylim    = buildLim(NULL, args$ylim)
     )

pos_after_group <- seq(from = nlevels(factor(df$proj_group)) + 0.5,
                       to = (nlevels(factor(df$proj_group)) * length(unique(df$interventions_tot_size))),
                       by = nlevels(factor(df$proj_group)))
abline(h = NULL, v = pos_after_group, col = 'darkgray', lty = 'solid')

grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)

legend('topleft', levels(factor(df$proj_group)), pch = 15, col = df$color, bty = 'n', title="Type")
