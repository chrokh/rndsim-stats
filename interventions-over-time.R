source('shared.R')



# ==================
# Arguments
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





# ==================
# Settings
# ==================

generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))





# ==================
# Prepare
# ==================

df <- getSet(args$input, args$cache, 'interventions-over-time.csv', function(df) {

  # Add years
  df$year <- df$TICK / 12

  # Group by run, year and intervention size
  df <- groupBy(df, c('RUN', 'year',  'interventions_tot_size'))

  # Group by year and intervention size, to calculate mean
  df <- ddply(df, c('year', 'interventions_tot_size'),
              summarise,
              mean_pois = mean(tot_pois))

  return(df)
})





# ==================
# Plot
# ==================

interventions    <- unique(df$interventions_tot_size)
numInterventions <- length(interventions)

colorscale <- generateColorscale(numInterventions)
df$color   <- colorscale[findInterval(df$interventions_tot_size, interventions)]

plotToFile(args$output)
plot(df$mean_pois ~ df$year,
     col  = df$color,
     pch  = 19,
     main = paste('Mean market entries per year', args$label),
     ylim = buildLim(NULL, args$ylim),
     xaxt = 'n', # Hide labels on x axis
     xlab = 'Years',
     ylab = 'Mean market entries'
     )

axis(1, at = seq(from = 0, to = (max(df$year)), by = 1))

for (bin in interventions) {
  sub <- subset(df, df$interventions_tot_size == bin)
  lines(sub$mean_pois ~ sub$year, col = sub$color)
}

legend('topleft', levels(factor(df$interventions_tot_size)), pch = 19, col = df$color, bty = 'n', title='Amount')

sub <- subset(df, df$year == max(df$year) | df$year == min(df$year) | (df$year) %% 10 == 0)
text(sub$year, sub$mean_pois, sub$interventions_tot_size, cex=0.9, pos=4, font=2, col=sub$color)
grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)
