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
generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))



# PREPARE
# ============================================

count_entries <- function(proj_is_at_poi) {
  sum(ifelse(proj_is_at_poi == 'true', 1, 0))
}
prepare <- function(df) {

  # Cumulate entries per tick
  all <- data.frame()
  for (tick in unique(df$TICK)) {

    print(paste('Grouping tick', tick))
    single <- subset(df, df$TICK <= tick)

    cols <- c('RUN', 'proj_group', 'interventions_tot_size')
    single <- ddply(single,
                    cols,
                    summarise,
                    num_pois = count_entries(proj_is_at_poi))

    single$year <- tick / 12
    all <- rbind(all, single)
  }

  return(all)

}
cols <- c('RUN', 'TICK', 'proj_group', 'proj_is_at_poi', 'interventions_tot_size')
df <- getSet(args$input, args$cache, 'cumulative-entries-over-time.csv', prepare, cols)





# PLOT
# ============================================
plotToFile(args$output)

if (!is.null(args$proj_group)) {
  print('Subset on project group')
  df <- subset(df, df$proj_group == args$proj_group)
} else {
  print('Sum up entries over project groups')
  df <- ddply(df,
              c('RUN', 'interventions_tot_size', 'year'),
              summarise,
              num_pois = sum(num_pois))
}

df <- ddply(df, c('year', 'interventions_tot_size'),
            summarise,
            mean_pois = mean(num_pois))

interventions    <- unique(df$interventions_tot_size)
numInterventions <- length(interventions)
colorscale       <- generateColorscale(numInterventions)

df$color  <- colorscale[findInterval(df$interventions_tot_size, interventions)]
plot(df$mean_pois ~ df$year,
     main = paste("Cumulative market entries (mean per run) per year", args$label),
     col  = df$color,
     pch  = 19,
     xlab = "Year",
     ylab = "Mean cumulative market entries",
     xlim = c(min(df$year), max(df$year)),
     ylim = buildLim(NULL, args$ylim),
     xaxt="n"
     )
axis(1, at = seq(from = 0, to = (max(df$year)), by = 1))
grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)
legend('topleft', levels(factor(df$interventions_tot_size)), pch = 19, col = df$color, bty = 'n', title="Top")
sub <- subset(df, df$year == max(df$year))
text(sub$year, sub$mean_pois, sub$interventions_tot_size, cex=0.8, pos=4, font=2, col=sub$color)

for (bin in interventions) {
  sub <- subset(df, df$interventions_tot_size == bin)
  lines(sub$mean_pois ~ sub$year, col = sub$color)
}
