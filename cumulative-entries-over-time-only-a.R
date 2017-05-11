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
intervention_bins <- c(0, 100, 200, 500, 700, 1000, 1500, 2000, 2500, 3000, 4000)
colorscale  <- brewer.pal(length(intervention_bins), 'RdYlGn')




# PREPARE
# ============================================

count_entries <- function(proj_is_at_poi) {
  sum(ifelse(proj_is_at_poi == 'true', 1, 0))
}
prepare <- function(df) {

  # Subset to only A
  df <- subset(df, df$proj_group == 'A')

  # Bin interventions
  df <- binInterventions(df, intervention_bins)

  # Cumulate entries per tick
  all <- data.frame()
  for (tick in unique(df$TICK)) {
    print(paste('Grouping tick', tick))
    single <- subset(df, df$TICK <= tick)
    single <- ddply(single, c('RUN', 'intervention_bin_top'), summarise,
                    num_pois = count_entries(proj_is_at_poi))
    single$year <- tick / 12
    all <- rbind(all, single)
  }

  return(all)

}
cols <- c('RUN', 'TICK', 'proj_is_at_poi', 'interventions_tot_size', 'proj_group')
df <- getSet(args$input, args$cache, 'cumulative-entries-over-time-only-a.csv', prepare, cols)





# PLOT
# ============================================

plotToFile(args$output)
p <- ddply(df, c('year', 'intervention_bin_top'), summarise, mean_pois = mean(num_pois))
p$color  <- colorscale[findInterval(p$intervention_bin_top, unique(p$intervention_bin_top))]
plot(p$mean_pois ~ p$year,
     main = paste("Cumulative market entries (mean per run) per year", args$label),
     sub  = '(Only project group A)',
     col  = p$color,
     pch  = 19,
     xlab = "Year",
     ylab = "Mean cumulative market entries",
     xlim = c(min(p$year), max(p$year)),
     ylim = buildLim(NULL, args$ylim),
     xaxt="n"
     )
axis(1, at = seq(from = 0, to = (max(p$year)), by = 1))
grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)
legend('topleft', levels(factor(p$intervention_bin_top)), pch = 19, col = p$color, bty = 'n', title="Top")
sub <- subset(p, p$year == max(p$year))
text(sub$year, sub$mean_pois, sub$intervention_bin_top, cex=0.8, pos=4, font=2, col=sub$color)

for (bin in intervention_bins) {
  sub <- subset(p, p$intervention_bin_top == bin)
  lines(sub$mean_pois ~ sub$year, col = sub$color)
}
