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
  names(df)[names(df) == 'orgs_infcap_thresh'] <- 'thresh'
  names(df)[names(df) == 'inv_rate'] <- 'rate'
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'
  names(df)[names(df) == 'proj_tot_cash'] <- 'cash'

  print('Subsetting on scenario')
  df <- subset(df,
               df$thresh >= 200 &
                 df$thresh <= 500 &
                 df$rate >= 0.2 &
                 df$cash <= 1500)

  print('Cumulate entries by tick')
  all <- data.frame()
  for (tick in unique(df$TICK)) {

    print(paste('Grouping tick', tick))
    single <- subset(df, df$TICK <= tick)

    single <- ddply(single,
                    c(
                      'RUN',
                      'intervention',
                      'intervention_size'
                      ),
                    summarise,
                    num_pois = countCompletes(proj_state))

    single$year <- tick / 12
    all <- rbind(all, single)
  }

  return(all)

}
df <- getSet(args$input,
             args$cache,
             'cumulative-entries.csv',
             prepare,
             c('RUN',
               'TICK',
               'proj_state',
               'intervention',
               'interventions_tot_size',
               'orgs_infcap_thresh',
               'proj_tot_cash',
               'inv_rate'
               ))




# PLOTTING FUNCTIONS
# ============================================

prepareSubset <- function(df) {
  df <- ddply(df, c('RUN', 'year', 'intervention_size'),
        summarise,
        num_pois = sum(num_pois))

  df <- ddply(df, c('year', 'intervention_size'),
        summarise,
        mean_pois = mean(num_pois))

  return(df)
}

plotSubset <- function(df, context, title) {
  interventions    <- unique(df$intervention_size)
  numInterventions <- length(interventions)
  generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))
  colorscale       <- generateColorscale(numInterventions)

  df$color  <- colorscale[findInterval(df$intervention_size, interventions)]
  plot(df$mean_pois ~ df$year,
       main = 'Cumulative Mean Entries Per Run',
       col  = df$color,
       pch  = 19,
       xlab = 'Year',
       ylab = 'Mean Cumulative Entries',
       xlim = c(min(df$year), max(df$year) + 2),
       ylim = c(min(context$mean_pois), max(context$mean_pois)),
       xaxt = 'n',
       yaxt = 'n'
       )

  xticks <- seq(round(min(context$year)), round(max(context$year) + 1), by = 1)
  yticks <- seq(round(min(context$mean_pois)), round(max(context$mean_pois)), by = 1)
  axis(1, at = xticks, las = 2)
  axis(2, at = yticks, las = 2)

  abline(h=yticks, v=xticks, col='darkgray', lty=3)

  legend('topleft', levels(factor(df$intervention_size)), pch = 19, col = df$color, bty = 'n')
  sub <- subset(df, df$year == max(df$year))
  text(sub$year, sub$mean_pois, sub$intervention_size, cex=0.8, pos=4, font=2, col=sub$color)

  for (bin in interventions) {
    sub <- subset(df, df$intervention_size == bin)
    lines(sub$mean_pois ~ sub$year, col = sub$color, lwd = 2)
  }

  mtext(title)
}






# PLOT
# ============================================

plotToPortrait(args$output)
layout(matrix(c(1,2), nrow = 2, byrow = FALSE))


# Subset for MERs
sub1 <- subset(df, df$intervention == 'FDMER')
sub2 <- subset(df, df$intervention == 'PDMER')

# Prepare for plotting
sub1 <- prepareSubset(sub1)
sub2 <- prepareSubset(sub2)

# Merge so plotting function can find where to do ylim
all <- rbind(sub1, sub2)

# Plot
plotSubset(sub1, all, 'Full Delinkage (BP Threshold 200-500 + VC DR 20-30% + Revenues <= 1.5B)')
plotSubset(sub2, all, 'Partial Delinkage (BP Threshold 200-500 + VC DR 20-30% + Revenues <= 1.5B)')
