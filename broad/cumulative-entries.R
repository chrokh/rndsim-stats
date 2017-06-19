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
                             default ='plot',
                             help    ='output file base [default= %default]',
                             metavar ='file'),
                 make_option(
                             c('-f', '--format'),
                             default ='pdf',
                             help    ='output file format [default= %default]',
                             metavar ='string'),
                 make_option(
                             c('-c', '--cache'),
                             default = '.',
                             help    = 'cache folder [default= %default]',
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))




# CONFIG
# ============================================
generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))



# PREPARE
# ============================================
prepare <- function(df) {

  print('Rename columns')
  names(df)[names(df) == 'orgs_infcap_thresh'] <- 'thresh'
  names(df)[names(df) == 'inv_rate'] <- 'rate'
  names(df)[names(df) == 'grants_tot_size'] <- 'grants'

  print('Round investor rates')
  df$rate <- roundToNearest(df$rate * 100, 2)

  # Cumulate entries per tick
  all <- data.frame()
  for (tick in unique(df$TICK)) {

    print(paste('Grouping tick', tick))
    single <- subset(df, df$TICK <= tick)

    single <- ddply(single,
                    c(
                      'RUN',
                      'intervention',
                      'interventions_tot_size',
                      'grants',
                      'thresh',
                      'rate'
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
               'grants_tot_size',
               'orgs_infcap_thresh',
               'inv_rate'
               ))




# PLOTTING FUNCTIONS
# ============================================

prepareSubset <- function(df) {
  df <- ddply(df, c('RUN', 'year', 'interventions_tot_size'),
        summarise,
        num_pois = sum(num_pois))

  df <- ddply(df, c('year', 'interventions_tot_size'),
        summarise,
        mean_pois = mean(num_pois))

  return(df)
}

plotSubset <- function(df, context, title) {
  interventions    <- unique(df$interventions_tot_size)
  numInterventions <- length(interventions)
  colorscale       <- generateColorscale(numInterventions)

  df$color  <- colorscale[findInterval(df$interventions_tot_size, interventions)]
  plot(df$mean_pois ~ df$year,
       main = '',
       col  = df$color,
       pch  = 19,
       xlab = 'Year',
       ylab = 'Mean Cumulative Entries',
       xlim = c(min(df$year), max(df$year) + 2),
       ylim = c(min(context$mean_pois), max(context$mean_pois)),
       xaxt='n'
       )
  axis(1, at = seq(from = 0, to = (max(df$year) + 1), by = 1))
  grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)
  legend('topleft', levels(factor(df$interventions_tot_size)), pch = 19, col = df$color, bty = 'n')
  sub <- subset(df, df$year == max(df$year))
  text(sub$year, sub$mean_pois, sub$interventions_tot_size, cex=0.8, pos=4, font=2, col=sub$color)

  for (bin in interventions) {
    sub <- subset(df, df$interventions_tot_size == bin)
    lines(sub$mean_pois ~ sub$year, col = sub$color)
  }

  mtext(title)
}






# PLOT SUBSETS
# ============================================

plotToPortrait(paste(args$output, '1.', args$format, sep = ''))
layout(matrix(c(1,2,3,4,5,6,7,8), nrow = 4, byrow = FALSE))


# Cutoff points for subsetting
cutThresh <- 500
cutRate   <- 24

# Subset using cutoffs
print('Create FD subsets')
sub1 <- subset(df, df$intervention == 'FDMER')
sub2 <- subset(sub1, sub1$thresh >= cutThresh)
sub3 <- subset(sub1, sub1$rate >= cutRate)
sub4 <- subset(sub1, sub1$thresh >= cutThresh & sub1$rate >= cutRate)
print('Create PD subsets')
sub5 <- subset(df, df$intervention == 'PDMER')
sub6 <- subset(sub5, sub5$thresh >= cutThresh)
sub7 <- subset(sub5, sub5$rate >= cutRate)
sub8 <- subset(sub5, sub5$thresh >= cutThresh & sub5$rate >= cutRate)

# Prepare for plotting
print('Count entries in subsets 1-4')
sub1 <- prepareSubset(sub1)
sub2 <- prepareSubset(sub2)
sub3 <- prepareSubset(sub3)
sub4 <- prepareSubset(sub4)
print('Count entries in subsets 5-8')
sub5 <- prepareSubset(sub5)
sub6 <- prepareSubset(sub6)
sub7 <- prepareSubset(sub7)
sub8 <- prepareSubset(sub8)

# Merge so plotting function can find where to do ylim
all <- rbind(sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8)

# Plot
plotSubset(sub1, all, 'All Observations (FD)')
plotSubset(sub2, all, 'Threshold >= 500M (FD)')
plotSubset(sub3, all, 'Discount Rate >= 24% (FD)')
plotSubset(sub4, all, 'Threshold + Discount Rate (FD)')
plotSubset(sub5, all, 'All Observations (PD)')
plotSubset(sub6, all, 'Threshold >= 500M (PD)')
plotSubset(sub7, all, 'Discount Rate >= 24% (PD)')
plotSubset(sub8, all, 'Threshold + Discount Rate (PD)')

# Print top title
mtext('Cumulative Mean Entries Per Run (Various subsets)', outer=TRUE,  cex=1, line=-2)






# PLOT GRANTS/NO GRANTS
# ============================================

plotToPortrait(paste(args$output, '2.', args$format, sep = ''))
layout(matrix(c(1,2,3,4,5,6), nrow = 3, byrow = FALSE))


# Subset using cutoffs
print('Create FD subsets')
sub1 <- subset(df, df$intervention == 'FDMER')
sub2 <- subset(sub1, sub1$grants > 0)
sub3 <- subset(sub1, sub1$grants == 0)
print('Create PD subsets')
sub4 <- subset(df, df$intervention == 'PDMER')
sub5 <- subset(sub4, sub4$grants > 0)
sub6 <- subset(sub4, sub4$grants == 0)

# Prepare for plotting
print('Count entries in subsets 1-3')
sub1 <- prepareSubset(sub1)
sub2 <- prepareSubset(sub2)
sub3 <- prepareSubset(sub3)
print('Count entries in subsets 4-6')
sub4 <- prepareSubset(sub4)
sub5 <- prepareSubset(sub5)
sub6 <- prepareSubset(sub6)

# Merge so plotting function can find where to do ylim
all <- rbind(sub1, sub2, sub3, sub4, sub5, sub6)

# Plot
plotSubset(sub1, all, 'Full Delinkage')
plotSubset(sub2, all, 'Full Delinkage + Grants')
plotSubset(sub3, all, 'Full Delinkage + No Grants')
plotSubset(sub4, all, 'Partial Delinkage')
plotSubset(sub5, all, 'Partial Delinkage + Grants')
plotSubset(sub6, all, 'Partial Delinkage + No Grants')

# Print top title
mtext('Cumulative Mean Entries Per Run (Grants / No Grants)', outer=TRUE,  cex=1, line=-2)
