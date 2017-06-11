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




# CONFIG
# ============================================
generateColorscale <- colorRampPalette(c('red', 'orange', 'green'))



# PREPARE
# ============================================
prepare <- function(df) {

  setnames(df, 'orgs_infcap_thresh', 'thresh')

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
                      'grants_preclinical_frac',
                      'grants_phase1_frac',
                      'grants_phase2_frac',
                      'thresh'
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
             'broad-cumulative-entries-over-time.csv',
             prepare,
             c('RUN',
               'TICK',
               'proj_state',
               'intervention',
               'interventions_tot_size',
               'grants_preclinical_frac',
               'grants_phase1_frac',
               'grants_phase2_frac',
               'orgs_infcap_thresh'
               ))




# PLOT
# ============================================
plotToPortrait(args$output)

layout(matrix(c(1,2,3,4,5,6), nrow = 3, byrow = TRUE))


prepareSingle <- function(df) {
  ddply(df, c('year', 'interventions_tot_size'),
        summarise,
        mean_pois = mean(num_pois))

}

plotSingle <- function(df) {
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
       ylim = c(bot, top),
       xaxt='n'
       )
  axis(1, at = seq(from = 0, to = (max(df$year) + 1), by = 1))
  grid(nx = 0, ny = NULL, col = 'darkgray', lty = 'dotted', equilogs = TRUE)
  legend('topleft', levels(factor(df$interventions_tot_size)), pch = 19, col = df$color, bty = 'n', title="Top")
  sub <- subset(df, df$year == max(df$year))
  text(sub$year, sub$mean_pois, sub$interventions_tot_size, cex=0.8, pos=4, font=2, col=sub$color)

  for (bin in interventions) {
    sub <- subset(df, df$interventions_tot_size == bin)
    lines(sub$mean_pois ~ sub$year, col = sub$color)
  }
}


nogrants <-
  subset(df, df$grants_preclinical_frac == 0 &
         df$grants_phase1_frac == 0 &
         df$grants_phase2_frac == 0)

threshstop <- 100
hithresh <- subset(df, df$thresh > threshstop)

df1 <- prepareSingle(subset(df, df$intervention == 'FDMER'))
df2 <- prepareSingle(subset(df, df$intervention == 'PDMER'))
df3 <- prepareSingle(subset(nogrants, nogrants$intervention == 'FDMER'))
df4 <- prepareSingle(subset(nogrants, nogrants$intervention == 'PDMER'))
df5 <- prepareSingle(subset(hithresh, hithresh$intervention == 'FDMER'))
df6 <- prepareSingle(subset(hithresh, hithresh$intervention == 'PDMER'))


all <- rbind(df1, df2, df3, df4, df5, df6)
top <- max(all$mean_pois)
bot <- min(all$mean_pois)


plotSingle(df1)
mtext('Full Delinkage')

plotSingle(df2)
mtext('Partial Delinkage')

plotSingle(df3)
mtext('Full Delinkage + No Grants')

plotSingle(df4)
mtext('Partial Delinkage + No Grants')

plotSingle(df5)
mtext(paste('Full Delinkage + Thresholds Over', threshstop))

plotSingle(df6)
mtext(paste('Partial Delinkage + Thresholds Over', threshstop))


mtext('Mean Market Entries Per Year', outer=TRUE,  cex=1, line=-2)


