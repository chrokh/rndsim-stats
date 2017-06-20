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
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))






# PREPARE
# ============================================
revenueBinSize <- 500
discountRateBinSize <- 2
prepare <- function(df) {

  print('Rename parameters')
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'
  names(df)[names(df) == 'proj_tot_cash'] <- 'cash'
  names(df)[names(df) == 'inv_rate'] <- 'rate'
  names(df)[names(df) == 'orgs_infcap_thresh'] <- 'thresh'

  print('Round project revenues')
  df$cash <- roundToNearest(df$cash, revenueBinSize)

  print('Round investor rates')
  df$rate <- roundToNearest(df$rate * 100, discountRateBinSize)

  print('Counting entries per group')
  df <- ddply(df,
              c('intervention',
                'thresh',
                'rate',
                'cash',
                'intervention_size'),
              summarise,
              tot_pois  = countCompletes(proj_state),
              projs     = length(unique(PROJ)))


  return(df)
}
df <- getSet(args$input,
             args$cache,
             'likelihood-vs-thresh.csv',
             prepare,
             c('RUN',
               'PROJ',
               'intervention',
               'interventions_tot_size',
               'proj_state',
               'proj_tot_cash',
               'inv_rate',
               'orgs_infcap_thresh'
               ))








# PLOT FUNCTIONS
# ============================================
generateColorscale <- function(n) {
  colorRampPalette(c('red', 'orange', 'green'))(n)
}

findMeans <- function(df) {
  df <- ddply(df, c('thresh', 'intervention_size'), summarise,
               tot_pois = sum(tot_pois),
               projs    = sum(projs))
  df$pois <- df$tot_pois / df$projs * 100

  colorscale <- generateColorscale(length(unique(df$intervention_size)))

  interventions  <- sort(unique(df$intervention_size))
  df$color  <- colorscale[findInterval(df$intervention_size, interventions)]

  return(df)
}

doPlot <- function(df, context, name) {
  tbl <- xtabs(df$pois ~ df$intervention_size + df$thresh)
  barplot(tbl, beside = TRUE,
          xlab = 'BP Threshold',
          ylab = 'Mean Likelihood of Entry (%)',
          ylim = c(min(context$pois), max(context$pois) + 1),
          col = generateColorscale(length(unique(df$intervention_size)))
          )

  half1 <- subset(df, df$intervention_size <= mean(df$intervention_size))
  half2 <- subset(df, df$intervention_size > mean(df$intervention_size))

  legend('top',
         levels(factor(half1$intervention_size)),
         pch = 19, col = half1$color, bty = 'n', title='',
         horiz = TRUE)

  legend('top',
         levels(factor(half2$intervention_size)),
         pch = 19, col = half2$color, bty = 'n', title='',
         inset = c(0, .055),
         horiz = TRUE)

  mtext(name)
}






# PLOT
# ============================================

plotToLandscape(args$output)
layout(matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))

scenario <- function(df) {
  return(subset(df, df$rate >= 20 & df$cash <= 1500))
}

sub1 <- subset(df, df$intervention == 'FDMER')
sub2 <- subset(df, df$intervention == 'PDMER')
sub3 <- scenario(sub1)
sub4 <- scenario(sub2)

sub1 <- findMeans(sub1)
sub2 <- findMeans(sub2)
sub3 <- findMeans(sub3)
sub4 <- findMeans(sub4)

all <- rbind(sub1, sub2, sub3, sub4)

doPlot(sub1, all, 'Full Delinkage (FD)')
doPlot(sub2, all, 'Partial Delinkage (PD)')
doPlot(sub3, all, 'FD (Revenues <= 1.5B + VC Discount Rate 20-30%)')
doPlot(sub4, all, 'PD (Revenues <= 1.5B + VC Discount Rate 20-30%)')

mtext('Mean Likelihood of Market Entry vs BP Threshold', outer=TRUE,  cex=1, line=-2, font=2)
mtext('Mean Likelihood of Market Entry vs BP Threshold For Suggested Scenario', outer=TRUE,  cex=1, line=-28.7, font=2)

