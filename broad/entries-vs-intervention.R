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




# CONFIG
# ============================================
colorscale <- function(df) {
  rev(brewer.pal(nlevels(factor(df$proj_group)), "Set1"))
}




# PREPARE
# ============================================
prepare <- function(df) {

  print('Rename columns')
  names(df)[names(df) == 'orgs_infcap_thresh'] <- 'thresh'
  names(df)[names(df) == 'inv_rate'] <- 'rate'
  names(df)[names(df) == 'proj_tot_cash'] <- 'cash'

  print('Round investor rates')
  df$rate <- roundToNearest(df$rate * 100, 2)

  print('Round project revenues')
  df$cash <- roundToNearest(df$cash, 500)

  df <- ddply(df, c(
                    'RUN',
                    'intervention',
                    'interventions_tot_size',
                    'cash',
                    'thresh',
                    'rate'
                    ), summarise,
              tot_pois  = countCompletes(proj_state))
  return(df)
}

df <- getSet(args$input,
             args$cache,
             'entries-vs-intervention.csv',
             prepare,
             c('RUN',
               'TICK',
               'intervention',
               'interventions_tot_size',
               'proj_state',
               'proj_tot_cash',
               'orgs_infcap_thresh',
               'inv_rate'
               ))





# PLOTTING FUNCTION
# ============================================

doPlot <- function(df, context, text) {
  generateColorscale <- colorRampPalette(c('steelblue1', 'steelblue4'))
  categories <- 2
  boxcolors  <- rep(generateColorscale(categories), categories)

  plot(df$tot_pois  ~
       interaction(
                   df$intervention,
                   as.factor(df$interventions_tot_size)
                   ),
       boxfill = boxcolors,
       ylab    = 'Number of entries',
       xlab    = '',
       las     = 2,
       ylim    = c(min(context$tot_pois), max(context$tot_pois))
       )

  mtext(text)

  group <- seq(from = categories + 0.5,
               to = categories * (length(unique(df$interventions_tot_size)) - 1),
               by = categories)
  abline(h = NULL, v = group, col = 'black', lty = 'solid')
}

preparePlot <- function(df) {
  df <- ddply(df, c(
                    'RUN',
                    'intervention',
                    'interventions_tot_size'
                    ), summarise,
              tot_pois  = sum(tot_pois))
  return(df)
}






# PLOT
# ============================================

plotToLandscape(args$output)
layout(matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))

# Rename intervention names
df$intervention <- mapvalues(df$intervention,
          from = c('FDMER', 'PDMER'),
          to   = c('FD', 'PD'))

# Cutoff points for subsetting
cutCash   <- 5000
cutThresh <- 500
cutRate   <- 24

# Subset using cutoffs
sub1 <- subset(df, df$cash <= cutCash)
sub2 <- subset(df, df$thresh >= cutThresh & df$rate >= cutRate)
sub3 <- subset(df, df$cash <= cutCash & df$thresh >= cutThresh & df$rate >= cutRate)

# Prepare for plotting
df <- preparePlot(df)
sub1 <- preparePlot(sub1)
sub2 <- preparePlot(sub2)
sub3 <- preparePlot(sub3)

# Merge so plotting function can find where to do ylim
all <- rbind(df, sub1, sub2, sub3)

# Plot
doPlot(df, all, 'All Observations')
doPlot(sub1, all, 'Total Projected Revenus <= 5B')
doPlot(sub2, all, 'BP Threshold >= 500M  +  VC Discount Rate >= 24%')
doPlot(sub3, all, 'Revenues <= 5B + Threshold >= 500M + Discount Rate >= 24%')

mtext('Entries Per Run, By Intervention (Partial and Full Delinkage)', outer=TRUE,  cex=1, line=-2)
