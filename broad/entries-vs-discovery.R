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
binSize <- 0.5
prepare <- function(df) {

  print('Rename columns')
  names(df)[names(df) == 'proj_type_discovery_rate'] <- 'discovery'
  names(df)[names(df) == 'grants_tot_size'] <- 'grants'

  print('Round discovery rates')
  df$discovery <- roundToNearest(df$discovery, binSize)

  print('Counting entries')
  df <- ddply(df, c(
                    'RUN',
                    'discovery',
                    'grants'
                    ), summarise,
              tot_pois  = countCompletes(proj_state))
  return(df)
}

df <- getSet(args$input,
             args$cache,
             'entries-vs-discovery.csv',
             prepare,
             c('RUN',
               'TICK',
               'proj_type_discovery_rate',
               'grants_tot_size',
               'proj_state'
               ))





# PLOTTING FUNCTION
# ============================================

plotMeans <- function(df, context, text) {
  generateColorscale <- colorRampPalette(c('steelblue1', 'steelblue4', 'azure3', 'azure4'))
  categories <- 2
  #boxcolors  <- rep(generateColorscale(categories * 2), categories * 2)
  df$color <- as.factor(df$grants)

  plot(df$pois ~ df$discovery,
       col     = df$color,
       #boxfill = boxcolors,
       ylab    = 'Mean Number of Market Entries Per Run',
       xlab    = 'Entry Rate',
       #las     = 2,
       ylim    = c(min(context$pois), max(context$pois))
       )

  mtext(text)

  for (bin in unique(df$grants)) {
    sub <- subset(df, df$grants == bin)
    lines(sub$pois ~ sub$discovery, col = sub$color)
  }
}

prepareMeans <- function(df) {
  df <- ddply(df, c(
                    'discovery',
                    'grants'
                    ), summarise,
              pois  = mean(tot_pois))
  return(df)
}

prepareTotals <- function(df) {
  df <- ddply(df, c(
                    'RUN',
                    'discovery',
                    'grants'
                    ), summarise,
              pois  = sum(tot_pois))
  return(df)
}

plotBoxes <- function(df, context, text) {
  generateColorscale <- colorRampPalette(c('steelblue1', 'steelblue4'))
  categories <- length(unique(df$grants))
  boxcolors  <- rep(generateColorscale(categories), categories)

  plot(df$pois ~ interaction(
                             as.factor(df$grants),
                             as.factor(df$discovery)
                             ),
       boxfill = boxcolors,
       ylab    = 'Number of Market Entries',
       xlab    = '',
       las     = 2,
       )

  mtext(text)

  group <- seq(from = categories + 0.5,
               to = categories * (length(unique(df$discovery))),
               by = categories)
  abline(h = NULL, v = group, col = 'black', lty = 'solid')
}






# PLOT
# ============================================

plotToLandscape(args$output)
layout(matrix(c(1,2), nrow = 2, byrow = TRUE))

sub <- prepareMeans(df)
plotMeans(sub, sub, 'No Intervention')

sub <- prepareTotals(df)
plotBoxes(sub, sub, 'No Intervention')


mtext('Market Entries By Entry Rate', outer=TRUE,  cex=1, line=-2)
