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


  print('Subsetting on scenario')
  df <- subset(df,
               df$thresh >= 200 &
                 df$thresh <= 500 &
                 df$rate >= 0.2 &
                 df$cash <= 1500)

  print('Counting entries')
  df <- ddply(df, c(
                    'RUN',
                    'intervention',
                    'interventions_tot_size'
                    ), summarise,
              pois  = countCompletes(proj_state),
              projs = length(unique(PROJ))
              )

  return(df)
}

df <- getSet(args$input,
             args$cache,
             'entries-vs-intervention.csv',
             prepare,
             c('RUN',
               'TICK',
               'PROJ',
               'intervention',
               'interventions_tot_size',
               'proj_state',
               'proj_tot_cash',
               'orgs_infcap_thresh',
               'inv_rate'
               ))





# PREPARE PLOT
# ============================================
df$pois <- df$pois / df$projs * 100




# PLOT
# ============================================

plotToLandscape(args$output)

# Rename intervention names
df$intervention <- mapvalues(df$intervention,
          from = c('FDMER', 'PDMER'),
          to   = c('FD', 'PD'))

generateColorscale <- colorRampPalette(c('wheat', 'wheat4'))
categories <- 2
boxcolors  <- rep(generateColorscale(categories), categories)

plot(df$pois  ~
     interaction(
                 df$intervention,
                 as.factor(df$interventions_tot_size)
                 ),
     boxfill = boxcolors,
     main    = 'Likelihood of Market Entry vs Intervention Size (Fully and Partially Delinked MER)',
     yaxt    = 'n',
     ylab    = 'Likelihood of Market Entry (%)',
     xlab    = '',
     las     = 2,
     main    = ''
     )

yticks <- seq(round(min(df$pois)), round(max(df$pois)), by = 1)
axis(side=2, col='black', las = 2, at = yticks)
abline(h=yticks, col='gray', lty=3)

# Subtitle
mtext('BP Threshold 200-500 + VC DR 20-30% + Revenues <= 1.5B')

group <- seq(from = categories + 0.5,
             to = categories * (length(unique(df$interventions_tot_size))),
             by = categories)

abline(h = NULL, v = group, col = 'black', lty = 'solid')


