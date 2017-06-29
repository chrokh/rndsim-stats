source('shared.R')
library(gplots)


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
  names(df)[names(df) == 'grants_avg_frac'] <- 'frac'
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'

  print('Counting entries')
  df <- ddply(df, c(
                    'frac',
                  #  'intervention',
                    'intervention_size'
                    ), summarise,
              pois  = countCompletes(proj_state),
              projs = length(unique(PROJ))
              )

  return(df)
}

df <- getSet(args$input,
             args$cache,
             # NOTE: Re-used cache
             'likelihood-heatmap.csv',
             prepare,
             c(
               'PROJ',
               'grants_avg_frac',
              # 'intervention',
               'interventions_tot_size',
               'proj_state'
               ))




# PREPARE PLOT
# ============================================
print('Recomputing market entries to %')
df$pois <- df$pois / df$projs * 100

print('Reformat from long to wide')
tbl <- xtabs(pois ~ intervention_size + frac, df)
tbl # for debugging

# PLOT
# ============================================

#plotToLandscape(args$output)
#heatmap.2(as.matrix(tbl),
#          dendrogram = c('none'),
#          main  = 'Push vs Pull',
#          Colv  = NA,
#          Rowv  = NA,
#          xlab  = 'Push (% funded)',
#          ylab  = 'Pull (Millino $)',
#          revC  = TRUE,
#          col   = colorRampPalette(c('white', 'darkred'))(n = 100),
#          )



plotToLandscape(args$output)
layout(matrix(c(1,2,2,2,2,2,2,2,2), nrow = 1, byrow = TRUE))


colorN = 100
colors = colorRampPalette(c('white', 'darkred'))(n = colorN)

z = matrix(1:100, nrow=1)
x = 1
y = seq(min(df$pois), max(df$pois), len=colorN)
image(x, y, z,
      col  = colors,
      xaxt = 'n',
      xlab = '',
      ylab = 'Mean Likelihood of Entry')



image(as.matrix(tbl),
          main  = 'Push vs Pull',
          ylab  = 'Push (Fraction funded in PC, P1, and P2)',
          xlab  = 'Pull (Million $)',
          xaxt  = 'n',
          yaxt  = 'n',
          col   = colors
          )

# x axis
sizes  <- sort(unique(df$intervention_size))
nSizes <- length(sizes)
axis(1, at = seq(from=0, to=1, by=(1/(nSizes-1))), labels=sizes)

# y axis
sizes  <- sort(unique(df$frac))
nSizes <- length(sizes)
axis(2, at = seq(from=0, to=1, by=(1/(nSizes-1))), labels=sizes)

