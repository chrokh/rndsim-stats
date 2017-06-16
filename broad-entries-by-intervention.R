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
colorscale <- function(df) {
  rev(brewer.pal(nlevels(factor(df$proj_group)), "Set1"))
}




# PREPARE
# ============================================
prepare <- function(df) {

  df <- ddply(df, c('RUN', 'intervention', 'interventions_tot_size'), summarise,
              tot_pois  = countCompletes(proj_state))

  return(df)
}

df <- getSet(args$input,
             args$cache,
             'broad-entries-by-intervention.csv',
             prepare,
             c('RUN',
               'TICK',
               'intervention',
               'interventions_tot_size',
               'proj_state'
               ))





# PLOT
# ============================================
plotToFile(args$output)
layout(matrix(c(1, 2, 2, 2, 2), nrow = 1, byrow = TRUE))




# Group observations
grp1 <- subset(df, df$intervention == 'FDMER' & df$interventions_tot_size > 0)
grp1$intervention <- 'FD'

grp2 <- subset(df, df$intervention == 'PDMER' & df$interventions_tot_size > 0)
grp2$intervention <- 'PD'

base <- subset(df, df$interventions_tot_size == 0)
base$intervention <- 'None'

grps <- rbind(grp1, grp2)




# Calculate sample sizes
print('Calculating mean sample count')
samples <- ddply(df, c('intervention', 'interventions_tot_size'), summarise, n = length(RUN))
meanN <- mean(samples$n)



# Plot baseline
plot(base$tot_pois  ~
     interaction(
                 as.factor(base$interventions_tot_size),
                 base$intervention
                 ),
     boxfill = c('peachpuff'),
     main    = '',
     ylab    = '',
     xlab    = paste('Mean N per group:', round(meanN, 2)),
     las     = 2,
     ylim    = range(min(grps$tot_pois), max(grps$tot_pois))
     )
mtext('No Intervention')



# Plot interventions
generateColorscale <- colorRampPalette(c('steelblue1', 'steelblue4'))
categories <- 2
boxcolors  <- rep(generateColorscale(categories), categories)

plot(grps$tot_pois  ~
     interaction(
                 grps$intervention,
                 as.factor(grps$interventions_tot_size)
                 ),
     boxfill = boxcolors,
     main    = paste('Total entries per Intervention Size', args$label),
     ylab    = 'Number of entries',
     xlab    = '',
     las     = 2,
     ylim    = range(min(grps$tot_pois), max(grps$tot_pois))
     )
mtext('Partial and Full Delinkage')

group <- seq(from = categories + 0.5,
             to = categories * (length(unique(df$interventions_tot_size)) - 1),
             by = categories)
abline(h = NULL, v = group, col = 'black', lty = 'solid')




# Print sample sizes
top <- max(grps$tot_pois)
samples <- subset(samples, samples$interventions_tot_size > 0)
samples <- samples[order(samples$interventions_tot_size),]
n <- 1
for (sample in samples$n) {
  text(x = n, y = top, labels = paste('N =', sample))
  n <- n + 1
}

