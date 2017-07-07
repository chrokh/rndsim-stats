source('shared.R')
library(tidyr)


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
                             default ='.',
                             help    ='output directory [default= %default]',
                             metavar ='folder'),
                 make_option(
                             c('-c', '--cache'),
                             default = '.',
                             help    = 'cache folder [default= %default]',
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))




# PREPARE
# ==================
prepare <- function(df) {

  print('Rename columns')
  names(df)[names(df) == 'grants_avg_frac']        <- 'push'
  names(df)[names(df) == 'interventions_tot_size'] <- 'pull'
  names(df)[names(df) == 'intervention']           <- 'mer'

  print('Counting entries')
  df <- ddply(df, c(
                    'push',
                    'pull',
                    'mer'
                    ), summarise,
              p0s   = countInStage('PreClinical')(proj_stage_group),
              p1s   = countInStage('Phase1')(proj_stage_group),
              p2s   = countInStage('Phase2')(proj_stage_group),
              p3s   = countInStage('Phase3')(proj_stage_group),
              p4s   = countInStage('Approval')(proj_stage_group),
              pois  = countCompletes(proj_state),
              projs = length(unique(PROJ))
              )
  return(df)
}
df <- getSet(args$input,
             args$cache,
             'funnel.csv',
             prepare,
             c('PROJ',
               'grants_avg_frac',
               'intervention',
               'interventions_tot_size',
               'proj_stage_group',
               'proj_state'
               ))





# SUBSET PREPARATION FUNCTIONS
# ==================
prepareSubset <- function(df, cols) {
  print(df)

  print('Re-aggregate')
  df <- ddply(df, cols, summarise,
        projs = sum(projs),
        p0s = sum(p0s),
        p1s = sum(p1s),
        p2s = sum(p2s),
        p3s = sum(p3s),
        p4s = sum(p4s),
        pois = sum(pois)
        )
  print(df)

  print('Compute likelihoods')
  df$p0s <- df$p0s / df$projs
  df$p1s <- df$p1s / df$projs
  df$p2s <- df$p2s / df$projs
  df$p3s <- df$p3s / df$projs
  df$p4s <- df$p4s / df$projs
  df$p5s <- df$pois / df$projs
  print(df)

  print('Drop unused columns')
  df <- subset(df, select = -c(projs, pois))
  print(df)

  return(df)
}


preparePush <- function(df) {

  print('Subset on pull size = 0')
  df <- subset(df, df$pull == 0)

  print('Prepare')
  df <- prepareSubset(df, c('push'))

  print('Transform wide to long')
  df <- gather(df, phase, likelihood, p0s:p5s)
  print(df)

  print('Remove pre-clinical')
  df <- subset(df, df$phase != 'p0s')
  print(df)

  new <- data.frame()
  for (ph in unique(df$phase)) {
    first <- subset(df, df$phase==ph & df$push==0)
    for (psh in sort(unique(df$push))) {
      before <- subset(df, df$phase==ph & df$push < psh)
      after  <- subset(df, df$phase==ph & df$push >= psh)
      this   <- subset(df, df$phase==ph & df$push == psh)
      last  = max(c(max(before$likelihood), 0))
      nxt   = min(this$likelihood)
      delta = max(nxt - last, 0)
      this$delta <- delta
      new <- rbind(new, this)
    }
  }
  df <- new
  print(df)

  print('Make factors')
  df$phase <- factor(df$phase,
                     levels = c(
                                'p5s',
                                'p4s',
                                'p3s',
                                'p2s',
                                'p1s'))
  print(df)

  print('Rename factors')
  df$phase <- revalue(df$phase, c('p1s'='Phase1', 'p2s'='Phase2', 'p3s'='Phase3', 'p4s'='Approval', 'p5s'='Market'))

  print('Convert to table')
  tbl <- xtabs(df$delta ~ df$push+ df$phase)
  print(tbl)

  tbl
}





prepareBasePush <- function(df) {

  print('Subset on pull size = 0')
  df <- subset(df, df$pull == 0 & df$push == 0)

  print('Prepare')
  df <- prepareSubset(df, c('push'))

  print('Transform wide to long')
  df <- gather(df, phase, likelihood, p0s:p5s)
  print(df)

  print('Remove pre-clinical')
  df <- subset(df, df$phase != 'p0s')
  print(df)

  df$delta <- df$likelihood

  print('Make factors')
  df$phase <- factor(df$phase,
                     levels = c(
                                'p5s',
                                'p4s',
                                'p3s',
                                'p2s',
                                'p1s'))
  print(df)

  print('Rename factors')
  df$phase <- revalue(df$phase, c('p1s'='Phase1', 'p2s'='Phase2', 'p3s'='Phase3', 'p4s'='Approval', 'p5s'='Market'))

  print('Convert to table')
  tbl <- xtabs(df$delta ~ df$push+ df$phase)
  print(tbl)

  tbl
}




prepareSimplePush <- function(df, minGrantFraction) {

  print('Subset on pull size = 0')
  df <- subset(df, df$pull == 0)

  print('Split into only either grants or no grants')
  df <- subset(df, df$push == 0 | df$push >= minGrantFraction)
  df$push <- ifelse(df$push == 0, 0, 1)

  print('Prepare')
  df <- prepareSubset(df, c('push'))

  print('Transform wide to long')
  df <- gather(df, phase, likelihood, p0s:p5s)
  print(df)

  print('Remove pre-clinical')
  df <- subset(df, df$phase != 'p0s')
  print(df)

  new <- data.frame()
  for (ph in unique(df$phase)) {
    no = subset(df, df$phase == ph & df$push==0)
    ys = subset(df, df$phase == ph & df$push==1)
    delta = max(ys$likelihood) - min(no$likelihood)
    no$delta <- no$likelihood
    ys$delta <- delta
    new <- rbind(new, no, ys)
  }
  df <- new
  print(df)

  print('Make factors')
  df$phase <- factor(df$phase,
                     levels = c(
                                'p5s',
                                'p4s',
                                'p3s',
                                'p2s',
                                'p1s'))
  print(df)

  print('Rename factors')
  df$phase <- revalue(df$phase, c('p1s'='Phase1', 'p2s'='Phase2', 'p3s'='Phase3', 'p4s'='Approval', 'p5s'='Market'))

  print('Convert to table')
  tbl <- xtabs(df$delta ~ df$push+ df$phase)
  print(tbl)

  tbl
}




# PLOT
# ==================
preparePull <- function(df, mertype) {

  print('Subset on push = 0')
  df <- subset(df, df$push == 0)

  print(paste('Subset on mer =', mertype))
  df <- subset(df, df$mer == mertype | df$pull == 0)

  print('Prepare')
  df <- prepareSubset(df, c('pull'))

  print('Transform wide to long')
  df <- gather(df, phase, likelihood, p0s:p5s)
  print(df)

  print('Remove pre-clinical')
  df <- subset(df, df$phase != 'p0s')
  print(df)

  new <- data.frame()
  for (ph in unique(df$phase)) {
    for (pll in sort(unique(df$pull))) {
      before <- subset(df, df$pull < pll & df$phase == ph)
      after  <- subset(df, df$pull >= pll & df$phase == ph)
      this   <- subset(df, df$pull == pll & df$phase == ph)
      last  = max(c(max(before$likelihood), 0))
      nxt   = min(this$likelihood)
      delta = max(nxt - last, 0)
      this$delta <- delta
      new <- rbind(new, this)
    }
  }
  df <- new
  print(df)

  print('Make factors')
  df$phase <- factor(df$phase,
                     levels = c(
                                'p5s',
                                'p4s',
                                'p3s',
                                'p2s',
                                'p1s'))
  print(df)

  print('Rename factors')
  df$phase <- revalue(df$phase, c('p1s'='Phase1', 'p2s'='Phase2', 'p3s'='Phase3', 'p4s'='Approval', 'p5s'='Market'))

  print('Convert to table')
  tbl <- xtabs(df$delta ~ df$pull+ df$phase)
  print(tbl)

  tbl
}








# PLOTTING FUNCTION
# ===========================================

doPlot <- function(tbl, name, labels) {
  print('Will plot')

  colors = colorRampPalette(c(
                             # 'gray80',
                              'cornsilk3',
                              'navajowhite',
                             # 'chocolate4',
                              'yellow',
                             # 'turquoise1',
                             # 'cyan',
                              'green3',
                              'forestgreen',
                              'darkgreen'
                              ))(length(labels))
  #colors = rainbow(length(labels))
  #colors  = terrain.colors(length(labels))
  #colors = colorRampPalette(c('bisque', 'brown2'))(length(labels))
  #colors = colorRampPalette(c('lightskyblue', 'dodgerblue4'))(length(labels))

  title = paste('Improvement in Likelihood of Phase Entry (',
                         name,
                         ')',
                         sep='')

  print(tbl)
  barplot(tbl,
          col    = colors,
          horiz  = TRUE,
          border = NA,
          main   = '',
          las    = 2,
          xlim   = c(0, .8), # Hard-coded
          xaxt   = 'n',
          xlab   = 'Likelihood (%)'
          )

  mtext(title, cex=1.05)

  xmarks <- seq(0, 100, by=5)
  xticks <- seq(0, 1,   by=0.05)
  axis(1, at=xticks, labels=xmarks)

  ## Vertical lines
  xminor <- seq(0.025, 1, by=0.025)
  xmajor <- seq(0, 1, by=0.05)
  abline(v=xminor, col='gray', lty=3)
  abline(v=xmajor, col='gray', lty=2)


  legend('bottomright',
         title   = name,
         legend  = labels,
         box.lwd = NA,
         pch     = 15,
         cex     = 1.25,
         col     = colors)
}









# PLOTTING FUNCTION
# ===========================================

df$pushPercentage <- df$push * 100

# Prepare plottable content
df.1 <- prepareSimplePush(df, 0.1)
df.2 <- preparePush(df)
df.3 <- preparePull(df, 'FDMER')
df.4 <- preparePull(df, 'PDMER')
#prepareBasePush(df)
#prepareSimplePush(df, 1)

# ONE PAGER
path   = makePath(args$output, 'funnel', 'pdf')
pdf(path, width = 15, height = 22, pointsize=15)
layout(matrix(c(1,2,3,4), nrow=4, byrow=TRUE))
doPlot(df.1, 'GRANTS FUNDING ANY %', c('No Grants', 'Grants'))
doPlot(df.2, 'GRANTS BY %',          sort(unique(df$pushPercentage)))
doPlot(df.3, 'FDMER',                sort(unique(df$pull)))
doPlot(df.4, 'PDMER',                sort(unique(df$pull)))

# SEPARATE PAGES
plotTo <- function(name) {
  path = makePath(args$output, name, 'pdf')
  pdf(path, width=15, height=7, pointsize=15)
}

plotTo('funnel-push-all')
doPlot(df.1, 'GRANTS FUNDING ANY %', c('No Grants', 'Grants'))

plotTo('funnel-push-each')
doPlot(df.2, 'GRANTS BY %', sort(unique(df$pushPercentage)))

plotTo('funnel-pull-fdmer')
doPlot(df.3, 'FDMER', sort(unique(df$pull)))

plotTo('funnel-pull-pdmer')
doPlot(df.4, 'PDMER', sort(unique(df$pull)))

