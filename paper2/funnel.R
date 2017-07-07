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
    for (psh in sort(unique(df$push))) {
      before <- subset(df, df$push < psh & df$phase == ph)
      after  <- subset(df, df$push >= psh & df$phase == ph)
      this   <- subset(df, df$push == psh & df$phase == ph)
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




# PLOT
# ==================
preparePull <- function(df, mertype) {

  print('Subset on push = 0')
  df <- subset(df, df$push == 0)

  print(paste('Subset on mer =', mertype))
  df <- subset(df, df$mer == mertype)

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








# PLOT
# ===========================================

path   = makePath(args$output, 'funnel', 'pdf')
pdf(path, width = 11, height = 17, pointsize=15)
layout(matrix(c(1,2,3), nrow=3, byrow=TRUE))


doPlot <- function(tbl, name, labels) {
  print('Will plot')

  colors = rainbow(length(labels))
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
          xaxt   = 'n',
          xlab   = 'Likelihood (%)'
          )

  mtext(title, cex=1.05)

  xmarks <- seq(0, 100, by=5)
  xticks <- seq(0, 1,   by=0.05)
  axis(1, at=xticks, labels=xmarks)


  legend('bottomright',
         title   = name,
         legend  = labels,
         box.lwd = NA,
         pch     = 15,
         cex     = 1.25,
         col     = colors)
}

df$pushPercentage <- df$push * 100
doPlot(preparePush(df), 'GRANTS %', sort(unique(df$pushPercentage)))
doPlot(preparePull(df, 'FDMER'), 'FDMER', sort(unique(df$pull)))
doPlot(preparePull(df, 'PDMER'), 'PDMER', sort(unique(df$pull)))


