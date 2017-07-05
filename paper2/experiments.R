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
  names(df)[names(df) == 'interventions_tot_size'] <- 'pull_size'
  names(df)[names(df) == 'intervention']           <- 'pull'
  names(df)[names(df) == 'grants_avg_frac']        <- 'push'

  print('Making grants binary')
  df$push <- ifelse(df$push > 0, TRUE, FALSE)

  print('Counting entries')
  df <- ddply(df, c(
                    'RUN',
                    'push',
                    'pull',
                    'pull_size'
                    ), summarise,
              pois  = countCompletes(proj_state),
              projs = length(unique(PROJ))
              )
  return(df)
}
df <- getSet(args$input,
             args$cache,
             'experiments.csv',
             prepare,
             c('RUN',
               'PROJ',
               'grants_avg_frac',
               'intervention',
               'interventions_tot_size',
               'proj_state'
               ))






# PLOT
# ==================
print('Plotting')

rangeA <- range(0.5, 3)
rangeB <- range(3, 8)
rangeC <- range(8, 10)

combinationsOfRecipients <- list(c('A','B','C'), c('A','B'), c('A'))
combinationsOfPush <- c('YES', 'NO', 'BOTH')
combinationsOfPull <- c('FDMER', 'PDMER')

colors <- colorRampPalette(c('chartreuse2', 'cyan3', 'violet'))(3)

for(recipients in combinationsOfRecipients) {
  for(psh in combinationsOfPush) {
    for(pll in combinationsOfPull) {

      sub <- df
      print('Restructure from TRUE/FALSE to number')
      sub$push <- ifelse(sub$push, 6, 8)
      pshN <- ifelse(psh=='YES', 6, ifelse(psh=='NO', 8, 2))

      # Create subset
      grants <- ifelse(psh=='YES', '_GRANTS', ifelse(psh=='NO', '_NOGRANTS', ''))
      name <- paste(paste(recipients, collapse='-'), '_', pll, grants, sep='')
      print(paste('Create subset', name))
      sub <- subset(sub, sub$push %% pshN == 0 & sub$pull == pll)

      print('Calculate mean likelihoods with intervention')
      sub$pois <- sub$pois / sub$projs * 100


      print('Duplicate data into groups and derive estimated pois')
      sub1 <- data.frame()
      sub2 <- data.frame()

      gc()
      print('Creating A')
      if ('A' %in% recipients) {
        setA <- sub
        setA$group <- 'A'
        setA$pois <- setA$pois * mean(rangeA)
        sub1 <- rbind(sub1, setA)
      } else {
        setA <- subset(sub, sub$pull_size == 0)
        setA$group <- 'A'
        setA$pois <- setA$pois * mean(rangeA)
        sub2 <- rbind(sub2, setA)
      }

      gc()
      print('Creating B')
      if ('B' %in% recipients) {
        setB <- sub
        setB$group <- 'B'
        setB$pois <- setB$pois * mean(rangeB)
        sub1 <- rbind(sub1, setB)
      } else {
        setB <- subset(sub, sub$pull_size == 0)
        setB$group <- 'B'
        setB$pois <- setB$pois * mean(rangeB)
        sub2 <- rbind(sub2, setB)
      }

      gc()
      print('Creating C')
      if ('C' %in% recipients) {
        setC <- sub
        setC$group <- 'C'
        setC$pois <- setC$pois * mean(rangeC)
        sub1 <- rbind(sub1, setC)
      } else {
        setC <- subset(sub, sub$pull_size == 0)
        setC$group <- 'C'
        setC$pois <- setC$pois * mean(rangeC)
        sub2 <- rbind(sub2, setC)
      }


      gc()
      print('Merge datasets')
      both  <- rbind(sub1, sub2)


      # Prepare counts and visuals
      nSub1 <- length(unique(sub1$group))
      nSub2 <- length(unique(sub2$group))
      yRange <- c(0, 43)
      #yRange <- c(min(both$poi), max(both$poi))
      yticks <- seq(min(yRange), max(yRange), by = 2)



      print('Plot')
      path <- makePath(args$output, name, 'pdf')
      print(paste('Will plot to', path))
      if (nrow(sub2) == 0) {
        layout(matrix(c(1), nrow = 1, byrow = TRUE))
        pdf(path, width = 9, height = 9, pointsize=11)
      } else if (nSub1 == 1) {
        pdf(path, width = 9, height = 9, pointsize=16)
        layout(matrix(c(1,1,1,2,2,2,2,2,2,2,2,2,2), nrow = 1, byrow = TRUE))
      } else if (nSub1 == 2) {
        pdf(path, width = 9, height = 9, pointsize=16)
        layout(matrix(c(1,1,1,2,2,2,2,2,2,2,2,2,2), nrow = 1, byrow = TRUE))
        layout(matrix(c(1,1,2,2,2,2,2,2,2,2), nrow = 1, byrow = TRUE))
      }

      if (nrow(sub2) > 0) {
        boxcolors <- rep(tail(colors, nSub2), nSub1)
        plot(sub2$pois  ~ as.factor(sub2$group),
             boxfill = boxcolors,
             axes    = FALSE,
             main    = '',
             ylim    = yRange,
             ylab    = '',
             xlab    = '',
             las     = 2
             )

        mtext('No pull', side=1, cex=0.75)
        mtext('Entries Per Run', side = 2, line = -6.5, cex = 0.75)
        abline(h=yticks, col='lightgray', lty=3)  # lines
      }

      boxcolors <- rep(head(colors, nSub1, nSub1))
      plot(sub1$pois  ~
           interaction(
                       sub1$group,
                       as.factor(sub1$pull_size)
                       ),
           boxfill = boxcolors,
           axes    = FALSE,
           yaxt    = 'n',
           frame.plot = FALSE,
           main    = 'Derived number of antibiotics per run over 30 years',
           ylim    = yRange,
           ylab    = '',
           xlab    = '',
           las     = 2
           )

      if (nrow(sub2) == 0) {
        mtext('Entries Per Run', side = 2, line = 2.5, cex = 1)
      }


      sizes  <- sort(unique(sub1$pull_size))
      pulls  <- sort(unique(sub1$pull))
      nSizes <- length(sizes)

      axis(side=2, las = 2, at = yticks, lwd=0) # y
      abline(h=yticks, col='lightgray', lty=3)  # lines

      ## x axis
      offset = 1 / nSub1 - 1
      start = nSub1
      finish = nSizes * nSub1
      step = nSub1
      axis(side=1, at = seq(start + offset, finish + offset, by=step), labels = sizes, lwd = 0, las=2)

      # Group colored reference lines
      for (grp in unique(both$group)) {
        d <- subset(both, both$group == grp & both$pull_size == 0)
        colorIndex <- which(c('A', 'B', 'C') == grp)
        abline(h=mean(d$pois), col=colors[colorIndex], lty=1, lwd=1.5)
      }

      # Vertical (group) lines
      offset = (nSizes * nSub1) / (nSizes * nSub1) / 2
      group <- seq(start + offset, finish + offset, by=step)
      abline(h = NULL, v = group, col = 'lightgray', lty = 'solid')

      # Legend
      legend('top',
             c('A', 'B', 'C'),
             pch = 15, col = c('chartreuse2', 'cyan3', 'violet'),
             box.col = 'white',
             bg  = 'white',
             cex = 1.3,
             yjust = 10
             )

      # Legend 2
      legend('topleft',
             c(
               'A mean (no pull)',
               'B mean (no pull)',
               'C mean (no pull)'
               ),
             col = c('chartreuse2', 'cyan3', 'violet'),
             lty = 1,
             lwd = 2,
             box.col = 'white',
             bg  = 'white',
             cex = 1.3
             )


    }
  }
}
