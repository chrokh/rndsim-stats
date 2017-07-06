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

colors  <- colorRampPalette(c('chartreuse2', 'cyan3', 'violet'))(3)
colors2 <- colorRampPalette(c('chartreuse4', 'darkcyan', 'blueviolet'))(3)


# Function for recomputing means
prepareGroup <- function(data, grp, discRange) {
  copy <- data
  print('Recalculating means')
  copy$group <- grp
  copy$pois <- copy$pois / copy$projs
  copy$pois <- copy$pois * mean(discRange) * 12 * 30
  return(copy)
}

for(recipients in combinationsOfRecipients) {
  for(psh in combinationsOfPush) {
    for(pll in combinationsOfPull) {

      # Use alias
      sub <- df

      # Codify grants (from true false to numeric representation to allow three-valued logic)
      sub$push <- ifelse(sub$push, 6, 8)
      pshN <- ifelse(psh=='YES', 6, ifelse(psh=='NO', 8, 2))
      grants <- ifelse(psh=='YES', '_GRANTS', ifelse(psh=='NO', '_NOGRANTS', ''))

      # Set and print dataset name
      name <- paste(paste(recipients, collapse='-'), '_', pll, grants, sep='')
      print(paste('Create subset', name))

      # Create subset
      sub <- subset(sub, sub$push %% pshN == 0 & sub$pull == pll)

      print('Duplicate data into groups and derive estimated pois')
      sub1 <- data.frame()
      sub2 <- data.frame()
      for(g in c('A', 'B', 'C')) {
        gc()
        discRange = NULL
        if (g == 'A') discRange = rangeA
        if (g == 'B') discRange = rangeB
        if (g == 'C') discRange = rangeC

        print(paste('Creating', g, 'based on entry rate', mean(discRange)))
        if (g %in% recipients) {
          set <- sub
          sub1 <- rbind(sub1, prepareGroup(set, g, discRange))
        } else {
          set <- sub
          set <- subset(set, set$pull_size == 0)
          sub2 <- rbind(sub2, prepareGroup(set, g, discRange))
        }
      }


      gc()
      print('Merge datasets')
      both  <- rbind(sub1, sub2)


      # Prepare counts and visuals
      nSub1 <- length(unique(sub1$group))
      nSub2 <- length(unique(sub2$group))
      #yRange = c(0, 135)
      yRange = c(0, 82)
      #yRange <- c(min(both$poi), max(both$poi))
      yticks <- seq(roundToNearest(min(yRange), 5), max(yRange), by = 5)



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
        boxcolors  <- rep(tail(colors, nSub2), nSub1)
        boxcolors2 <- rep(tail(colors2, nSub2), nSub1)
        plot(sub2$pois  ~ as.factor(sub2$group),
             boxfill   = boxcolors,
             whiskcol  = 'darkgray',
             outcol    = 'darkgray',
             staplecol = 'darkgray',
             boxcol   = boxcolors2,
             axes    = FALSE,
             main    = '',
             ylim    = yRange,
             ylab    = '',
             xlab    = '',
             las     = 2
             )

        mtext('No Pull', side=1, cex=0.75)
        mtext('Market Approvals Per Run', side = 2, line = -6.5, cex = 0.75)
        abline(h=yticks, col='gray', lty=3)  # lines
      }

      boxcolors  <- rep(head(colors, nSub1, nSub1))
      boxcolors2 <- rep(head(colors2, nSub1, nSub1))
      plot(sub1$pois  ~
           interaction(
                       sub1$group,
                       as.factor(sub1$pull_size)
                       ),
           boxfill   = boxcolors,
           whiskcol  = 'darkgray',
           outcol    = 'darkgray',
           staplecol = 'darkgray',
           boxcol   = boxcolors2,
           axes    = FALSE,
           frame.plot = FALSE,
           main    = 'Derived Number of Antibiotics Per Run Over 30 years',
           ylim    = yRange,
           ylab    = '',
           xlab    = '',
           las     = 2
           )

      if (nrow(sub2) == 0) {
        mtext('Entries Per Run', side = 2, line = 2.5, cex = 1)
      }


      # Convenience
      sizes  <- sort(unique(sub1$pull_size))
      pulls  <- sort(unique(sub1$pull))
      nSizes <- length(sizes)

      # y axis
      axis(side=2, las = 2, at = yticks, lwd=0) # y
      abline(h=yticks, col='gray', lty=3)  # lines

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
        abline(h=mean(d$pois), col=colors[colorIndex], lty=1, lwd=2)
        #abline(h=median(d$pois), col=colors[colorIndex], lty=1, lwd=2)
      }

      # Vertical (group) lines
      offset = (nSizes * nSub1) / (nSizes * nSub1) / 2
      group <- seq(start + offset, finish + offset, by=step)
      abline(h = NULL, v = group, col = 'lightgray', lty = 'solid')

      # Legend 2
      par(xpd=TRUE) # Turn off clipping (for putting legend outside plot region)

      legend('top',
             c(
               'A     ',
               'B     ',
               'C     '
               ),
             inset = c(0, -0.05),
             col = colors,
             pch = 15,
             lwd = 2,
             box.col = NA,
             horiz = TRUE,
             cex = 1.3
             )

    }
  }
}
