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
                             default ='plot.png',
                             help    ='output file name [default= %default]',
                             metavar ='file'),
                 make_option(
                             c('-c', '--cache'),
                             default = 'cache.csv',
                             help    = 'cache file [default= %default]',
                             metavar = 'file')
                 ),
            function(args) !is.null(args$input))







# PREPARE
# ============================================
prepare <- function(df) {

  print('Rename columns')
  names(df)[names(df) == 'interventions_tot_size'] <- 'intervention_size'
  names(df)[names(df) == 'grants_avg_frac'] <- 'grants'

  print('Counting entries')
  df <- ddply(df, c(
                    'RUN',
                    'grants',
                    'intervention',
                    'intervention_size'
                    ), summarise,
              p1s   = countInStage('Phase1')(proj_stage_group),
              p2s   = countInStage('Phase2')(proj_stage_group),
              pois  = countCompletes(proj_state),
              projs = length(unique(PROJ))
              )

  return(df)
}

df <- getSet(args$input,
             NULL,
             args$cache,
             prepare,
             c('RUN',
               'PROJ',
               'grants_avg_frac',
               'intervention',
               'interventions_tot_size',
               'proj_stage_group',
               'proj_state'
               ))




# PLOT: LIKELIHOOD ~ INTERVENTION + SIZE
# ============================================
plot1 <- function(df) {

  # TODO: Is this really needed?
  # The intention is to remove the duplicate entries due to grants.
  df <- ddply(df, c('RUN', 'intervention', 'intervention_size'), summarise,
              pois = sum(pois),
              projs = sum(projs))

  df$pois <- df$pois / df$projs * 100

  sizes <- sort(unique(df$intervention_size))
  types <- sort(unique(df$intervention))


  df$color <- ifelse(df$intervention == 'FDMER', fdColor, pdColor)
  colors <- unique(df$color)
  generateColorscale <- colorRampPalette(colors)
  boxcolors  <- rep(generateColorscale(length(types)), length(types))

  # Rename intervention names
  df$intervention <- mapvalues(df$intervention,
                               from = c('FDMER', 'PDMER'),
                               to   = c('FD', 'PD'))
  plot(df$pois  ~
       interaction(
                   df$intervention,
                   as.factor(df$intervention_size)
                   ),
       boxfill = boxcolors,
       main    = 'Likelihood of Market Approval Per Run',
       axes    = FALSE,
       ylab    = 'Likelihood (%) Per Run',
       xlab    = 'Pull (million $)',
       las     = 2,
       main    = '',
       frame.plot = FALSE
       )


  # y axis
  yticks <- seq(round(min(df$pois)), round(max(df$pois)), by = 0.5)
  axis(side=2, las = 2, at = yticks, lwd=0)

  # x axis
  axis(side=1, at = seq(1.5,25, by=2), labels = sizes, lwd = 0)

  # Horizontal lines
  abline(h=yticks, col='lightgray', lty=3)

  group <- seq(from = length(types) + 0.5,
               to = length(types) * (length(unique(df$intervention_size))),
               by = length(types))

  # Vertical (group) lines
  abline(h = NULL, v = group, col = 'black', lty = 'solid')
}





# PLOT: MEAN LIKELIHOOD ~ SIZE
# ============================================
plot2 <- function(df) {
  df$grants <- ifelse(df$grants > 0, TRUE, FALSE)

  df <- ddply(df, c('grants', 'intervention', 'intervention_size'), summarise,
              pois = sum(pois),
              projs = sum(projs))

  df$pois <- df$pois / df$projs * 100

  interventions <- sort(unique(df$intervention))
  grants        <- sort(unique(df$grants))

  ltypes <- c(1:length(grants))

  # Prepare colors
  df$color <- ifelse(df$intervention == 'FDMER', fdColor, pdColor)
  df$lty   <- ltypes[as.numeric(as.factor(df$grants))]

  plot(df$pois ~ df$intervention_size,
       pch = 16,
       col     = df$color,
       axes    = FALSE,
       main    = 'Mean Likelihood of Market Approval',
       ylab    = 'Mean Likelihood (%)',
       xlab    = 'Pull (million $)',
       frame.plot = FALSE)



  # Define tick marks
  yticks <- seq(round(min(df$pois), 1), round(max(df$pois), 1), by = 0.5)
  xticks <- sort(unique(df$intervention_size))

  # Axes
  axis(1, xticks, las=2)
  axis(2, yticks, las=2)

  # Horizontal lines
  abline(h=yticks, v=xticks, col='lightgray', lty=3)

  for (gr in grants) {
    for (intr in interventions) {
      sub <- subset(df, df$intervention == intr & df$grants == gr)
      lines(sub$pois ~ sub$intervention_size,
            lty = sub$lty,
            col = sub$color)
    }
  }
}




# PLOT: DELTAS
# ============================================
plot3 <- function(df) {
  df$grants <- ifelse(df$grants > 0, TRUE, FALSE)

  df <- ddply(df, c('grants', 'intervention', 'intervention_size'), summarise,
              pois = sum(pois),
              projs = sum(projs))

  df$pois <- df$pois / df$projs * 100

  interventions <- sort(unique(df$intervention))
  grants        <- sort(unique(df$grants))

  addDeltas <- function(df) {
    sizes <- sort(unique(df$intervention_size))
    new <- data.frame()
    last = mean(subset(df, df$intervention_size == min(df$intervention_size))$pois)
    for (size in sizes) {
      sub <- subset(df, df$intervention_size == size)
      current = mean(sub$pois)
      sub$delta <- current - last
      last = current
      new <- rbind(new, sub)
    }
    new
  }

  # CALCULATE DELTAS
  all <- data.frame()
  for (grnt in grants) {
    for (intr in interventions) {
      sub <- subset(df, df$intervention == intr & df$grants == grnt)
      all <- rbind(all, addDeltas(sub))
    }
  }
  df <- all

  ltypes <- c(1:length(grants))

  # Prepare colors
  df$color <- ifelse(df$intervention == 'FDMER', fdColor, pdColor)
  df$lty   <- ltypes[as.numeric(as.factor(df$grants))]

  plot(df$delta ~ df$intervention_size,
       pch     = 16,
       col     = df$color,
       las     = 1,
       main    = 'Improvement of Mean Likelihood of Approval',
       ylab    = 'Improvement (percentage points)',
       xlab    = 'Pull (million $)',
       axes    = FALSE,
       frame.plot = FALSE)

  # Define tick marks
  yticks <- seq(round(min(df$delta), 1), round(max(df$delta), 1), by = 0.2)
  xticks <- sort(unique(df$intervention_size))

  # Axes
  axis(1, xticks, las=2)
  axis(2, yticks, las=2)

  # Horizontal lines
  abline(h=yticks, v=xticks, col='lightgray', lty=3)

  for (gr in grants) {
    for (intr in interventions) {
      sub <- subset(df, df$intervention == intr & df$grants == gr)
      lines(sub$delta ~ sub$intervention_size,
            lty = sub$lty,
            col = sub$color)
    }
  }

}




# PLOT: HEATMAP
# ============================================
plot4 <- function(df, heatMapColors, name) {

  df <- ddply(df, c('intervention_size', 'grants'), summarise,
              p1s   = sum(p1s),
              p2s   = sum(p2s),
              projs = sum(projs),
              pois  = sum(pois))

  print('Recomputing entries to %')
  df$pois <- df$pois / df$projs * 100
  df$p1s <- df$p1s / df$projs * 100
  df$p2s <- df$p2s / df$projs * 100

  print('Reformat from long to wide')
  tbl <- xtabs(pois ~ intervention_size + grants, df)
  tblP1s <- xtabs(p1s ~ intervention_size + grants, df)
  tblP2s <- xtabs(p2s ~ intervention_size + grants, df)

  legendNumColors <- 12
  legendColors <- heatMapColors(legendNumColors)
  z = matrix(1:legendNumColors, nrow=1)
  x = 1
  y = seq(min(df$pois), max(df$pois), len=length(legendColors))
  image(x, y, z,
        col  = legendColors,
        las  = 2,
        xaxt = 'n',
        xlab = '',
        ylab = 'Mean Likelihood (%)')

  num <- length(unique(df$grants)) * length(unique(df$intervention_size))
  colors <- heatMapColors(num)
  image(as.matrix(tbl),
        main  = paste('Push vs Pull (', name, ')', sep=''),
        ylab  = 'Push (% of PC, P1, P2 funded)',
        xlab  = 'Pull (million $)',
        xaxt  = 'n',
        yaxt  = 'n',
        col   = colors,
        frame.plot = FALSE
        )

  # Convinience
  sizes  = sort(unique(df$intervention_size))
  grants = sort(unique(df$grants))

  # Print value on every tile on tile
  numXs <- length(unique(df$intervention_size))
  numYs <- length(unique(df$grants))
  xBinSize <- 1/(numXs-1)
  yBinSize <- 1/(numYs-1)
  xs <- seq(from=0, to=1, by=xBinSize)
  ys <- seq(from=0, to=1, by=yBinSize)

  mat <- as.matrix(tbl)
  dimnames(mat) <- NULL
  matP1 <- as.matrix(tblP1s)
  dimnames(matP1) <- NULL
  matP2 <- as.matrix(tblP2s)
  dimnames(matP2) <- NULL

  xIndex <- 1
  for (x in xs) {
    yIndex <- 1
    for (y in ys) {

      # Extract likelihood of market entry
      likelihood = mat[xIndex, yIndex] / 100
      percentage = round(likelihood * 100, 1)

      # Calculate pull spend
      size = sizes[xIndex]
      pullSpend = likelihood * size

      # Extract likelihood of PC, P1, P2 entry
      likelihoodPC = 1
      likelihoodP1 = matP1[xIndex, yIndex] / 100
      likelihoodP2 = matP2[xIndex, yIndex] / 100

      # Setup average phase grant costs
      costPC = 21.1
      costP1 = 24
      costP2 = 24.55

      # Calculate push spend
      frac = grants[yIndex]
      pushSpend =
        likelihoodPC * costPC * frac +
        likelihoodP1 * costP1 * frac +
        likelihoodP2 * costP2 * frac

      # Print total spend
      cost = round(pullSpend + pushSpend, 1)
      offset = - 1 / length(ys) / 3.5 # offset downwards slightly
      text(x, y + offset, cost, cex = 0.7)

      # Print likelihood of entry
      offset = 1 / length(ys) / 7 # offset upwards slightly
      text(x, y + offset, percentage, cex = 0.9)


      yIndex = yIndex + 1
    }
    xIndex = xIndex + 1
  }

  # x axis
  sizes  <- sort(unique(df$intervention_size))
  nSizes <- length(sizes)
  axis(1,
       lwd = 1,
       las = 2,
       at = seq(from=0, to=1, by=(1/(nSizes-1))), labels=sizes)

  # y axis
  sizes  <- sort(unique(df$grants))
  nSizes <- length(sizes)
  axis(2,
       at = seq(from=0, to=1, by=(1/(nSizes-1))), labels=sizes,
       lwd = 1,
       las = 2)

  # y axis (grant size estimation per project)
  sizes  <- sort(unique(round(df$grants * (costPC + costP1 + costP2))))
  nSizes <- length(sizes)
  axis(4,
       at = seq(from=0, to=1, by=(1/(nSizes-1))),
       labels=sizes,
       lwd = 1,
       las = 2)

}













# CONFIG
# ============================================
# ============================================
# # # # # # # # # # # # # # # # # # # # # # ##

numHeatMapColors = 100
fdHeatMapColors = colorRampPalette(c('white', 'indianred1'))
pdHeatMapColors = colorRampPalette(c('white', 'dodgerblue1'))
fdColor = 'indianred1'
pdColor = 'dodgerblue1'

# # # # # # # # # # # # # # # # # # # # # # ##
# ============================================
# ============================================




# PREPARE PAGE
# ============================================
plotToLandscape(args$output)
layout(matrix(c(
                1,1,1,1,1,2,2,2,2,2,
                1,1,1,1,1,3,3,3,3,3,
                4,5,5,5,5,6,7,7,7,7), nrow = 3, byrow = TRUE))

plot1(df)
plot2(df)
plot3(df)
plot4(subset(df, df$intervention == 'FDMER'), fdHeatMapColors, 'Full Delinkage')
plot4(subset(df, df$intervention == 'PDMER'), pdHeatMapColors, 'Partial Delinkage')

