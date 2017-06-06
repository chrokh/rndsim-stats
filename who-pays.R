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


minus <- function(a, b) {
  if (a - b < 0) {
    return(0)
  } else {
    return(a - b)
  }
}


prepare <- function(df) {

  print('Subsetting on only reached market')
  df <- only_reached_market(df)

  print('Identifying vc spend')
  df$vc_spent <- ifelse(
                        (df$proj_partners == 0 & df$proj_owner_type == 'Smaller'),
                        df$proj_capital_spent, 0)

  df$bp_incl <- ifelse(
                       df$proj_owner_type == 'Larger',
                       df$proj_capital_spent, 0)

  df$partner_incl <- ifelse(
                             (df$proj_partners == 1 & df$proj_owner_type == 'Smaller'),
                             df$proj_capital_spent, 0)

  print('Grouping to find maxes')
  df <- ddply(df, c('RUN', 'PROJ'),
              summarise,
              max_grants_spent         = max(proj_grants_spent),
              max_vc_spent             = max(vc_spent),
              max_partner_spent_incl   = max(partner_incl),
              max_bp_spent_incl        = max(bp_incl))

  print('Grouping to find partner spend')
  df <- ddply(df,
              c('RUN',
                'PROJ',
                'max_vc_spent',
                'max_grants_spent',
                'max_bp_spent_incl',
                'max_partner_spent_incl'
                ),
              summarise,
              max_bp_spent      = minus(max_bp_spent_incl,      max_vc_spent),
              max_partner_spent = minus(max_partner_spent_incl, max_vc_spent))

  df <- ddply(df, c('RUN'), summarise,
              sum_vc_spent           = sum(max_vc_spent),
              sum_partner_spent      = sum(max_partner_spent),
              sum_bp_spent           = sum(max_bp_spent),
              sum_grants_spent       = sum(max_grants_spent))

  return(df)
}

df <- getSet(args$input,
             args$cache,
             'who-pays.csv',
             prepare,
             c('RUN',
               'PROJ',
               'proj_is_at_poi',
               'proj_partners',
               'proj_owner_type',
               'proj_stage_group',
               'proj_grants_spent',
               'proj_capital_spent'
               ))




# PLOT
# ============================================
plotToFile(args$output)

grp1 <- df
grp1$amount  <- grp1$sum_vc_spent
grp1$spender <- 'VC'

grp2 <- df
grp2$amount  <- grp2$sum_bp_spent
grp2$spender <- 'Exits'

grp3 <- df
grp3$amount  <- grp3$sum_grants_spent
grp3$spender <- 'Grants'

grp4 <- df
grp4$amount  <- grp4$sum_partner_spent
grp4$spender <- 'Partners'

df <- rbind(grp1, grp2, grp3, grp4)

boxcolors  <- rep(head(c('red', 'green', 'blue', 'yellow'), 4), 4)
boxplot(df$amount ~ df$spender,
        boxfill = boxcolors,
        las = 2,
        main = paste('Who pays for projects (that reach market)?', args$label),
        ylab = 'Total spend per run'
        )
