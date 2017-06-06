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
                             c('--only_entries'),
                             default = FALSE,
                             help    = 'subset on projects that reached market [default= %default]',
                             metavar = 'bool'),
                 make_option(
                             c('--ylimit'),
                             default = NULL,
                             help    = 'ylim max of plot [default= %default]',
                             metavar = 'float')
                 ),
            function(args) !is.null(args$input))



# PREPARATION SCRIPTS
# ============================================
minus <- function(a, b) {
  if (a - b < 0) {
    return(0)
  } else {
    return(a - b)
  }
}

prepare <- function(df) {

  if (args$only_entries) {
    print('Subsetting on only reached market')
    df <- only_reached_market(df)
  }

  print('Subsetting on only development stages')
  df <- only_before_market(df)

  prepareStage <- function(df, stage) {

    print(paste('Subsetting on stage ', stage))
    df <- subset(df, df$proj_stage_group == stage)

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

    df$stage <- stage

    return(df)
  }

  # Prepare stage by stage
  stages <- unique(df$proj_stage_group)
  all <- data.frame()
  for (stage in stages) {
    single <- prepareStage(df, stage)
    all <- rbind(all, single)
  }

  return(all)
}



# PREPARE
# ============================================
if (args$only_entries) {
  cache <- 'who-pays-for-entries-when.csv'
} else {
  cache <- 'who-pays-when.csv'
}
df <- getSet(args$input,
             args$cache,
             cache,
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
grp4$spender <- 'Prtnrs'

df <- rbind(grp1, grp2, grp3, grp4)

if (args$only_entries) {
  title <- paste('Who pays for projects (that reach market) and when?', args$label)
} else {
  title <- paste('Who pays for projects and when?', args$label)
}


df$stage <- droplevels(simplifyStages(factorizeStages(df$stage)))

boxcolors  <- rep(head(c('red', 'green', 'blue', 'yellow'), 4), 4)
boxplot(df$amount ~ interaction(df$spender, df$stage),
        boxfill = boxcolors,
        las = 2,
        main = title,
        ylab = 'Total cumulative spend per run per stage',
        ylim = buildLim(NULL, args$ylim)
        )

pos_after_group <-
  seq(
      from = 4 + 0.5,
      to = (nlevels(factor(df$stage)) * 4),
      by = 4
      )
abline(h = NULL, v = pos_after_group, col = 'black', lty = 'solid')
