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





# PREPARE
# ============================================

prepare <- function(df) {

  print('(subsetting on only preclinical, phase1, phase2)')
  df <- subset(df,
               df$proj_stage_group == 'PreClinical' |
                 df$proj_stage_group == 'Phase1' |
                 df$proj_stage_group == 'Phase2')

  print('Building Group1: Grants spent')
  grp1 <- df
  grp1$proj_spend <- df$proj_grants_spent

  print('Building Group2: Capital spent')
  grp2 <- df
  grp2$proj_spend <- df$proj_capital_spent


  calculate_mean_spend <- function(df) {

    print('(Grouping by project)')
    df <- ddply(df, c('RUN', 'PROJ', 'proj_stage_group', 'proj_spend'),
                summarise,
                max_proj_spend = max(proj_spend))


    print('(Grouping by run and stage group)')
    df <- ddply(df, c('RUN', 'proj_stage_group'),
                summarise,
                mean_proj_spend = mean(max_proj_spend))

    return (df)
  }

  grp1 <- calculate_mean_spend(grp1)
  grp2 <- calculate_mean_spend(grp2)

  print('Adding identifying columns to groups')
  grp1$group = 'Grants'
  grp2$group = 'Costs'

  print('Vertically merge groups')
  df <- rbind(grp1, grp2)

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'mean-grants-capital-split-per-stage.csv',
             prepare,
             c('RUN',
               'PROJ',
               'proj_stage_group',
               'proj_grants_spent',
               'proj_capital_spent'))





# PLOT
# ============================================
plotToFile(args$output)
head(df)

print('Renaming factors')
df$proj_stage_group <- revalue(df$proj_stage_group,
                               c(
                                 'PreClinical'='PC',
                                 'Phase1'='P1',
                                 'Phase2'='P2'))

print('Reordering factors')
df$proj_stage_group <-
  factor(df$proj_stage_group,
         levels = c('PC', 'P1', 'P2'))

plot(
     interaction(
                 df$group,
                 df$proj_stage_group
                 ),
     df$mean_proj_spend,
     main  = paste('Cumulative mean grants/capital costs split per run and stage', args$label),
     xlab = '',
     ylab = 'Mean cumulative grants/capital',
     ylim = buildLim(NULL, args$ylim),
     las  = 2,
     )
