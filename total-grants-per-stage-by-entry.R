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
                             metavar = 'float'),
                 make_option(
                             c('--proj_group'),
                             default = NULL,
                             help    = 'subset on project group [default= %default]',
                             metavar = 'string')
                 ),
            function(args) !is.null(args$input))





# PREPARE
# ============================================

prepare <- function(df) {

  if (!is.null(args$proj_group)) {
    print('(Subsetting by proj group passed as argument)')
    df <- subset(df, df$proj_group == args$proj_group)
    print('WARN: Subsetted data set will be written to cache!')
  }

  print('(Subsetting on only projects that received grants)')
  df <- subset(df, df$proj_grants_accumulated > 0)

  print('Building Group1: All projects that reached market')
  grp1 <- only_reached_market(df)

  print('Building Group2: All projects that did NOT reach market')
  grp2 <- only_did_not_reach_market(df)


  calculate_total_grants <- function(df) {
    print('(Subsetting on only PreClinical, Phase1, Phase2)')
    df <- subset(df, df$proj_stage_group %in% c('PreClinical', 'Phase1', 'Phase2'))

    print('(Grouping by project)')
    df <- ddply(df, c('RUN', 'PROJ', 'proj_stage_group'),
                summarise,
                proj_grants = max(proj_grants_accumulated))

    print('(Grouping by run and stage group)')
    df <- ddply(df, c('RUN', 'proj_stage_group'),
                summarise,
                tot_grants = sum(proj_grants))

    return (df)
  }

  grp1 <- calculate_total_grants(grp1)
  grp2 <- calculate_total_grants(grp2)

  print('Adding identifying columns to groups')
  grp1$group = 'Yes'
  grp2$group = 'No'

  print('Vertically merge groups')
  df <- rbind(grp1, grp2)

  return(df)
}
df <- getSet(args$input,
             args$cache,
             'total-grants-per-stage-by-entry.csv',
             prepare,
             c('RUN',
               'PROJ',
               'proj_group',
               'proj_stage_group',
               'proj_is_at_poi',
               'proj_grants_accumulated'))





# PLOT
# ============================================
plotToFile(args$output)

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
     df$tot_grants,
     main  = paste('Total grants paid per run and stage', args$label),
     xlab = '',
     ylab = 'Total Grants Paid Per Run (up to P2)',
     ylim = buildLim(NULL, args$ylim),
     las  = 2,
     )
