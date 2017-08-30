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
                             default ='./',
                             help    ='output folder [default= %default]',
                             metavar ='folder'),
                 make_option(
                             c('-c', '--cache'),
                             default = '.',
                             help    = 'cache folder [default= %default]',
                             metavar = 'folder')
                 ),
            function(args) !is.null(args$input))




# PREPARE
# ============================================
prepare <- function(df) {
  subset(df, df$proj_stage_group == 'MarketEntry')
}

df <- getSet(args$input,
             args$cache,
             'sensitivity-on-entries.csv',
             prepare,
             c(
               'RUN',
               'intervention',
               'interventions_tot_size',
               'proj_stage_group',
               'proj_tot_cash',
               'proj_tot_cost',
               'proj_tot_prob',
               'orgs_infcap_thresh',
               'inv_rate',
               'proj_capital_accumulated',
               'proj_grants_accumulated'
               ))



plotY <- function(df, name, arg) {
  plotToFile(makePath(args$output, name, 'pdf'))
  plot(
       df[[arg]] ~ interaction(df$intervention, df$interventions_tot_size),
       las = 2,
       ylab = name,
       boxfill = c('darkgrey', 'lightblue')
       )
}

plotMany <- function(pre, df) {
  plotY(df, paste(pre, 'revenues', sep='-'),         'proj_tot_cash')
  plotY(df, paste(pre, 'cost', sep='-'),             'proj_tot_cost')
  plotY(df, paste(pre, 'probability', sep='-'),      'proj_tot_prob')
  plotY(df, paste(pre, 'threshold', sep='-'),        'orgs_infcap_thresh')
  plotY(df, paste(pre, 'discount rate', sep='-'),    'inv_rate')
  plotY(df, paste(pre, 'vc investments', sep='-'),   'proj_capital_accumulated')
  plotY(df, paste(pre, 'grants invested', sep='-'),  'proj_grants_accumulated')
}

print('Without subsetting')
plotMany('all', df)
plotMany('sub', subset(df,
                       df$orgs_infcap_thresh >= 200 &
                         df$orgs_infcap_thresh <= 500 &
                         df$inv_rate >= 0.18 &
                         df$proj_tot_cash <= 1500
                       ))

