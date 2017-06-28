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
                             help    ='output folder [default= %default]',
                             metavar ='path')
                 ),
                 function(args) !is.null(args$input))


if (args$input == args$output) {
  print('ERROR: Input file is the same as output... you probably don\'t want this.')
  exit()
}


# Read file
df <- load(args$input)


csv <- function(df, name) {
  output <- paste(args$output, '/', name, '.csv', sep='')
  print(output)
  ensurePathExists(args$output)
  print(paste('Writing to', args$output))
  write.csv(file = output, x = df)
}


head(df)

sub <- subset(df, df$proj_tot_cash <= 1000)
csv(sub, 'market-geq-1000')

sub <- subset(df, df$inv_rate >= 0.28)
csv(sub, 'vc-geq-28')

sub <- subset(df, df$orgs_infcap_thresh == 500)
csv(sub, 'bp-eq-500')


sub <- subset(df,
              df$proj_tot_cash <= 1000 &
                df$inv_rate >= 28 &
                df$orgs_infcap_thresh == 500)
csv(sub, 'market-geq-1000_vc-geq-28_bp-eq-500')

