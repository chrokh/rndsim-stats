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
                             c('-r', '--runs'),
                             default = NULL,
                             help    = 'number of runs to keep [default= %default]',
                             metavar = 'string')
                 ),
                 function(args) !is.null(args$input))




# Read file
df <- load(args$input)

# Subset on runs
df <- subset(df, df$RUN <= args$runs)

# Write shorter file
ensurePathExists(args$output)
print(paste('Writing to', args$output))
write.csv(file = args$output, x = df)

