INPUT=${1%/}
OUTPUT=${2%/}
CACHE=${3%/}

echo ''
echo 'Example:   all.sh input.csv output.pdf cache'
echo ''
echo "Input file:        $INPUT"
echo "Output directory:  $OUTPUT"
echo "Cache directory:   $CACHE"
echo ''
echo 'Press any key to confirm...'
read

rscript 2017-06-21/likelihood-vs-cash.R -i $INPUT -o $OUTPUT/likelihood-vs-cash.pdf -c $CACHE
rscript 2017-06-21/likelihood-vs-discount.R -i $INPUT -o $OUTPUT/likelihood-vs-discount.pdf -c $CACHE
rscript 2017-06-21/likelihood-vs-thresh.R -i $INPUT -o $OUTPUT/likelihood-vs-thresh.pdf -c $CACHE
rscript 2017-06-21/entries-vs-intervention.R -i $INPUT -o $OUTPUT/entries-vs-intervention.pdf -c $CACHE
rscript 2017-06-21/cumulative-entries.R -i $INPUT -o $OUTPUT/cumulative-entries.pdf -c $CACHE
#rscript 2017-06-21/entries-vs-discovery.R -i $INPUT -o $OUTPUT/entries-vs-discovery.pdf -c $CACHE
