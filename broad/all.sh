INPUT=${1%/}
OUTPUT=${2%/}
CACHE=${3%/}

echo ''
echo "Input file:        $INPUT"
echo "Output directory:  $OUTPUT"
echo "Cache directory:   $CACHE"
echo ''
echo 'Press any key to confirm...'
read

rscript broad/likelihood-vs-cost.R -i $INPUT -o $OUTPUT/likelihood-vs-cost.pdf -c $CACHE
rscript broad/likelihood-vs-cash.R -i $INPUT -o $OUTPUT/likelihood-vs-cash.pdf -c $CACHE
rscript broad/likelihood-vs-peak.R -i $INPUT -o $OUTPUT/likelihood-vs-peak.pdf -c $CACHE
rscript broad/likelihood-vs-roi.R -i $INPUT -o $OUTPUT/likelihood-vs-roi.pdf -c $CACHE
rscript broad/likelihood-vs-thresh.R -i $INPUT -o $OUTPUT/likelihood-vs-thresh -f pdf -c $CACHE
rscript broad/likelihood-vs-prob.R -i $INPUT -o $OUTPUT/likelihood-vs-prob.pdf -c $CACHE
rscript broad/likelihood-vs-discovery.R -i $INPUT -o $OUTPUT/likelihood-vs-discovery.pdf -c $CACHE
