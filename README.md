# rndsim-stats

Plot scripts in R for rndsim.

# Usage

```
$ git clone https://github.com/chrokh/rndsim-stats.git
$ cd rndsim-stats
$ rscript <script.R> <--flag> <value> ...
```

The scripts are loading shared files using relative paths based on execution location. This means that you **must `cd` to the location of the script** before attempting to run it. I.e. you must run it from the directory in which it resides.

Different scripts may support different flags/arguments. Run the script with the `--help` or `-h` flag (or without any arguments) to get a list of flags available for that particular script.
