### this script file is intended to be run in a docker container controlled by tripleR
### produce random uniform distributed numbers
### Arguments:
### howmany - how many random numbers to generate
### lowerBound - lower boundary of the range of uniform distribution
### upperBound - upper boundary
### inputFile - unused
### outputFile - what to name the output file


library(assertive)

outDir <- "/root/rrrTemp" ### IMPORTANT this must be defined manually (for now), if not, the file would be saved inside the docker container's filesystem and would get removed when the container is deleted by tripleR. the "rrrTemp" must match the temp directory used by rrr.

args = commandArgs(trailingOnly=TRUE)

assert_is_of_length( args, 5 )
assert_is_character( args[5] )
assert_is_not_null( args[5] )

ret <- runif( as.numeric( args[1] ), as.numeric( args[2] ), as.numeric( args[3] ) )

Sys.sleep(35) ### otherwise this script would complete in a blink

saveRDS(ret, paste0( outDir, "/", args[5] ))

