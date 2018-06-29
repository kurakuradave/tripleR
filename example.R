### source the tripleR.R file
source('./tripleR.R')

### create an instance, passing in relevant arguments.
### see function "initialize" in tripleR.R for available parameters.
### at the minimum, you must specify details of the docker container to be used
### in this example using the image "kurakuradave/rarazon:0.01"
rrr <- RRR( "kurakuradave", "rarazon", "0.01" )

### there are two kinds of paralel execution:
### 1. run slightly different analytical procedures on the SAME input data
### 2. run exact same procedure on a large input data, first dividing them into SEGMENTS
### so there are two functions available, runEach() and runSplitN()

### let's look at runEach() first
### specify the intended R script containing the analytical procedures,
### the name of the input file (NOTE: this MUST be supplied even if the
### analytical procedure does not require an input file),
### and also the arguments to the parameters, like so:
myScript <- "dockerRunif.R"
inFile <- "thisData.rds"
jobs <- list( c("101", "50", "100"),
              c("101", "0.5", "1" ),
              c("101", "3", "7")
             )
### then create the final list of arguments that will be passed into runEach()
### this final list merges all the information above, adding name of output file
myDockerArgs <- lapply(jobs, function(x){
    c(x, inFile, rrr$makeOutFile( myScript, myInFile, unlist(x) ) )
})
### and finally, call runEach like so:
res <- rrr$runEach( myScript, myDockerArgs )

### rrr will run the "dockerRunif.R" script in paralel, each container running one job with its own unique arguments and output file.
### after starting the containers, rrr will continuously loop to check whether the containers had completed their job, and merge the output of each container into one data frame and returns it (into res)

### res is a list containing two elements:
### the first element is a string describing the outcome of the paralel execution ("COMPLETE/TIMED OUT")
### the second element contains all available outputs, access with res[[2]]
res[[1]]
dim( res[[2]] )

### checkout dockerRunif.R for details on what the script does

### Now lets look at runSplitN()
### Essentially this simply comprise of:
### - splitting the input data into n segments,
### - save each segment into the temp directory
### - call runEach(), using the names of each segment as input file
### Only that these steps are performed AUTOMATICALLY by rrr.
### When all jobs are done, rrr will merge the output from each segments and return it as one data frame
### exact example coming soon

