### GUERILLA UNIT TESTING
### don't execute unless you are ABSOLUTELY sure what each line does
### you don't need this file at all 
### if you only want to use tripleR in your analysis workflow     
####################################################################

### START TEST

source('./tripleR.R')

### clear previous test rrr object
if( "rrr" %in% ls() ){
    rm("rrr")
}




### create new test object
rrr <- RRR("kurakuradave", "rarazon", "0.01", timeout_mins=1, check_secs=10)
"TripleR" %in% class( rrr )




### make and remove temp directory
print( paste( "Original condition: temp dir doesn't exist - ",
             FALSE == ( rrr$tempDir %in% list.files() )
             ))
rrr$mkdirTemp()
print( paste("creating temp dir, should now exist - ",
             TRUE == ( rrr$tempDir %in% list.files() )
            ))
rrr$rmdirTemp()
print( paste("deleted, shouldn't exist now - ",
            FALSE ==  (rrr$tempDir %in% list.files() )
           ))




### makeVolReference
rrr$makeVolReference()




### make docker image fullname
rrr$makeDImgFullname()




### make output filename
myScript <- "thisScript.R"
myInFile <- "thisData.rds"
myArgs <- c( "value1",
             "value2",
             "value3"
           )
rrr$makeOutFile( myScript, myInFile, myArgs )




### test runEach total timeout
myScript <- "thisScript.R"
inFile <- "thisData.rds"
jobs <- list( c("v1", "v2", "v3"),
              c("v4", "v2", "v3"),
              c("v5", "v2", "v3")
             )
myDockerArgs <- lapply(jobs, function(x){
                    c(x, inFile, rrr$makeOutFile( myScript, myInFile, unlist(x) ) )
                })
resFAIL <- rrr$runEach( myScript, myDockerArgs )




### test runEach successful completion, combine all completed files
a <- mtcars[1:10,]
b <- mtcars[11:20,]
c <- mtcars[21:30,]
rrr$mkdirTemp()
saveRDS(a, './rrrTemp/thisData.rds_thisScript.R_v1_v2_v3.rds')
saveRDS(b, './rrrTemp/thisData.rds_thisScript.R_v4_v2_v3.rds')
saveRDS(c, './rrrTemp/thisData.rds_thisScript.R_v5_v2_v3.rds')
resSUCCESS <- rrr$runEach( myScript, myDockerArgs )
print(resSUCCESS[[1]])
see <- resSUCCESS[[2]]
sum(mtcars[1:30,1] - see[,1]) == 0



### test runEach INCOMPLETE timed out
system2( "rm", "./rrrTemp/thisData.rds_thisScript.R_v5_v2_v3.rds" )
resINCOMPLETE <- rrr$runEach( myScript, myDockerArgs )
print(resINCOMPLETE[[1]])
see <- resINCOMPLETE[[2]]
sum(mtcars[1:20,1] - see[,1]) == 0



### test splitToDisk
rrr$splitToDisk_test( mtcars, 3 )


### test makeSplitNArgs
rrr$makeSplitNArgs_test( "thisScript.R", c("V8", "V9", "V10") )




### test FAIL runSplitN
system2("rm", "-rf rrrTemp/")
splitNArgs <- c( "V8", "V9", "V10" )
splitNScript <- "thisScript.R"
resFAIL <- rrr$runSplitN( mtcars, 3, splitNScript, splitNArgs )
resFAIL[[1]] == "TIMED OUT"



### test SUCCESS runSplitN
a <- mtcars[1:11,]
b <- mtcars[12:21,]
c <- mtcars[22:32,]
rrr$mkdirTemp()
saveRDS(a, "rrrTemp/chunk_1_11_.rds_thisScript.R_V8_V9_V10.rds" )
saveRDS(b, "rrrTemp/chunk_12_21_.rds_thisScript.R_V8_V9_V10.rds")
saveRDS(c, "rrrTemp/chunk_22_32_.rds_thisScript.R_V8_V9_V10.rds")
splitNArgs <- c( "V8", "V9", "V10" )
splitNScript <- "thisScript.R"
resSUCCESS <- rrr$runSplitN( mtcars, 3, splitNScript, splitNArgs )
resSUCCESS[[1]] == "COMPLETED"
identical( dim( resSUCCESS[[2]] ), dim(mtcars) )




### test INCOMPLETE runSplitN
system2("rm", "rrrTemp/chunk_22_32_.rds_thisScript.R_V8_V9_V10.rds")
resINCOMPLETE <- rrr$runSplitN( mtcars, 3, splitNScript, splitNArgs )
resINCOMPLETE[[1]] == "TIMED OUT"
dim(resINCOMPLETE[[2]])




### test generate random numbers
### see dockerRunif.R
myScript <- "dockerRunif.R"
inFile <- "thisData.rds"
jobs <- list( c("101", "50", "100"),
              c("101", "0.5", "1" ),
              c("101", "3", "7")
             )
myDockerArgs <- lapply(jobs, function(x){
                    c(x, inFile, rrr$makeOutFile( myScript, myInFile, unlist(x) ) )
                })
res <- rrr$runEach( myScript, myDockerArgs )


