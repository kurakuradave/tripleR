################
###  Triple R
################

library(R6)
library(assertive)
library(lubridate)


tripleR_factory <- R6Class(

    "TripleR",

    private = list(
        
        ..timeoutMins = 30,
        ..checkSecs = 30,
        ..dockerAccount = "",
        ..dockerRepository = "",
        ..dockerTag = "",
        ..tempDir = "rrrTemp",
        ..dataDir = "data",
        ..jobsStarted = FALSE,
        ..jobsStartTime = NULL,
        ..jobsCompleted = FALSE,
        ..jobsCompleteTime = NULL,
        ..elapsed = NULL,
        ..hasTimedOut = FALSE,
        ..targetOutputFiles = NULL,
        ..maxContainers = 20,
        ..commands = NULL,
        ..autoCleanup = FALSE,
        ..completedFiles = NULL,
        ..splitInFiles = NULL,
        ..splitNArgs = NULL,
        ..dockerArgs = NULL,
        ..autoBindResults = FALSE,
        
        createTempDir = function(){
            if( !(private$..tempDir %in% list.files() ) ){
                system2( "mkdir", paste0("./", private$..tempDir)  )
            }    
        },
        
        removeTempDir = function(){
            if( private$..tempDir %in% list.files() ){
                system2( "rm", paste0( "-rf ", private$..tempDir ) )
            }
        },
        
        makeDockerCommand = function( scriptName, scriptArgs ){
            ## returns the string of command arguments needed for running docker containers
            pcs <- c(
                "run",
                "-d",
                "--rm",
                "-v",
                self$makeVolReference(),
                self$makeDImgFullname(),
                "Rscript",
                paste0( private$makeVolContainer(), "/", scriptName ),
                paste( scriptArgs )
            )
            return( paste(pcs, collapse=" ") )
        },
        
        makeVolHost = function(){
            return( getwd() )  
        },
        
        makeVolContainer = function(){
            return( "/root" )
        },

        touchJobFile = function(){
            nowDate <- gsub( " ", "_", as.character( private$..jobsStartTime ) )
            write.table( data.frame( commands = unlist( private$..commands ) ),
                        file=paste0("./", private$..tempDir, "/jobs_run_at_", nowDate, ".csv" ),
                        sep=",",
                        row.names = FALSE
                     )
        },

        startJobs = function(){
            private$..jobsStartTime <- now()
            private$..jobsStarted <- TRUE
            private$..jobsCompleted <- FALSE
            private$..hasTimedOut <- FALSE
            private$touchJobFile()
            lapply(private$..commands, function(x){
                message( paste( "RUNNING :", x ) )
                system2( "docker", x )
            })            
        },

        checkJobsStatus = function(){
            message("checking jobs")
            ### check for timeout
            private$..elapsed <- now() - private$..jobsStartTime
            dispElapsed <- round(as.numeric(as.duration(private$..elapsed)), 4)
            dispTimeout <- private$..timeoutMins*60

            message(paste("elapsed :", dispElapsed, "time out :", dispTimeout ))
           if( dispElapsed> dispTimeout ){
                private$..hasTimedOut <- TRUE
            }
            
            ### check for completion
            sumDone <- sum( sapply( private$..targetOutputFiles, function(x){
                x %in% list.files(private$..tempDir)
            }) )

            message( paste( "completed :", sumDone, " of ", length(private$..targetOutputFiles) ) )

            if(sumDone == length( private$..targetOutputFiles )){
                private$..jobsCompleted <- TRUE
                private$..jobsCompleteTime <- now()
            }
        },

        populateTargetOutputFiles = function( someArgs ){
        ## TARGET OUTPUT FILENAME IS ALWAYS THE LAST ARGUMENT
            private$..targetOutputFiles <- lapply(someArgs, function(x){
                x[length(x)]
            })
        },

        statusNow = function(){
            ret <- "RUNNING"
            if( private$..hasTimedOut ){
                ret <- "TIMED OUT"
            } else if( private$..jobsCompleted ) {
                ret <- "COMPLETED"
            }
            return( ret )
        },

        tabulateCompletedFiles = function(){
            contents <- list.files( private$..tempDir )
            hasFile <- sapply( contents, function(x){
                x %in% private$..targetOutputFiles
            } )
            return(contents[hasFile])
        },

        doCleanup = function(){
            if( private$..autoCleanup ){
                system2( "rm", paste( "-rf", private$..tempDir ) )
            }
        },

        displaySummaryMsg = function(){
            msg <- paste( "TIMED OUT! Some jobs are NOT done! \n",
                          "See", paste0( "./", private$..tempDir, "/jobs_run_at_XXX_.csv" ), "to find out missing output(s) and re-run the docker job(s) manually" )
            if( private$..jobsCompleted ){
                msg <- "ALL container jobs DONE!"
            }
            message( msg )
        },
        
        checkTilDone = function(){
            ret = NULL
            while( !(private$..hasTimedOut | private$..jobsCompleted ) ) {
                Sys.sleep( private$..checkSecs)                
                private$checkJobsStatus()                
            }
            status <- private$statusNow()

            ### combine completed files
            private$..completedFiles <- private$tabulateCompletedFiles()
            results <- lapply(private$..completedFiles, function(x){
                readRDS(paste0(private$..tempDir, "/", x))
            })
            
            if( private$..autoBindResults ){
                results <- do.call( "rbind", result )
            }

            ## wrap final returned object
            ret <- list(status, results)

            private$displaySummaryMsg()
            
            ## cleanup 
            private$doCleanup()
            
            return(ret)
        },

        splitToDisk = function(someDF, n){
            ## return a vector of input file names
            ## also split the DF into chunks and saved into temp directory on disk
            splitBoundaries <- self$getSplitBoundaries( someDF, n )
            self$mkdirTemp()
            sapply( splitBoundaries, function(x){
                fname <- paste( c( "chunk", x[1], x[2], ".rds" ), collapse="_")
                rows <- someDF[ x[1]:x[2] , ]
                saveRDS( rows, paste0( private$..tempDir, "/", fname ) )
                return(fname)
            } )
        },

        makeSplitNArgs = function(someScript, someArgs){
            ## return arguments to the script suitable to be run in docker containers
            assert_is_vector( someArgs )
            assert_is_character( someArgs )
            
            ## needs:
            ## script name
            ## script args
            ## private$..splitInFiles
            ## returns a list of arguments for each container job
            
            ret <- lapply( private$..splitInFiles, function(x){
                fullArg <- c( someArgs, x, self$makeOutFile( someScript, x, someArgs ) )
                return( fullArg )
            } )
            return( ret )
        }
    ),
    
    public = list(
        
        mkdirTemp = function(){
            private$createTempDir()
        },

        rmdirTemp = function(){
            private$removeTempDir()
        },
        
        makeOutFile = function( scriptName, inFile, scriptArgs ){
            assert_is_character( scriptArgs )
            paste0( paste0(c(inFile, scriptName, scriptArgs ), collapse="_"), ".rds" )
        },
        
        makeVolReference = function(){
            paste0( private$makeVolHost(), ":", private$makeVolContainer() )
        },
        
        makeDImgFullname = function(){
            paste0( private$..dockerAccount, "/", private$..dockerRepository, ":", private$..dockerTag )
        },

        getSplitBoundaries = function( someDF, n ) {
            lengthDF <- dim(someDF)[1]
            interval <- lengthDF / n
            cumStep <- 1
            nowHead <- 1
            nowTail <- 0
            rep <- 1
            boundaries <- list()
            while( cumStep < lengthDF ){
                nowTail <- round( rep * interval )
                nowBoundaries <- c( nowHead, nowTail )
                boundaries[[rep]] <- nowBoundaries
                rep <- rep + 1
                nowHead <- nowTail + 1
                cumStep <- nowTail
            }
            return(boundaries)
        },

        runEach = function( someScript, someArgs ){
            ## each element in someArgs will be run in a container
            ## someArgs : second last element is input file
            ##            last element is output file
            assert_is_list( someArgs )
            if( length(someArgs) > private$..maxContainers ){ 
                stop("Attempting to run too many containers!")
            } else {
                private$..commands <- lapply( someArgs, function(x){
                                          private$makeDockerCommand( someScript, x )
                } )
                private$populateTargetOutputFiles( someArgs )
                private$createTempDir()
                private$startJobs()
                ret <- private$checkTilDone()
                return( ret )
            }    
        },
        
        runSplitN = function( someDF, n, someScript, someArgs ){
            ## split the DF into n parts, save each to disk, make arguments, then call runEach()
            private$..splitInFiles <- private$splitToDisk( someDF, n )
            private$..splitNArgs <- private$makeSplitNArgs(someScript, someArgs)
            ret <- self$runEach( someScript, private$..splitNArgs )
            return( ret )
        },

        ## ######## for testing
        splitToDisk_test= function( someDF, n ){
            private$..splitInFiles <- private$splitToDisk( someDF, n )
        },

        makeSplitNArgs_test = function( someScript, someArgs ){
            private$makeSplitNArgs( someScript, someArgs )
        },
        
        ## INIT ##
        initialize = function( 
                               dockerAccount,
                               dockerRepository,
                               dockerTag,
                               timeoutMins = 30,
                               checkSecs = 30,
                               maxContainers = 20,
                               autoCleanup = FALSE,
                               autoBindResults = FALSE,
        ){
            private$..timeoutMins <- timeoutMins
            private$..checkSecs <- checkSecs
            private$..dockerAccount <- dockerAccount
            private$..dockerRepository <- dockerRepository
            private$..dockerTag <- dockerTag
            private$..maxContainers <- maxContainers
            private$..autoCleanup <- autoCleanup
        }        
    ),
    
    active = list(
        
        timeoutMins = function( value ){
            if(missing(value)) {
                private$..timeoutMins                
            } else {
                assert_is_a_number( value )
                assert_all_are_in_closed_range(value, 2, 1000)
                private$..timeoutMins <- value
            }            
        },

        checkSecs = function( value ){
            if(missing(value)){
                private$..checkSecs
            } else{
                assert_is_a_number(value)
                asser_all_are_greater_than( 5 )
                private$..checkSecs <- value
            }
        },
        
        dockerAccount = function( value ){
            if( missing(value) ){
                private$..dockerAccount
            } else {
                assert_is_character( value )
                assert_is_non_empty( value )
                private$..dockerAccount <- value
            }
        },
        
        dockerRepository = function( value ){
            if( missing(value) ){
                private$..dockerRepository
            } else {
                assert_is_character( value )
                assert_is_non_empty( value )
                private$..dockerRepository <- value
            }
        },

        dockerTag = function( value ){
            if( missing(value) ){
                private$..dockerTag
            } else {
                assert_is_character( value )
                assert_is_non_empty( value )
                private$..dockerTag <- value
            }
        },

        dataDir = function( value ){
            if( missing(value) ){
                private$..dataDir
            } else {
                assert_is_character( value )
                assert_is_non_empty( value )
                private$..dataDir <- value
            }
        },

        tempDir = function( value ){
            if( missing(value) ){
                private$..tempDir
            } else {
                assert_is_character( value )
                assert_is_non_empty( value )
                private$..tempDir <- value
            }
        },

        maxContainers = function( value ){
            if(missing(value)){
                private$..maxContainers
            } else{
                assert_is_a_number(value)
                asser_all_are_in_closed_range(value, 2, 100)
                private$..maxContainers <- value
            }
        },

        autoCleanup = function( value ){
            if( missing(value) ){
                private$..autoCleanup
            } else {
                assert_all_are_logical_strings( value )
                private$..autoCleanup <- value
            }
        },
        
        autoBindResults = function( value ){
            if( missing(value) ){
                private$..autoBindResults
            } else {
                assert_all_are_logical_strings( value )
                private$..autoBindResults <- value
            }
        }
    )   
)



RRR <- function( dockerAccount,
                 dockerRepository,
                 dockerTag,
                 ... ){                               
    tripleR_factory$new( dockerAccount, dockerRepository, dockerTag, ...)
}






