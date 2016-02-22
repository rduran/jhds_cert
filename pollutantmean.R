##
## Assignment for week 2:  PART 1
##      Write a function named 'pollutantmean' that calculates the mean of a 
## pollutant (sulfate or nitrate) across a specified list of monitors. The 
## function 'pollutantmean' takes three arguments: 'directory', 'pollutant', 
## and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that 
## monitors' particulate matter data from the directory specified in the 
## 'directory' argument and returns the mean of the pollutant across all of the 
## monitors, ignoring any missing values coded as NA.
##
## CSV column names: DATE, sulfate, nitrate, ID
pollutantmean <- function (directory, pollutant, id = 1:332) {
        
        ## Check if the directory is valid
        if (! file_test("-d", directory)) {
                write2log("pollutantmean.log", "Invalid directory:", directory)
                return ( "ERROR: The specified path is not a valid directory")
        }
        write2log("pollutantmean.log", "Valid directory:", directory)
        
        if (! pollutant %in% c("sulfate", "nitrate")) {
                write2log("pollutantmean.log", "Invalid pollutant:", pollutant)
                return ( "ERROR: Valid pollutants are \"sulfate\" or \"nitrate\".")
        }
        write2log("pollutantmean.log", "Valid pollutant:", pollutant)
        
        mean_vector <- c()
        for (file_id in id) {
                
                # Assemble filenames to comply with 3 digit numbering names: 001.csv, 010.csv, 111.csv
                file_id_str <- file_id                
                if (file_id < 10) { 
                        file_id_str <- paste("00", file_id, sep = "")
                }
                if (file_id >= 10 && file_id < 100 ){
                        file_id_str <- paste("0", file_id, sep = "")
                }
                filename <- file.path(directory, paste(file_id_str, ".csv", sep = ""))
                
                # read file into data frame
                frame <- read.csv(file = filename)
                
                # eliminate NAs
                pollutant_vector <- frame[[pollutant]][ !is.na(frame[[pollutant]])]
                

                #store in file mean array
                mean_vector <- (c(mean_vector, pollutant_vector))
                write2log("pollutantmean.log", "mean_vector: ", paste(mean_vector)) 
                
                
        }
        write2log("pollutantmean.log", "mean_value: ", mean(mean_vector)) 
        mean(mean_vector)
        
}

##
## Function to streanline logging
##
write2log <- function(filename, ... ) {
        cat(  paste( format( Sys.time(), "%Y-%b-%d %a %H:%M:%OS3 :"), ... ),
              sep = "\n",
              file = filename, 
              append = TRUE
        )
}
