##
## Assignment for week 2:  PART 3
##      Write a function that takes a directory of data files and a threshold for 
## complete cases and calculates the correlation between sulfate and nitrate for
## monitor locations where the number of completely observed cases (on all 
## variables) is greater than the threshold. The function should return a vector 
## of correlations for the monitors that meet the threshold requirement. If no 
## monitors meet the threshold requirement, then the function should return a 
## numeric vector of length 0.
##
## CSV column names: DATE, sulfate, nitrate, ID
corr <- function (directory, threshold = 0) {
        
        ## Check if the directory is valid
        if (! file_test("-d", directory)) {
                write2log("corr.log", "Invalid directory:", directory)
                return ( "ERROR: The specified path is not a valid directory")
        }
        write2log("corr.log", "Valid directory:", directory)
        
        # get all complete cases from all files in the directory
        complete <- complete(directory)
        write2log("corr.log", "complete(directory) function executed successfully")
        
        # filter cases by thresold
        complete <- complete[complete$nobs > threshold, ]
        n <- nrow(complete)
        result <- vector(length = n)

        # threshold criteria not met. return numeric vector of lenght 0
        if (n == 0) return( numeric() );
        
        # get the pollutant values
        for (i in 1:n) {
                
                # Assemble filenames to comply with 3 digit numbering names: 001.csv, 010.csv, 111.csv
                file_id_str <- complete$id[i]                
                if ( complete$id[i] < 10) { 
                        file_id_str <- paste("00", complete$id[i], sep = "")
                }
                if (complete$id[i] >= 10 && complete$id[i] < 100 ){
                        file_id_str <- paste("0", complete$id[i], sep = "")
                }
                filename <- file.path(directory, paste(file_id_str, ".csv", sep = ""))
                
                # read file into data frame
                write2log("corr.log", filename)
                frame <- read.csv(file = filename)
                
                # get all complete cases
                frame <- frame[ complete.cases(frame), ]
                
                # calculate the correlation between pollutants
                correlation <- cor(frame$sulfate,frame$nitrate)
                write2log("corr.log", paste("correlation is:", correlation, sep = " "))
                
                result[i] <- correlation
                
                
        }
        return(result)
}
