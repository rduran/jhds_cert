##
## Assignment for week 2:  PART 2
##      Write a function that reads a directory full of files and reports the 
## number of completely observed cases in each data file. The function should 
## return a data frame where the first column is the name of the file and the 
## second column is the number of complete cases. 
##
## CSV column names: DATE, sulfate, nitrate, ID
complete <- function (directory, id = 1:332) {

        ## Check if the directory is valid
        if (! file_test("-d", directory)) {
                write2log("complete.log", "Invalid directory:", directory)
                return ( "ERROR: The specified path is not a valid directory")
        }
        write2log("complete.log", "Valid directory:", directory)
 
        n <- length(id)
        result_frame <- as.data.frame(matrix(nrow = n, ncol = 2))
        names(result_frame) <- c("id", "nobs")
        
        for (i in 1:n) {
                
                # Assemble filenames to comply with 3 digit numbering names: 001.csv, 010.csv, 111.csv
                file_id_str <- id[i]                
                if ( id[i] < 10) { 
                        file_id_str <- paste("00", id[i], sep = "")
                }
                if (id[i] >= 10 && id[i] < 100 ){
                        file_id_str <- paste("0", id[i], sep = "")
                }
                filename <- file.path(directory, paste(file_id_str, ".csv", sep = ""))
                
                # read file into data frame
                write2log("complete.log", filename)
                frame <- read.csv(file = filename)

                # get all complete cases
                frame <- frame[ complete.cases(frame), ]
                write2log("complete.log", paste(id[i], nrow(frame), sep = " "))

                # store in file mean array
                # write2log("complete.log", frame$id[1], length(frame$id))
                result_frame[i, ] <- c(id[i], nrow(frame))
                
                
        }
        return(result_frame)
}
