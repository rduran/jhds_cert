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
 
        result_frame <- data.frame()
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
                frame <- frame[ complete.cases(frame), ]
                cat(frame)
                write2log("complete.log", frame$id[1], length(frame$id))

                # store in file mean array
                # write2log("complete.log", frame$id[1], length(frame$id))
                result_frame <- merge(result_frame, c(frame$id[1], length(frame$id)))
                
                
        }
#        names(result_frame) <- c("id", "nobs")
        return(result_frame)
}
