#
# Structured Data Preprocessing for AphasiaBank
# Preprocessing: Data selection, formatting, replacement
# of missing values, normalization and transformations.
#
# By Paula 
#

library(tools) #for removing file name extension
library(data.table)
library(plyr)

#----------------------------------------------
# Functions
#----------------------------------------------
addOutcomeVar <- function (listTables){
  # Sets up the file for classification
  #
  # Args: 
  #   listTables: A list containing one or more data tables/frames
  #   outcomeVar: 0 for no aphasia, 1 for aphasia
  #
  # Returns:
  #   updated data table/frame with outcome column for classification
  #
  for (i in (1:(length(listTables)))){
    listTables[[i]]$outcome <- rep(as.numeric(names(listTables[i])),
                                   nrow(listTables[[i]]))
  }
  return (listTables)
}

checkTranscriptProtocol <- function (x, transcript_path, sample, 
                                     unique_protocol, file_ID){
  # Checks transcript files to see if they follow AphasiaBank Protocols by
  # referencing the @G: annotation in AphasiaBank.
  #
  # Args: 
  #   x: Data table of fully updated sample
  #   transcript_path: String path to transcript folder
  #   sample: String folder name containing transcripts
  #   unique_protocol: String for unique protocol i.e. "Illness" vs. "Stroke"
  #   file_ID: removed output file ID to differenciate samples
  #
  # Returns:
  #   Updated x with participants whose data follows protocol
  #
  protocol_result <- data.table(Participant.ID = x[,x$Participant.ID])
  protocol_id <- c(unique_protocol, "Important","Window", "Umbrella", "Cat", 
                   "Cinderella", "Sandwich")
  for (i in protocol_id){
    protocol_result <- protocol_result[, paste(i, ".Protocol", sep = "") 
                                       := lapply (protocol_result$Participant.ID, 
                                                  function(x) ifelse(length(grep(
                                                    paste0("@G:\\s\\w*\\s{0,2}", i), 
                                                    readLines(paste0(transcript_path, sample, 
                                                                     x, ".txt")), value = "Y", 
                                                    ignore.case = TRUE))>0, "Y",NA))]
  }
  protocol_result <- protocol_result[rowSums(!is.na(protocol_result))==
                                       length(protocol_result),]
  removedData(protocol_result, x, paste("notProtocol", file_ID, sep = ""),"")
  x <- x[(x$Participant.ID %in% protocol_result$Participant.ID), ]
  return(x)
}

formatCatData <- function (x) {
  # Formats category columns with numeric identifiers instead of given factors
  #
  # Args: 
  #   x: Data table for formatting
  #
  # Returns:
  #   x: newly formatted x matrix with categories as numeric values
  #
  cat_columns <- match(c("Sex","Race","Handedness","Knows.Cinderella"),names(x))
  for (i in cat_columns) {
    x[[i]] <- as.numeric(factor(x[[i]])) 
  }
  return(x)
}

findOutliers <- function(x){
  # Locates outliers in the data
  #
  # Args: 
  #   x: Data table or frame of the population
  #
  # Returns:
  #   Void. Prints information but can be changed to alter x.
  #
  #print(length(colnames(x)))
  #store the outlier data below in a vector for each key and remove if greater than 25% (par#26 aphasia)
  for (id_x in colnames(x)){
    print(id_x)
    count<-0
    lapply(x[[id_x]], function(x_i){ 
      count <<- count+1
      if (abs(x_i-mean(x[[id_x]],na.rm=TRUE)) > 3*sd(x[[id_x]],na.rm=TRUE)){
        #print(paste("col:", id_x, "  mean:", mean(x[[id_x]]), "  sd:",
        #            sd(x[[id_x]]), "  thresh:", 3*sd(x[[id_x]])))
        print(paste("    outlier:", x_i))
        print(paste("    participant id:", count))
      }
    })
  }
  #print('--end of sample---')
}

formatIDs <- function (x, index) {
  # Formats participant IDs to lowercase and without file extension
  #
  # Args: 
  #   x: Data table or frame to be changed
  #   index: Numeric identifier for the type of table/frame, used in output
  #
  # Returns:
  #   Fully formatted column of participant IDs for input table/frame.
  #
  x_orig <- x
  names(x)[[1]] <- "Participant.ID"
  x$Participant.ID <- tolower(basename(file_path_sans_ext(x$Participant.ID))) 
  x$Participant.ID <- gsub(" ", "", x$Participant.ID, fixed = TRUE)
  removedData(x, x_orig, "capitalizedIDs", index)
  return(x)
}

makeChar <- function(listTables){
  # Changes list variables as factors to char, a bug in R
  #
  # Args: 
  #   listTables: A list containing two or more data tables/frames
  #
  # Returns:
  #   Newly formatted listTables that will not alter values with rbind.
  #
  for (i in 1:length(listTables)){
    for(j in names(listTables[[i]])){
      if (is.factor(listTables[[i]][[j]])){
        listTables[[i]][[j]] <- as.character(listTables[[i]][[j]])
      }
    }
  }
  return(listTables)
}

mergeTables <- function (listTables, fileID){
  # Merges all data tables/frames within a list by participant ID. Ignores
  # participants that are missing in one, or multiple data tables.
  #
  # Args: 
  #   listTables: A list containing two or more data tables/frames
  #   fileID: String identifier for the type of file population in listTables
  #
  # Returns:
  #   One combined data table of all within listTables
  #
  merged <- listTables[[1]]
  for (i in (1:(length(listTables) - 1))){
    merged <- merge(merged, listTables[[i + 1]], by = "Participant.ID")
    removedData(merged, listTables[[1]], 
                paste("mergedData", fileID, sep = ""), i)
  }
  return (merged)
}

removeEmptyCol <- function (x, index){
  # Removes empty columns in a data table/frame
  #
  # Args: 
  #   x: Data table or frame to be changed
  #   index: Identifier for the type of table/frame, used in output
  #
  # Returns:
  #   Updated data table/frame with no empty columns
  #
  x [x == ""] <- NA
  no_empty_cols <- Filter(function(na)!all(is.na(na)), x)
  write.csv(sapply(x, function(x)all(is.na(x))), 
            paste(file_path,"emptyColumns", index, ".csv", sep = ""))
  return(no_empty_cols)
}

removeEmptyParticipants <- function (x, index){
  # Removes rows with no participant ID present
  #
  # Args: 
  #   x: Data table or frame to be changed
  #   index: Numeric identifier for the type of table/frame, used in output
  #
  # Returns:
  #   Updated data table/frame with no empty participant rows.
  #
  no_empty_par <- data.table(x[!(is.na(x$Participant.ID)|
                                   x$Participant.ID == ""), ]) 
  removedData(no_empty_par, x, "emptyParticipants", index)
  return(no_empty_par)
}

removeNonMonolingual <- function (x, addt_diagnosis, fileID){
  # Removes rows of participants that speak more than one language. If 
  # addt_diagnosis is "Y" or "YES" then checks for additional diagnosis using
  # removeAddtDiagnosis method.
  #
  # Args: 
  #   x: Data table or frame to be changed
  #   add_diagnosis: variable of "Y"/"YES" or "N"/"NO" indicating whether rows
  #   should also be deleted that match a diagnosis of Apraxia or Dysarthia
  #   fileID: String identifier for x
  #
  # Returns:
  #   Fully formatted column of participant IDs for input table/frame.
  #
  only_monolingual <- x[grep("MON", x$Language.Status),]
  removedData(only_monolingual, x, "onlyMonolingual", fileID)
  if (addt_diagnosis %in% c("Y", "YES")) {
    only_monolingual <- removeAddtDiagnosis(only_monolingual, fileID)
  }
  return(only_monolingual)
}

removePercentMissing <- function(x, index, percent) {
  # Removes columns where more than 90% of data is missing (NA).
  #
  # Args: 
  #   x: Data table or frame to be changed
  #   index: Numeric identifier for x
  #
  # Returns:
  #   Updated x with columns where data is 90% or more, populated.
  #
  if (nrow(subset(x,select = x[, -which(colMeans(is.na(x)|
                                                 is.null(x))>percent)])) != 0){
    missing_data <- subset(x, select=x[, which(colMeans(is.na(x)|
                                                          is.null(x))>percent)])
    write.csv(missing_data, paste0(file_path, "remove90Missing", index, ".csv"))
    x <- subset(x, select=x [, -which(colMeans(is.na(x)|is.null(x))>percent)])
  }
  return(x)
}

removeAddtDiagnosis <- function (x, index){
  # Removes rows of participants that are diagnosed with apraxia or dysartia
  #
  # Args: 
  #   x: Data table or frame to be changed
  #   index: Numeric identifier for x
  #
  # Returns:
  #   Updated x with participants who do not have apraxia or dysartia.
  #
  missing_data <- x[grepl("Y", x$Apraxia.of.Speech)]
  missing_data1 <- x[grepl("Y", x$Dysarthria)]
  write.csv(missing_data, paste(file_path,"noApraxia", index, ".csv", sep = ""))
  write.csv(missing_data1, paste0(file_path,"noDysartia", index, ".csv"))
  x <- x[!grepl("Y",x$Apraxia.of.Speech),] 
  x <- x[!grepl("Y",x$Dysarthria),]
  return(x)
}

removeDuplicateIDs <- function (x, index){
  # Removes participant IDs that ignoring the last letter, are repeated.
  #
  # Args: 
  #   x: Data table or frame to be changed
  #   index: Numeric identifier for x
  #
  # Returns:
  #   Updated x with unique participant IDs, and repeated that end with "a"
  #
  output <- matrix(data = NA, nrow=nrow(x), ncol = 1)
  count <- 1
  for (i in 1:(length(x$Participant.ID) - 1)) {
    curr_id <- substr(x$Participant.ID[i], 1, nchar(x$Participant.ID[i])-1)
    next_id <- substr(x$Participant.ID[i + 1], 1, 
                      nchar(x$Participant.ID[i + 1]) - 1)
    if (curr_id == next_id){
      output[count,1] <- x$Participant.ID[i]
      output[count + 1,1] <- x$Participant.ID[i + 1]
      count <- count + 2
    }
  }
  output<-data.table(output)
  remove_ids <- data.frame(subset(output$V1,grepl("^.+[^a]$",output$V1)))
  names(remove_ids)[[1]] <- "Participant.ID"
  remove_ids <- unique(remove_ids)
  x <- x[!(x$Participant.ID %in% remove_ids$Participant.ID), ]
  write.csv(remove_ids, paste0(file_path,"duplicateIDs", index, ".csv"))
  return(x)
}

removedData <- function (x,y,fileName, index){
  # Identifies changes made within two data tables and outputs to csv
  #
  # Args: 
  #   x: Data table for comparison, must be shorter than y
  #   y: Data table for comparison, must be longer than x
  #   fileName: String identifier for x and y
  #   index: Numeric identifier for x,y, prevents overwriting
  #
  # Returns:
  #   Void. Only a csv listing the differences between x,y
  #
  missing_data <- y[y$Participant.ID %in% setdiff(y$Participant.ID, 
                                                  x$Participant.ID), ]
  write.csv(missing_data, paste(file_path,fileName, index, ".csv", sep = ""))
}

updateNA <- function (xCol) {
  # Updates NA values to average of column for classification
  #
  # Args: 
  #   xCol: non-categorical data column for formatting NA variables
  #
  # Returns:
  #   xCol: newly formatted xCol with averages in place of NA variables
  #
  xCol[xCol=='U'] <- NA
  index_mean <- which(!is.na(xCol))
  col_mean <- mean(as.numeric(xCol[index_mean]))
  for (i in 1:length(xCol)){
    if (is.na(xCol[i])){
      xCol[i] <- format(round(col_mean, 2), nsmall = 2) 
    }
  } 
  return(xCol)
}

#----------------------------------------------
# Main Execution
#----------------------------------------------

# Data selection
file_path <- paste(getwd(),"/Removed Data/", sep="")
output_path <- paste(getwd(),"/Output CSV/", sep="")
transcript_path <- paste(getwd(),"/Transcript Files/", sep="")

input_aphasia <- read.csv("aphasiaNoCompNum.csv") 
input_aDemogr <- read.csv("aphasiaDemographics.csv")
input_aTests <- read.csv("aphasiaTestResults.csv")
input_control <- read.csv("controlNoCompNum.csv") 
input_cDemogr <- read.csv("controlDemographics.csv")

# Data formatting
df_list <- list(input_aphasia,input_aDemogr,input_aTests,input_control,
                input_cDemogr)
index <- 0
df_list <- lapply(df_list, function(x) {
  index <<- index + 1
  x <- formatIDs(x, index)
  x <- removeEmptyParticipants(x, index)
  x <- removeEmptyCol(x, index)
  return(x)
})

input_aphasia <- data.table(df_list[[1]])
input_aDemogr <- data.table(df_list[[2]])
input_aTests <- data.table(df_list[[3]])
input_control <- data.table(df_list[[4]])
input_cDemogr <- data.table(df_list[[5]])

# Data sampling: history of stroke but no aphasia
stroke_noaph <- input_aTests[grep("not aphasic", input_aTests$WAB.Type), ] 
aDemo_noaph <- input_aDemogr[(input_aDemogr$Participant.ID %in% 
                                stroke_noaph$Participant.ID),]
# Data sampling: aphasia
aDemo_aphasia <- input_aDemogr[!(input_aDemogr$Participant.ID %in% 
                                   stroke_noaph$Participant.ID),]
aphasiaby_bost <- aDemo_aphasia[grep(
  "BRO", aDemo_aphasia$Aphasia.Type...Clin.Impression....Boston),]
aphasiaby_clin <- aDemo_aphasia[grep(
  "NFL", aDemo_aphasia$Aphasia.Category....Clin.Impression),]
aphasiaby_wab <- input_aTests[grep("Broca", input_aTests$WAB.Type),] 
par_names <- c(aphasiaby_bost$Participant.ID, aphasiaby_clin$Participant.ID, 
               aphasiaby_wab$Participant.ID)
aDemo_aphasia <- aDemo_aphasia[(aDemo_aphasia$Participant.ID %in% par_names),]

# Filter participants with stroke/aphasia by non-monolingual and protocol
pop_list <- list(aDemo_aphasia, aDemo_noaph)
index <- 0
pop_list <- lapply(pop_list, function(x) {
  index <<- index + 1
  id <- paste0("S", index)
  x <- removeDuplicateIDs(x, id)
  mergeList <- list(input_aTests, input_aphasia, x)
  x <- mergeTables(mergeList, id)
  x <- removeNonMonolingual(x, "Y", id)
  x <- removePercentMissing(x, id, 0.90)
  x <- checkTranscriptProtocol(x, transcript_path, "aphasia/", "Stroke", id)
  return (x)
})
aphasia <- data.table(pop_list[[1]])
stroke_noaph <- data.table(pop_list[[2]])

write.csv(aphasia, paste0(output_path, "aphasia.csv"))
write.csv(stroke_noaph, paste0(output_path, "stroke.csv"))

# Data sampling: control (neither a history of stroke nor aphasia)
# control CSV data is already separated, no need to do steps L364.
input_cDemogr <- removeDuplicateIDs(input_cDemogr, "C1")
mergeList <- list(input_control,input_cDemogr)
all_control <- mergeTables(mergeList, "C")
all_control <- removeNonMonolingual (all_control, "N", "C1")
all_control <- removePercentMissing(all_control, "C1", 0.90)
all_control <- checkTranscriptProtocol(all_control, transcript_path, "control/", 
                                       "Illness", "C1")
write.csv(all_control, paste0(output_path, "control.csv"))

# Data formatting: setup for testing
classify_list <- list('0' = all_control, '0'= stroke_noaph, '1' = aphasia)
classify_list <- makeChar(classify_list)
classify_list <- addOutcomeVar(classify_list)

index <- 0
classify_list <- lapply(classify_list, function(x) {
  x$Years.of.Education <- updateNA(x$Years.of.Education)
  x$Age <- gsub("[[:punct:]][[:digit:]]+", "", x$Age)
  x$Age <- updateNA(x$Age)
  return (x)
})

# Let's not include stroke ([[2]]) for now:
classify_matrix <- rbind.fill(classify_list[[1]],classify_list[[3]])
# Remove columns with >20% missing values
classify_matrix <- removePercentMissing(data.table(classify_matrix), "Classify", 0.20)
# Remove columns with no unique data
unique_cols <- sapply(classify_matrix,function(x) length(unique(x)))
classify_matrix <- subset(classify_matrix, select=unique_cols>1)
# Remove repeating columns and multinominal values
classify_matrix <- classify_matrix[ , (c("Corpus","Group","Test.Date", "DOB", 
                                         "Age.at.Testing","Gender", "Occupation", 
                                         "Employment..Status","Birth.Country", 
                                         "Primary.Language","General.Health"
)) := NULL ] 
classify_matrix <- formatCatData(classify_matrix)
classify_matrix$Duration <- gsub(":", '', classify_matrix$Duration, fixed = TRUE)
classify_matrix$Duration <- as.character(classify_matrix$Duration)
write.csv(classify_matrix, paste0(output_path, "combined.csv"))