#' @description This function imports the raw test results and checks the column values for the right format. The function 
#' imports all the columns in the file including the headers. The function generates two dataframes, one with test configuration
#' data and another one with test results.
#' @import dplyr
#' @import data.table
#' @param fileNamePath: File path name.
#' @return processedDataFrame: Formatted table data frame.
#' @warning This function is specific to the raw file provided by the Tobii software.
#' @author Heinz Lugo.
library(dplyr)
library(stringi)
library(readr)
rawTestResultsImport <- function(fileNamePath)
{
  ## Step 1. Reading of the file.
  rawTable <- dplyr::tbl_df(readr::read_delim(file = fileNamePath, delim = "\t", col_names = FALSE, col_types = cols(.default = col_character())))
  rawTable <- rawTable[, c(1:ncol(rawTable) -  1)]
  rawTableIdentifiers <- rawTable[1, ]
  rawTableIdentifiers <- stringi::stri_trans_totitle(rawTableIdentifiers)
  rawTableIdentifiers <- gsub(pattern = " ", replacement = "", x = rawTableIdentifiers)
  rawTableIdentifiers <- gsub(pattern = "(^[[:alpha:]])", replacement = "\\L\\1", x = rawTableIdentifiers, perl=TRUE)
  rawTable <- rawTable[-1, ]
  names(rawTable) <- rawTableIdentifiers
  ## Step 2. Removal of irrelevant columns.
  testDetails <- dplyr::select(rawTable, projectName, participantName, recordingName,
                               recordingFixationFilterName) %>% dplyr::distinct()
  rawTable <-  dplyr::select(rawTable, -c(projectName))
  ## Step 3. Add event marker column.
  eventMarkerColumn <- vector(mode = "character", length = nrow(rawTable))
  rawTable$event <- tolower(rawTable$event)
  startEventIndexes <- grep(pattern = "^start(.+)", x = rawTable$event, perl = TRUE)
  eventNames <- sub(pattern = "^start", replacement = "", x = rawTable$event[startEventIndexes])
  stopEventIndexes <- as.character(sapply(eventNames, function (x) paste("stop", x, sep = "")))
  uniqueStopEvents <- unique(stopEventIndexes)
  for(i in 1:length(uniqueStopEvents))
  {
    tempIndexes <- which(rawTable$event == uniqueStopEvents[i])
    if(i == 1)
    {
      stopEventIndexes <- tempIndexes
    }
    else
    {
      stopEventIndexes <- c(stopEventIndexes, tempIndexes)
    }
  }
  if(length(startEventIndexes) != length(stopEventIndexes))
  {
    stop("A start event does not have the corresponding stop event.")
  }
  for(i in 1:length(startEventIndexes))
  {
    eventMarkerColumn[startEventIndexes[i]:stopEventIndexes[i]] <- eventNames[i]
  }
  rawTable <- dplyr::mutate(rawTable, eventMarkerColumn)
  rm(eventMarkerColumn)
  ## Step 3. Format correction for relevant columns.
  if(length(names(rawTable)) != 16 | length(names(rawTable)) != 15)
  {
    if("eyeMovementTypeIndex" %in% names(rawTable))
    {
      numericColumnsIdentifier <- c(4:9, 11:14)
    }
    else if(!("eyeMovementTypeIndex" %in% names(rawTable)))
    {
      numericColumnsIdentifier <- c(4:9, 11:13)
    }
    else
    {
      stop("The column data fields from the dataframe do not coincide with the expected fields.")
    }
  }
  else
  {
    stop("The column data fields from the dataframe do not coincide with the expected fields.")
  }
  rawTable[, numericColumnsIdentifier] <- apply(X = rawTable[, numericColumnsIdentifier], MARGIN = 2, FUN = function(x) as.numeric(x))
  ## Step 4. Return of the processed dataframe.
  resultsList <- vector(mode = "list", length = 2)
  resultsList[[1]] <- testDetails
  resultsList[[2]] <- rawTable
  resultsList
}