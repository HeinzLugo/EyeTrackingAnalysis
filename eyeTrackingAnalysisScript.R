#' @description This script calls all the required functionality to analyse the eye tracking data.
#' @reminder Change the participant index where the data and the results will be stored.
#' @author Heinz Lugo.
## Step 0. Clean up the environment, load the required libraries and set the loading and storing folder paths.
rm(list=ls())
source("rawTestResultsImport.R")
source("eyeTrackingAnalysis.R")

##--FILE PATH. CAN BE CHANGED AS LONG AS FOLDER STRUCTURE IS THE AGREED ONE.--##
loadFolderPath <- paste(getwd(), "RawData", sep = "/")
storeFolderPath <- paste(getwd(), "AnalysisResults", "Individual", sep = "/")

filesList <- list.files(path = loadFolderPath)
filesList <- filesList[order(nchar(filesList), filesList)]
for(i in 1:length(filesList))
{
  print(i)
  ##----THIS SECTION IS NEEDED FOR THE FIRST OF TESTS TO SKIP THE FIRST SAMPLE WHICH WAS A TEST FILE.----##
  # if(i != 1)
  # {
  #   ## Step 1. Raw test results import.
  #   fileNamePath <- paste(loadFolderPath, filesList[i], sep = "/")
  #   fileNamePath <- paste(fileNamePath, paste(filesList[i], ".tsv", sep = ""), sep = "/")
  #   importDataResults <- rawTestResultsImport(fileNamePath = fileNamePath)
  #   ## Step 2. Eye tracking analysis.
  #   eyeTrackingResults <- eyeTrackingAnalysis(eyeTrackingDataFrame = importDataResults[[2]])
  #   ## Step 3. Store the RDS files.
  #   fileNamePath <- paste(storeFolderPath, filesList[i], sep = "/")
  #   # saveRDS(object = importDataResults, file = paste(fileNamePath, "importDataResults.Rds", sep = "/"))
  #   # saveRDS(object = eyeTrackingResults, file = paste(fileNamePath, "eyeTrackingResults.Rds", sep = "/"))
  # }
  ##-----------------------------------------------------------------------------------------------------##
  ## Step 1. Raw test results import.
  fileNamePath <- paste(loadFolderPath, filesList[i], sep = "/")
  fileNamePath <- paste(fileNamePath, paste(filesList[i], ".tsv", sep = ""), sep = "/")
  importDataResults <- rawTestResultsImport(fileNamePath = fileNamePath)
  ## Step 2. Eye tracking analysis.
  eyeTrackingResults <- eyeTrackingAnalysis(eyeTrackingDataFrame = importDataResults[[2]])
  ## Step 3. Store the RDS files.
  fileNamePath <- paste(storeFolderPath, filesList[i], sep = "/")
  saveRDS(object = importDataResults, file = paste(fileNamePath, "importDataResults.Rds", sep = "/"))
  saveRDS(object = eyeTrackingResults, file = paste(fileNamePath, "eyeTrackingResults.Rds", sep = "/"))
}