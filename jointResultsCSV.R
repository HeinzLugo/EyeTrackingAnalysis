#' @description This script joins all the individual results in csv files.
#' @author Heinz Lugo.
## Step 0. Clean up the environment, load the required libraries and set the loading and storing folder paths.
rm(list=ls())
library(dplyr)
library(data.table)
loadFolderPath <- file.path("", "Users", "heinzlugo", "Dropbox", "EyeTrackingTests", "TestResults", "Individual")
storeFolderPath <- file.path("", "Users", "heinzlugo", "Dropbox", "EyeTrackingTests", "TestResults", "Joint")
filesList <- list.files(path = loadFolderPath)
filesList <- filesList[order(nchar(filesList), filesList)]
## Step 1. Dataframe join.
for(i in 1:length(filesList))
{
  print(i)
  resultList <- readRDS(file = paste(paste(loadFolderPath, filesList[i], sep = "/"), "eyeTrackingResults.Rds", sep = "/"))
  ## 1. Pupil diametre analysis.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[1]]), participant = i)
  if(i == 1)
  {
    pupilDiametreAnalysis <- tempDataFrame
  }
  else
  {
    pupilDiametreAnalysis <- rbind.data.frame(pupilDiametreAnalysis, tempDataFrame)
  }
  ## 2. Cumulative time above mean pupil diametre.
  tempList <- resultList[[2]]
  for(j in 1:length(tempList))
  {
    tempDataFrame <- tempList[[j]][[2]]
    if(j == 1)
    {
      auxiliaryDataFrame <- tempDataFrame
    }
    else
    {
      auxiliaryDataFrame <- rbind.data.frame(auxiliaryDataFrame, tempDataFrame)
    }
  }
  auxiliaryDataFrame <- dplyr::mutate(auxiliaryDataFrame, participant = i)
  if(i == 1)
  {
    cumulativePupilDiametreTimeAboveMean <- auxiliaryDataFrame
  }
  else
  {
    cumulativePupilDiametreTimeAboveMean <- rbind.data.frame(cumulativePupilDiametreTimeAboveMean, auxiliaryDataFrame)
  }
  ## 3. Fixation event duration analysis.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[3]][[3]]), participant = i)
  if(i == 1)
  {
    fixationEventDurationAnalysis <- tempDataFrame
  }
  else
  {
    fixationEventDurationAnalysis <- rbind.data.frame(fixationEventDurationAnalysis, tempDataFrame)
  }
  ## 4. Cumulative fixation event time above mean.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[3]][[5]]), participant = i)
  if(i == 1)
  {
    cumulativeFixationEventTimeAboveMean <- tempDataFrame
  }
  else
  {
    cumulativeFixationEventTimeAboveMean <- rbind.data.frame(cumulativeFixationEventTimeAboveMean, tempDataFrame)
  }
  ## 5. Time spent in fixation.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[3]][[6]]), participant = i)
  if(i == 1)
  {
    timeSpentInFixation<- tempDataFrame
  }
  else
  {
    timeSpentInFixation <- rbind.data.frame(timeSpentInFixation, tempDataFrame)
  }
  ## 6. Saccade event duration analysis.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[4]][[2]]), participant = i)
  if(i == 1)
  {
    saccadeEventDurationAnalysis<- tempDataFrame
  }
  else
  {
    saccadeEventDurationAnalysis <- rbind.data.frame(saccadeEventDurationAnalysis, tempDataFrame)
  }
  ## 7. Cumulative saccade time above the mean.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[4]][[4]]), participant = i)
  if(i == 1)
  {
    cumulativeSaccadeEventTimeAboveMean<- tempDataFrame
  }
  else
  {
    cumulativeSaccadeEventTimeAboveMean <- rbind.data.frame(cumulativeSaccadeEventTimeAboveMean, tempDataFrame)
  }
  ## 8. Time spent in saccade.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[4]][[5]]), participant = i)
  if(i == 1)
  {
    timeSpentInSaccade<- tempDataFrame
  }
  else
  {
    timeSpentInSaccade <- rbind.data.frame(timeSpentInSaccade, tempDataFrame)
  }
  ## 9. Look at screen duration analysis.
  tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[5]][[3]]), participant = i)
  if(tempDataFrame[1,1] != "No look at screen events detected.")
  {
    if(!exists("lookAtScreenEventDurationAnalysis"))
    {
      lookAtScreenEventDurationAnalysis <- tempDataFrame
    }
    else
    {
      lookAtScreenEventDurationAnalysis <- rbind.data.frame(lookAtScreenEventDurationAnalysis, tempDataFrame)
    }
    ## 10. Look at screen cumulative event duration above the mean.
    tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[5]][[5]]), participant = i)
    if(!exists("cumulativeLookAtScreenEventTimeAboveMean"))
    {
      cumulativeLookAtScreenEventTimeAboveMean <- tempDataFrame
    }
    else
    {
      cumulativeLookAtScreenEventTimeAboveMean <- rbind.data.frame(cumulativeLookAtScreenEventTimeAboveMean, tempDataFrame)
    }
    ## 11. Time spent in Look at screen event.
    tempDataFrame <- dplyr::mutate(as.data.frame(resultList[[5]][[6]]), participant = i)
    if(!exists("timeSpentInLookAtScreen"))
    {
      timeSpentInLookAtScreen<- tempDataFrame
    }
    else
    {
      timeSpentInLookAtScreen <- rbind.data.frame(timeSpentInLookAtScreen, tempDataFrame)
    }
  }
}
## Step 2. Save files.
fwrite(x = pupilDiametreAnalysis, file = paste(storeFolderPath, "pupilDiametreAnalysis.csv", sep = "/"))
fwrite(x = cumulativePupilDiametreTimeAboveMean, file = paste(storeFolderPath, "cumulativePupilDiametreTimeAboveMean.csv", sep = "/"))
fwrite(x = fixationEventDurationAnalysis, file = paste(storeFolderPath, "fixationEventDurationAnalysis.csv", sep = "/"))
fwrite(x = cumulativeFixationEventTimeAboveMean, file = paste(storeFolderPath, "cumulativeFixationEventTimeAboveMean.csv", sep = "/"))
fwrite(x = timeSpentInFixation, file = paste(storeFolderPath, "timeSpentInFixation.csv", sep = "/"))
fwrite(x = saccadeEventDurationAnalysis, file = paste(storeFolderPath, "saccadeEventDurationAnalysis.csv", sep = "/"))
fwrite(x = cumulativeSaccadeEventTimeAboveMean, file = paste(storeFolderPath, "cumulativeSaccadeEventTimeAboveMean.csv", sep = "/"))
fwrite(x = timeSpentInSaccade, file = paste(storeFolderPath, "timeSpentInSaccade.csv", sep = "/"))
fwrite(x = lookAtScreenEventDurationAnalysis, file = paste(storeFolderPath, "lookAtScreenEventDurationAnalysis.csv", sep = "/"))
fwrite(x = cumulativeLookAtScreenEventTimeAboveMean, file = paste(storeFolderPath, "cumulativeLookAtScreenEventTimeAboveMean.csv", sep = "/"))
fwrite(x = timeSpentInLookAtScreen, file = paste(storeFolderPath, "timeSpentInLookAtScreen.csv", sep = "/"))





