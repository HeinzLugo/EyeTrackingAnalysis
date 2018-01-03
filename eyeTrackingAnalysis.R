#' @description This function analyses the eye tracking test dataframe.
#' @import dplyr
#' @import lsr
#' @import reshape2
#' @import tibble
#' @param eyeTrackingDataFrame: Test data dataframe.
#' @return processedDataFrame: Formatted table data frame.
#' @warning This function is specific to the raw file provided by the Tobii software.
#' @author Heinz Lugo.
library(dplyr)
library(lsr)
library(reshape2)
library(tibble)
library(data.table)
eyeTrackingAnalysis <- function(eyeTrackingDataFrame)
{
  resultsList <- vector(mode = "list", length = 5)
  ##---SECTION 1. PUPIL DIAMETER---##
  ## Step 1. Pupil diametre analysis.
  ## Step 1.1. Movement type and event marker per eye analysis.
  analysisDataFrame <- dplyr::select(eyeTrackingDataFrame, pupilDiameterLeft, pupilDiameterRight, eyeMovementType, eventMarkerColumn)
  analysisDataFrame <- reshape2::melt(data = analysisDataFrame, id = c("eyeMovementType", "eventMarkerColumn"), value.name = "diameter")
  analysisDataFrame$variable <- as.character(analysisDataFrame$variable)
  analysisDataFrame <- dplyr::group_by(analysisDataFrame, eyeMovementType, eventMarkerColumn, variable)
  resultsDataFrame <- dplyr::summarise(analysisDataFrame, meanValue = mean(x = diameter, na.rm = TRUE), 
                                    sdValue = sd(x = diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[2], 
                                    lowerLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(diameter, na.rm = TRUE),
                                    minValue = min(diameter, na.rm = TRUE), sampleNumber = length(eyeMovementType))
  resultsDataFrame <- dplyr::filter(resultsDataFrame, eventMarkerColumn != "") %>% dplyr::ungroup()
  ## Step 1.2. Movement type and event marker combined eye analysis.
  analysisDataFrame <- dplyr::ungroup(analysisDataFrame) %>% dplyr::group_by(eyeMovementType, eventMarkerColumn)
  tempDataFrame <- dplyr::summarise(analysisDataFrame, meanValue = mean(x = diameter, na.rm = TRUE), 
                                    sdValue = sd(x = diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[2], 
                                    lowerLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(diameter, na.rm = TRUE),
                                    minValue = min(diameter, na.rm = TRUE), sampleNumber = length(eyeMovementType))
  tempDataFrame <- dplyr::filter(tempDataFrame, eventMarkerColumn != "")
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), variable = rep(x = "combined", times = nrow(tempDataFrame)), .after = 2)
  resultsDataFrame <- rbind.data.frame(resultsDataFrame, tempDataFrame)
  ## Step 1.3. Movement type per eye analysis.
  analysisDataFrame <- dplyr::ungroup(analysisDataFrame) %>% dplyr::group_by(eyeMovementType, variable)
  tempDataFrame <- dplyr::summarise(analysisDataFrame, meanValue = mean(x = diameter, na.rm = TRUE), 
                                    sdValue = sd(x = diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[2], 
                                    lowerLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(diameter, na.rm = TRUE),
                                    minValue = min(diameter, na.rm = TRUE), sampleNumber = length(eyeMovementType))
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), eventMarkerColumn = rep(x = "total", times = nrow(tempDataFrame)), .before = 2)
  resultsDataFrame <- rbind.data.frame(resultsDataFrame, tempDataFrame)
  ## Step 1.4. Movement type combined eye analysis.
  analysisDataFrame <- dplyr::ungroup(analysisDataFrame) %>% dplyr::group_by(eyeMovementType)
  tempDataFrame <- dplyr::summarise(analysisDataFrame, meanValue = mean(x = diameter, na.rm = TRUE), 
                                    sdValue = sd(x = diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[2], 
                                    lowerLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(diameter, na.rm = TRUE),
                                    minValue = min(diameter, na.rm = TRUE), sampleNumber = length(eyeMovementType))
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), eventMarkerColumn = rep(x = "total", times = nrow(tempDataFrame)), .before = 2)
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), variable = rep(x = "combined", times = nrow(tempDataFrame)), .after = 2)
  resultsDataFrame <- rbind.data.frame(resultsDataFrame, tempDataFrame)
  ## Step 1.5. Event marker per eye analysis.
  analysisDataFrame <- dplyr::ungroup(analysisDataFrame) %>% dplyr::group_by(eventMarkerColumn, variable)
  tempDataFrame <- dplyr::summarise(analysisDataFrame, meanValue = mean(x = diameter, na.rm = TRUE), 
                                    sdValue = sd(x = diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[2], 
                                    lowerLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(diameter, na.rm = TRUE),
                                    minValue = min(diameter, na.rm = TRUE), sampleNumber = length(eventMarkerColumn))
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), eyeMovementType = rep(x = "combined", times = nrow(tempDataFrame)), .before = 2)
  resultsDataFrame <- rbind.data.frame(resultsDataFrame, tempDataFrame)
  ## Step 1.6. Event marker combined eye analysis.
  analysisDataFrame <- dplyr::ungroup(analysisDataFrame) %>% dplyr::group_by(eventMarkerColumn)
  tempDataFrame <- dplyr::summarise(analysisDataFrame, meanValue = mean(x = diameter, na.rm = TRUE), 
                                    sdValue = sd(x = diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[2], 
                                    lowerLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(diameter, na.rm = TRUE),
                                    minValue = min(diameter, na.rm = TRUE), sampleNumber = length(eventMarkerColumn))
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), eyeMovementType = rep(x = "combined", times = nrow(tempDataFrame)), .before = 1)
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), variable = rep(x = "combined", times = nrow(tempDataFrame)), .after = 2)
  resultsDataFrame <- rbind.data.frame(resultsDataFrame, tempDataFrame)
  ## Step 1.7. Per eye analysis.
  analysisDataFrame <- dplyr::ungroup(analysisDataFrame) %>% dplyr::group_by(variable)
  tempDataFrame <- dplyr::summarise(analysisDataFrame, meanValue = mean(x = diameter, na.rm = TRUE), 
                                    sdValue = sd(x = diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[2], 
                                    lowerLimit = lsr::ciMean(x = diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(diameter, na.rm = TRUE),
                                    minValue = min(diameter, na.rm = TRUE), sampleNumber = length(variable))
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), eventMarkerColumn = rep(x = "total", times = nrow(tempDataFrame)), .before = 1)
  tempDataFrame <- tibble::add_column(.data = as.data.frame(tempDataFrame), eyeMovementType = rep(x = "combined", times = nrow(tempDataFrame)), .before = 1)
  resultsDataFrame <- rbind.data.frame(resultsDataFrame, tempDataFrame)
  ## Step 1.8. Combined eye analysis.
  analysisDataFrame <- dplyr::ungroup(analysisDataFrame)
  tempDataFrame <- dplyr::tbl_df(data.frame(eyeMovementType = "combined", eventMarkerColumn = "total", variable = "combined",
                                            meanValue = mean(x = analysisDataFrame$diameter, na.rm = TRUE),
                                            sdValue = sd(x = analysisDataFrame$diameter, na.rm = TRUE), upperLimit = lsr::ciMean(x = analysisDataFrame$diameter, conf = 0.95, na.rm = TRUE)[2], 
                                            lowerLimit = lsr::ciMean(x = analysisDataFrame$diameter, conf = 0.95, na.rm = TRUE)[1], maxValue = max(analysisDataFrame$diameter, na.rm = TRUE),
                                            minValue = min(analysisDataFrame$diameter, na.rm = TRUE), sampleNumber = nrow(analysisDataFrame)))
  resultsDataFrame <- rbind.data.frame(resultsDataFrame, tempDataFrame)
  resultsDataFrame <- dplyr::filter(resultsDataFrame, eventMarkerColumn != "")
  resultsList[[1]] <- resultsDataFrame
  
  ##---SECTION 2. TIME ABOVE MEAN PUPIL DIAMETER---##
  ## Step 2. Time above mean pupil diameter.
  analysisDataFrame <- dplyr::select(eyeTrackingDataFrame, recordingTimestamp, pupilDiameterLeft, pupilDiameterRight, eyeMovementType, eventMarkerColumn)
  analysisDataFrame <- reshape2::melt(data = analysisDataFrame, id = c("recordingTimestamp", "eyeMovementType", "eventMarkerColumn"), value.name = "diameter")
  analysisDataFrame$variable <- as.character(analysisDataFrame$variable)
  eventDetailsDataFrame <- dplyr::filter(resultsDataFrame, !is.nan(meanValue)) %>% dplyr::select(eyeMovementType:meanValue)
  timeAboveEventList <- vector(mode = "list", length = (nrow(eventDetailsDataFrame) + 1))
  tempEventList <- vector(mode = "list", length = 2)
  deltaTime <- 1 / 50
  for(i in 1:(length(timeAboveEventList) - 1))
  {
    eyeMovementTypeFilter <- eventDetailsDataFrame$eyeMovementType[i]
    eventMarkerColumnFilter <- eventDetailsDataFrame$eventMarkerColumn[i]
    variableFilter <- eventDetailsDataFrame$variable[i]
    meanValue <- eventDetailsDataFrame$meanValue[i]
    if(eyeMovementTypeFilter != "combined")
    {
      tempDataFrame <- dplyr::filter(analysisDataFrame, eyeMovementType == eyeMovementTypeFilter)
    }
    else
    {
      tempDataFrame <- analysisDataFrame
    }
    if(eventMarkerColumnFilter != "total")
    {
      tempDataFrame <- dplyr::filter(tempDataFrame, eventMarkerColumn == eventMarkerColumnFilter)
    }
    if(variableFilter != "combined")
    {
      tempDataFrame <- dplyr::filter(tempDataFrame, variable == variableFilter)
    }
    ## Step 2.1. Time above event.
    tempDataFrame <- dplyr::filter(tempDataFrame, diameter >= meanValue)
    tempEventList[[1]] <- tempDataFrame
    ## Step 2.2. Cumulative time above event.
    tempDataFrame <- dplyr::tbl_df(data.frame(eyeMovementType = eyeMovementTypeFilter,
                                              eventMarkerColumn = eventMarkerColumnFilter,
                                              variable = variableFilter,
                                              timeAboveMean = nrow(tempDataFrame) * deltaTime))
    tempDataFrame <- dplyr::mutate(tempDataFrame, percentageTimeAboveMean = timeAboveMean / (nrow(eyeTrackingDataFrame) * deltaTime))
    tempEventList[[2]] <- tempDataFrame
    ## Step 2.3. Store results.
    timeAboveEventList[[i]] <- tempEventList
  }
  timeAboveEventList[[length(timeAboveEventList)]] <- eventDetailsDataFrame
  resultsList[[2]] <- timeAboveEventList
  
  ##---SECTION 3. FIXATION DURATION---##
  ## Step 3. Fixation duration.
  tempEventList <- vector(mode = "list", length = 6)
  ## Step 3.1. Fixation duration event detections.
  analysisDataFrame <- dplyr::select(eyeTrackingDataFrame, recordingTimestamp, eyeMovementType, eventMarkerColumn) %>%
    dplyr::filter(eyeMovementType == "Fixation") %>% dplyr::filter(eventMarkerColumn != "")
  durationEventIndex <- rep(x = 1, tims = nrow(analysisDataFrame))
  indexEvent <- 1
  for(i in 2: nrow(analysisDataFrame))
  {
    if(analysisDataFrame$recordingTimestamp[i] - analysisDataFrame$recordingTimestamp[i - 1] != 20)
    {
      indexEvent <- indexEvent + 1
    }
    durationEventIndex[i] <- indexEvent
  }
  analysisDataFrame <- dplyr::mutate(analysisDataFrame, durationEventIndex)
  tempEventList[[1]] <- analysisDataFrame
  ## Step 3.2. Fixation duration analysis.
  ## Step 3.2.1. Fixation event duration calculation.
  analysisDataFrame <- dplyr::group_by(analysisDataFrame, eyeMovementType, eventMarkerColumn, durationEventIndex)
  tempDataFrame <- dplyr::summarise(analysisDataFrame, durationEvent = length(durationEventIndex) * deltaTime)
  tempEventList[[2]] <- tempDataFrame
  ## Step 3.2.2. Fixation event distribution analysis.
  tempDataFrame <- dplyr::ungroup(tempDataFrame) %>% dplyr::group_by(eventMarkerColumn) 
  tempDataFrameResults <- dplyr::summarise(tempDataFrame, meanValue = mean(durationEvent, na.rm = TRUE),
                                           sdValue = sd(durationEvent, na.rm = TRUE),
                                           upperLimit = lsr::ciMean(x = durationEvent, conf = 0.95, na.rm = TRUE)[2], 
                                           lowerLimit = lsr::ciMean(x = durationEvent, conf = 0.95, na.rm = TRUE)[1], maxValue = max(durationEvent, na.rm = TRUE),
                                           minValue = min(durationEvent, na.rm = TRUE), sampleNumber = length(eventMarkerColumn))
  tempDataFrame <- dplyr::ungroup(tempDataFrame)
  tempResult <- data.frame(eventMarkerColumn = "combined", meanValue = mean(tempDataFrame$durationEvent, na.rm = TRUE),
                           sdValue = sd(tempDataFrame$durationEvent, na.rm = TRUE),
                           upperLimit = lsr::ciMean(x = tempDataFrame$durationEvent, conf = 0.95, na.rm = TRUE)[2], 
                           lowerLimit = lsr::ciMean(x = tempDataFrame$durationEvent, conf = 0.95, na.rm = TRUE)[1], maxValue = max(tempDataFrame$durationEvent, na.rm = TRUE),
                           minValue = min(tempDataFrame$durationEvent, na.rm = TRUE), sampleNumber = nrow(tempDataFrame))
  tempDataFrameResults <- rbind(tempDataFrameResults, tempResult)
  tempEventList[[3]] <- tempDataFrameResults
  ## Step 3.2.3. Fixation event time above mean.
  for(i in 1:nrow(tempDataFrameResults))
  {
    eventMarkerColumnFilter <- tempDataFrameResults$eventMarkerColumn[i]
    if(eventMarkerColumnFilter != "combined")
    {
      tempResultIntermediate <- dplyr::filter(tempDataFrame, eventMarkerColumn == eventMarkerColumnFilter) %>%
        dplyr::filter(durationEvent >= tempDataFrameResults$meanValue[i])
    }
    else
    {
      tempResultIntermediate <- dplyr::filter(tempDataFrame, durationEvent >= tempDataFrameResults$meanValue[i]) %>%
        dplyr::mutate(eventMarkerColumn = "combined")
    }
    if(i == 1)
    {
      tempResult <- tempResultIntermediate
    }
    else
    {
      tempResult <- rbind.data.frame(tempResult, tempResultIntermediate)
    }
  }
  tempEventList[[4]] <- tempResult
  ## Step 3.2.4. Cumulative fixation event time above mean.
  tempResult <- dplyr::group_by(tempResult, eventMarkerColumn)
  tempResult <- dplyr::summarise(tempResult, timeAboveMean = sum(durationEvent))
  tempResult <- dplyr::mutate(tempResult, percentageTimeAboveMean = timeAboveMean / (nrow(eyeTrackingDataFrame) * deltaTime))
  tempEventList[[5]] <- tempResult  
  ## Step 3.2.5. Time spent in fixation.
  analysisDataFrame <- tempEventList[[1]]
  analysisDataFrame <- dplyr::group_by(analysisDataFrame, eventMarkerColumn)
  tempDataFrameResults <- dplyr::summarise(analysisDataFrame, timeFixation = length(eventMarkerColumn) * deltaTime)
  tempDataFrameResults <- rbind.data.frame(tempDataFrameResults, data.frame(eventMarkerColumn = "combined", timeFixation = nrow(analysisDataFrame) * deltaTime))
  tempDataFrameResults <- dplyr::mutate(tempDataFrameResults, percentageTimeFixation = timeFixation / (nrow(eyeTrackingDataFrame) * deltaTime))
  tempEventList[[6]] <- tempDataFrameResults
  resultsList[[3]] <- tempEventList
  
  ##---SECTION 4. SACCADE DURATION---##
  ## Step 4. Saccade duration.
  tempEventList <- vector(mode = "list", length = 5)
  ## Step 4.1. Saccade duration event detections.
  analysisDataFrame <- dplyr::select(eyeTrackingDataFrame, recordingTimestamp, eyeMovementType, gaze3DPositionCombinedX, gaze3DPositionCombinedY, eventMarkerColumn)
  saccadeIndexes <- which(analysisDataFrame$eyeMovementType == "Saccade")
  tempColumn <- rep(x = 0, times = 2 * length(saccadeIndexes))
  for(i in 1:length(saccadeIndexes))
  {
    index <- saccadeIndexes[i]
    if(index == 1)
    {
      tempColumn[(2 * i) - 1 ] <- index
      tempColumn[2*i] <- index + 1
    }
    else if(index != length(saccadeIndexes))
    {
      tempColumn[(2 * i) - 1 ] <- index - 1
      tempColumn[2*i] <- index + 1
    }
    else
    {
      tempColumn[(2 * i) - 1 ] <- index - 1
      tempColumn[2*i] <- index 
    }
  }
  if(length(which(tempColumn == 0)) != 0)
  {
    stop("Check the saccade event detection method.")
  }
  saccadeIndexes <- c(saccadeIndexes, tempColumn)
  saccadeIndexes <- sort(saccadeIndexes)
  analysisDataFrame <- analysisDataFrame[c(saccadeIndexes), ]
  analysisDataFrame <- analysisDataFrame[!duplicated(analysisDataFrame), ]
  analysisDataFrame <- dplyr::filter(analysisDataFrame, eventMarkerColumn != "")
  eventMarkerColumnOptions <- unique(analysisDataFrame$eventMarkerColumn)
  for(i in 1:length(eventMarkerColumnOptions))
  {
    tempDataFrame <- dplyr::filter(analysisDataFrame, eventMarkerColumn == eventMarkerColumnOptions[i])
    differencesX <- tempDataFrame$gaze3DPositionCombinedX - data.table::shift(x = tempDataFrame$gaze3DPositionCombinedX, n = 1, type = "lag") 
    differencesY <- tempDataFrame$gaze3DPositionCombinedY - data.table::shift(x = tempDataFrame$gaze3DPositionCombinedY, n = 1, type = "lag")
    tempDataFrame <- dplyr::mutate(tempDataFrame, differencesX, differencesY)
    tempDataFrame <- dplyr::mutate(tempDataFrame, distanceTraveled = sqrt(differencesX^2 + differencesY^2))
    if(i == 1)
    {
      tempDataFrameResults <- tempDataFrame
    }
    else
    {
      tempDataFrameResults <- rbind.data.frame(tempDataFrameResults, tempDataFrame)
    }
  }
  potentialFalseEventIndexes <- which(tempDataFrameResults$eyeMovementType != "Saccade")
  for(i in 1:length(potentialFalseEventIndexes))
  {
    index <- potentialFalseEventIndexes[i]
    if(index != 1)
    {
      if(tempDataFrameResults$eyeMovementType[index - 1] != "Saccade" && !is.na(tempDataFrameResults$eyeMovementType[index - 1]))
      {
        tempDataFrameResults$differencesX[index] <- NA
        tempDataFrameResults$differencesY[index] <- NA
        tempDataFrameResults$distanceTraveled[index] <- NA
      }
    }
  }
  tempEventList[[1]] <- tempDataFrameResults
  ## Step 4.2. Saccade analysis.
  tempDataFrameGrouped <- dplyr::group_by(tempDataFrameResults, eventMarkerColumn)
  tempDataFrameResults <- dplyr::summarise(tempDataFrameGrouped, meanValue = mean(distanceTraveled, na.rm = TRUE),
                                           sdValue = sd(distanceTraveled, na.rm = TRUE),
                                           upperLimit = lsr::ciMean(x = distanceTraveled, conf = 0.95, na.rm = TRUE)[2], 
                                           lowerLimit = lsr::ciMean(x = distanceTraveled, conf = 0.95, na.rm = TRUE)[1], maxValue = max(distanceTraveled, na.rm = TRUE),
                                           minValue = min(distanceTraveled, na.rm = TRUE), totalValue = sum(distanceTraveled, na.rm = TRUE),sampleNumber = length(eventMarkerColumn))
  tempDataFrameGrouped <- dplyr::ungroup(tempDataFrameGrouped)
  tempResultIntermediate <- data.frame(eventMarkerColumn = "combined", meanValue = mean(tempDataFrameGrouped$distanceTraveled, na.rm = TRUE),
                                       sdValue = sd(tempDataFrameGrouped$distanceTraveled, na.rm = TRUE),
                                       upperLimit = lsr::ciMean(x = tempDataFrameGrouped$distanceTraveled, conf = 0.95, na.rm = TRUE)[2], 
                                       lowerLimit = lsr::ciMean(x = tempDataFrameGrouped$distanceTraveled, conf = 0.95, na.rm = TRUE)[1], maxValue = max(tempDataFrameGrouped$distanceTraveled, na.rm = TRUE),
                                       minValue = min(tempDataFrameGrouped$distanceTraveled, na.rm = TRUE), totalValue = sum(tempDataFrameGrouped$distanceTraveled, na.rm = TRUE), sampleNumber = nrow(tempDataFrameGrouped))
  tempDataFrameResults <- rbind.data.frame(tempDataFrameResults, tempResultIntermediate)
  tempEventList[[2]] <- tempDataFrameResults
  ## Step 4.3. Saccade movement above the mean.
  tempDataFrame <- dplyr::ungroup(tempDataFrameGrouped)
  for(i in 1:nrow(tempDataFrameResults))
  {
    eventMarkerColumnFilter <- tempDataFrameResults$eventMarkerColumn[i]
    if(eventMarkerColumnFilter != "combined")
    {
      tempResultIntermediate <- dplyr::filter(tempDataFrame, eventMarkerColumn == eventMarkerColumnFilter) %>%
        dplyr::filter(distanceTraveled >= tempDataFrameResults$meanValue[i])
    }
    else
    {
      tempResultIntermediate <- dplyr::filter(tempDataFrame, distanceTraveled >= tempDataFrameResults$meanValue[i]) %>%
        dplyr::mutate(eventMarkerColumn = "combined")
    }
    if(i == 1)
    {
      tempResult <- tempResultIntermediate
    }
    else
    {
      tempResult <- rbind.data.frame(tempResult, tempResultIntermediate)
    }
  }
  tempEventList[[3]] <- tempResult
  ## Step 4.4. Cumulative saccade event time above mean.
  tempResult <- dplyr::group_by(tempResult, eventMarkerColumn)
  tempResult <- dplyr::summarise(tempResult, timeAboveMean = length(eventMarkerColumn) * deltaTime)
  tempResult <- dplyr::mutate(tempResult, percentageTimeAboveMean = timeAboveMean / (nrow(eyeTrackingDataFrame) * deltaTime))
  tempEventList[[4]] <- tempResult  
  ## Step 4.5. Time spent in saccade.
  analysisDataFrame <- tempEventList[[1]]
  analysisDataFrame <- dplyr::filter(analysisDataFrame, !is.na(distanceTraveled))
  analysisDataFrame <- dplyr::group_by(analysisDataFrame, eventMarkerColumn)
  tempDataFrameResults <- dplyr::summarise(analysisDataFrame, timeSaccade = length(eventMarkerColumn) * deltaTime)
  tempDataFrameResults <- rbind.data.frame(tempDataFrameResults, data.frame(eventMarkerColumn = "combined", timeSaccade = nrow(analysisDataFrame) * deltaTime))
  tempDataFrameResults <- dplyr::mutate(tempDataFrameResults, percentageTimeSaccade = timeSaccade / (nrow(eyeTrackingDataFrame) * deltaTime))
  tempEventList[[5]] <- tempDataFrameResults
  resultsList[[4]] <- tempEventList

  ##---SECTION 5. EVENT DURATION---##
  tempEventList <- vector(mode = "list", length = 6)
  ## Step 5.1. Look at event detections.
  startEventIndexes <- grep(pattern = "^startlookat.+", x = eyeTrackingDataFrame$event)
  eventNames <- sub(pattern = "^start", replacement = "", x = eyeTrackingDataFrame$event[startEventIndexes])
  stopEventNames <- as.character(sapply(eventNames, function (x) paste("stop", x, sep = "")))
  stopEventNames <- unique(stopEventNames)
  if(length(stopEventNames) == 0)
  {
    stop("No stop events are detected.")
  }
  else
  {
    for(i in 1:length(stopEventNames))
    {
      indexes <- which(eyeTrackingDataFrame$event == stopEventNames[i])
      if(i == 1)
      {
        stopEventIndexes <- indexes
      }
      else
      {
        stopEventIndexes <- c(stopEventIndexes, indexes)
      }
    }
  }
  if(length(startEventIndexes) != length(stopEventIndexes))
  {
    stop("A start event does not have the corresponding stop event.")
  }
  eventIndexes <- c(startEventIndexes, stopEventIndexes)
  eventIndexes <- sort(eventIndexes)
  analysisDataFrame <- eyeTrackingDataFrame[c(eventIndexes), ]
  analysisDataFrame <- dplyr::select(analysisDataFrame, recordingTimestamp, event)
  for(i in 1:(nrow(analysisDataFrame) / 2))
  {
    tempIndexes <- rep(x = i, times = 2)
    if(i == 1)
    {
      eventIndexesColumn <- tempIndexes
    }
    else
    {
      eventIndexesColumn <- c(eventIndexesColumn, tempIndexes)
    }
  }
  if(length(stopEventIndexes) != 0)
  {
    analysisDataFrame <- dplyr::mutate(analysisDataFrame, eventIndexesColumn, eventNames = rep(x = eventNames, each = 2))
    tempEventList[[1]] <- analysisDataFrame
    ## Step 5.2. Event duration. The results are presented in seconds.
    millisecondsToSecondsFactor <- 1/1000
    analysisDataFrame <- dplyr::group_by(analysisDataFrame, eventIndexesColumn, eventNames)
    tempDataFrameResults <- dplyr::summarise(analysisDataFrame, difference = (recordingTimestamp[2] - recordingTimestamp[1]) * millisecondsToSecondsFactor)
    tempEventList[[2]] <- tempDataFrameResults
    ## Step 5.3. Event duration analysis.
    tempDataFrameResults <- dplyr::group_by(tempDataFrameResults, eventNames)
    tempDataFrameIntermediate <- dplyr::summarise(tempDataFrameResults, meanValue = mean(difference, na.rm = TRUE),
                                                  sdValue = sd(difference, na.rm = TRUE),
                                                  upperLimit = lsr::ciMean(x = difference, conf = 0.95, na.rm = TRUE)[2],
                                                  lowerLimit = lsr::ciMean(x = difference, conf = 0.95, na.rm = TRUE)[1],
                                                  maxValue = max(difference, na.rm = TRUE),
                                                  minValue = min(difference, na.rm = TRUE),
                                                  sampleNumber = length(eventNames))
    tempEventList[[3]] <- tempDataFrameIntermediate
    tempDataFrameResults <- dplyr::ungroup(tempDataFrameResults)
    ## Step 5.4. Event duration above the mean.
    eventsNamesTypes <- unique(tempDataFrameResults$eventNames)
    for(i in 1:length(eventsNamesTypes))
    {
      tempDataFrame <-  dplyr::filter(tempDataFrameResults, eventNames == eventsNamesTypes[i]) %>%
        dplyr::filter(difference >= dplyr::filter(tempDataFrameIntermediate, eventNames == eventsNamesTypes[i])$meanValue)
      if(i == 1)
      {
        tempResult <- tempDataFrame
      }
      else
      {
        tempResult <- rbind.data.frame(tempResult, tempDataFrame)
      }
    }
    tempEventList[[4]] <- tempResult
    ## Step 5.5. Cumulative event duration above the mean.
    for(i in 1:length(eventsNamesTypes))
    {
      tempDataFrame <- dplyr::filter(tempResult, eventNames == eventsNamesTypes[i])
      tempDataFrame <- tbl_df(data.frame(eventNames = unique(tempDataFrame$eventNames),
                                         timeAboveMean = sum(tempDataFrame$difference), 
                                         percentageTimeAboveMeanEvent = sum(tempDataFrame$difference) / sum(dplyr::filter(tempDataFrameResults, eventNames == eventsNamesTypes[i])$difference), 
                                         percentageTimeAboveMean = sum(tempDataFrame$difference) / (nrow(eyeTrackingDataFrame) * deltaTime)))
      if(i == 1)
      {
        tempDataFrameIntermediate <- tempDataFrame
      }
      else
      {
        tempDataFrameIntermediate <- rbind.data.frame(tempDataFrameIntermediate, tempDataFrame)
      }
    }
    tempEventList[[5]] <- tempDataFrameIntermediate
    ## Step 5.6. Time spent at event.
    tempDataFrameResults <- tempEventList[[2]]
    tempDataFrameResults <- dplyr::group_by(tempDataFrameResults, eventNames)
    tempDataFrameResults <- dplyr::summarise(tempDataFrameResults, timeAtEvent = sum(difference), percentageTimeAtEvent = timeAtEvent / (nrow(eyeTrackingDataFrame) * deltaTime))
    tempEventList[[6]] <- tempDataFrameResults
  }
  else
  {
    tempEventList[[1]] <- "No look at events detected."
    tempEventList[[2]] <- "No look at events detected."
    tempEventList[[3]] <- "No look at events detected."
    tempEventList[[4]] <- "No look at events detected."
    tempEventList[[5]] <- "No look at events detected."
    tempEventList[[6]] <- "No look at events detected."
  }
  resultsList[[5]] <- tempEventList
  resultsList
}