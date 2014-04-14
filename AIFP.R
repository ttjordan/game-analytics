rm(list=ls())

numLevels <- 13
lockBinding("numLevels", globalenv())
#filename <- "/Users/tjordan/Desktop/AI/FinalProjectData/ImpulseExport_2013-07-12_15-29-40.csv"
filename <- "/Users/tjordan/Desktop/AI/FinalProjectData/ID158_CODING_NEU.csv"

playData <- read.csv(filename)

numberObservations <- nrow(playData)
playData <- playData[order(playData$Timestamp, playData$X.),] 

levelIndex <- mat.or.vec(numberObservations, 1)
playInLevelIndex <- mat.or.vec(numberObservations, 1)

levelStartEvents <- c("Level  1 Start", "Level  2 Start", "Level  3 Start", "Level  4 Start", "Level  5 Start", "Level  6 Start"
                      , "Level  7 Start", "Level  8 Start", "Level  9 Start", "Level  10 Start", "Level  11 Start", "Level  12 Start"
                      , "Level  13 Start")

levelEndEvents <- c("Level  1 End", "Level  2 End", "Level  3 End", "Level  4 End", "Level  5 End", "Level  6 End"
                    , "Level  7 End", "Level  8 End", "Level  9 End", "Level  10 End", "Level  11 End", "Level  12 End"
                    , "Level  13 End")

levelRestartEvent <- "Restart Button Clicked"
mouseClickEvent <- "Mouse Click Impulse"

playData <- cbind(levelIndex, playInLevelIndex, playData)
currentLevel <- 1
currentPlay <- mat.or.vec(numLevels, 1)
currentPlay <- currentPlay + 1

row <- 1
previousPlayNumber <- 0
while(row < numberObservations) {
  startIndex <- sum(which(levelStartEvents==playData[row, 7],arr.ind=TRUE))
  if(startIndex != 0) {
    previousPlayNumber <- currentPlay[startIndex]
    currentLevel <- startIndex
    playData[row, 1] <- currentLevel
    playData[row, 2] <- currentPlay[startIndex]
    levelNotEnded = TRUE
    while(levelNotEnded) {
      row <- row + 1
      endIndex <- sum(which(levelEndEvents==playData[row, 7],arr.ind=TRUE))
      if(endIndex == 0) {
        playData[row, 1] <- currentLevel
        playData[row, 2] <- currentPlay[startIndex]
      } else {
        levelNotEnded <- FALSE
        playData[row, 1] <- currentLevel
        playData[row, 2] <- currentPlay[startIndex]
        currentPlay[startIndex] <- currentPlay[startIndex] + 1
      }
    }
  } else {
    if(playData[row, 7] == levelRestartEvent) {
      playData[row, 1] <- currentLevel
      playData[row, 2] <- previousPlayNumber
    }
  }
  row <- row + 1
}

restartPerLevel <- mat.or.vec(numLevels, 1)
timePerLevelInTimestamps <- mat.or.vec(numLevels, 1)
normalizedRestartPerLevel <- mat.or.vec(numLevels, 2)

for(level in 1:numLevels) {
  levelSubset <- subset(playData, levelIndex == level)
  numPlays <- max(levelSubset$playInLevelIndex)
  for(subLevel in 1:numPlays) {
    subLevelSubset <- subset(levelSubset, playInLevelIndex == subLevel)
    diff <- max(subLevelSubset$Timestamp) - min(subLevelSubset$Timestamp)
    timePerLevelInTimestamps[level] <- timePerLevelInTimestamps[level] + diff
  }
}

for(level in 1:numLevels) {
  restartPerLevel[level] <- nrow(subset(playData, levelIndex == level & Event == levelRestartEvent))
  normalizedRestartPerLevel[level, 1] <- level
  normalizedRestartPerLevel[level, 2] <- nrow(subset(playData, levelIndex == level & Event == levelRestartEvent))/timePerLevelInTimestamps[level]
}


playStats <- mat.or.vec(1, 4)
colnames(playStats) <- list("level", "playNumber", "avgDistanceBetweenImpulseAndBall", "averageRoundDuration")

for(level in 1:numLevels) {
  levelSubset <- subset(playData, levelIndex == level)
  numPlays <- max(levelSubset$playInLevelIndex)
  distance <- 0
  for(subLevel in 1:numPlays) {
    subLevelSubset <- subset(levelSubset, playInLevelIndex == subLevel & Event == mouseClickEvent)
    if(nrow(subLevelSubset) == 0) {
      newRow <- t(c(level, subLevel, -1, subLevelSubset$F59..Round.Duration[1]))
      playStats <- rbind(playStats, newRow)
    } else {
      for(i in 1:nrow(subLevelSubset)) {
        distance <- distance + subLevelSubset[i,]$F19..Proximity.of.Impulse.to.Player
      }
      distance <- distance / nrow(subLevelSubset)
      newRow <- t(c(level, subLevel, distance, subLevelSubset$F59..Round.Duration[1]))
      playStats <- rbind(playStats, newRow)
    }
    distance <- 0
  }
}
playStats <- playStats[2:nrow(playStats),]




countImpulseClosestToOtherBall <- mat.or.vec(nrow(playStats),1)
timestampList <- list()
cc <- 1
counter <- 1
for(level in 1:numLevels) {
  levelSubset <- subset(playData, levelIndex == level)
  numPlays <- max(levelSubset$playInLevelIndex)
  
  for(subLevel in 1:numPlays) {
    subLevelSubset <- subset(levelSubset, playInLevelIndex == subLevel & Event == mouseClickEvent)
    if(nrow(subLevelSubset) == 0) {
      countImpulseClosestToOtherBall[counter] <- 0
    } else {
      for(i in 1:nrow(subLevelSubset)) {
        if(!is.na(subLevelSubset$F25..Distance.of.Impulse.to.Closest.Other.Ball[i]) & subLevelSubset$F25..Distance.of.Impulse.to.Closest.Other.Ball[i] < subLevelSubset$F19..Proximity.of.Impulse.to.Player[i]) {
          countImpulseClosestToOtherBall[counter] <- countImpulseClosestToOtherBall[counter] + 1
          timestampList[cc] <- subLevelSubset$Timestamp[i]
          cc <- cc + 1
        }
      }
    }
    counter <- counter + 1
  }
}

impulseInFrontCounter <- mat.or.vec(nrow(playStats),1)
angleBetweenImpulsePlayer <- mat.or.vec(nrow(playStats),1)
for(level in 1:numLevels) {
  levelSubset <- subset(playData, levelIndex == level)
  numPlays <- max(levelSubset$playInLevelIndex)
  counter <- 0
  for(subLevel in 1:numPlays) {
    subLevelSubset <- subset(levelSubset, playInLevelIndex == subLevel & Event == mouseClickEvent)
    if(nrow(subLevelSubset) == 0) {
      angleBetweenImpulsePlayer[counter] <- -361
      impulseInFrontCounter[counter] <- -1
    } else {
      for(i in 1:nrow(subLevelSubset)) {
        if(subLevelSubset$F5..Player.Vector.X[i] != 0 & subLevelSubset$F5..Player.Vector.Y[i] != 0) {
          pVector <-c(subLevelSubset$F5..Player.Vector.X[i], subLevelSubset$F5..Player.Vector.Y[i])
          PlayerToImpulseVector <-c(subLevelSubset$F1..Player.Position.X[i] - subLevelSubset$F2..Impulse.X[i], subLevelSubset$F1..Player.Position.Y[i] - subLevelSubset$F2..Impulse.Y[i])
          
          dotProduct <- pVector %*% PlayerToImpulseVector
          magnitudePlayerVector <- sqrt(sum(pVector^2))
          magnitudePlayerToImpulseVector <- sqrt(sum(PlayerToImpulseVector^2))
          
          angleBetween <- acos(dotProduct / (magnitudePlayerVector * magnitudePlayerToImpulseVector))
          angleBetween <- angleBetween / pi * 180
          angleBetweenImpulsePlayer[counter] <- angleBetween
          if(abs(angleBetween) <= 10) {
            impulseInFrontCounter[counter] <- 1
          } else {
            impulseInFrontCounter[counter] <- 0
          }
        } else {
          angleBetweenImpulsePlayer[counter] <- -361
          impulseInFrontCounter[counter] <- -1
        }
        counter <- counter + 1
      }
    }
  }
}

playStats <- cbind(playStats, countImpulseClosestToOtherBall, angleBetweenImpulsePlayer, impulseInFrontCounter)

