myGameId = 66

load("aurora.RData")
library(magrittr)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)

myGame <- filter(Game, GameID == myGameId)
myRace <- filter(Race, GameID == myGameId, NPR == 0)
mySystems <- filter(RaceSysSurvey, RaceID == myRace$RaceID) %>%
  inner_join(System) %>%
  select(SystemID, Name, ControlRaceID, SectorID,
         Discovered, SurveyDone, GeoSurveyDone,
         FixedName, Age, Stars)

myPopulatedSystems <- select(inner_join(SystemPopulations, mySystems),SystemID, TotalPopulation)
myHomeSysID <- arrange(myPopulatedSystems,desc(TotalPopulation))$SystemID[1]
myCurrentDate <- {
  elapse_y <- myGame$GameTime / (60*60*24*30*12)
  elapse_mo <- elapse_y %>% mod(1) %>% {.*12}
  elapse_md <- elapse_mo %>% mod(1) %>% {.*30}
  elapse_h <- elapse_md %>% mod(1) %>% {.*24}
  elapse_m <- elapse_h %>% mod(1) %>% {.*60}
  years(myGame$StartYear + floor(elapse_y)) + 
      months(1+floor(elapse_mo)) + 
      days(1+floor(elapse_md)) + 
      hours(floor(elapse_h)) +
      minutes(floor(elapse_m))}



myWarpPairs <- select(mutate(WarpPoint, JG = as.logical(JumpGateRaceID)),
                    SP = WarpPointID, DP = WPLink, SS = SystemID, JG) %>%
  inner_join(WarpPoint, c("DP" = "WarpPointID")) %>%
  select(SS, DS = SystemID, JG)

myJumps <-  apply(myWarpPairs, 1, function(w) {sort(c(w[1], w[2])) %>% c(w[3])}) %>% t.default() %>% unique.matrix()

myPopulatedSystems <- SystemPopulations[which(SystemPopulations$SystemID %in% mySystems$SystemID),]

myPopulatedPlanets <- Population[which(Population$SystemID %in% myPopulatedSystems$SystemID),] %>%
  inner_join(SystemBody)

# http://stackoverflow.com/questions/11340444/is-there-an-r-function-to-format-number-using-unit-prefix
f2si2<-function (number,rounding=F,digits=ifelse(rounding, NA, 2))
{
  lut <- c(1, 1000, 1e+06, 1e+09, 1e+12)
  pre <- c("", "k", "M", "B", "T")
  ix <- findInterval(number, lut)
  if (lut[ix]!=1) {
    if (rounding==T) {
      sistring <- paste(signif(number/lut[ix], digits), pre[ix])
    } else {
      sistring <- paste(number/lut[ix], pre[ix])
    } 
  }   else {
    sistring <- as.character(number)
  }
  return(sistring)
}



myPopulatedPlanets$Name %<>% as.character

myLabelIDs <- filter(myPopulatedSystems, TotalPopulation>0) %$% SystemID

myNodeLabels <- sapply(myLabelIDs, function(s) {
  referencedRows <- filter(myPopulatedPlanets, SystemID == s, Population>0)
  c(s,paste0(
    referencedRows$Name,": ",referencedRows$Population,collapse="<br>"  ))})
myNodeLabels <- t.default(myNodeLabels) %>% as.data.frame(stringsAsFactors = F)
names(myNodeLabels) <- c("SystemID", "label")
myNodeLabels$SystemID %<>% as.integer

mySystems %<>% left_join(myNodeLabels)


rm(list=ls(pattern = "^[^my]"))
save.image("shiny.RData")

