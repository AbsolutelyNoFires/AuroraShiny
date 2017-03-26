
####### Change this bit #####

my_aurora_installation_folder = "D:/Games/Aurora"

##############################


aurora <- function(folder) {
library(RODBC)

Stevefire <- paste0(my_aurora_installation_folder,"/Stevefire.mdb")

sFire <-  odbcConnectAccess(Stevefire, pwd = "raistlin31")
on.exit(odbcClose(sFire))

tbls <- readLines("tables.txt")
sapply(tbls, function(x) {
  assign(x,
         sqlFetch(sFire, x),
         envir = .GlobalEnv)})}


aurora(my_aurora_installation_folder)
save.image("aurora.RData")