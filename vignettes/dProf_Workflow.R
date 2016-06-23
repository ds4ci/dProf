## ------------------------------------------------------------------------
library(dProf)
dpProj <- dpMakeProject("Test", "A simple dprof test", "Jim P")
str(dpProj)

## ------------------------------------------------------------------------
TblName <- "NewsCurrentEvents"
TblSource <- system.file("extdata", "NewsCurrentEvents.csv",
                         package = "dProf")
dpTables <- dpMakeTable(dpTest$project_id[1], TblName, TblSource, 
                       "Summarize email campaigns", "Jim P",
                       notes = "Using the example csv in project")
str(dpTables)

