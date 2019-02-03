library(data.table)
library(tidyverse)


#Completed Files
setwd("C:/Users/Alan/Documents/Temp/")
files  <- list.files(pattern = '\\.csv')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)

StudentIDsComp <- combined.df[seq(246, nrow(combined.df), 246),]
StudentIDsComp$Number <- 1:203
StudentIDsComp <- subset(StudentIDsComp, select = c(Number, responses, time_elapsed))

write.csv(StudentIDsComp, file=("C:/Users/Alan/Documents/StudentIDsComp.csv"))

#"Failed" Files
setwd("C:/Users/Alan/Documents/Failures/")
files  <- list.files(pattern = '\\.csv')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)

combined.df <- subset(combined.df, select = c(responses, time_elapsed))

ID <- combined.df[seq(6, nrow(combined.df), 6),]
Consent <- combined.df[seq(3, nrow(combined.df), 6),]

StudentIDsFail <- cbind(ID, Consent)
colnames(StudentIDsFail) <- c("ID", "Time1", "Consent", "Time2")
StudentIDsFail<- separate(StudentIDsFail, col= Consent, into = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6"), sep=",")

#Challenge for Kat- Take a look at how the data in the file I've sent you is formatted and compare it to the output of the file generated below-
#how would you write R code that formatted the data in that way
write.csv(StudentIDsFail, file=("C:/Users/Alan/Documents/StudentIDsFail.csv"))


#Stick them all together and throw out duplicates
StudentIDsAll <- c(as.character(StudentIDsComp$responses), as.character(StudentIDsFail$ID))

uniqueIDs <- unique(StudentIDsAll)


write.csv(uniqueIDs, file=("C:/Users/Alan/Documents/uniqueIDs.csv"))
