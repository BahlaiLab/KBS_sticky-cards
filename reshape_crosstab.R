#import crosstab files
msce2021<-read.csv(file="2021_LTER_MCSE_crosstab.csv")
forest2021<-read.csv(file="2021_LTER_forest_crosstab.csv")

#get rid of blank lines in both sets. Because excel file.
msce2021<- na.omit(msce2021)
forest2021<- na.omit(forest2021)

#reshape to list form

library(reshape2)
msce2021.list<-melt(data=msce2021, id.vars=c("DATE","TREAT","REP","STATION"))
forest2021.list<-melt(data=forest2021, id.vars=c("DATE","TREAT","TRAP"))
names(forest2021.list)[3]<-"STATION"


#rename variable and value columns to harmonize with main database
names(msce2021.list)[5]<-"SPID"
names(msce2021.list)[6]<-"ADULTS"
summary(msce2021.list)

names(forest2021.list)[4]<-"SPID"
names(forest2021.list)[5]<-"ADULTS"
summary(forest2021.list)

#index by trapID (not optimal, but how it's done in main database. Ugh)
#import trapID index
trapindex<-read.table(file="MainSite_TrapInfo.txt", sep=",", header=TRUE)
names(trapindex)[3]<-"REP"

trapindexforest<-read.table(file="UnmanagedSites_TrapInfo.txt", sep=",", header=TRUE)
names(trapindexforest)[2]<-"TREAT"

#Treatment variable listed not by number, but by T[x]. Gotta get that crap out
trapindex$TREAT<-gsub("T","", trapindex$TREAT)

#merge that crud
indexed.msce2021.list<-merge(msce2021.list, trapindex, by=c("TREAT","REP","STATION"), all.x=TRUE)
indexed.forest2021.list<-merge(forest2021.list, trapindexforest, by=c("TREAT","STATION"), all.x=TRUE)

#strip out the variables that aren't in the main database which you really should fix to be completely usable, Christie
#ugh I hate myself for doing this
indexed.msce2021.list$TREAT<-NULL
indexed.msce2021.list$REP<-NULL
indexed.msce2021.list$STATION<-NULL
indexed.msce2021.list$PLOT<-NULL
indexed.msce2021.list$XUTM83<-NULL
indexed.msce2021.list$YUTM83<-NULL

indexed.forest2021.list$TREAT<-NULL
indexed.forest2021.list$STATION<-NULL
indexed.forest2021.list$HABITAT<-NULL
indexed.forest2021.list$TREAT_DESC<-NULL
indexed.forest2021.list$REP<-NULL


#export to CSV for loading into the big database

write.csv(indexed.msce2021.list, file="msce2021_cleaned.csv", row.names=FALSE)
write.csv(indexed.forest2021.list, file="forest2021_cleaned.csv", row.names=FALSE)

####

#import crosstab files
msce2020<-read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2020_LTER_MCSE_crosstab.csv")
forest2020<-read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2020_LTER_forest_crosstab.csv")

#get rid of blank lines in both sets. Because excel file.
msce2020<- na.omit(msce2020)
forest2020<- na.omit(forest2020)

#reshape to list form

library(reshape2)
msce2020.list<-melt(data=msce2020, id.vars=c("DATE","TREAT","REP","STATION"))
forest2020.list<-melt(data=forest2020, id.vars=c("DATE","TREAT","TRAP"))
names(forest2020.list)[3]<-"STATION"

#rename variable and value columns to harmonize with main database
names(msce2020.list)[5]<-"SPID"
names(msce2020.list)[6]<-"ADULTS"
summary(msce2020.list)

names(forest2020.list)[4]<-"SPID"
names(forest2020.list)[5]<-"ADULTS"
summary(forest2020.list)

#index by trapID (not optimal, but how it's done in main database. Ugh)
#import trapID index
trapindex<-read.table(file="MainSite_TrapInfo.txt", sep=",", header=TRUE)
names(trapindex)[3]<-"REP"

trapindexforest<-read.table(file="UnmanagedSites_TrapInfo.txt", sep=",", header=TRUE)
names(trapindexforest)[2]<-"TREAT"

#Treatment variable listed not by number, but by T[x]. Gotta get that crap out
trapindex$TREAT<-gsub("T","", trapindex$TREAT)

#merge that crud
indexed.msce2020.list<-merge(msce2020.list, trapindex, by=c("TREAT","REP","STATION"), all.x=TRUE)
indexed.forest2020.list<-merge(forest2020.list, trapindexforest, by=c("TREAT","STATION"), all.x=TRUE)

#strip out the variables that aren't in the main database which you really should fix to be completely usable, Christie
#ugh I hate myself for doing this
indexed.msce2020.list$TREAT<-NULL
indexed.msce2020.list$REP<-NULL
indexed.msce2020.list$STATION<-NULL
indexed.msce2020.list$PLOT<-NULL
indexed.msce2020.list$XUTM83<-NULL
indexed.msce2020.list$YUTM83<-NULL

indexed.forest2020.list$TREAT<-NULL
indexed.forest2020.list$STATION<-NULL
indexed.forest2020.list$HABITAT<-NULL
indexed.forest2020.list$TREAT_DESC<-NULL
indexed.forest2020.list$REP<-NULL


#export to CSV for loading into the big database

write.csv(indexed.msce2020.list, file="msce2021_cleaned.csv", row.names=FALSE)
write.csv(indexed.forest2020.list, file="forest2021_cleaned.csv", row.names=FALSE)

