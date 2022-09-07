#bring in data sets from github

new <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/New_Insect%20ID%20-%20sticky%20card%20comparison%20-%20as%20of%203.22.22.csv",na.strings = NULL)
old <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/Old_Insect%20ID%20-%20sticky%20card%20comparison%20-%20as%20of%203.22.22.csv",na.strings = NULL)

#taxa <- read.csv("")

#combine data tables 
library (plyr)
insects <- rbind.fill (new, old)
#change Rep and Station to characters
insects$Rep <- as.character(insects$Rep)
insects$Station <- as.character(insects$Station)
summary(insects)

####
#NMDS of insect community between card types
library (vegan)

#Create matrix of environmental variables
env.matrix<-insects[c(1:5)]
#create matrix of community variables
com.matrix<-insects[c(6:10)]

#ordination by NMDS
NMDS<-metaMDS(com.matrix, distance="bray", k=2, autotransform=FALSE, trymax=100)
stressplot(NMDS)
#stress=

####

#bring in data set of all KBS field data with week already attached 
KBS <- read.csv ("", na.strings = NULL)

#bring in data set with all insects we IDed in the Bahlai lab
Bahlai <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/Insect%20ID%202021_sticky%20card.csv", na.strings = NULL)

#combining two counting periods for each individual sticky card into one line
#at KBS sticky cards were out for 2 weeks
#at the end of week 1, lady beetles and other focal specimens would be counted and removed from cards
#at the end of week 2, cards would be removed from the field and new specimens collected were counted 

#need to perform this code for each species
#starting with ABIPN
cumABIPN<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-ABIPN[i]
  }else{
    bugcount<-ABIPN[i]+ ABIPN[i-1]
  }
  cumABIPN<-c(cumABIPN, bugcount)
}

datacombined<-cbind(data, cumABIPN)

#BURSI
cumBURSI<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-BURSI[i]
  }else{
    bugcount<-BURSI[i]+ BURSI[i-1]
  }
  cumBURSI<-c(cumBURSI, bugcount)
}

#C7
cumC7<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-C7[i]
  }else{
    bugcount<-C7[i]+ C7[i-1]
  }
  cumC7<-c(cumC7, bugcount)
}

#CMAC
cumCMAC<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-CMAC[i]
  }else{
    bugcount<-CMAC[i]+ CMAC[i-1]
  }
  cumCMAC<-c(cumCMAC, bugcount)
}

#CSTIG
cumCSTIG<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-CSTIG[i]
  }else{
    bugcount<-CSTIG[i]+ CSTIG[i-1]
  }
  cumCSTIG<-c(cumCSTIG, bugcount)
}

#CTRIF
cumCTRIF<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-CTRIF[i]
  }else{
    bugcount<-CTRIF[i]+ CTRIF[i-1]
  }
  cumCTRIF<-c(cumCTRIF, bugcount)
}

#CYCSP
cumCYCSP<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-CYCSP[i]
  }else{
    bugcount<-CYCSP[i]+ CYCSP[i-1]
  }
  cumCYCSP<-c(cumCYCSP, bugcount)
}

#H13
cumH13<-c()
for(i in 1:length(column)){
  if (KBS$is-collected=="no"){
    bugcount<-H13[i]
  }else{
    bugcount<-H13[i]+ H13[i-1]
  }
  cumH13<-c(cumH13, bugcount)
}