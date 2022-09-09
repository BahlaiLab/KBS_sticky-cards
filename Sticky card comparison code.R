#bring in data set of all KBS field data with week and DOY already attached 
#note this data had the june 10th data from treatments 1 and 4 removed because those cards only have one entry
#this data will come back after the accumulation loop
KBS <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_LTER_without%20june%2010th%20treat%201%20and%204.csv", na.strings = NULL)

#change Rep and Station to characters
KBS$REP <- as.character(KBS$REP)
KBS$STATION <- as.character(KBS$STATION)
#change order of data set
KBS<-KBS[order(KBS$TREAT, KBS$REP, KBS$STATION, KBS$DOY),]
str(KBS)

#bring in data set with all insects we IDed in the Bahlai lab
Bahlai <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/Insect%20ID%202021_sticky%20card.csv", na.strings = NULL)
#change Rep and Station to characters
Bahlai$REP <- as.character(Bahlai$REP)
Bahlai$STATION <- as.character(Bahlai$STATION)
str(Bahlai)

#combining two counting periods for each individual sticky card into one line
#at KBS sticky cards were out for 2 weeks
#at the end of week 1, lady beetles and other focal specimens would be counted and removed from cards
#at the end of week 2, cards would be removed from the field and new specimens collected were counted 

#need to perform this code for each species
#starting with ABIPN
cumABIPN<-c()
for(i in 1:length(KBS$ABIPN)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$ABIPN[i]
  }else{
    bugcount<-KBS$ABIPN[i]+ KBS$ABIPN[i-1]
  }
  cumABIPN<-c(cumABIPN, bugcount)
}

#testing the loop
#thing<-c()
 # for(i in 1:length(KBS$is.collected)){
  #  if (KBS$is.collected[i]=="yes"){
   #   bugcount<-"true"
    #}else{
     # bugcount<-"false"
    #}
    #thing<-c(thing, bugcount)
  #}
#test<-cbind(KBS, thing)


#BURSI
cumBURSI<-c()
for(i in 1:length(KBS$BURSI)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$BURSI[i]
  }else{
    bugcount<-KBS$BURSI[i]+ KBS$BURSI[i-1]
  }
  cumBURSI<-c(cumBURSI, bugcount)
}

#C7
cumC7<-c()
for(i in 1:length(KBS$C7)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$C7[i]
  }else{
    bugcount<-KBS$C7[i]+ KBS$C7[i-1]
  }
  cumC7<-c(cumC7, bugcount)
}

#CMAC
cumCMAC<-c()
for(i in 1:length(KBS$CMAC)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$CMAC[i]
  }else{
    bugcount<-KBS$CMAC[i]+ KBS$CMAC[i-1]
  }
  cumCMAC<-c(cumCMAC, bugcount)
}

#CSTIG
cumCSTIG<-c()
for(i in 1:length(KBS$CSTIG)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$CSTIG[i]
  }else{
    bugcount<-KBS$CSTIG[i]+ KBS$CSTIG[i-1]
  }
  cumCSTIG<-c(cumCSTIG, bugcount)
}

#CTRIF
cumCTRIF<-c()
for(i in 1:length(KBS$CTRIF)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$CTRIF[i]
  }else{
    bugcount<-KBS$CTRIF[i]+ KBS$CTRIF[i-1]
  }
  cumCTRIF<-c(cumCTRIF, bugcount)
}

#CYCSP
cumCYCSP<-c()
for(i in 1:length(KBS$CYCSP)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$CYCSP[i]
  }else{
    bugcount<-KBS$CYCSP[i]+ KBS$CYCSP[i-1]
  }
  cumCYCSP<-c(cumCYCSP, bugcount)
}

#H13
cumH13<-c()
for(i in 1:length(KBS$H13)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$H13[i]
  }else{
    bugcount<-KBS$H13[i]+ KBS$H13[i-1]
  }
  cumH13<-c(cumH13, bugcount)
}

#HAXY
cumHAXY<-c()
for(i in 1:length(KBS$HAXY)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$HAXY[i]
  }else{
    bugcount<-KBS$HAXY[i]+ KBS$HAXY[i-1]
  }
  cumHAXY<-c(cumHAXY, bugcount)
}

#HCONV
cumHCONV<-c()
for(i in 1:length(KBS$HCONV)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$HCONV[i]
  }else{
    bugcount<-KBS$HCONV[i]+ KBS$HCONV[i-1]
  }
  cumHCONV<-c(cumHCONV, bugcount)
}

#HGLAC
cumHGLAC<-c()
for(i in 1:length(KBS$HGLAC)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$HGLAC[i]
  }else{
    bugcount<-KBS$HGLAC[i]+ KBS$HGLAC[i-1]
  }
  cumHGLAC<-c(cumHGLAC, bugcount)
}

#HPARN
cumHPARN<-c()
for(i in 1:length(KBS$HPARN)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$HPARN[i]
  }else{
    bugcount<-KBS$HPARN[i]+ KBS$HPARN[i-1]
  }
  cumHPARN<-c(cumHPARN, bugcount)
}

#HVAR
cumHVAR<-c()
for(i in 1:length(KBS$HVAR)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$HVAR[i]
  }else{
    bugcount<-KBS$HVAR[i]+ KBS$HVAR[i-1]
  }
  cumHVAR<-c(cumHVAR, bugcount)
}

#PQUA
cumPQUA<-c()
for(i in 1:length(KBS$PQUA)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$PQUA[i]
  }else{
    bugcount<-KBS$PQUA[i]+ KBS$PQUA[i-1]
  }
  cumPQUA<-c(cumPQUA, bugcount)
}

#CANTHARID
cumCANTHARID<-c()
for(i in 1:length(KBS$CANTHARID)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$CANTHARID[i]
  }else{
    bugcount<-KBS$CANTHARID[i]+ KBS$CANTHARID[i-1]
  }
  cumCANTHARID<-c(cumCANTHARID, bugcount)
}

#LAMPY
cumLAMPY<-c()
for(i in 1:length(KBS$LAMPY)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$LAMPY[i]
  }else{
    bugcount<-KBS$LAMPY[i]+ KBS$LAMPY[i-1]
  }
  cumLAMPY<-c(cumLAMPY, bugcount)
}

#LCW
cumLCW<-c()
for(i in 1:length(KBS$LCW)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$LCW[i]
  }else{
    bugcount<-KBS$LCW[i]+ KBS$LCW[i-1]
  }
  cumLCW<-c(cumLCW, bugcount)
}

#MECOP
cumMECOP<-c()
for(i in 1:length(KBS$MECOP)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$MECOP[i]
  }else{
    bugcount<-KBS$MECOP[i]+ KBS$MECOP[i-1]
  }
  cumMECOP<-c(cumMECOP, bugcount)
}

#X20SPOT
cumX20SPOT<-c()
for(i in 1:length(KBS$X20SPOT)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$X20SPOT[i]
  }else{
    bugcount<-KBS$X20SPOT[i]+ KBS$X20SPOT[i-1]
  }
  cumX20SPOT<-c(cumX20SPOT, bugcount)
}

#OTHER
cumOTHER<-c()
for(i in 1:length(KBS$OTHER)){
  if (KBS$is.collected[i]=="no"){
    bugcount<-KBS$OTHER[i]
  }else{
    bugcount<-KBS$OTHER[i]+ KBS$OTHER[i-1]
  }
  cumOTHER<-c(cumOTHER, bugcount)
}

#put cumulative counts into dataframe
KBS_cum<-cbind(KBS, cumABIPN, cumBURSI, cumC7, cumCMAC, cumCSTIG, cumCTRIF, 
               cumCYCSP, cumH13, cumHAXY, cumHCONV, cumHGLAC, cumHPARN, cumHVAR, 
               cumPQUA, cumCANTHARID, cumLAMPY, cumLCW, cumMECOP, cumX20SPOT, cumOTHER)

#just want "yes" lines because those show cumulative amounts
KBS_cum_final <- KBS_cum[which(KBS_cum$is.collected=="yes"),] 

#print data into cvs file
write.csv(KBS_cum_final, file="2021_LTER_cumulative.csv", row.names=FALSE)

#after deleting non-cumulative counts, and adding in the data collected in the Bahlai lab, add the data file back in 
insects <- read.csv ("", na.strings = NULL)





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
