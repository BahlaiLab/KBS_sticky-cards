#bring in data set of all KBS field data with week and DOY already attached 
#also contains column "is.collected" --> no = not the week collected, yes = card collected
#note this data had the june 10th data from treatments 1 and 4 removed because those cards only have one entry
#this data will come back after the accumulation loop
KBS <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_LTER_without%20june%2010th%20treat%201%20and%204.csv", na.strings = NULL)

#change Rep and Station to characters
KBS$REP <- as.character(KBS$REP)
KBS$STATION <- as.character(KBS$STATION)
#change order of data set
KBS<-KBS[order(KBS$TREAT, KBS$REP, KBS$STATION, KBS$DOY),]
str(KBS)

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

#print data into csv file
write.csv(KBS_cum_final, file="2021_LTER_cumulative.csv", row.names=FALSE)

#after deleting non-cumulative counts, and adding in the data from treatments 1 and 4 from june 10th, 
#add the cumulative count data file back in to reorder again for matching
KBS_cum_final_2.0 <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_LTER_cumulative%202.0.csv", na.strings = NULL)
#reorder
KBS_cum_final_2.0<-KBS_cum_final_2.0[order(KBS_cum_final_2.0$TREAT, KBS_cum_final_2.0$REP, KBS_cum_final_2.0$STATION, KBS_cum_final_2.0$DOY),]
str(KBS_cum_final_2.0)

#print data into csv file
write.csv(KBS_cum_final_2.0, file="2021_LTER_final.csv", row.names=FALSE)

#bring in data set with all insects we IDed in the Bahlai lab
Bahlai <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_Bahlai%20-%20with%20corrections.csv", na.strings = NULL)

#change order of data set
Bahlai<-Bahlai[order(Bahlai$TREAT, Bahlai$REP, Bahlai$STATION, Bahlai$DOY),]
str(Bahlai)

#print into csv file
write.csv(Bahlai, file="2021_Bahlai_reordered.csv", row.names=FALSE)

##LTER (2021_LTER_final) + Bahlai (2021_Bahlai_reordered) = 2021_LTERandBahlai  <- used for analyses

###

#2020

#bring in data set of all KBS field data with week and DOY already attached 
#also contains column "is.collected" --> no = not the week collected, yes = card collected
#note this data had multiple lines temporarily removed because those cards only have one entry
  #June 18th treatment 4, reps 1-6
  #July 9th treatment 4, rep 1, stations 2-5
  #August 18th treatment 6, reps 1-6
  #last collection date = September 3rd (MCSE) and 4th (forest)
#this data will come back after the accumulation loop
KBS20 <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2020_LTER.csv", na.strings = NULL)

#change Rep and Station to characters
KBS20$REP <- as.character(KBS20$REP)
KBS20$STATION <- as.character(KBS20$STATION)
#change order of data set
KBS20<-KBS20[order(KBS20$TREAT, KBS20$REP, KBS20$STATION, KBS20$DOY),]
str(KBS20)

#combining two counting periods for each individual sticky card into one line
#at KBS sticky cards were out for 2 weeks
#at the end of week 1, lady beetles and other focal specimens would be counted and removed from cards
#at the end of week 2, cards would be removed from the field and new specimens collected were counted 

#need to perform this code for each species
#starting with ABIPN
cumABIPN<-c()
for(i in 1:length(KBS20$ABIPN)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$ABIPN[i]
  }else{
    bugcount<-KBS20$ABIPN[i]+ KBS20$ABIPN[i-1]
  }
  cumABIPN<-c(cumABIPN, bugcount)
}

#BURSI
cumBURSI<-c()
for(i in 1:length(KBS20$BURSI)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$BURSI[i]
  }else{
    bugcount<-KBS20$BURSI[i]+ KBS20$BURSI[i-1]
  }
  cumBURSI<-c(cumBURSI, bugcount)
}

#C7
cumC7<-c()
for(i in 1:length(KBS20$C7)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$C7[i]
  }else{
    bugcount<-KBS20$C7[i]+ KBS20$C7[i-1]
  }
  cumC7<-c(cumC7, bugcount)
}

#CMAC
cumCMAC<-c()
for(i in 1:length(KBS20$CMAC)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$CMAC[i]
  }else{
    bugcount<-KBS20$CMAC[i]+ KBS20$CMAC[i-1]
  }
  cumCMAC<-c(cumCMAC, bugcount)
}

#CSTIG
cumCSTIG<-c()
for(i in 1:length(KBS20$CSTIG)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$CSTIG[i]
  }else{
    bugcount<-KBS20$CSTIG[i]+ KBS20$CSTIG[i-1]
  }
  cumCSTIG<-c(cumCSTIG, bugcount)
}

#CTRIF
cumCTRIF<-c()
for(i in 1:length(KBS20$CTRIF)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$CTRIF[i]
  }else{
    bugcount<-KBS20$CTRIF[i]+ KBS20$CTRIF[i-1]
  }
  cumCTRIF<-c(cumCTRIF, bugcount)
}

#CYCSP
cumCYCSP<-c()
for(i in 1:length(KBS20$CYCSP)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$CYCSP[i]
  }else{
    bugcount<-KBS20$CYCSP[i]+ KBS20$CYCSP[i-1]
  }
  cumCYCSP<-c(cumCYCSP, bugcount)
}

#H13
cumH13<-c()
for(i in 1:length(KBS20$H13)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$H13[i]
  }else{
    bugcount<-KBS20$H13[i]+ KBS20$H13[i-1]
  }
  cumH13<-c(cumH13, bugcount)
}

#HAXY
cumHAXY<-c()
for(i in 1:length(KBS20$HAXY)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$HAXY[i]
  }else{
    bugcount<-KBS20$HAXY[i]+ KBS20$HAXY[i-1]
  }
  cumHAXY<-c(cumHAXY, bugcount)
}

#HCONV
cumHCONV<-c()
for(i in 1:length(KBS20$HCONV)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$HCONV[i]
  }else{
    bugcount<-KBS20$HCONV[i]+ KBS20$HCONV[i-1]
  }
  cumHCONV<-c(cumHCONV, bugcount)
}

#HGLAC
cumHGLAC<-c()
for(i in 1:length(KBS20$HGLAC)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$HGLAC[i]
  }else{
    bugcount<-KBS20$HGLAC[i]+ KBS20$HGLAC[i-1]
  }
  cumHGLAC<-c(cumHGLAC, bugcount)
}

#HPARN
cumHPARN<-c()
for(i in 1:length(KBS20$HPARN)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$HPARN[i]
  }else{
    bugcount<-KBS20$HPARN[i]+ KBS20$HPARN[i-1]
  }
  cumHPARN<-c(cumHPARN, bugcount)
}

#HVAR
cumHVAR<-c()
for(i in 1:length(KBS20$HVAR)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$HVAR[i]
  }else{
    bugcount<-KBS20$HVAR[i]+ KBS20$HVAR[i-1]
  }
  cumHVAR<-c(cumHVAR, bugcount)
}

#PQUA
cumPQUA<-c()
for(i in 1:length(KBS20$PQUA)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$PQUA[i]
  }else{
    bugcount<-KBS20$PQUA[i]+ KBS20$PQUA[i-1]
  }
  cumPQUA<-c(cumPQUA, bugcount)
}

#CANTHARID
cumCANTHARID<-c()
for(i in 1:length(KBS20$CANTHARID)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$CANTHARID[i]
  }else{
    bugcount<-KBS20$CANTHARID[i]+ KBS20$CANTHARID[i-1]
  }
  cumCANTHARID<-c(cumCANTHARID, bugcount)
}

#LAMPY
cumLAMPY<-c()
for(i in 1:length(KBS20$LAMPY)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$LAMPY[i]
  }else{
    bugcount<-KBS20$LAMPY[i]+ KBS20$LAMPY[i-1]
  }
  cumLAMPY<-c(cumLAMPY, bugcount)
}

#LCW
cumLCW<-c()
for(i in 1:length(KBS20$LCW)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$LCW[i]
  }else{
    bugcount<-KBS20$LCW[i]+ KBS20$LCW[i-1]
  }
  cumLCW<-c(cumLCW, bugcount)
}

#MECOP
cumMECOP<-c()
for(i in 1:length(KBS20$MECOP)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$MECOP[i]
  }else{
    bugcount<-KBS20$MECOP[i]+ KBS20$MECOP[i-1]
  }
  cumMECOP<-c(cumMECOP, bugcount)
}

#X20SPOT
cumX20SPOT<-c()
for(i in 1:length(KBS20$X20SPOT)){
  if (KBS20$is.collected[i]=="no"){
    bugcount<-KBS20$X20SPOT[i]
  }else{
    bugcount<-KBS20$X20SPOT[i]+ KBS20$X20SPOT[i-1]
  }
  cumX20SPOT<-c(cumX20SPOT, bugcount)
}

#no column for other in 2020
#OTHER
#cumOTHER<-c()
#for(i in 1:length(KBS20$OTHER)){
 # if (KBS20$is.collected[i]=="no"){
  #  bugcount<-KBS20$OTHER[i]
  #}else{
   # bugcount<-KBS20$OTHER[i]+ KBS20$OTHER[i-1]
  #}
  #cumOTHER<-c(cumOTHER, bugcount)
#}

#put cumulative counts into dataframe
KBS20_cum<-cbind(KBS20, cumABIPN, cumBURSI, cumC7, cumCMAC, cumCSTIG, cumCTRIF, 
               cumCYCSP, cumH13, cumHAXY, cumHCONV, cumHGLAC, cumHPARN, cumHVAR, 
               cumPQUA, cumCANTHARID, cumLAMPY, cumLCW, cumMECOP, cumX20SPOT)

#just want "yes" lines because those show cumulative amounts
KBS20_cum_final <- KBS20_cum[which(KBS20_cum$is.collected=="yes"),] 

#print data into csv file
write.csv(KBS20_cum_final, file="2020_LTER_cumulative.csv", row.names=FALSE)

#after deleting non-cumulative counts, and adding in the data that only had one week for a card, 
#add the cumulative count data file back in to reorder again for matching
KBS20_cum_final_2.0 <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2020_LTER_cumulative%202.0.csv", na.strings = NULL)
#reorder
KBS20_cum_final_2.0<-KBS20_cum_final_2.0[order(KBS20_cum_final_2.0$TREAT, KBS20_cum_final_2.0$REP, KBS20_cum_final_2.0$STATION, KBS20_cum_final_2.0$DOY),]
str(KBS20_cum_final_2.0)

#print data into FINAL csv file
write.csv(KBS20_cum_final_2.0, file="2021_LTER_final.csv", row.names=FALSE)

###

#2022

#bring in data set of all KBS field data with week and DOY already attached 
#also contains column "is.collected" --> no = not the week collected, yes = card collected
#note this data had multiple lines temporarily removed because those cards only have one entry
  #May 13th treatment 5, rep 5, station 1
  #June 6th treatments 1 & 2, reps 1-6
  #July 5th treatment 7, rep 6
  #last sampling date = August 22nd (MCSE)  <--- DOUBLE CHECK
  #June 20th CF, rep 3, station 5
  #August 8th SF, rep 1, station 1
  #August 8th SF, rep 3, station 4
#this data will come back after the accumulation loop
KBS22 <- read.csv ("", na.strings = NULL)

#change Rep and Station to characters
KBS22$REP <- as.character(KBS22$REP)
KBS22$STATION <- as.character(KBS22$STATION)
#change order of data set
KBS22<-KBS22[order(KBS22$TREAT, KBS22$REP, KBS22$STATION, KBS22$DOY),]
str(KBS22)

#combining two counting periods for each individual sticky card into one line
#at KBS sticky cards were out for 2 weeks
#at the end of week 1, lady beetles and other focal specimens would be counted and removed from cards
#at the end of week 2, cards would be removed from the field and new specimens collected were counted 

#need to perform this code for each species
#starting with ABIPN
cumABIPN<-c()
for(i in 1:length(KBS22$ABIPN)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$ABIPN[i]
  }else{
    bugcount<-KBS22$ABIPN[i]+ KBS22$ABIPN[i-1]
  }
  cumABIPN<-c(cumABIPN, bugcount)
}

#BURSI
cumBURSI<-c()
for(i in 1:length(KBS22$BURSI)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$BURSI[i]
  }else{
    bugcount<-KBS22$BURSI[i]+ KBS22$BURSI[i-1]
  }
  cumBURSI<-c(cumBURSI, bugcount)
}

#C7
cumC7<-c()
for(i in 1:length(KBS22$C7)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$C7[i]
  }else{
    bugcount<-KBS22$C7[i]+ KBS22$C7[i-1]
  }
  cumC7<-c(cumC7, bugcount)
}

#CMAC
cumCMAC<-c()
for(i in 1:length(KBS22$CMAC)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$CMAC[i]
  }else{
    bugcount<-KBS22$CMAC[i]+ KBS22$CMAC[i-1]
  }
  cumCMAC<-c(cumCMAC, bugcount)
}

#CSTIG
cumCSTIG<-c()
for(i in 1:length(KBS22$CSTIG)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$CSTIG[i]
  }else{
    bugcount<-KBS22$CSTIG[i]+ KBS22$CSTIG[i-1]
  }
  cumCSTIG<-c(cumCSTIG, bugcount)
}

#CTRIF
cumCTRIF<-c()
for(i in 1:length(KBS22$CTRIF)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$CTRIF[i]
  }else{
    bugcount<-KBS22$CTRIF[i]+ KBS22$CTRIF[i-1]
  }
  cumCTRIF<-c(cumCTRIF, bugcount)
}

#CYCSP
cumCYCSP<-c()
for(i in 1:length(KBS22$CYCSP)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$CYCSP[i]
  }else{
    bugcount<-KBS22$CYCSP[i]+ KBS22$CYCSP[i-1]
  }
  cumCYCSP<-c(cumCYCSP, bugcount)
}

#H13
cumH13<-c()
for(i in 1:length(KBS22$H13)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$H13[i]
  }else{
    bugcount<-KBS22$H13[i]+ KBS22$H13[i-1]
  }
  cumH13<-c(cumH13, bugcount)
}

#HAXY
cumHAXY<-c()
for(i in 1:length(KBS22$HAXY)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$HAXY[i]
  }else{
    bugcount<-KBS22$HAXY[i]+ KBS22$HAXY[i-1]
  }
  cumHAXY<-c(cumHAXY, bugcount)
}

#HCONV
cumHCONV<-c()
for(i in 1:length(KBS22$HCONV)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$HCONV[i]
  }else{
    bugcount<-KBS22$HCONV[i]+ KBS22$HCONV[i-1]
  }
  cumHCONV<-c(cumHCONV, bugcount)
}

#HGLAC
cumHGLAC<-c()
for(i in 1:length(KBS22$HGLAC)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$HGLAC[i]
  }else{
    bugcount<-KBS22$HGLAC[i]+ KBS22$HGLAC[i-1]
  }
  cumHGLAC<-c(cumHGLAC, bugcount)
}

#HPARN
cumHPARN<-c()
for(i in 1:length(KBS22$HPARN)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$HPARN[i]
  }else{
    bugcount<-KBS22$HPARN[i]+ KBS22$HPARN[i-1]
  }
  cumHPARN<-c(cumHPARN, bugcount)
}

#HVAR
cumHVAR<-c()
for(i in 1:length(KBS22$HVAR)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$HVAR[i]
  }else{
    bugcount<-KBS22$HVAR[i]+ KBS22$HVAR[i-1]
  }
  cumHVAR<-c(cumHVAR, bugcount)
}

#PQUA
cumPQUA<-c()
for(i in 1:length(KBS22$PQUA)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$PQUA[i]
  }else{
    bugcount<-KBS22$PQUA[i]+ KBS22$PQUA[i-1]
  }
  cumPQUA<-c(cumPQUA, bugcount)
}

#CANTHARID
cumCANTHARID<-c()
for(i in 1:length(KBS22$CANTHARID)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$CANTHARID[i]
  }else{
    bugcount<-KBS22$CANTHARID[i]+ KBS22$CANTHARID[i-1]
  }
  cumCANTHARID<-c(cumCANTHARID, bugcount)
}

#LAMPY
cumLAMPY<-c()
for(i in 1:length(KBS22$LAMPY)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$LAMPY[i]
  }else{
    bugcount<-KBS22$LAMPY[i]+ KBS22$LAMPY[i-1]
  }
  cumLAMPY<-c(cumLAMPY, bugcount)
}

#LCW
cumLCW<-c()
for(i in 1:length(KBS22$LCW)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$LCW[i]
  }else{
    bugcount<-KBS22$LCW[i]+ KBS22$LCW[i-1]
  }
  cumLCW<-c(cumLCW, bugcount)
}

#MECOP
cumMECOP<-c()
for(i in 1:length(KBS22$MECOP)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$MECOP[i]
  }else{
    bugcount<-KBS22$MECOP[i]+ KBS22$MECOP[i-1]
  }
  cumMECOP<-c(cumMECOP, bugcount)
}

#X20SPOT
cumX20SPOT<-c()
for(i in 1:length(KBS22$X20SPOT)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$X20SPOT[i]
  }else{
    bugcount<-KBS22$X20SPOT[i]+ KBS22$X20SPOT[i-1]
  }
  cumX20SPOT<-c(cumX20SPOT, bugcount)
}

#OTHER
cumOTHER<-c()
for(i in 1:length(KBS22$OTHER)){
  if (KBS22$is.collected[i]=="no"){
    bugcount<-KBS22$OTHER[i]
  }else{
    bugcount<-KBS22$OTHER[i]+ KBS22$OTHER[i-1]
  }
  cumOTHER<-c(cumOTHER, bugcount)
}

#put cumulative counts into dataframe
KBS22_cum<-cbind(KBS22, cumABIPN, cumBURSI, cumC7, cumCMAC, cumCSTIG, cumCTRIF, 
               cumCYCSP, cumH13, cumHAXY, cumHCONV, cumHGLAC, cumHPARN, cumHVAR, 
               cumPQUA, cumCANTHARID, cumLAMPY, cumLCW, cumMECOP, cumX20SPOT, cumOTHER)

#just want "yes" lines because those show cumulative amounts
KBS22_cum_final <- KBS22_cum[which(KBS22_cum$is.collected=="yes"),] 

#print data into csv file
write.csv(KBS22_cum_final, file="2022_LTER_cumulative.csv", row.names=FALSE)

#after deleting non-cumulative counts, and adding in the data that only had one week for a card, 
#add the cumulative count data file back in to reorder again for matching
KBS22_cum_final_2.0 <- read.csv ("", na.strings = NULL)
#reorder
KBS22_cum_final_2.0<-KBS22_cum_final_2.0[order(KBS22_cum_final_2.0$TREAT, KBS22_cum_final_2.0$REP, KBS22_cum_final_2.0$STATION, KBS22_cum_final_2.0$DOY),]
str(KBS22_cum_final_2.0)

#print data into final csv file
write.csv(KBS22_cum_final_2.0, file="2021_LTER_final.csv", row.names=FALSE)