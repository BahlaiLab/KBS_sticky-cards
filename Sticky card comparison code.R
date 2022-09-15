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
write.csv(KBS_cum_final_2.0, file="2021_LTER_cumulative 3.0.csv", row.names=FALSE)

#bring in data set with all insects we IDed in the Bahlai lab
Bahlai <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_Bahlai%20-%20with%20corrections.csv", na.strings = NULL)

#change order of data set
Bahlai<-Bahlai[order(Bahlai$TREAT, Bahlai$REP, Bahlai$STATION, Bahlai$DOY),]
str(Bahlai)

#print into csv file
write.csv(Bahlai, file="2021_Bahlai_reordered.csv", row.names=FALSE)


###


#bring in final data file of everything combined
#LTER (2021_LTER_cumulative 3.0) + Bahlai (2021_Bahlai_reordered)
insects <- read.csv ("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_LTERandBahlai.csv", na.strings = NULL)
#change DOY and week to characters
insects$DOY <- as.character(insects$DOY)
insects$week <- as.character(insects$week)
#change CARD, TREAT, REP, and STATION to factor
insects$CARD <- as.factor(insects$CARD)
insects$TREAT <- as.factor(insects$TREAT)
insects$REP <- as.factor(insects$REP)
insects$STATION <- as.factor(insects$STATION)
str(insects)
summary(insects)

#"insects" is final cleaned data set -- begin data analyses

###

#NMDS of insect community between card types
library (vegan)

#Create matrix of environmental variables
env.matrix<-insects[c(3:7)]
#create matrix of community variables
com.matrix<-insects[c(8:32)]

#ordination by NMDS

#have to get rid of any rows (samples) that have only zeros
library(dplyr)
#com.matrix <- com.matrix %>% 
 # filter_all(any_vars(. != 0))

#NMDS<-metaMDS(com.matrix, distance="bray", k=2, autotransform=TRUE, trymax=100)
NMDS
stressplot(NMDS)
#stress=.25 -- no convergence

#NMDS visualization
#DOES NOT WORK
plot(NMDS, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old")
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New") 
#display ground trap data as solid shapes - pitfall=circle, ramp trap=square, jar=triangle, flying trap as triangle outline
points(NMDS, display="sites", select=which(env.matrix$Trap=="pitfall"),pch=19, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$Trap=="jar"), pch=17, col="#009E73")
#add legend
legend(1.0,1.51, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=.7, legend=c("Old cards", "New cards"))
#add insect taxa as text
#ordilabel(NMDS, display="species", select =which (include_func==TRUE & crawling_func == TRUE), cex=0.6, col="black", fill="white")
#ordilabel(NMDS, display="species", select =which (include_func==TRUE & flying_func == TRUE), cex=0.6, col="white", fill="black")
#ordilabel(NMDS, display="species", select =which (include_func==TRUE & intermediate_func == TRUE), cex=0.6, col="black", fill="gray")

#bootstrapping and testing for differences between the groups (cards)
fit<-adonis(com.matrix ~ CARD, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARD))
#P-value = ??? -- cannot assume homogeneity of multivariate dispersion

library(pairwiseAdonis)
pairwise.adonis(com.matrix, env.matrix$CARD)

#

#subset by card type
new <- insects[which(insects$CARD=="New"),] 
old <- insects[which(insects$CARD=="Old"),] 

#calculate mean and SE richness and abundance of each card
insects.abun <- rowSums(new[,8:32])
new$abundance <- insects.abun
insects.rowsums <- rowSums(new[,8:32]>0)
new$richness <- insects.rowsums

insects.abun <- rowSums(old[,8:32])
old$abundance <- insects.abun
insects.rowsums <- rowSums(old[,8:32]>0)
old$richness <- insects.rowsums

mean(new$abundance) #43.41
sd(new$abundance)/sqrt(10) #17.56

mean(new$richness) #3.53
sd(new$richness)/sqrt(10) #0.48

mean(old$abundance) #36.85
sd(old$abundance)/sqrt(10) #13.64

mean(old$richness) #3.55
sd(old$richness)/sqrt(10) #0.49

###

#To obtain richness counts
insects.rowsums <- rowSums(insects[,8:32]>0)
insects$richness <- insects.rowsums

#To obtain abundance counts
insects.abun <- rowSums(insects[,8:32])
insects$abundance <- insects.abun

#calculate Shannon diversity
diversity <-diversity(insects[,8:32])
insects$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(insects[,8:32]))
insects$evenness <- evenness

###

#Mixed effects models
library(lme4)
library(lmerTest) #to obtain p values
library (emmeans) #for pairwise comparisons
library (multcompView) #to view letters
library (car) #Anova (needed because of negative binomial)
citation("car")
library (nortest)
library(bbmle)
library(DHARMa)
library(ggplot2)
library(sjPlot)
library (jtools)
library(interactions)

#WORK ON THIS -- need to change model to fix normality

#order richness
#AIC 5275
richness.model<-lm(richness ~ CARD + week + TREAT:REP, data=insects)
summary(richness.model)
Anova (richness.model)
AIC(richness.model)
#results: cards not sig diff

#check assumptions
dotchart(insects$richness, main = "richness", group = insects$CARD) # way to visualize outliers

with(insects, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(insects, bartlett.test(richness ~ CARD)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.3607

plot(richness.model) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richness.model))
qqline(resid(richness.model))

plot(simulateResiduals(richness.model)) # another way to check for normailty and homogeneity of variance
#KS test: p = SIG
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(richness.model)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richness.model)
influenceIndexPlot(richness.model, vars = c("Cook"), id = list(n = 3))

#

#WORK ON THIS -- need to change model to fix normality

#order abundance
##AIC 15754
abundance.model<-lm(abundance ~ CARD + week + TREAT:REP, data=insects)
#abundance.model<-glmer(abundance ~ CARD + week + (1 | TREAT:REP), data=insects, family = negative.binomial (4))
summary(abundance.model)
Anova(abundance.model)
AIC(abundance.model)
#results: cards are sig diff (p = 0.0027)

#check assumptions
dotchart(insects$abundnce, main = "abundance", group = insects$CARD) # way to visualize outliers

with(insects, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(insects, bartlett.test(abundance ~ CARD)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 5.785e-12

plot(abundance.model) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abundance.model))
qqline(resid(abundance.model))

plot(simulateResiduals(abundance.model)) # another way to check for normailty and homogeneity of variance
#KS test: p = 
#dispersion test: p = 
#outlier test: p =
#no significant problems detected 

densityPlot(rstudent(abundance.model)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abundance.model)
influenceIndexPlot(abundance.model, vars = c("Cook"), id = list(n = 3))

#

#STOPPED HERE ^

#order diversity
##AIC 132
#Date is not significant
diversity.model_order<-lmer(diversity ~ Trap + Date + (1 | Site:Replicate), data=insects_order)
summary(diversity.model_order)
Anova(diversity.model_order)
AIC(diversity.model_order)
#pairwise comparison 
div.emm_order<-emmeans(diversity.model_order,pairwise~Trap)
div.emm_order
#results: no sig diff jar-pitfall (0.4395), jar-sticky (0.8075), pitfall-sticky (0.0859); sig between rest
div.cld_order<-multcomp::cld(div.emm_order, alpha = 0.05, Letters = LETTERS)
div.cld_order

#order evenness
##AIC -184
evenness.model_order<-lmer(evenness ~ Trap + Date + (1 | Site:Replicate), data=insects_order)
summary(evenness.model_order)
Anova(evenness.model_order)
AIC(evenness.model_order)
#pairwise comparison 
even.emm_order<-emmeans(evenness.model_order,pairwise~Trap)
even.emm_order
#results: no sig diff between jar-pitfall (0.1060), jar-ramp (0.8689),jar-sticky (0.1062), ramp-sticky (0.4298); sig btw rest
even.cld_order<-multcomp::cld(even.emm_order, alpha = 0.05, Letters = LETTERS)
even.cld_order






#species accumulation
library (BiodiversityR)
library(ggplot2)

#individual curves for each trap (card) type
new.com.matrix<-new[c(8:32)]
new_curve<-accumresult(new.com.matrix, method = "exact", permutations = 1000)

old.com.matrix<-old[c(8:32)]
old_curve<-accumresult(old.com.matrix, method = "exact", permutations = 1000)

#first-order jackknife estimates are based on the number of singletons
#second-order jackknife estimates are based on the number of singletons and doubletons

#calculates species richness for each sample
specnumber(com.matrix) #ranges from 1 to 8?

#calculates species richness by treatment (CARD)
specnumber(com.matrix, groups = insects$CARD) #new=21; sticky=21

#total richness and jackknife
rich <- diversityresult(com.matrix, y=NULL, index = "richness")
rich # 21
j1 <- diversityresult(com.matrix, y=NULL, index = "jack1")
j1 # 21
#100%
j2 <- diversityresult(com.matrix, y=NULL, index = "jack2")
j2 # 21
#100%

#new jackknife; richness = 21
j1.new <- diversityresult(new.com.matrix, y=NULL, index = "jack1")
j1.new # 21.998366
#95%
j2.new <- diversityresult(new.com.matrix, y=NULL, index = "jack2")
j2.new # 22.995098
#91%

#old jackknife; richness = 21
j1.old <- diversityresult(old.com.matrix, y=NULL, index = "jack1")
j1.old # 21
#100%
j2.old <- diversityresult(old.com.matrix, y=NULL, index = "jack2")
j2.old # 20.003267
#104% --> 100%

#BiodiversityR::accumcomp
Accum.1_functional <- accumcomp(com.matrix, y=env.matrix, factor='CARD', 
                                method='random', conditioned=FALSE, plotit=FALSE)
Accum.1_functional

#BiodiversityR::accumcomp.long
accum.long1_functional <- accumcomp.long(Accum.1_functional, ci=NA, label.freq=5)
head(accum.long1_functional)

#plot
#empty canvas
BioR.theme <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

accum <- ggplot(data=accum.long1_functional, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_color_manual(values=c("#009E73","#E69F00"))+
  scale_shape_manual(values=c(19,17))+
  geom_line(aes(colour=Grouping), size=0.1) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, 0.3))), 
              show.legend=FALSE, linetype = 0) + 
  geom_point(data=subset(accum.long1_functional, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=3) +
  BioR.theme +
  labs(x = "", y = "Richness", colour = "CARD", shape = "CARD")
accum

pdf("accumulation curve.pdf", height=6, width=8) #height and width in inches
accum
dev.off()
