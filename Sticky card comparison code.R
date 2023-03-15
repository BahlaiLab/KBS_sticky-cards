##2021 - comparing Old and New cards within year

#bring in data
insects21 <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_LTER_for_analyses.csv", na.strings = NULL)

#change week, CARD, TREAT, and STATION to factor
insects21$week <- as.factor(insects21$week)
insects21$CARD <- as.factor(insects21$CARD)
insects21$TREAT <- as.factor(insects21$TREAT)
insects21$STATION <- as.factor(insects21$STATION)

str(insects21)
summary(insects21)

###

#NMDS of insect community between card types
library (vegan)

#Create matrix of environmental variables
env.matrix<-insects21[c(1:4,30)]
#create matrix of community variables
com.matrix<-insects21[c(5:29)]

#ordination by NMDS
NMDS21<-metaMDS(com.matrix, distance="bray", k=2, autotransform=TRUE, trymax=300) #stress=.23
NMDS21

#NMDS visualization
#8x9
plot(NMDS21, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS21, env.matrix$CARD, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old21")
ordiellipse(NMDS21, env.matrix$CARD, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New21") 
points(NMDS21, display="sites", select=which(env.matrix$CARD=="Old21"),pch=19, col="#E69F00")
points(NMDS21, display="sites", select=which(env.matrix$CARD=="New21"), pch=17, col="#009E73")
#add legend
legend(0.933,1.399, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.2, legend=c("2021 Old cards", "2021 New cards"))

#bootstrapping and testing for differences between the groups (cards)
fit<-adonis2(com.matrix ~ CARD, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 0.3 -- no sig diff between card types

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARD))
#P-value = 0.5337 -- cannot assume homogeneity of multivariate dispersion

#

#subset by card type
new <- insects21[which(insects21$CARD=="New21"),] 
old <- insects21[which(insects21$CARD=="Old21"),] 

#calculate mean and SE richness and abundance of each card pooled by rep
insects.abun <- rowSums(new[,5:29])
new$abundance <- insects.abun
insects.rowsums <- rowSums(new[,5:29]>0)
new$richness <- insects.rowsums

insects.abun <- rowSums(old[,5:29])
old$abundance <- insects.abun
insects.rowsums <- rowSums(old[,5:29]>0)
old$richness <- insects.rowsums

mean(new$abundance) #220.84
sd(new$abundance)/sqrt(10) #67.86

mean(new$richness) #6.65
sd(new$richness)/sqrt(10) #0.68

mean(old$abundance) #187.85
sd(old$abundance)/sqrt(10) #57.00

mean(old$richness) #6.64
sd(old$richness)/sqrt(10) #0.73

#counts from each individual card -- calculated before cards were pooled across reps
mean(new$abundance) #43.41
sd(new$abundance)/sqrt(10) #17.56

mean(new$richness) #3.52
sd(new$richness)/sqrt(10) #0.48

mean(old$abundance) #36.85
sd(old$abundance)/sqrt(10) #13.64

mean(old$richness) #3.55
sd(old$richness)/sqrt(10) #0.49

###

#To obtain richness counts
insects.rowsums <- rowSums(insects21[,5:29]>0)
insects21$richness <- insects.rowsums

#To obtain abundance counts
insects.abun <- rowSums(insects21[,5:29])
insects21$abundance <- insects.abun

#calculate Shannon diversity
diversity <-diversity(insects21[,5:29])
insects21$diversity <-diversity

###
#Generalized linear models
library (car) #Anova 

library(lme4) #for linear mixed effects models
library(lmerTest) #to obtain p values with linear mixed effects models
library(emmeans)

library (nortest)
library(bbmle)
library(DHARMa)
library(ggplot2)
library(sjPlot)
library (jtools)
library(interactions)

#richness
#AIC 1310
richness.model<-glm(richness ~ CARD + week + TREAT + offset(TRAPS), data=insects21, family = poisson)
summary(richness.model)
Anova (richness.model)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p=0.80)
AIC(richness.model)

#check assumptions
dotchart(insects$richness, main = "richness", group = insects$CARD) # way to visualize outliers

with(insects, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 1.577e-10

with(insects, bartlett.test(richness ~ CARD)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.3633

plot(richness.model) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richness.model))
qqline(resid(richness.model))

plot(simulateResiduals(richness.model)) # another way to check for normality and homogeneity of variance
#KS test: p = 0.60684
#dispersion test: p = 0.608
#outlier test: p = 0.73751
#no significant problems detected 

densityPlot(rstudent(richness.model)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richness.model)
influenceIndexPlot(richness.model, vars = c("Cook"), id = list(n = 3))

#

#abundance
##AIC 17070
abundance.model<-glm(abundance ~ CARD + week + TREAT + offset(TRAPS), data=insects21, family = poisson)
summary(abundance.model)
Anova(abundance.model)
#all sig
#results: sig diff btw cards (p = <0.0001)
AIC(abundance.model)

#check assumptions
dotchart(insects$abundance, main = "abundance", group = insects$CARD) # way to visualize outliers
##most are clustered towards zero

with(insects, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(insects, bartlett.test(abundance ~ CARD)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.03574

plot(abundance.model) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abundance.model))
qqline(resid(abundance.model))

plot(simulateResiduals(abundance.model)) # another way to check for normality and homogeneity of variance
##won't run with glm
#KS test: p = 
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(abundance.model)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abundance.model)
influenceIndexPlot(abundance.model, vars = c("Cook"), id = list(n = 3))

#

#diversity
##AIC 107
diversity.model<-glm(diversity ~ CARD + week + TREAT + offset(TRAPS), data=insects21)
summary(diversity.model)
Anova(diversity.model)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p = 0.21)
AIC(diversity.model)

#check assumptions
dotchart(insects$diversity, main = "diversity", group = insects$CARD) # way to visualize outliers

with(insects, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value 0.2938

with(insects, bartlett.test(diversity ~ CARD)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.7498

plot(diversity.model) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(diversity.model))
qqline(resid(diversity.model))

plot(simulateResiduals(diversity.model)) # another way to check for normality and homogeneity of variance
#KS test: p = 0.15437
#dispersion test: p =  0.608
#outlier test: p = SIG
#no significant problems detected 

densityPlot(rstudent(diversity.model)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(diversity.model)
influenceIndexPlot(diversity.model, vars = c("Cook"), id = list(n = 3))

###
library(ggplot2)

#richness by card type
richness.plot<-ggplot(insects21, aes(x = factor(CARD), y = richness, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
richness.plot

#abundance by card type
abundance.plot<-ggplot(insects21, aes(x = factor(CARD), y = abundance, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Abundance (log10)")+
  scale_y_continuous(trans="log10")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
abundance.plot

#diveristy by card type
diversity.plot<-ggplot(insects21, aes(x = factor(CARD), y = diversity, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
diversity.plot

#
#mush together plots
library(ggpubr) 
boxplot_2021 <- ggarrange(richness.plot, abundance.plot, diversity.plot, 
                              ncol = 3, nrow = 1,
                              common.legend = TRUE, legend = "bottom")
boxplot_2021

pdf("boxplot_2021.pdf", height=5, width=8) #height and width in inches
boxplot_2021
dev.off()

##

#Examining individual taxa for 2021

#calculate taxa abundance
colSums(insects21[,5:29]) #all 2021 cards

old <- insects21[which(insects21$CARD=="Old21"),] 
new <- insects21[which(insects21$CARD=="New21"),] 
colSums(old[,5:29])
colSums(new[,5:29])

#ABIPN
ABIPN<-insects21[c(1:5,30)]
ABIPN.glm<-glm(ABIPN ~ CARD + week + TREAT + offset(TRAPS), data=ABIPN, family=poisson)
summary(ABIPN.glm)
Anova (ABIPN.glm)
#did not converge because zero caught
#results: no sig diff btw cards (p=1)
AIC(ABIPN.glm) #32

#BURSI
BURSI<-insects21[c(1:4,6,30)]
BURSI.glm<-glm(BURSI ~ CARD + week + TREAT + offset(TRAPS), data=BURSI, family=poisson)
summary(BURSI.glm)
Anova (BURSI.glm)
#card and treat: not sig, week: sig
#results: no sig diff btw cards (p=.9)
AIC(BURSI.glm) #111

#C7
C7<-insects21[c(1:4,7,30)]
C7.glm<-glm(C7 ~ CARD + week + TREAT + offset(TRAPS), data=C7, family=poisson)
summary(C7.glm)
Anova (C7.glm)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p=.45)
AIC(C7.glm) #804

#CMAC
CMAC<-insects21[c(1:4,8,30)]
glm<-glm(CMAC ~ CARD + week + TREAT + offset(TRAPS), data=CMAC, family=poisson)
summary(glm)
Anova (glm)
#card and treat: not sig, week: sig
#results: no sig diff btw cards (p=.54)
AIC(glm) #222

#CSTIG
CSTIG<-insects21[c(1:4,9,30)]
glm<-glm(CSTIG ~ CARD + week + TREAT + offset(TRAPS), data=CSTIG, family=poisson)
summary(glm)
Anova (glm)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p=.71)
AIC(glm) #78

#CTRIF
CTRIF<-insects21[c(1:4,10,30)]
glm<-glm(CTRIF ~ CARD + week + TREAT + offset(TRAPS), data=CTRIF, family=poisson)
summary(glm)
Anova (glm)
#did not converge because zero caught
#results: no sig diff btw cards (p=1)
AIC(glm) #32

#CYCSP
CYCSP<-insects21[c(1:4,11,30)]
glm<-glm(CYCSP ~ CARD + week + TREAT + offset(TRAPS), data=CYCSP, family=poisson)
summary(glm)
Anova (glm)
#week: not sig, card and treat: sig
#results: sig diff btw cards (p=.01)
AIC(glm) #196

#H13
H13<-insects21[c(1:4,12,30)]
glm<-glm(H13 ~ CARD + week + TREAT + offset(TRAPS), data=H13, family=poisson)
summary(glm)
Anova (glm)
#did not converge because zero caught
#results: no sig diff btw cards (p=1)
AIC(glm) #32

#HAXY
HAXY<-insects21[c(1:4,13,30)]
glm<-glm(HAXY ~ CARD + week + TREAT + offset(TRAPS), data=HAXY, family=poisson)
summary(glm)
Anova (glm)
#all sig 
#results: sig diff btw cards (p=.049) 
AIC(glm) #1344

#HCONV
HCONV<-insects21[c(1:4,14,30)]
glm<-glm(HCONV ~ CARD + week + TREAT + offset(TRAPS), data=HCONV, family=poisson)
summary(glm)
Anova (glm)
#card and treat: not sig, week: sig
#results: no sig diff btw cards (p=.27)
AIC(glm) #103

#HGLAC
HGLAC<-insects21[c(1:4,15,30)]
glm<-glm(HGLAC ~ CARD + week + TREAT + offset(TRAPS), data=HGLAC, family=poisson)
summary(glm)
Anova (glm)
#did not converge because zero caught
#results: no sig diff btw cards (p=1)
AIC(glm) #32

#HPARN
HPARN<-insects21[c(1:4,16,30)]
glm<-glm(HPARN ~ CARD + week + TREAT + offset(TRAPS), data=HPARN, family=poisson)
summary(glm)
Anova (glm)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p=.56)
AIC(glm) #250

#HVAR
HVAR<-insects21[c(1:4,17,30)]
glm<-glm(HVAR ~ CARD + week + TREAT + offset(TRAPS), data=HVAR, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #463

#PQUA
PQUA<-insects21[c(1:4,18,30)]
glm<-glm(PQUA ~ CARD + week + TREAT + offset(TRAPS), data=PQUA, family=poisson)
summary(glm)
Anova (glm)
#card: not sig, week and treat: sig
AIC(glm) #444
#pairwise comparison 
emm<-emmeans(glm,pairwise~CARD)
emm
#results: no sig diff btw cards (p=.16)

#CANTHARID
CANTHARID<-insects21[c(1:4,19,30)]
glm<-glm(CANTHARID ~ CARD + week + TREAT + offset(TRAPS), data=CANTHARID, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p=.0067)
AIC(glm) #495

#LAMPY
LAMPY<-insects21[c(1:4,20,30)]
glm<-glm(LAMPY ~ CARD + week + TREAT + offset(TRAPS), data=LAMPY, family=poisson)
summary(glm)
Anova (glm)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p=.1)
AIC(glm) #1430

#LCW
LCW<-insects21[c(1:4,21,30)]
glm<-glm(LCW ~ CARD + week + TREAT + offset(TRAPS), data=LCW, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #1035

#MECOP
MECOP<-insects21[c(1:4,22,30)]
glm<-glm(MECOP ~ CARD + week + TREAT + offset(TRAPS), data=MECOP, family=poisson)
summary(glm)
Anova (glm)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p=.16)
AIC(glm) #571

#X20SPOT
X20SPOT<-insects21[c(1:4,23,30)]
glm<-glm(X20SPOT ~ CARD + week + TREAT + offset(TRAPS), data=X20SPOT, family=poisson)
summary(glm)
Anova (glm)
#card and week: not sig, treat: sig
#results: no sig diff btw cards (p=.64)
AIC(glm) #158

#OTHER
OTHER<-insects21[c(1:4,24,30)]
glm<-glm(OTHER ~ CARD + week + TREAT + offset(TRAPS), data=OTHER, family=poisson)
summary(glm)
Anova (glm)
#card and week: not sig, treat: sig
#results: no sig diff btw cards (p=.53)
AIC(glm) #61

#Syrphidae
Syrphidae<-insects21[c(1:4,25,30)]
glm<-glm(Syrphidae ~ CARD + week + TREAT + offset(TRAPS), data=Syrphidae, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p < 0.0001)
AIC(glm) #858

#Ichneumonoidae
Ichneumonoidae<-insects21[c(1:4,26,30)]
glm<-glm(Ichneumonoidae ~ CARD + week + TREAT + offset(TRAPS), data=Ichneumonoidae, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p=0.004)
AIC(glm) #2229

#Chalcidoidae
Chalcidoidae<-insects21[c(1:4,27,30)]
glm<-glm(Chalcidoidae ~ CARD + week + TREAT + offset(TRAPS), data=Chalcidoidae, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #16349

#Lepidoptera
Lepidoptera<-insects21[c(1:4,28,30)]
glm<-glm(Lepidoptera ~ CARD + week + TREAT + offset(TRAPS), data=Lepidoptera, family=poisson)
summary(glm)
Anova (glm)
#card: not sig, week and treat: sig
#results: no sig diff btw cards (p=0.25)
AIC(glm) #702

#Orthoptera
Orthoptera<-insects21[c(1:4,29,30)]
glm<-glm(Orthoptera ~ CARD + week + TREAT + offset(TRAPS), data=Orthoptera, family=poisson)
summary(glm)
Anova (glm)
#treat: not sig, card and week: sig
#results: sig diff btw cards (p=0.04)
AIC(glm) #65

###

#boxplots for significantly different taxa

CYCSP.plot<-ggplot(CYCSP, aes(x = CARD, y = CYCSP, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Cycloneda munda")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
CYCSP.plot

HAXY.plot<-ggplot(HAXY, aes(x = CARD, y = HAXY, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Harmonia axyridis")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
HAXY.plot

HVAR.plot<-ggplot(HVAR, aes(x = CARD, y = HVAR, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Hippodamia variegata")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
HVAR.plot

CANTHARID.plot<-ggplot(CANTHARID, aes(x = CARD, y = CANTHARID, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Cantharidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
CANTHARID.plot

LCW.plot<-ggplot(LCW, aes(x = CARD, y = LCW, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Chrysopidae and Hemerobiidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
LCW.plot

Syrphidae.plot<-ggplot(Syrphidae, aes(x = CARD, y = Syrphidae, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Syrphidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
Syrphidae.plot

Ichneumonoidae.plot<-ggplot(Ichneumonoidae, aes(x = CARD, y = Ichneumonoidae, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Ichneumonoidea")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
Ichneumonoidae.plot

Chalcidoidae.plot<-ggplot(Chalcidoidae, aes(x = CARD, y = Chalcidoidae, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Chalcidoidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
Chalcidoidae.plot

Lepidoptera.plot<-ggplot(Lepidoptera, aes(x = CARD, y = Lepidoptera, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Lepidoptera")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
Lepidoptera.plot

Orthoptera.plot<-ggplot(Orthoptera, aes(x = CARD, y = Orthoptera, fill=CARD))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Orthoptera")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#E69F00","#009E73"),name="Card type:",
                    breaks=c("Old21", "New21"),
                    labels=c("Old cards 2021", "New cards 2021"))
Orthoptera.plot

#merge some of these plots
library (ggpubr)
taxa.plots_2021 <-ggarrange(HAXY.plot, HVAR.plot, LCW.plot, Syrphidae.plot, Ichneumonoidae.plot, Chalcidoidae.plot, 
                           #labels = c("A", "B"),
                           ncol = 3, nrow = 2,
                           common.legend = TRUE, legend = "bottom")
taxa.plots_2021

pdf("taxa.plots_2021.pdf", height=8, width=8) #height and width in inches
taxa.plots_2021
dev.off()

#

#species accumulation for 2021
library (BiodiversityR)
library(ggplot2)

#individual curves for each trap (card) type
new.com.matrix<-new[c(5:29)]
new_curve<-accumresult(new.com.matrix, method = "exact", permutations = 1000)

old.com.matrix<-old[c(5:29)]
old_curve<-accumresult(old.com.matrix, method = "exact", permutations = 1000)

#first-order jackknife estimates are based on the number of singletons
#second-order jackknife estimates are based on the number of singletons and doubletons

#calculates species richness for each sample
specnumber(com.matrix) #ranges from 1 to 12

#calculates species richness by treatment (CARD)
specnumber(com.matrix, groups = insects21$CARD) #new=21; old=21

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
  labs(x = "Samples", y = "Richness", colour = "CARD", shape = "CARD")
accum

pdf("accumulation curve with pooled data.pdf", height=6, width=8) #height and width in inches
accum
dev.off()

##

#2020-2023 - to look at same card types between years

#bring in data from all years
insects_all <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/all%20years_LTER_for_analyses.csv", na.strings = NULL)

#change week, CARD, TREAT, and STATION to factor
insects_all$week <- as.factor(insects_all$week)
insects_all$CARDYEAR <- as.factor(insects_all$CARDYEAR)
insects_all$CARD <- as.factor(insects_all$CARD)
insects_all$TREAT <- as.factor(insects_all$TREAT)
insects_all$STATION <- as.factor(insects_all$STATION)

str(insects_all)
summary(insects_all)

#calculate total abundance from all three years (of field collected data)
colSums(insects_all[,6:24])

#2020 abundance
old20 <- insects_all[which(insects_all$CARDYEAR=="Old20"),]
colSums(old20[,6:24])

#2022 abundance
new22 <- insects_all[which(insects_all$CARDYEAR=="New22"),] 
colSums(new22[,6:24])

##Old20 and Old21

#pull out that data  
old20 <- insects_all[which(insects_all$CARDYEAR=="Old20"),] 
old21 <- insects_all[which(insects_all$CARDYEAR=="Old21"),] 
#combine
library (plyr)
old20_old21 <- rbind.fill (old20, old21)

#NMDS of insect community between Old20 and Old21 cards
library (vegan)

#first need to get rid of any rows that sum to zero
#so sum (aka get abundance per row)
insects.abun <- rowSums(old20_old21[,6:24])
old20_old21$abundance <- insects.abun

#then remove rows = zero
old20_old21_nonzero <- old20_old21[rowSums(old20_old21[26])>0,]
str(old20_old21_nonzero)

#Create matrix of environmental variables
env.matrix<-old20_old21_nonzero[c(1:5,25)]
#create matrix of community variables
com.matrix<-old20_old21_nonzero[c(6:24)]

#ordination by NMDS
NMDSold<-metaMDS(com.matrix, distance="bray", k=2, autotransform=TRUE, trymax=100) #stress=.18
NMDSold

#NMDS visualization with trap types by year
#8x9
plot(NMDSold, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDSold, env.matrix$CARDYEAR, draw="polygon", col="#ffba21",kind="sd", conf=0.95, label=FALSE, show.groups = "Old20")
ordiellipse(NMDSold, env.matrix$CARDYEAR, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old21")
points(NMDSold, display="sites", select=which(env.matrix$CARDYEAR=="Old20"),pch=15, col="#ffba21")
points(NMDSold, display="sites", select=which(env.matrix$CARDYEAR=="Old21"),pch=19, col="#E69F00")
#add legend
legend(0.88,1.817, title=NULL, pch=c(15,19), col=c("#ffba21", "#E69F00"), cex=1.2, legend=c("2020 Old cards", "2021 Old cards"))

#bootstrapping and testing for differences between the groups (cardyear)
fit<-adonis2(com.matrix ~ CARDYEAR, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 0.001 -- sig diff in old cards btw 2020 and 2021

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARDYEAR))
#P-value = 2.512e-10 -- assumes homogeneity of multivariate dispersion

###

#To obtain richness counts
insects.rowsums <- rowSums(old20_old21[,6:24]>0)
old20_old21$richness <- insects.rowsums

#To obtain abundance counts
insects.abun <- rowSums(old20_old21[,6:24])
old20_old21$abundance <- insects.abun

#calculate Shannon diversity
diversity <-diversity(old20_old21[,6:24])
old20_old21$diversity <-diversity

#Generalized linear models
library (car) #Anova 

#richness
#AIC 1936
richness.model<-glm(richness ~ CARDYEAR + week + TREAT + offset(TRAPS), data=old20_old21, family=poisson)
summary(richness.model)
summary(anova(richness.model))
Anova (richness.model)
#all sig (CARDYEAR, week, treatment)
#results: sig diff btw cards (p<0.0001)
AIC(richness.model)

#abundance
##AIC 4253
abundance.model<-glm(abundance ~ CARDYEAR + week + TREAT + offset(TRAPS), data=old20_old21, family=poisson)
summary(abundance.model)
Anova(abundance.model)
#all sig (CARDYEAR, week, treatment)
#results: sig diff btw cards (p<0.0001)
AIC(abundance.model)

#diversity
##AIC 731
diversity.model<-glm(diversity ~ CARDYEAR + week + TREAT + offset(TRAPS), data=old20_old21)
summary(diversity.model)
Anova(diversity.model)
#all sig
#results: sig diff btw cards (p=0.0001)
AIC(diversity.model)

##

#CARDYEAR richness by card type
richness.plot<-ggplot(old20_old21, aes(x = factor(CARDYEAR), y = richness, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
richness.plot

#CARDYEAR abundance by card type
abundance.plot<-ggplot(old20_old21, aes(x = factor(CARDYEAR), y = abundance, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Abundance (log10)")+
  scale_y_continuous(trans="log10")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
abundance.plot

#CARDYEAR diveristy by card type
diversity.plot<-ggplot(old20_old21, aes(x = factor(CARDYEAR), y = diversity, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
diversity.plot

#
#mush together plots
library(ggpubr) 
oldcard_boxplot <- ggarrange(richness.plot, abundance.plot, diversity.plot, 
                                ncol = 3, nrow = 1,
                                common.legend = TRUE, legend = "bottom")
oldcard_boxplot

pdf("oldcard_boxplot.pdf", height=5, width=8) #height and width in inches
oldcard_boxplot
dev.off()

##

#Examining individual taxa for Old 2020 and Old 2021

#calculate taxa abundance
colSums(old20_old21[,6:24]) #all old cards
colSums(old20[,6:24]) #2020 old cards
colSums(old21[,6:24]) #2021 old cards

#ABIPN
ABIPN<-old20_old21[c(1:6,25)]
ABIPN.glm<-glm(ABIPN ~ CARDYEAR + week + TREAT + offset(TRAPS), data=ABIPN, family=poisson)
summary(ABIPN.glm)
Anova (ABIPN.glm)
#did not converge because zero caught
#results: no sig diff btw cards (p=1)
AIC(ABIPN.glm) #40

#BURSI
BURSI<-old20_old21[c(1:5,7,25)]
BURSI.glm<-glm(BURSI ~ CARDYEAR + week + TREAT + offset(TRAPS), data=BURSI, family=poisson)
summary(BURSI.glm)
Anova (BURSI.glm)
#cardyear: not sig, week and treat: sig
#results: no sig diff btw cards (0.15)
AIC(BURSI.glm) #191

#C7
C7<-old20_old21[c(1:5,8,25)]
C7.glm<-glm(C7 ~ CARDYEAR + week + TREAT + offset(TRAPS), data=C7, family=poisson)
summary(C7.glm)
Anova (C7.glm)
#cardyear: not sig, week and treat: sig
#results: no sig diff btw cards (0.16)
AIC(C7.glm) #1398

#CMAC
CMAC<-old20_old21[c(1:5,9,25)]
glm<-glm(CMAC ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CMAC, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (<0.0001)
AIC(glm) #929

#CSTIG
CSTIG<-old20_old21[c(1:5,10,25)]
glm<-glm(CSTIG ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CSTIG, family=poisson)
summary(glm)
Anova(glm)
#week: not sig, cardyear and treat: sig
#results: sig diff btw cards (p=.0006)
AIC(glm) #65

#CTRIF
CTRIF<-old20_old21[c(1:5,11,25)]
glm<-glm(CTRIF ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CTRIF, family=poisson)
summary(glm)
Anova (glm)
#did not coverge because zero caught
#results: no sig diff btw cards (p=1)
AIC(glm) #40

#CYCSP
CYCSP<-old20_old21[c(1:5,12,25)]
glm<-glm(CYCSP ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CYCSP, family=poisson)
summary(glm)
Anova (glm)
#cardyear: not sig, week and treat: sig
#results: no sig diff btw cards (p=.22)
AIC(glm) #430

#H13
H13<-old20_old21[c(1:5,13,25)]
glm<-glm(H13 ~ CARDYEAR + week + TREAT + offset(TRAPS), data=H13, family=poisson)
summary(glm)
Anova (glm)
#nothing sig
#results: no sig diff btw cards (p=.17)
AIC(glm) #49

#HAXY
HAXY<-old20_old21[c(1:5,14,25)]
glm<-glm(HAXY ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HAXY, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #1769

#HCONV
HCONV<-old20_old21[c(1:5,15,25)]
glm<-glm(HCONV ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HCONV, family=poisson)
summary(glm)
Anova (glm)
#week and treat: not sig, cardyear: sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #93

#HGLAC
HGLAC<-old20_old21[c(1:5,16,25)]
glm<-glm(HGLAC ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HGLAC, family=poisson)
summary(glm)
Anova (glm)
#nothing sig
#results: no sig diff btw cards (p=.33)
AIC(glm) #45

#HPARN
HPARN<-old20_old21[c(1:5,17,25)]
glm<-glm(HPARN ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HPARN, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p=.044)
AIC(glm) #703

#HVAR
HVAR<-old20_old21[c(1:5,18,25)]
glm<-glm(HVAR ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HVAR, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #1273

#PQUA
PQUA<-old20_old21[c(1:5,19,25)]
glm<-glm(PQUA ~ CARDYEAR + week + TREAT + offset(TRAPS), data=PQUA, family=poisson)
summary(glm)
Anova (glm)
#cardyear: not sig, week and treat: sig
#results: no sig diff btw cards (p=.19)
AIC(glm) #855

#CANTHARID
CANTHARID<-old20_old21[c(1:5,20,25)]
glm<-glm(CANTHARID ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CANTHARID, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #1025

#LAMPY
LAMPY<-old20_old21[c(1:5,21,25)]
glm<-glm(LAMPY ~ CARDYEAR + week + TREAT + offset(TRAPS), data=LAMPY, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #2164

#LCW
LCW<-old20_old21[c(1:5,22,25)]
glm<-glm(LCW ~ CARDYEAR + week + TREAT + offset(TRAPS), data=LCW, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #1480

#MECOP
MECOP<-old20_old21[c(1:5,23,25)]
glm<-glm(MECOP ~ CARDYEAR + week + TREAT + offset(TRAPS), data=MECOP, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #955

#X20SPOT
X20SPOT<-old20_old21[c(1:5,24,25)]
glm<-glm(X20SPOT ~ CARDYEAR + week + TREAT + offset(TRAPS), data=X20SPOT, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #753

##

#Plots of taxa that were significantly different

CMAC.plot<-ggplot(CMAC, aes(x = CARDYEAR, y = CMAC, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Colemegilla maculata")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
CMAC.plot

CSTIG.plot<-ggplot(CSTIG, aes(x = CARDYEAR, y = CSTIG, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Chilochous stigma")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
CSTIG.plot

HAXY.plot<-ggplot(HAXY, aes(x = CARDYEAR, y = HAXY, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Harmonia axyridis")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HAXY.plot

HCONV.plot<-ggplot(HCONV, aes(x = CARDYEAR, y = HCONV, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Hippodamia convergens")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HCONV.plot

HPARN.plot<-ggplot(HPARN, aes(x = CARDYEAR, y = HPARN, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Hippodamia parenthesiS")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HPARN.plot

HVAR.plot<-ggplot(HVAR, aes(x = CARDYEAR, y = HVAR, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Hippodamia variegata")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HVAR.plot

X20SPOT.plot<-ggplot(X20SPOT, aes(x = CARDYEAR, y = X20SPOT, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Psyllobora virgintimaculata")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
X20SPOT.plot

CANTHARID.plot<-ggplot(CANTHARID, aes(x = CARDYEAR, y = CANTHARID, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Cantharidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
CANTHARID.plot

LAMPY.plot<-ggplot(LAMPY, aes(x = CARDYEAR, y = LAMPY, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Lampyridae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
LAMPY.plot

LCW.plot<-ggplot(LCW, aes(x = CARDYEAR, y = LCW, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Chrysopidae and Hemerobiidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
LCW.plot

MECOP.plot<-ggplot(MECOP, aes(x = CARDYEAR, y = MECOP, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Mecoptera")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
MECOP.plot

#

#merge some of these plots
library (ggpubr)
taxa.plots_old <-ggarrange(HAXY.plot, HVAR.plot, HCONV.plot, CANTHARID.plot, LCW.plot, MECOP.plot,
                            #labels = c("A", "B"),
                            ncol = 3, nrow = 2,
                            common.legend = TRUE, legend = "bottom")
taxa.plots_old

pdf("taxa.plots_old.pdf", height=8, width=8) #height and width in inches
taxa.plots_old
dev.off()

#

##New21 and New22

#pull out that data  
new21 <- insects_all[which(insects_all$CARDYEAR=="New21"),] 
new22 <- insects_all[which(insects_all$CARDYEAR=="New22"),] 
#combine
library (plyr)
new21_new22 <- rbind.fill (new21, new22)

#NMDS of insect community between New21 and New22 cards
library (vegan)

#first need to get rid of any rows that sum to zero
#so sum (aka get abundance per row)
insects.abun <- rowSums(new21_new22[,6:24])
new21_new22$abundance <- insects.abun

#then remove rows = zero
new21_new22_nonzero <- new21_new22[rowSums(new21_new22[26])>0,]
str(new21_new22_nonzero)

#Create matrix of environmental variables
env.matrix<-new21_new22_nonzero[c(1:5,25)]
#create matrix of community variables
com.matrix<-new21_new22_nonzero[c(6:24)]

#ordination by NMDS
NMDSnew<-metaMDS(com.matrix, distance="bray", k=2, autotransform=TRUE, trymax=100) #stress=.23
NMDSnew

#NMDS visualization with trap types by year
#8x9
plot(NMDSnew, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDSnew, env.matrix$CARDYEAR, draw="polygon", col="#00c690",kind="sd", conf=0.95, label=FALSE, show.groups = "New22")
ordiellipse(NMDSnew, env.matrix$CARDYEAR, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New21")
points(NMDSnew, display="sites", select=which(env.matrix$CARDYEAR=="New21"),pch=17, col="#009E73")
points(NMDSnew, display="sites", select=which(env.matrix$CARDYEAR=="New22"),pch=18, col="#00c690")
#add legend
legend(0.895,1.375, title=NULL, pch=c(17,18), col=c("#009E73", "#00c690"), cex=1.2, legend=c("2021 New cards", "2022 New cards"))

#bootstrapping and testing for differences between the groups (cardyear)
fit<-adonis2(com.matrix ~ CARDYEAR, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 0.001 -- sig diff in New cards btw 2021 and 2022

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARDYEAR))
#P-value = .5006 -- cannot assume homogeneity of multivariate dispersion

##

#To obtain richness counts
insects.rowsums <- rowSums(new21_new22[,6:24]>0)
new21_new22$richness <- insects.rowsums

#To obtain abundance counts
insects.abun <- rowSums(new21_new22[,6:24])
new21_new22$abundance <- insects.abun

#calculate Shannon diversity
diversity <-diversity(new21_new22[,6:24])
new21_new22$diversity <-diversity

#Generalized linear models for New cards
library (car) #Anova 

#richness
#AIC 1234
richness.model<-glm(richness ~ CARDYEAR + week + TREAT + offset(TRAPS), data=new21_new22, family=poisson)
summary(richness.model)
Anova (richness.model)
#CARDYEAR: not sig , week and treatment: sig
#results: no sig diff btw cards (p=0.28)
AIC(richness.model)

#abundance
##AIC 2695
abundance.model<-glm(abundance ~ CARDYEAR + week + TREAT + offset(TRAPS), data=new21_new22, family=poisson)
summary(abundance.model)
Anova(abundance.model)
#all sig (CARDYEAR, week, treatment)
#results: sig diff btw cards (p<0.0001)
AIC(abundance.model)

#diversity
##AIC 569
diversity.model<-glm(diversity ~ CARDYEAR + week + TREAT + offset(TRAPS), data=new21_new22)
summary(diversity.model)
Anova(diversity.model)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(diversity.model)

##

#CARDYEAR richness by card type
richness.plot<-ggplot(new21_new22, aes(x = factor(CARDYEAR), y = richness, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
richness.plot

#CARDYEAR abundance by card type
abundance.plot<-ggplot(new21_new22, aes(x = factor(CARDYEAR), y = abundance, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Abundance (log10)")+
  scale_y_continuous(trans="log10")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
abundance.plot

#CARDYEAR diveristy by card type
diversity.plot<-ggplot(new21_new22, aes(x = factor(CARDYEAR), y = diversity, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
diversity.plot

#
#mush together plots
library(ggpubr) 
newcard_boxplot <- ggarrange(richness.plot, abundance.plot, diversity.plot, 
                             ncol = 3, nrow = 1,
                             common.legend = TRUE, legend = "bottom")
newcard_boxplot

pdf("newcard_boxplot.pdf", height=5, width=8) #height and width in inches
newcard_boxplot
dev.off()

##

#Examining individual taxa for New 2021 and New 2022

#calculate taxa abundance
colSums(new21_new22[,6:24]) #all new cards
colSums(new21[,6:24]) #2021 new cards
colSums(new22[,6:24]) #2022 new cards

#ABIPN
ABIPN<-new21_new22[c(1:6,25)]
ABIPN.glm<-glm(ABIPN ~ CARDYEAR + week + TREAT + offset(TRAPS), data=ABIPN, family=poisson)
summary(ABIPN.glm)
Anova (ABIPN.glm)
#did not converge because zero caught
#results: no sig diff btw cards (p=1)
AIC(ABIPN.glm) #46

#BURSI
BURSI<-new21_new22[c(1:5,7,25)]
BURSI.glm<-glm(BURSI ~ CARDYEAR + week + TREAT + offset(TRAPS), data=BURSI, family=poisson)
summary(BURSI.glm)
Anova (BURSI.glm)
#cardyear and treat: not sig, week: sig 
#results: no sig diff btw cards (p=0.31)
AIC(BURSI.glm) #142

#C7
C7<-new21_new22[c(1:5,8,25)]
C7.glm<-glm(C7 ~ CARDYEAR + week + TREAT + offset(TRAPS), data=C7, family=poisson)
summary(C7.glm)
Anova (C7.glm)
#all sig
#results: sig diff btw cards (0.0001)
AIC(C7.glm) #957

#CMAC
CMAC<-new21_new22[c(1:5,9,25)]
glm<-glm(CMAC ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CMAC, family=poisson)
summary(glm)
Anova (glm)
#cardyear: not sig, week and treat: sig 
#results: no sig diff btw cards (0.82)
AIC(glm) #504

#CSTIG
CSTIG<-new21_new22[c(1:5,10,25)]
glm<-glm(CSTIG ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CSTIG, family=poisson)
summary(glm)
Anova(glm)
#cardyear and week: not sig, treat: sig 
#results: no sig diff btw cards (p=1)
AIC(glm) #62

#CTRIF
CTRIF<-new21_new22[c(1:5,11,25)]
glm<-glm(CTRIF ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CTRIF, family=poisson)
summary(glm)
Anova (glm)
#did not converge because zero caught
#results: no sig diff btw cards (p=1)
AIC(glm) #46

#CYCSP
CYCSP<-new21_new22[c(1:5,12,25)]
glm<-glm(CYCSP ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CYCSP, family=poisson)
summary(glm)
Anova (glm)
#cardyear: not sig, week and treat: sig 
#results: no sig diff btw cards (p=0.48)
AIC(glm) #263

#H13
H13<-new21_new22[c(1:5,13,25)]
glm<-glm(H13 ~ CARDYEAR + week + TREAT + offset(TRAPS), data=H13, family=poisson)
summary(glm)
Anova (glm)
#treat: not sig, cardyear and week: sig
#results: sig diff btw cards (p=0.0086)
AIC(glm) #655

#HAXY
HAXY<-new21_new22[c(1:5,14,25)]
glm<-glm(HAXY ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HAXY, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #1177

#HCONV
HCONV<-new21_new22[c(1:5,15,25)]
glm<-glm(HCONV ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HCONV, family=poisson)
summary(glm)
Anova (glm)
#week and treat: sig, cardyear: not sig
#results: no sig diff btw cards (p=.9999)
AIC(glm) #142

#HGLAC
HGLAC<-new21_new22[c(1:5,16,25)]
glm<-glm(HGLAC ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HGLAC, family=poisson)
summary(glm)
Anova (glm)
#nothing sig
#results: no sig diff btw cards (p=1)
AIC(glm) #56

#HPARN
HPARN<-new21_new22[c(1:5,17,25)]
glm<-glm(HPARN ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HPARN, family=poisson)
summary(glm)
Anova (glm)
#cardyear and treat: sig, week: not sig
#results: sig diff btw cards (p=.01)
AIC(glm) #156

#HVAR
HVAR<-new21_new22[c(1:5,18,25)]
glm<-glm(HVAR ~ CARDYEAR + week + TREAT + offset(TRAPS), data=HVAR, family=poisson)
summary(glm)
Anova (glm)
#week and treat: sig, cardyear: not sig
#results: no sig diff btw cards (p=.58)
AIC(glm) #243

#PQUA
PQUA<-new21_new22[c(1:5,19,25)]
glm<-glm(PQUA ~ CARDYEAR + week + TREAT + offset(TRAPS), data=PQUA, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #468

#CANTHARID
CANTHARID<-new21_new22[c(1:5,20,25)]
glm<-glm(CANTHARID ~ CARDYEAR + week + TREAT + offset(TRAPS), data=CANTHARID, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p<0.0001)
AIC(glm) #886

#LAMPY
LAMPY<-new21_new22[c(1:5,21,25)]
glm<-glm(LAMPY ~ CARDYEAR + week + TREAT + offset(TRAPS), data=LAMPY, family=poisson)
summary(glm)
Anova (glm)
#all sig
#results: sig diff btw cards (p=0.0006)
AIC(glm) #1548

#LCW
LCW<-new21_new22[c(1:5,22,25)]
glm<-glm(LCW ~ CARDYEAR + week + TREAT + offset(TRAPS), data=LCW, family=poisson)
summary(glm)
Anova (glm)
#cardyear: not sig, week and treat: sig
#results: no sig diff btw cards (p=0.166)
AIC(glm) #808

#MECOP
MECOP<-new21_new22[c(1:5,23,25)]
glm<-glm(MECOP ~ CARDYEAR + week + TREAT + offset(TRAPS), data=MECOP, family=poisson)
summary(glm)
Anova (glm)
#cardyear: not sig, week and treat: sig
#results: no sig diff btw cards (p=0.053)
AIC(glm) #378

#X20SPOT
X20SPOT<-new21_new22[c(1:5,24,25)]
glm<-glm(X20SPOT ~ CARDYEAR + week + TREAT + offset(TRAPS), data=X20SPOT, family=poisson)
summary(glm)
Anova (glm)
#cardyear and week: not sig, treat: sig
#results: no sig diff btw cards (p=0.15)
AIC(glm) #114

###

#Plots of taxa that were significantly different
# NEED TO EDIT TO BE FOR NEW CARDS

CMAC.plot<-ggplot(CMAC, aes(x = CARDYEAR, y = CMAC, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Colemegilla maculata")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
CMAC.plot

CSTIG.plot<-ggplot(CSTIG, aes(x = CARDYEAR, y = CSTIG, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Chilochous stigma")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
CSTIG.plot

HAXY.plot<-ggplot(HAXY, aes(x = CARDYEAR, y = HAXY, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Harmonia axyridis")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HAXY.plot

HCONV.plot<-ggplot(HCONV, aes(x = CARDYEAR, y = HCONV, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Hippodamia convergens")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HCONV.plot

HPARN.plot<-ggplot(HPARN, aes(x = CARDYEAR, y = HPARN, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Hippodamia parenthesiS")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HPARN.plot

HVAR.plot<-ggplot(HVAR, aes(x = CARDYEAR, y = HVAR, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Hippodamia variegata")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
HVAR.plot

X20SPOT.plot<-ggplot(X20SPOT, aes(x = CARDYEAR, y = X20SPOT, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Psyllobora virgintimaculata")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
X20SPOT.plot

CANTHARID.plot<-ggplot(CANTHARID, aes(x = CARDYEAR, y = CANTHARID, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Cantharidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
CANTHARID.plot

LAMPY.plot<-ggplot(LAMPY, aes(x = CARDYEAR, y = LAMPY, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Lampyridae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
LAMPY.plot

LCW.plot<-ggplot(LCW, aes(x = CARDYEAR, y = LCW, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Chrysopidae and Hemerobiidae")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
LCW.plot

MECOP.plot<-ggplot(MECOP, aes(x = CARDYEAR, y = MECOP, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Mecoptera")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00"),name="Card:",
                    breaks=c("Old20", "Old21"),
                    labels=c("Old cards 2020", "Old cards 2021"))
MECOP.plot

##

#for paper

#Doesn't work ...
#merge all 3 NMDSs
pdf("combined NMDSs.pdf", height=10, width=13)
par(mfrow=c(1,3), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS21, disp='sites', type="n")
title(main="A", adj = 0.01, line = -2, cex.main=2.5)
ordiellipse(NMDS21, env.matrix$CARD, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old21")
ordiellipse(NMDS21, env.matrix$CARD, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New21") 
points(NMDS21, display="sites", select=which(env.matrix$CARD=="Old21"),pch=19, col="#E69F00")
points(NMDS21, display="sites", select=which(env.matrix$CARD=="New21"), pch=17, col="#009E73")
legend(0.99,1.39, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.2, legend=c("2021 Old cards", "2021 New cards"))

plot(NMDSold, disp='sites', type="n")
title(main="B", adj = 0.01, line = -2, cex.main=2.5)
ordiellipse(NMDSold, env.matrix$CARDYEAR, draw="polygon", col="#ffba21",kind="sd", conf=0.95, label=FALSE, show.groups = "Old20")
ordiellipse(NMDSold, env.matrix$CARDYEAR, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old21")
points(NMDSold, display="sites", select=which(env.matrix$CARDYEAR=="Old21"),pch=19, col="#E69F00")
points(NMDSold, display="sites", select=which(env.matrix$CARDYEAR=="Old20"),pch=15, col="#ffba21")
legend(0.97,1.78, title=NULL, pch=c(15,19), col=c("#ffba21", "#E69F00"), cex=1.2, legend=c("2020 Old cards", "2021 Old cards"))

plot(NMDSnew, disp='sites', type="n")
title(main="C", adj = 0.01, line = -2, cex.main=2.5)
ordiellipse(NMDSnew, env.matrix$CARDYEAR, draw="polygon", col="#00c690",kind="sd", conf=0.95, label=FALSE, show.groups = "New22")
ordiellipse(NMDSnew, env.matrix$CARDYEAR, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New21")
points(NMDSnew, display="sites", select=which(env.matrix$CARDYEAR=="New21"),pch=17, col="#009E73")
points(NMDSnew, display="sites", select=which(env.matrix$CARDYEAR=="New22"),pch=18, col="#00c690")
legend(0.905,1.393, title=NULL, pch=c(17,18), col=c("#009E73", "#00c690"), cex=1.2, legend=c("2021 New cards", "2022 New cards"))
dev.off()
###


###All years combined -- not using

#first need to get rid of any rows that sum to zero
#so sum (aka get abundance per row)
insects.abun <- rowSums(insects_all[,6:24])
insects_all$abundance <- insects.abun

#then remove rows = zero
insects_all_nonzero <- insects_all[rowSums(insects_all[26])>0,]
str(insects_all_nonzero)

#Create matrix of environmental variables
env.matrix<-insects_all_nonzero[c(1:5,25)]
#create matrix of community variables
com.matrix<-insects_all_nonzero[c(6:24)]

#ordination by NMDS
NMDS<-metaMDS(com.matrix, distance="bray", k=2, autotransform=TRUE, trymax=100) #stress=.20 (.198)
NMDS

#NMDS visualization with trap types by year
#8x9
plot(NMDS, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#ffba21",kind="sd", conf=0.95, label=FALSE, show.groups = "Old20")
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#00c690",kind="sd", conf=0.95, label=FALSE, show.groups = "New22")
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New21") 
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old21")
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="Old21"),pch=20, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="New21"), pch=17, col="#009E73")
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="Old20"),pch=18, col="#ffba21")
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="New22"), pch=15, col="#00c690")
#add legend
legend(0.97,1.78, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73", "#ffba21","#00c690"), cex=1.2, legend=c("2021 Old cards", "2021 New cards", "2020 Old cards", "2022 New cards"))

#bootstrapping and testing for differences between the groups (cardyear)
fit<-adonis2(com.matrix ~ CARDYEAR, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 0.001 -- sig diff between card types (but that is Old20, Old21, New21, New22)

#pairwise comparison
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library (pairwiseAdonis)
pairwise.adonis(com.matrix, env.matrix$CARDYEAR)
#no diff between cards in 2021 (within year)
#sig diff(0.001) between old and new cards in diff years and same card between years

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARDYEAR))
#P-value = 4.753e-12 -- assumes homogeneity of multivariate dispersion

#NMDS visualization for old and new combined between years
#8x9
plot(NMDS, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old")
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New") 
points(NMDS, display="sites", select=which(env.matrix$CARD=="Old"),pch=19, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$CARD=="New"), pch=17, col="#009E73")
#add legend
legend(1.2,1.78, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.2, legend=c("Old cards", "New cards"))

#bootstrapping and testing for differences between the groups (cards)
fit<-adonis2(com.matrix ~ CARD, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 0.001 -- sig diff between card types 

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARD))
#P-value = 1.185e-06 -- assumes homogeneity of multivariate dispersion

###

#To obtain richness counts
insects.rowsums <- rowSums(insects_all[,6:24]>0)
insects_all$richness <- insects.rowsums

#To obtain abundance counts
insects.abun <- rowSums(insects_all[,6:24])
insects_all$abundance <- insects.abun

#calculate Shannon diversity
diversity <-diversity(insects_all[,6:24])
insects_all$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(insects_all[,6:24]))
insects_all$evenness <- evenness

#calculate taxa abundance
colSums(insects_all[,6:24])

###
#Generalized linear models
library(lme4)
library(lmerTest) #to obtain p values
library (car) #Anova (needed because of negative binomial)  ##if we don't use neg binomial switch to "anova"
library(emmeans)

library (nortest)
library(bbmle)
library(DHARMa)
library(ggplot2)
library(sjPlot)
library (jtools)
library(interactions)

#richness
#AIC 3212
richness.model<-glm(richness ~ CARDYEAR + week + TREAT + offset(TRAPS), data=insects_all, family=poisson)
summary(richness.model)
Anova (richness.model)
AIC(richness.model)
#all sig (CARDYEAR, week, treatment)
#pairwise comparison 
rich.emm<-emmeans(richness.model,pairwise~CARDYEAR)
rich.emm
#results: Sig diff = New21-Old20, New22-Old21, Old20-Old21
#no sig diff = New21-New22,New21-Old21 and New22-Old20
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 
#New22 AB
#Old20 A
#New21 BC
#Old21 C

#

#abundance
##AIC 7164
abundance.model<-glm(abundance ~ CARDYEAR + week + TREAT + offset(TRAPS), data=insects_all, family=poisson)
summary(abundance.model)
Anova(abundance.model)
#all sig (CARDYEAR, week, treatment)
AIC(abundance.model)
#pairwise comparison 
abun.emm<-emmeans(abundance.model,pairwise~CARDYEAR)
abun.emm
#results: sig diff btw all comparisons
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 
#A B C D 

#

#diversity
##AIC 1594
diversity.model<-glm(diversity ~ CARDYEAR + week + TREAT + offset(TRAPS), data=insects_all)
summary(diversity.model)
Anova(diversity.model)
#CARDYEAR no sig, week and treat significant
AIC(diversity.model)
#pairwise comparison 
div.emm<-emmeans(diversity.model,pairwise~CARDYEAR)
div.emm
#results: no sig diff btw any comparisons
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 
#A A A A 

#

#evenness
##AIC Inf
evenness.model<-glm(evenness ~ CARDYEAR + week + TREAT + offset(TRAPS), data=insects_all, family = poisson)
summary(evenness.model)
Anova(evenness.model)
#CARDYEAR and week not sig, treatment sig
AIC(evenness.model)
#pairwise comparison 
even.emm<-emmeans(evenness.model,pairwise~CARDYEAR)
even.emm
#results: no sig diff btw any comparisons
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld 
#A A A A 

##

#CARDYEAR richness by card type
richness.plot<-ggplot(insects_all, aes(x = factor(CARD,level = c("Old","New")), y = richness, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card type and year:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
richness.plot

#CARDYEAR abundance by card type
abundance.plot<-ggplot(insects_all, aes(x = factor(CARD,level = c("Old","New")), y = abundance, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Abundance (log10)")+
  scale_y_continuous(trans="log10")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card type and year:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
abundance.plot

#CARDYEAR diveristy by card type
diversity.plot<-ggplot(insects_all, aes(x = factor(CARD,level = c("Old","New")), y = diversity, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card type and year:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
diversity.plot

#CARDYEAR evenness by card type
evenness.plot<-ggplot(insects_all, aes(x = factor(CARD,level = c("Old","New")), y = evenness, fill=CARDYEAR))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#ffba21","#E69F00","#009E73","#00c690"),name="Card type and year:",
                    breaks=c("Old20", "Old21", "New21", "New22"),
                    labels=c("Old cards 2020", "Old cards 2021", "New cards 2021", "New cards 2022"))
evenness.plot

#
#mush together plots
library(ggpubr) 
allyears_boxplot <- ggarrange(richness.plot, abundance.plot, diversity.plot, evenness.plot, 
                              ncol = 2, nrow = 2,
                              common.legend = TRUE, legend = "bottom")
allyears_boxplot

pdf("allyears_boxplot.pdf", height=8, width=8) #height and width in inches
allyears_boxplot
dev.off()
