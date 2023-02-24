##2020

#bring in data
insects20 <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2020_LTER_for_analyses.csv", na.strings = NULL)

#change week to characters
insects20$week <- as.character(insects20$week)
#change CARD, TREAT, and STATION to factor
insects20$CARD <- as.factor(insects20$CARD)
insects20$TREAT <- as.factor(insects20$TREAT)
insects20$STATION <- as.factor(insects20$STATION)

str(insects20)
summary(insects20)

###

##2021

#bring in data
insects21 <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2021_LTER_for_analyses.csv", na.strings = NULL)

#change week to characters
insects21$week <- as.character(insects21$week)
#change CARD, TREAT, and STATION to factor
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
NMDS<-metaMDS(com.matrix, distance="bray", k=2, autotransform=TRUE, trymax=300) #stress=.24 
#NMDS<-metaMDS(com.matrix, distance="bray", k=2, autotransform=FALSE, trymax=100) #stress=.06 -- no convergence
NMDS

#NMDS visualization
plot(NMDS, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old21")
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New21") 
#display ground trap data as solid shapes - pitfall=circle, ramp trap=square, jar=triangle, flying trap as triangle outline
points(NMDS, display="sites", select=which(env.matrix$CARD=="Old21"),pch=19, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$CARD=="New21"), pch=17, col="#009E73")
#add legend
legend(1.315,1.684, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.2, legend=c("Old cards", "New cards"))

#bootstrapping and testing for differences between the groups (cards)
fit<-adonis2(com.matrix ~ CARD, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 0.3 -- no sig diff between card types

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARD))
#P-value = 0.6973 -- cannot assume homogeneity of multivariate dispersion

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

#calculate Evenness
evenness <-diversity/log(specnumber(insects21[,5:29]))
insects21$evenness <- evenness

###
#Generalized linear models
library(lme4)
library(lmerTest) #to obtain p values
library (car) #Anova (needed because of negative binomial)  ##if we don't use neg binomial switch to "anova"
library (nortest)
library(bbmle)
library(DHARMa)
library(ggplot2)
library(sjPlot)
library (jtools)
library(interactions)
library(emmeans)

#richness
#AIC 1183
richness.model<-glm(richness ~ CARD + week + TREAT + offset(TRAPS), data=insects)
summary(richness.model)
Anova (richness.model)
AIC(richness.model)
#pairwise comparison 
rich.emm<-emmeans(richness.model,pairwise~CARD)
rich.emm
#results: no sig diff btw cards (p=0.8897)

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
##AIC 3574
abundance.model<-glm(abundance ~ CARD + week + TREAT + offset(TRAPS), data=insects, family = negative.binomial (4))
#abundance.model<-glm(abundance ~ CARD + week + TREAT + offset(TRAPS), data=insects)  #does not meet normality assumptions
summary(abundance.model)
Anova(abundance.model)
AIC(abundance.model)
#pairwise comparison 
abun.emm<-emmeans(abundance.model,pairwise~CARD)
abun.emm
#results: sig diff btw cards (p = 0.0076)

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
##AIC 151
diversity.model<-glm(diversity ~ CARD + week + TREAT + offset(TRAPS), data=insects)
#diversity.model<-lmer(diversity ~ CARD + week + (1 | TREAT:STATION), data=insects) #no sig diff btw cards #AIC = 116
summary(diversity.model)
Anova(diversity.model)
AIC(diversity.model)
#pairwise comparison 
div.emm<-emmeans(diversity.model,pairwise~CARD)
div.emm
#results: no sig diff btw cards (p = 0.2475)

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

#

#evenness
##AIC -52
evenness.model<-glm(evenness ~ CARD + week + TREAT + offset(TRAPS), data=insects, family = poisson)
summary(evenness.model)
Anova(evenness.model)
AIC(evenness.model)
#pairwise comparison 
even.emm<-emmeans(evenness.model,pairwise~CARD)
even.emm
#results: no sig diff btw cards (p = 0.4079)

#check assumptions
dotchart(insects$evenness, main = "evenness", group = insects$CARD) # way to visualize outliers

with(insects, ad.test(evenness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 0.02624

with(insects, bartlett.test(evenness ~ CARD)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.8218

plot(evenness.model) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(evenness.model))
qqline(resid(evenness.model))

plot(simulateResiduals(evenness.model)) # another way to check for normality and homogeneity of variance
#KS test: p = SIG
#dispersion test: p = 
#outlier test: p = SIG
#no significant problems detected 

densityPlot(rstudent(evenness.model)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(evenness.model)
influenceIndexPlot(evenness.model, vars = c("Cook"), id = list(n = 3))

###

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
  labs(x = "Samples", y = "Richness", colour = "CARD", shape = "CARD")
accum

pdf("accumulation curve with pooled data.pdf", height=6, width=8) #height and width in inches
accum
dev.off()

###

##2022

#bring in data
insects22 <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/2022_LTER_for_analyses.csv", na.strings = NULL)

#change week to characters
insects22$week <- as.character(insects22$week)
#change CARD, TREAT, and STATION to factor
insects22$CARD <- as.factor(insects22$CARD)
insects22$TREAT <- as.factor(insects22$TREAT)
insects22$STATION <- as.factor(insects22$STATION)

str(insects22)
summary(insects22)


###

##all years

#bring in data
insects_all <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/all%20years_LTER_for_analyses.csv", na.strings = NULL)

#change week to characters
insects_all$week <- as.character(insects_all$week)
#change CARD, TREAT, and STATION to factor
insects_all$CARDYEAR <- as.factor(insects_all$CARDYEAR)
insects_all$CARD <- as.factor(insects_all$CARD)
insects_all$TREAT <- as.factor(insects_all$TREAT)
insects_all$STATION <- as.factor(insects_all$STATION)

str(insects_all)
summary(insects_all)

###

#NMDS of insect community between card types
library (vegan)

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
plot(NMDS, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#ffba21",kind="sd", conf=0.95, label=FALSE, show.groups = "Old20")
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#00c690",kind="sd", conf=0.95, label=FALSE, show.groups = "New22") 
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old21")
ordiellipse(NMDS, env.matrix$CARDYEAR, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New21") 
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="Old21"),pch=20, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="New21"), pch=17, col="#009E73")
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="Old20"),pch=18, col="#ffba21")
points(NMDS, display="sites", select=which(env.matrix$CARDYEAR=="New22"), pch=15, col="#00c690")
#add legend
legend(1.315,1.684, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73", "#ffba21","#00c690"), cex=1.2, legend=c("2021 Old cards", "2021 New cards", "2020 Old cards", "2022 New cards"))

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
plot(NMDS, disp='sites', type="n")
title(main="", adj = 0.01, line = -2, cex.main=2.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Old")
ordiellipse(NMDS, env.matrix$CARD, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "New") 
points(NMDS, display="sites", select=which(env.matrix$CARD=="Old"),pch=19, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$CARD=="New"), pch=17, col="#009E73")
#add legend
legend(1.315,1.684, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.2, legend=c("Old cards", "New cards"))

#bootstrapping and testing for differences between the groups (cards)
fit<-adonis2(com.matrix ~ CARD, data = env.matrix, permutations = 999, method="bray")
fit
#P-value = 0.001 -- sig diff between card types 

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$CARD))
#P-value = 1.185e-06 -- assumes homogeneity of multivariate dispersion
