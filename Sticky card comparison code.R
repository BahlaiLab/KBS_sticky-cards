#bring in data sets from github

new <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/New_Insect%20ID%20-%20sticky%20card%20comparison%20-%20as%20of%203.22.22.csv",na.strings = NULL)
old <- read.csv("https://raw.githubusercontent.com/BahlaiLab/KBS_sticky-cards/main/Old_Insect%20ID%20-%20sticky%20card%20comparison%20-%20as%20of%203.22.22.csv",na.strings = NULL)


taxa <- read.csv("")

#combine data tables 
library (plyr)
insects <- rbind.fill (new, old)

#remove rows containing all zeros
library(dplyr)

insects <- insects %>% 
  filter_all(any_vars(. != 0))

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

