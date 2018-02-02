library(readr)
library(ggplot2)

library(Devtools)
install.github("hadley/ggplot2")

CP_Camas <- read.csv("~/Desktop/Davis/R/Github Repos/Camas_Prairie/Data/CP_Camas_data.csv")

summary(CP_Camas)

str(CP_Camas)

ggplot(data = CP_Camas) + 
  geom_point(mapping = aes(x = NUMBER_CAMAS, y = NUMBER_SCAPES, color = YR_POST_HARVEST, ALPHA = TREATMENT))

ggplot(data = CP_Camas) + 
  geom_point(mapping = aes(x = TREATMENT, y = NUMBER_CAMAS, color = YR_POST_HARVEST))

ggplot(data = CP_Camas) + 
  geom_point(mapping = aes(x = TREATMENT, y = NUMBER_CAMAS, color = YR_POST_HARVEST)) +
  coord_flip()


### trying to add treatment in a difference shading or shape, but not working
ggplot(data = CP_Camas, aes(x = NUMBER_CAMAS, y = NUMBER_SCAPES)) + 
  geom_point(aes(color = TREATMENT)) +
  facet_wrap(~YR_POST_HARVEST)

ggplot(CP_Camas, aes(NUMBER_CAMAS, fill = YR_POST_HARVEST)) +
  geom_histogram(binwidth = 10)

library(magrittr)
library(vegan)
library(tidyverse)


### Using monoMDS to get a global NMDS
### Using year 1 species data to calculate BC matrix
YR1 <- read_csv("~/Desktop/Davis/R/GitHub Repos/Camas_Prairie/Data/YR1_SPP.csv")
YR1_mat <- as.matrix(YR1[,-(1:2)])
YR1_SPP <- vegan::vegdist(YR1_mat, method = "bray")

### Now the actual NMDS:
NMDS <- monoMDS(YR1_SPP, k = 2, 
                model = c("global"),  
                maxit = 5000, 
                weakties = TRUE, 
                stress = 1,
                scaling = TRUE, 
                pc = TRUE,
                smin = 1e-4, sfgrmin = 1e-7,  sratmax=0.99999)

NMDS

### show stress
NMDS$stress

### Seepard diagram
goodness(NMDS)
stressplot(NMDS, p.col = "blue", l.col = "red",  lwd = 2)

### Ignore this. Could plot 2D graphs using scores() and plot(), but ggplot is better. Scores() selects the axes to plot; this is then passed on to plot()
### Look at output of scores(). What is the last line (X54....1)??
# scores(HNMDS, choices = c(1,2,3))
# HNMDSplot <- scores(HNMDS, choices = c(1,2))
# plot(HNMDSplot, type = "p", main = "Bray-Curtis ordination of traps", xlab = "Axis 1", ylab = "Axis 2")

### To create csv file of HNMDS output, use this function, then add columns for cover, moisture (done manually):
### Don't forget that there is no trap 29, so that needs to be fixed manually too
write.csv(NMDS$points, file = "YR1_NMDS.csv")

### Using ggplot to create a nice 2D plot (added columns for plot ID and treatment)
MDSout2D <- read.csv("~/Desktop/Davis/R/GitHub Repos/Camas_Prairie/Data_output/YR1_NMDS.csv")


ggplot(data = MDSout2D, aes(MDS2, MDS1), color = "TRTMT") + 
  geom_point(mapping = aes(x = MDS2, y = MDS1, color = TRTMT), size = 2) +
  scale_shape_manual(values=c(0,17)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  stat_ellipse(geom = "polygon", alpha = 0.05, aes(fill = TRTMT), level = 0.9) 



  labs(geom = "polygon", fill = "Habitat cluster", shape = "Mesic vs. wet", color = "Cover type") +
  guides(fill = guide_legend(order=3), shape = guide_legend(order=1), color = guide_legend(order=2)) +
  annotate(geom="text", x=-1.35, y = -0.35, label = "Open/Ecotone", size = 3.5)+
  annotate(geom="text", x=0.85, y = -0.9, label = "Mesic", size = 3.5)+
  annotate(geom="text", x=0.75, y = 1, label = "Wet", size = 3.5)

### YR2
  ### Using monoMDS to get a global NMDS
  ### Using year 1 species data to calculate BC matrix
  YR2 <- read_csv("~/Desktop/Davis/R/GitHub Repos/Camas_Prairie/Data/YR2_SPP.csv")
  YR2_mat <- as.matrix(YR2[,-(1:2)])
  YR2_SPP <- vegan::vegdist(YR2_mat, method = "bray")
  
  ### Now the actual NMDS:
  NMDS <- monoMDS(YR2_SPP, k = 2, 
                  model = c("global"),  
                  maxit = 5000, 
                  weakties = TRUE, 
                  stress = 1,
                  scaling = TRUE, 
                  pc = TRUE,
                  smin = 1e-4, sfgrmin = 1e-7,  sratmax=0.99999)
  
  NMDS
  
  ### show stress
  NMDS$stress
  
  ### Seepard diagram
  goodness(NMDS)
  stressplot(NMDS, p.col = "blue", l.col = "red",  lwd = 2)
  
  ### Ignore this. Could plot 2D graphs using scores() and plot(), but ggplot is better. Scores() selects the axes to plot; this is then passed on to plot()
  ### Look at output of scores(). What is the last line (X54....1)??
  # scores(HNMDS, choices = c(1,2,3))
  # HNMDSplot <- scores(HNMDS, choices = c(1,2))
  # plot(HNMDSplot, type = "p", main = "Bray-Curtis ordination of traps", xlab = "Axis 1", ylab = "Axis 2")
  
  ### To create csv file of HNMDS output, use this function, then add columns for cover, moisture (done manually):
  ### Don't forget that there is no trap 29, so that needs to be fixed manually too
  write.csv(NMDS$points, file = "YR2_NMDS.csv")
  
  ### Using ggplot to create a nice 2D plot (added columns for plot ID and treatment)
  MDSout2D <- read.csv("~/Desktop/Davis/R/GitHub Repos/Camas_Prairie/Data_output/YR2_NMDS.csv")
  
  
  ggplot(data = MDSout2D, aes(MDS2, MDS1), color = "TRTMT") + 
    geom_point(mapping = aes(x = MDS2, y = MDS1, color = TRTMT), size = 2) +
    scale_shape_manual(values=c(0,17)) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    stat_ellipse(geom = "polygon", alpha = 0.05, aes(fill = TRTMT), level = 0.9) 
  
  
  
  labs(geom = "polygon", fill = "Habitat cluster", shape = "Mesic vs. wet", color = "Cover type") +
    guides(fill = guide_legend(order=3), shape = guide_legend(order=1), color = guide_legend(order=2)) +
    annotate(geom="text", x=-1.35, y = -0.35, label = "Open/Ecotone", size = 3.5)+
    annotate(geom="text", x=0.85, y = -0.9, label = "Mesic", size = 3.5)+
    annotate(geom="text", x=0.75, y = 1, label = "Wet", size = 3.5)

  ### All years
  ### Using monoMDS to get a global NMDS
  ### Using year 1 species data to calculate BC matrix
  ### Deleted trap 10 and some badly id'd species
  ALL <- read_csv("~/Desktop/Davis/R/GitHub Repos/Camas_Prairie/Data/ALL_YRS.csv")
  ALL_mat <- as.matrix(ALL[,(5:65)])
  ALL_SPP <- vegan::vegdist(ALL_mat, method = "bray")

  ### Now the actual NMDS:
  NMDS <- monoMDS(ALL_SPP, k = 2, 
                  model = c("hybrid"),  
                  maxit = 5000, 
                  weakties = TRUE, 
                  stress = 1,
                  scaling = TRUE, 
                  pc = TRUE,
                  smin = 1e-4, sfgrmin = 1e-7,  sratmax=0.99999)
  
  NMDS
  
  ### show stress
  NMDS$stress
  
  ### Seepard diagram
  goodness(NMDS)
  stressplot(NMDS, p.col = "blue", l.col = "red",  lwd = 2)
  
  ### To create csv file of HNMDS output, use this function, then add columns for cover, moisture (done manually):
  write.csv(NMDS$points, file = "ALL_NMDS3.csv")
  
  ### Using ggplot to create a nice 2D plot (added columns for yr post treatment and treatment)
  MDSout2D <- read.csv("~/Desktop/Davis/R/GitHub Repos/Camas_Prairie/Data_output/ALL_NMDS4.csv")
  
  
  ggplot(data = MDSout2D, aes(MDS2, MDS1), color = "TRTMT") + 
    geom_point(mapping = aes(x = MDS2, y = MDS1, color = YR, shape = TRTMT), size = 2) +
    scale_shape_manual(values=c(0,17)) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    stat_ellipse(geom = "polygon", alpha = 0.1, aes(fill = ALL), level = 0.9) 
  

  
  labs(geom = "polygon", fill = "Habitat cluster", shape = "Mesic vs. wet", color = "Cover type") +
    guides(fill = guide_legend(order=3), shape = guide_legend(order=1), color = guide_legend(order=2)) +
    annotate(geom="text", x=-1.35, y = -0.35, label = "Open/Ecotone", size = 3.5)+
    annotate(geom="text", x=0.85, y = -0.9, label = "Mesic", size = 3.5)+
    annotate(geom="text", x=0.75, y = 1, label = "Wet", size = 3.5)

  
### Main PERMANOVA
comp.data <- read.csv("~/Desktop/Davis/R/GitHub Repos/Camas_Prairie/Data/ALL_YRS.csv", header = T)
View(comp.data)
###Pairwise PERMANOVA can be done with package RVAideMemoire
###install.packages("RVAideMemoire")
###library(RVAideMemoire)
###adonis(comp.data[,6:59]~cover,data=comp.data,method="euclidian")
###pairwise.perm.manova(dist(comp.data[,6:59], "euclidean"),comp.data$forest_moisture,nperm=999)

### YR and TRTMT effect?
comp.sub <- subset(comp.data, select = ACMAME:OXACOR)
comp.env <- subset(comp.data, select = c(YR:TRTMT))
attach(comp.env)
adonis2(comp.sub ~ TRTMT * YR)

### Pairwise PERMANOVA (code from https://www.researchgate.net/post/How_can_I_do_PerMANOVA_pairwise_contrasts_in_R)
pairwise.adonis <- function(x,factors, sim.method = 'bray', p.adjust.m ='bonferroni')
{
  library(vegan)
  co = combn(unique(factors),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in% c(co[1,elem],co[2,elem]),] ~ factors[factors %in% c(co[1,elem],co[2,elem])] , method =sim.method);
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
    
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
} 
View(ALL[,66:67])
pairwise.adonis(ALL[,5:65], ALL$YR)
pairwise.adonis(ALL[,5:65], ALL$TRTMT)
pairwise.adonis(ALL[,5:65], ALL$YR_TRTMT)
pairwise.adonis(ALL[,66:67], ALL$YR_TRTMT)

