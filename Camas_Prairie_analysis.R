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

