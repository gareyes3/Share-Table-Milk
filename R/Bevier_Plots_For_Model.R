library(tidyverse)
library(reshape2)


Bevier_Trial_For_Model<-read.csv("Bevier_Trial_Temps_formodel.csv")
Bevier_Trial_For_Model_Melted<-melt(Bevier_Trial_For_Model,id.vars = "Time")

Bevier_Trial_For_Model_Melted %>% 
  ggplot(aes(x = Time , y  = value))+
  geom_point(color = "skyblue")+
  geom_line()+
  facet_wrap(~variable ,nrow = 5)+
  theme_bw()+
  labs(y = "Temperature (°C)", x = "Time (min)")

