setwd("path/to/working/directory")

#load packages
library(ggplot2)
library(dplyr)

colors <- c("#F6D2E0", "#C8E7F5")

#Importing data set
lysotracker_VNC <- read.csv("Lysotracker_quantification_VNC_manual.csv")

#summary data set for bar graphs
summary_stats <- lysotracker_VNC%>%
  group_by(Genotype,Sex)%>%
  summarise(
    percent_area = mean(Percent_area),
    sd = sd(Percent_area),
    N = n(),
    SE = sd / sqrt(N)
  )

summary_stats$Sex <- factor(summary_stats$Sex,
                            levels = c("F", "M"),
                            labels = c("Female", "Male"))
summary_stats$Genotype <- factor(summary_stats$Genotype,
                                 levels = c("WG2", "KO", "Y160C", "W422X"))


#make graphs
ggplot(data = summary_stats, aes(x = Genotype, y = percent_area, fill=Sex))+
  geom_bar(stat="summary", color = "black", position=position_dodge(), width = 0.7)+
  labs(x="Genotype", y= "Positive Staining (% Area)")+
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y=c(0,1.2))+
  scale_fill_manual(values = colors)+
  theme_classic() + theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 15))+
  geom_errorbar(aes(x=Genotype, ymin = percent_area - SE , 
                    ymax = percent_area + SE),
                width = 0.2, position = position_dodge(0.7))
