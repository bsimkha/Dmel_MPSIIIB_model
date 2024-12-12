setwd("path/to/working/directory")

#load packages
library(ggplot2)
library(dplyr)

colors <- c("#F6D2E0", "#C8E7F5")

#Importing data set
HS_measurement <- read.csv("HS_levels_week0.csv")

#summary data set for bar graphs
summary_stats <- HS_measurement%>%
  group_by(Genotype,Sex)%>%
  summarise(
    HS_Levels = mean(HS_levels),
    sd = sd(HS_levels),
    N = n(),
    SE = sd / sqrt(N)
  )

summary_stats$Sex <- factor(summary_stats$Sex,
                            levels = c("F", "M"),
                            labels = c("Female", "Male"))
summary_stats$Genotype <- factor(summary_stats$Genotype,
                                 levels = c("WG2", "KO", "Y160C", "W422X"))


#make graphs
ggplot(data = summary_stats, aes(x = Genotype, y = HS_Levels, fill=Sex))+
  geom_bar(stat="summary", color = "black", position=position_dodge(), width = 0.7)+
  labs(x="Genotype", y= "Total HS levels\n(µg HS / g tissue)")+
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y=c(0,60))+
  scale_fill_manual(values = colors)+
  theme_classic() + theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 15))+
  geom_errorbar(aes(x=Genotype, ymin = HS_Levels - SE , 
                    ymax = HS_Levels + SE),
                width = 0.2, position = position_dodge(0.7))
