setwd("path/to/working/directory")

library(ggplot2)
library(dplyr)
colors_F <- c("#F6D2E0", "#F575A6")
colors_M <- c("#C8E7F5", "#447AC4")

#import data
lysotracker <- read.csv("Lysotracker_quantification_manual.csv")
lysotracker$Age <- recode(lysotracker$Age, "0" = "Week 0", "3" = "Week 3")

#create summaries for bar garph
summary_stats <- lysotracker%>%
  group_by(Genotype,Sex,Age)%>%
  summarise(
    percent_area = mean(Percent_area),
    sd = sd(Percent_area),
    N = n(),
    SE = sd / sqrt(N)
  )

#subset by sex
lysotracker_M <- subset(summary_stats, Sex == "M")
lysotracker_M$Genotype <- factor(lysotracker_M$Genotype,
                                 levels = c("WG2", "KO", "Y160C", "W422X"))
lysotracker_F <- subset(summary_stats, Sex == "F")
lysotracker_F$Genotype <- factor(lysotracker_F$Genotype,
                                 levels = c("WG2", "KO", "Y160C", "W422X"))

#bar graphs
ggplot(data = lysotracker_F, aes(x = Genotype, y = percent_area, fill=Age))+
  geom_bar(stat="summary", color = "black", position=position_dodge(), width = 0.7)+
  labs(x="Genotype", y= "Positive Staining (% Area)")+
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y=c(0,1.2))+
  scale_fill_manual(values = colors_F)+
  theme_classic() + theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 15))+
  geom_errorbar(aes(x=Genotype, ymin = percent_area - SE , 
                    ymax = percent_area + SE),
                width = 0.2, position = position_dodge(0.7))

ggplot(data = lysotracker_M, aes(x = Genotype, y = percent_area, fill=Age))+
  geom_bar(stat="summary", color = "black", position=position_dodge(), width = 0.7)+
  labs(x="Genotype", y= "Positive Staining(% Area)")+
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y=c(0,1.2))+
  scale_fill_manual(values = colors_M)+
  theme_classic() + theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 15))+
  geom_errorbar(aes(x=Genotype, ymin = percent_area - SE , 
                    ymax = percent_area + SE),
                width = 0.2, position = position_dodge(0.7))
