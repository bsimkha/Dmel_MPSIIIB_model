setwd("path/to/working/directory")

library(ggplot2)
library(dplyr)
colors_F <- c("#F6D2E0", "#F575A6")
colors_M <- c("#C8E7F5", "#447AC4")

#import dataset
enzyme_activity <- read.csv("Enzyme_activity_measurements.csv")
enzyme_activity$Age <- recode(enzyme_activity$Age, "W0" = "Week 0", "W3" = "Week 3")

#Summarize data for bargraphs
summary_stats <- enzyme_activity%>%
  group_by(Genotype,Sex,Age)%>%
  summarise(
    nmol_per_hr_per_mg = mean(nmol_per_hr_per_mg),
    SD = sd(nmol_per_hr_per_mg),
    N = n(),
    SE = SD / sqrt(N)
  )

summary_stats$Age <- recode(summary_stats$Age, "W0" = "Week 0", "W3" = "Week 3")


#Subset by sex
enzyme_activity_M <- subset(summary_stats, Sex == "M")
enzyme_activity_M$Genotype <- factor(enzyme_activity_M$Genotype,
                                     levels = c("WG2", "KO", "Y160C", "W422X"))
enzyme_activity_F <- subset(summary_stats, Sex == "F")
enzyme_activity_F$Genotype <- factor(enzyme_activity_F$Genotype,
                                     levels = c("WG2", "KO", "Y160C", "W422X"))

#Bargraphs
#females
ggplot(data = enzyme_activity_F, aes(x = Genotype, y = nmol_per_hr_per_mg, fill=Age))+
  geom_bar(stat="summary", color = "black", position=position_dodge(), width = 0.7)+
  labs(x="Genotype", y= "Enzyme activity\n(nmol/hr/mg)")+
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y=c(0,80))+
  scale_fill_manual(values = colors_F)+
  theme_classic() + theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 15))+
  geom_errorbar(aes(x=Genotype, ymin = nmol_per_hr_per_mg - SE , 
                    ymax = nmol_per_hr_per_mg + SE),
                width = 0.2, position = position_dodge(0.7))

#males
ggplot(data = enzyme_activity_M, aes(x = Genotype, y = nmol_per_hr_per_mg, fill=Age))+
  geom_bar(stat="summary", color = "black", position=position_dodge(), width = 0.7)+
  labs(x="Genotype", y= "Enzyme activity\n(nmol/hr/mg)")+
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y=c(0,80))+
  scale_fill_manual(values = colors_M)+
  theme_classic() + theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 15))+
  geom_errorbar(aes(x=Genotype, ymin = nmol_per_hr_per_mg - SE , 
                    ymax = nmol_per_hr_per_mg + SE),
                width = 0.2, position = position_dodge(0.7))