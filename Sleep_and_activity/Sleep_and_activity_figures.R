setwd("path/to/working/directory")

library(ggplot2)
library(dplyr)
colors <- c("#F6D2E0", "#F575A6", "#C8E7F5", "#447AC4")


#Total_Activity_Bout_Length####
total_activity_length <- read.csv("Total_activity_bout_length.csv")

total_activity_length$Legend <- paste(total_activity_length$Sex, total_activity_length$Age, sep = "_")

labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

total_activity_length$Legned = factor(total_activity_length$Legend,
                                   levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                                   labels = labels)

total_activity_length$Genotype = factor(total_activity_length$Genotype,
                                    levels = c("WG2","KO","Y160C","W422X"))

ggplot(data = total_activity_length, aes(x = Genotype, y = Bout_Length, fill = Legend)) + 
  geom_boxplot(width = 0.5) +
  labs(x = "Genotype", y = "Total Activity Bout Length (min)") +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +  # Adjust x-axis labels
  scale_y_discrete(limit = c(0,20,40,60,80))+
  expand_limits(y=c(0,80))+
  scale_fill_manual(values = colors, labels = labels) +  # Adjust legend labels
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15))


#Total_Locomotor_activity####
total_locomotor_activity <- read.csv("Total_locomotor_activity.csv")

total_locomotor_activity$Legend <- paste(total_locomotor_activity$Sex, total_locomotor_activity$Age, sep = "_")

labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

total_locomotor_activity$Legned = factor(total_locomotor_activity$Legend,
                                      levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                                      labels = labels)

total_locomotor_activity$Genotype = factor(total_locomotor_activity$Genotype,
                                        levels = c("WG2","KO","Y160C","W422X"))

ggplot(data = total_locomotor_activity, aes(x = Genotype, y = Activity, fill = Legend)) + 
  geom_boxplot(width = 0.5) +
  labs(x = "Genotype", y = "Total Locomotor Activity (counts)") +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +  # Adjust x-axis labels
  scale_y_discrete(limit = c(0,1000, 2000, 3000))+
  expand_limits(y=c(0,3000))+
  scale_fill_manual(values = colors, labels = labels) +  # Adjust legend labels
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15))

#Total_night_sleep####
total_night_sleep <- read.csv("Total_night_sleep.csv")

total_night_sleep$Legend <- paste(total_night_sleep$Sex, total_night_sleep$Age, sep = "_")

labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

total_night_sleep$Legned = factor(total_night_sleep$Legend,
                                         levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                                         labels = labels)

total_night_sleep$Genotype = factor(total_night_sleep$Genotype,
                                           levels = c("WG2","KO","Y160C","W422X"))

ggplot(data = total_night_sleep, aes(x = Genotype, y = mean_sleep_per_ind*12, fill = Legend)) + 
  geom_boxplot(width = 0.5) +
  labs(x = "Genotype", y = "Total Night Sleep (h)") +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +  # Adjust x-axis labels
  expand_limits(y=c(0,12))+
  scale_fill_manual(values = colors, labels = labels) +  # Adjust legend labels
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15))

#Nighttime_sleep_bout_count####
night_sleep_count <- read.csv("Nighttime_sleep_bout_counts.csv")

night_sleep_count$Legend <- paste(night_sleep_count$Sex, night_sleep_count$Age, sep = "_")

labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

night_sleep_count$Legned = factor(night_sleep_count$Legend,
                                  levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                                  labels = labels)

night_sleep_count$Genotype = factor(night_sleep_count$Genotype,
                                    levels = c("WG2","KO","Y160C","W422X"))

ggplot(data = night_sleep_count, aes(x = Genotype, y = Bout_Count, fill = Legend)) + 
  geom_boxplot(width = 0.5) +
  labs(x = "Genotype", y = "Nighttime Sleep Bout Count") +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"+"),
                              expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +  # Adjust x-axis labels
  #scale_y_discrete(limit = c(0,1000, 2000, 3000))+
  expand_limits(y=c(0,25))+
  scale_fill_manual(values = colors, labels = labels) +  # Adjust legend labels
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15))
