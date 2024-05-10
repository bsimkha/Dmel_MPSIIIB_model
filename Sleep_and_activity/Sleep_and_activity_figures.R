setwd("path/to/working/directory")

library(ggplot2)
library(dplyr)
colors <- c("#F6D2E0", "#F575A6", "#C8E7F5", "#447AC4")

#Total Night Sleep####
night_sleep <- read.csv("Total_night_sleep.csv")

night_sleep <- subset(night_sleep, Genotype != "WG2")

night_sleep$Legend <- paste(night_sleep$Sex, night_sleep$Age, sep = "_")

Labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

night_sleep$Legned = factor(night_sleep$Legend,
                            levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                            labels = Labels)

night_sleep$Genotype <- factor(night_sleep$Genotype,
                               levels = c("KO", "Y160C", "W422X"))

ggplot(data = night_sleep, aes(x = Genotype, y = Adj_mean*12, fill = Legend)) +
  geom_bar(stat = "summary", color = "black", position = position_dodge()) +
  labs(x = "Genotype", y = "Night  sleep (hrs)\n(Mutant - Control)", fill = NULL) +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y = c(-1, 0.5)) +
  scale_fill_manual(values = colors, labels = Labels) +
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = (Adj_mean - Adj_SE)*12, ymax = (Adj_mean + Adj_SE)*12),
                width = 0.2, position = position_dodge(0.9))


#Nighttime Sleep Bout Count####
nighttime_sleep_count <- read.csv("Nighttime_sleep_bout_count.csv")

nighttime_sleep_count <- subset(nighttime_sleep_count, Genotype != "WG2")

nighttime_sleep_count$Legend <- paste(nighttime_sleep_count$Sex, nighttime_sleep_count$Age, sep = "_")

Labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

nighttime_sleep_count$Legned = factor(nighttime_sleep_count$Legend,
                                      levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                                      labels = Labels)

nighttime_sleep_count$Genotype <- factor(nighttime_sleep_count$Genotype,
                                         levels = c("KO", "Y160C", "W422X"))

ggplot(data = nighttime_sleep_count, aes(x = Genotype, y = Adj_mean, fill = Legend)) +
  geom_bar(stat = "summary", color = "black", position = position_dodge()) +
  labs(x = "Genotype", y = "Nighttime sleep bout count\n(Mutant - Control)", fill = NULL) +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y = c(-2, 0.5)) +
  scale_fill_manual(values = colors, labels = Labels) +
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = (Adj_mean - Adj_SE), ymax = (Adj_mean + Adj_SE)),
                width = 0.2, position = position_dodge(0.9))


#Total Locomotor Activity####
total_locomotor <- read.csv("Total_locomotor_activity.csv")

total_locomotor <- subset(total_locomotor, Genotype != "WG2")

total_locomotor$Legend <- paste(total_locomotor$Sex, total_locomotor$Age, sep = "_")

Labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

total_locomotor$Legned = factor(total_locomotor$Legend,
                                levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                                labels = Labels)

total_locomotor$Genotype <- factor(total_locomotor$Genotype,
                                   levels = c("KO", "Y160C", "W422X"))

ggplot(data = total_locomotor, aes(x = Genotype, y = Adj_mean, fill = Legend)) +
  geom_bar(stat = "summary", color = "black", position = position_dodge()) +
  labs(x = "Genotype", y = "Total locomotor activity (counts)\n(Mutant - Control)", fill = NULL) +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y = c(-2, 0.5)) +
  scale_fill_manual(values = colors, labels = Labels) +
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = (Adj_mean - Adj_SE), ymax = (Adj_mean + Adj_SE)),
                width = 0.2, position = position_dodge(0.9))



#Total Activity Bout Length####
total_activity_length <- read.csv("Total_activity_bout_length.csv")

total_activity_length <- subset(total_activity_length, Genotype != "WG2")

total_activity_length$Legend <- paste(total_activity_length$Sex, total_activity_length$Age, sep = "_")

Labels <- c("F, Week 0", "F, Week 3", "M, Week 0", "M, Week 3")

total_activity_length$Legned = factor(total_activity_length$Legend,
                                      levels = c("F_W0", "F_W3", "M_W0", "M_W3"),
                                      labels = Labels)

total_activity_length$Genotype <- factor(total_activity_length$Genotype,
                                         levels = c("KO", "Y160C", "W422X"))

ggplot(data = total_activity_length, aes(x = Genotype, y = Adj_mean, fill = Legend)) +
  geom_bar(stat = "summary", color = "black", position = position_dodge()) +
  labs(x = "Genotype", y = "Total activity bout length (min)\n(Mutant - Control)", fill = NULL) +
  scale_x_discrete(labels = c(expression(italic("Naglu")^"KO"),
                              expression(italic("Naglu")^"Y160C"),
                              expression(italic("Naglu")^"W422X"))) +
  expand_limits(y = c(-2, 0.5)) +
  scale_fill_manual(values = colors, labels = Labels) +
  theme_classic() +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = (Adj_mean - Adj_SE), ymax = (Adj_mean + Adj_SE)),
                width = 0.2, position = position_dodge(0.9))

