setwd("path/to/working/directory")
library(dplyr)
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("gaospecial/ggVennDiagram")
library(ggvenn)
library(ggVennDiagram)
library(VennDiagram)


#import dataset
ls_means <- read.csv("LSMEANS_MPSIIIB_GENOTYPE.csv")

#subset by genotype
WG2 <- subset(ls_means, Genotype == "WG2")
KO <- subset(ls_means, Genotype == "KO")
Y160C <- subset(ls_means, Genotype == "Y160C")
W422X <- subset(ls_means, Genotype == "W422X")

#check to see the genes are ordered the same for all subsetted dataframes
table(WG2[1] == KO[1])
table(WG2[1]==Y160C[1])
table(WG2[1]==W422X[1])

#calculate the fold change
KO$FC <- KO$norm_countsLSMean/WG2$norm_countsLSMean
Y160C$FC <- Y160C$norm_countsLSMean/WG2$norm_countsLSMean
W422X$FC <- W422X$norm_countsLSMean/WG2$norm_countsLSMean

#calculate log2(FC)
KO$log2_FC <- log2(KO$FC)
Y160C$log2_FC <- log2(Y160C$FC)
W422X$log2_FC <- log2(W422X$FC)

#import datasets with significant genes
KO_genes <- read.csv("KO_vs_WG2_sig_genes_by_pvalue.csv")
Y160C_genes <- read.csv("Y160C_vs_WG2_sig_genes_by_pvalue.csv")
W422X_genes <- read.csv("W422X_vs_WG2_sig_genes_by_pvalue.csv")

#Add FC values to the genes in these list
KO_genes <- KO_genes %>%
  left_join(KO %>% select(Flybase_ID, log2_FC), by = "Flybase_ID")
Y160C_genes <- Y160C_genes %>%
  left_join(Y160C %>% select(Flybase_ID, log2_FC), by = "Flybase_ID")
W422X_genes <- W422X_genes %>%
  left_join(W422X %>% select(Flybase_ID, log2_FC), by = "Flybase_ID")

#Label for Up or Down regulation
KO_genes$Regulation <- ifelse(KO_genes$log2_FC > 0, "Up", "Down")
Y160C_genes$Regulation <- ifelse(Y160C_genes$log2_FC > 0, "Up", "Down")
W422X_genes$Regulation <- ifelse(W422X_genes$log2_FC > 0, "Up", "Down")

#Write export files
write.csv(KO_genes, file = "KO_genes_log_FC.csv")
write.csv(Y160C_genes, file = "Y160C_genes_log_FC.csv")
write.csv(W422X_genes, file = "W422X_genes_log_FC.csv")

#Subsetting by Up and Down regulation
KO_genes_up <- subset(KO_genes, Regulation == "Up")
KO_genes_down <- subset(KO_genes, Regulation == "Down")

Y160C_genes_up <- subset(Y160C_genes, Regulation == "Up")
Y160C_genes_down <- subset(Y160C_genes, Regulation == "Down")

W422X_genes_up <- subset(W422X_genes, Regulation == "Up")
W422X_genes_down <- subset(W422X_genes, Regulation == "Down")

