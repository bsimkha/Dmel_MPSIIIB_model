setwd("path/to/working/directory")


#import the list of significant genes from overall ANOVA
sig_genes_df <- read.csv("Significant_genes_by_Genotype.csv")
sig_genes <- sig_genes_df$Flybase_ID #vector with significant genes list

#KO
#import ANOVA dataset
KO_anova <- read.csv("MODELANOVA_MPSIIIB_KO.csv") #MODELANOVA_MPSIIIB_KO is an output file from SAS
KO_anova <- subset(KO_anova, HypothesisType == 3)

#subset the ANOVA dataset to have only the significant genes
KO_anova <- KO_anova[KO_anova$Flybase_ID %in% sig_genes, ]

#separate by model terms
KO_genotype <- subset(KO_anova, Source == "Genotype")

#create a new dataframe with just gene name, p value and FDR
ID_pval <- subset(KO_genotype, select = c("Flybase_ID", "ProbF", "FDR_Genotype")) #here, FDR is not relevant

#filter for significant genes p < 0.05
ID_pval0.05 <- subset(ID_pval, (format(ProbF, scientific = FALSE)) < 0.05)

#write output file with list of significant genes
write.csv(ID_pval0.05, file = "KO_vs_WG2_sig_genes_by_pvalue.csv")


#W422X
#import ANOVA dataset
W422X_anova <- read.csv("MODELANOVA_MPSIIIB_W422X.csv")
W422X_anova <- subset(W422X_anova, HypothesisType == 3)

#subset the ANOVA dataset to have only the significant genes
W422X_anova <- W422X_anova[W422X_anova$Flybase_ID %in% sig_genes, ]

#separate by model terms
W422X_genotype <- subset(W422X_anova, Source == "Genotype")

#create a new dataframe with just gene name, p value and FDR
ID_pval <- subset(W422X_genotype, select = c("Flybase_ID", "ProbF", "FDR_Genotype"))

#filter for significant genes p < 0.05
ID_pval0.05 <- subset(ID_pval, (format(ProbF, scientific = FALSE)) < 0.05)

#write output file with list of significant genes
write.csv(ID_pval0.05, file = "W422X_vs_WG2_sig_genes_by_pvalue.csv")

#Y160C
#import ANOVA dataset
Y160C_anova <- read.csv("MODELANOVA_MPSIIIB_Y160C.csv")
Y160C_anova <- subset(Y160C_anova, HypothesisType == 3)

#subset the ANOVA dataset to have only the significant genes
Y160C_anova <- Y160C_anova[Y160C_anova$Flybase_ID %in% sig_genes, ]

#separate by model terms
Y160C_genotype <- subset(Y160C_anova, Source == "Genotype")

#create a new dataframe with just gene name, p value and FDR
ID_pval <- subset(Y160C_genotype, select = c("Flybase_ID", "ProbF", "FDR_Genotype"))

#filter for significant genes p < 0.05
ID_pval0.05 <- subset(ID_pval, (format(ProbF, scientific = FALSE)) < 0.05)

#write output file with list of significant genes
write.csv(ID_pval0.05, file = "Y160C_vs_WG2_sig_genes_by_pvalue.csv")
