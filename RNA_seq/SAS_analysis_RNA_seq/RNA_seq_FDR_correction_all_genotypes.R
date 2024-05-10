setwd("path/to/working/directory")

#import data and select for only hypothesis type 3
model_anova <- read.csv("MODELANOVA_MPSIIIB.csv") #MODELANOVA_MPSIIIB is an output file from SAS
model_anova_T3 <- subset(model_anova, HypothesisType == 3)

#sub-setting the data set for each factor
Model_genotype <- subset(model_anova_T3, Source == "Genotype")
Model_sex <- subset(model_anova_T3, Source == "Sex")
Model_age <- subset(model_anova_T3, Source == "Age")
Model_gxs <- subset(model_anova_T3, Source == "Genotype*Sex")
Model_gxa <- subset(model_anova_T3, Source == "Genotype*Age")
Model_sxa <- subset(model_anova_T3, Source == "Sex*Age")
Model_gxsxa <- subset(model_anova_T3, Source == "Genotype*Sex*Age")

#apply for FDR. This creates vectors with FDR adjusted p-values
FDR_genotype <- p.adjust(Model_genotype$ProbF, method = "fdr")
FDR_sex <- p.adjust(Model_sex$ProbF, method = "fdr")
FDR_age <- p.adjust(Model_age$ProbF, method = "fdr")
FDR_gxs <- p.adjust(Model_gxs$ProbF, method = "fdr")
FDR_gxa <- p.adjust(Model_gxa$ProbF, method = "fdr")
FDR_sxa <- p.adjust(Model_sxa$ProbF, method = "fdr")
FDR_gxsxa <- p.adjust(Model_gxsxa$ProbF, method = "fdr")

#Compare datasets to make sure the order of genes is the same
table(Model_genotype$Flybase_ID == Model_age$Flybase_ID)
table(Model_genotype$Flybase_ID == Model_sex$Flybase_ID)
table(Model_genotype$Flybase_ID == Model_gxs$Flybase_ID)
table(Model_genotype$Flybase_ID == Model_gxa$Flybase_ID)
table(Model_genotype$Flybase_ID == Model_sxa$Flybase_ID)
table(Model_genotype$Flybase_ID == Model_gxsxa$Flybase_ID)

#create vector list with flybase IDs
FB_ID <- Model_genotype$Flybase_ID

#create a dataframe with flybase ID and all original and adjusted p-values
ID_pval <- cbind(FB_ID, Model_genotype$ProbF, FDR_genotype, Model_sex$ProbF, FDR_sex, 
                 Model_age$ProbF, FDR_age, Model_gxs$ProbF, FDR_gxs, Model_gxa$ProbF, FDR_gxa,
                 Model_sxa$ProbF, FDR_sxa, Model_gxsxa$ProbF, FDR_gxsxa)
colnames(ID_pval) <- c("Flybase_ID", "Genotype_ProbF", "Genotype_FDR", "Sex_ProbF", "Sex_FDR",
                       "Age_ProbF", "Age_FDR","GxS_ProbF", "GxS_FDR", "GxA_ProbF", "GxA_FDR",
                       "SxA_ProbF", "SxA_FDR","GxSxA_ProbF", "GxSxA_FDR")
ID_pval <- as.data.frame(ID_pval)
write.csv(ID_pval, file = "4) FDR correction/All Genotypes/All_genes_FDR_adjusted.csv")


#produce table with gene counts with FDR < 0.05 for each model term
FDR_sig_genes_0.05 <- c(
  sum(as.numeric(format(ID_pval$Genotype_FDR, scientific = FALSE)) < 0.05),
  sum(as.numeric(format(ID_pval$Sex_FDR, scientific = FALSE)) < 0.05),
  sum(as.numeric(format(ID_pval$Age_FDR, scientific = FALSE)) < 0.05),
  sum(as.numeric(format(ID_pval$GxS_FDR, scientific = FALSE)) < 0.05),
  sum(as.numeric(format(ID_pval$GxA_FDR, scientific = FALSE)) < 0.05),
  sum(as.numeric(format(ID_pval$SxA_FDR, scientific = FALSE)) < 0.05),
  sum(as.numeric(format(ID_pval$GxSxA_FDR, scientific = FALSE)) < 0.05)
)
Model_items <- c("Genotype", "Sex","Age","GxS","GxA","SxA","GxSxA")

FDR_table_0.05 <- cbind(Model_items, FDR_sig_genes_0.05)
write.csv(FDR_table_0.05, file = "Number_of_sig_genes_FDR0.05.csv")

#Create a list ofsignificant genes for each model term
sig_genes_Genotype <- subset(ID_pval, as.numeric(format(Genotype_FDR, scientific = FALSE)) < 0.05, 
                             select = c("Flybase_ID", "Genotype_FDR"))
sig_genes_Sex <- subset(ID_pval, as.numeric(format(Sex_FDR, scientific = FALSE)) < 0.05, 
                             select = c("Flybase_ID", "Sex_FDR"))
sig_genes_Age <- subset(ID_pval, as.numeric(format(Age_FDR, scientific = FALSE)) < 0.05, 
                        select = c("Flybase_ID", "Age_FDR"))
sig_genes_SxA <- subset(ID_pval, as.numeric(format(SxA_FDR, scientific = FALSE)) < 0.05, 
                        select = c("Flybase_ID", "SxA_FDR"))
write.csv(sig_genes_Genotype, file = "Significant_genes_by_Genotype.csv")
write.csv(sig_genes_Sex, file = "Significant_genes_by_Sex.csv")
write.csv(sig_genes_Age, file = "Significant_genes_by_Age.csv")
write.csv(sig_genes_SxA, file = "Significant_genes_by_SxA.csv")



#Higher FDR values for GO enrichment since no genes enriched for 0.075
FDR_sig_genes_0.075 <- c(
  sum(as.numeric(format(ID_pval$Genotype_FDR, scientific = FALSE)) < 0.075),
  sum(as.numeric(format(ID_pval$Sex_FDR, scientific = FALSE)) < 0.075),
  sum(as.numeric(format(ID_pval$Age_FDR, scientific = FALSE)) < 0.075),
  sum(as.numeric(format(ID_pval$GxS_FDR, scientific = FALSE)) < 0.075),
  sum(as.numeric(format(ID_pval$GxA_FDR, scientific = FALSE)) < 0.075),
  sum(as.numeric(format(ID_pval$SxA_FDR, scientific = FALSE)) < 0.075),
  sum(as.numeric(format(ID_pval$GxSxA_FDR, scientific = FALSE)) < 0.075)
)

FDR_table_0.075 <- cbind(Model_items, FDR_sig_genes_0.075)
write.csv(FDR_table_0.075, file = "Number_of_sig_genes_FDR0.075.csv")

#FDR <0.1
FDR_sig_genes_0.1 <- c(
  sum(as.numeric(format(ID_pval$Genotype_FDR, scientific = FALSE)) < 0.1),
  sum(as.numeric(format(ID_pval$Sex_FDR, scientific = FALSE)) < 0.1),
  sum(as.numeric(format(ID_pval$Age_FDR, scientific = FALSE)) < 0.1),
  sum(as.numeric(format(ID_pval$GxS_FDR, scientific = FALSE)) < 0.1),
  sum(as.numeric(format(ID_pval$GxA_FDR, scientific = FALSE)) < 0.1),
  sum(as.numeric(format(ID_pval$SxA_FDR, scientific = FALSE)) < 0.1),
  sum(as.numeric(format(ID_pval$GxSxA_FDR, scientific = FALSE)) < 0.1)
)

FDR_table_0.1 <- cbind(Model_items, FDR_sig_genes_0.1)
write.csv(FDR_table_0.1, file = "Number_of_sig_genes_FDR0.1.csv")



