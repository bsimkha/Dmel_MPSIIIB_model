setwd("path/to/working/directory")
library(ggplot2)
library(dplyr)
library(edgeR)
library(tidyr)

#importing raw data
raw_data <- read.table("combined_counts.txt", header = T)

#calculating median for each transcript's expression
median <- apply(raw_data[, 3:ncol(raw_data)], MARGIN = 1, median)
raw_data$Median <- median

#subsetting to get better visualization
raw_data_modified <- subset(raw_data, median <= 12500)

ggplot(raw_data_modified, aes(x = log(Median))) +
  geom_histogram(binwidth = 0.1)+
  coord_cartesian(ylim = c(0, 10))
#or
hist(log(raw_data_modified$Median), breaks = 100, xlim = c(0,10))


#put the median cutoff of 2 for filtering
median_filt <- subset(raw_data, log(Median) >= 2)
#check to see filtering worked 
hist(log(median_filt$Median), breaks = 100, xlim = c(0,10))

#applying zero filter, here 36 comes from 0.75*48, i.e. 25% of sample have non-zero values for that gene
final_data <- median_filt[rowSums(median_filt == 0) <=36,]

#creating a metadata sheet with all experimental conditions
final_data_trimmed <- final_data %>%
  select(-c(1, 2, ncol(.)))

#making experimental conditions such that it has same number of variables (to match naming convention for all groups)
sample_names <- colnames(final_data_trimmed) 
sample_names_modified <- gsub("^NAGLU_", "", colnames(final_data_trimmed))
sample_names_modified <- sub("_[^_]*$", "", sample_names_modified)
one_underscore <- !grepl("_.*_", sample_names_modified)
sample_names_modified[one_underscore] <- paste(sample_names_modified[one_underscore], "week0", sep = "_")

# Extract Genotype, Sex, and Age from sample names to create a metadata
conditions <- data.frame(
  SampleID = sample_names,
  Genotype = sapply(strsplit(sample_names_modified, "_"), `[`, 1),
  Age = sapply(strsplit(sample_names_modified, "_"), `[`, 3),
  Sex = sapply(strsplit(sample_names_modified, "_"), `[`, 2)
)
conditions <- conditions %>%
  mutate(Sex = case_when(
    startsWith(Sex, "M") ~ paste0("Male_Rep", substring(Sex, 2)),
    startsWith(Sex, "F") ~ paste0("Female_Rep", substring(Sex, 2)),
    TRUE ~ Sex
  ))
conditions$Age <- tolower(conditions$Age)
write.table(t(conditions), sep = ",", file = "metadata.txt", row.names = TRUE, col.names = FALSE, quote = FALSE)

#creating a dummy variable that combines all factors
meta_data <- read.table("metadata.txt", row.names = 1, header = TRUE, sep = ",")
group <- as.factor(paste0(c(meta_data[1,]), "_", c(meta_data[2,]), "_", c(meta_data[3,])))

#GETMM normalization
#dividing all counts by respective gene length, counts start at 3rd column, and last column is median
x <- final_data
row.names(x) <- x[,1]
rpk <- x[,3:(ncol(x)-1)] / x[,2]

#creating DGE object
rpk_norm <- DGEList(counts = rpk, group = group)

#normalize using GeTMM
rpk_norm <- calcNormFactors(rpk_norm)

#Extract CPM
norm_counts_rpk <- cpm(rpk_norm)

#renaming the variables for better formatting
colnames(norm_counts_rpk) <- group

#export normalized dataset
write.csv(norm_counts_rpk, file = "Normalized_counts_MPSIIIB.csv")

#Tranforming dataset using pivoltlonger
norm_counts <- as.data.frame(norm_counts_rpk)
norm_counts$Flybase_ID <- rownames(norm_counts)#creating another column to add gene names

norm_counts_long <- pivot_longer(norm_counts, cols = -Flybase_ID, names_to = "Sample_ID", values_to = "norm_counts")

#boxplot to check normalized data looks uniform across samples
ggplot(norm_counts_long, aes(x = Sample_ID, y = norm_counts) +
         geom_boxplot() +
         labs(title = "Box Plot", x = "Samples", y = "Values") +
         theme(axis.text.x = element_text(angle = 90, hjust = 1)))

#Separating Sample ID into individual parameters i.e Genotype, Sex, Age and Rep
norm_counts_long <- separate(norm_counts_long, Sample_ID, into = c("Genotype", "Age", "Sex", "Rep"), sep = "_")

write.csv(norm_counts_long, file = "Normalized_Counts_MPSIIIB_transformed.csv")

#use this file for analyses in SAS


