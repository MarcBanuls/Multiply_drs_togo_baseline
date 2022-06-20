library(tidyr)
#read csv exported
drs_data<-read.csv('MULTIPLYSPResistance_DATA_2022-06-20_0730.csv')
colnames(drs_data)[1] <- 'record_id'

# study_number_manual check no duplicates
drs_data_clean <- separate(drs_data, 'study_number_manual', sep = '-', into = c('study','site','timepoint','study_number'),
                           fill = 'left',remove = FALSE)

#duplicates?
n_occur <- data.frame(table(drs_data_clean$study_number))
n_occur[n_occur$Freq > 1,]

# sample1_id_manual  has to be the same (except -1 and -2 for the samples "sample1" and "sample2")
sample1 <- as.data.frame(gsub('-1$', '',drs_data_clean$sample1_id_manual))
colnames(sample1)[1] <- 'sample1'
sample2 <- as.data.frame(gsub('-2$', '',drs_data_clean$sample2_id_manual))
colnames(sample2)[1] <- 'sample2'

#sample1
sample1$study_number <- drs_data$study_number_manual
sample1['test'] <- NA

sample1$test <- ifelse(sample1$study_number == sample1$sample1, "OK", "ERROR" )

#sample2
sample2$study_number <- drs_data$study_number_manual
sample2['test'] <- NA

sample2$test <- ifelse(sample2$study_number == sample2$sample2, "OK", "ERROR" )

#comparison between sample 1 and sample 2
comp_samples <- as.data.frame(gsub('-1$', '',drs_data_clean$sample1_id_manual))
colnames(comp_samples)[1] <- 'sample1'
comp_samples$sample2 <- gsub('-2$', '',drs_data_clean$sample2_id_manual)
comp_samples['test'] <- NA
comp_samples$test <- ifelse(comp_samples$sample1 == comp_samples$sample2, "OK", "ERROR" )
