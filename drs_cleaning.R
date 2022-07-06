library(tidyr)
library(dplyr)
#read csv exported
drs_data<-read.csv('MULTIPLYSPResistance_DATA_2022-07-04_1139.csv')
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
sample1$record_id <- drs_data_clean$record_id

sample1$test <- ifelse(sample1$study_number == sample1$sample1, "OK", "ERROR" )

#sample2
sample2$study_number <- drs_data$study_number_manual
sample2['test'] <- NA
sample2$record_id <- drs_data_clean$record_id

sample2$test <- ifelse(sample2$study_number == sample2$sample2, "OK", "ERROR" )

#comparison between sample 1 and sample 2
comp_samples <- as.data.frame(gsub('-1$', '',drs_data_clean$sample1_id_manual))
colnames(comp_samples)[1] <- 'sample1'
comp_samples$sample2 <- gsub('-2$', '',drs_data_clean$sample2_id_manual)
comp_samples['test'] <- NA
comp_samples$record_id <- drs_data_clean$record_id
comp_samples$study_number <- drs_data$study_number_manual
comp_samples$test <- ifelse(comp_samples$sample1 == comp_samples$sample2, "OK", "ERROR" )

sample1_error <- sample1[sample1$test == 'ERROR',]
sample2_error <- sample2[sample2$test == 'ERROR',]
comp_samples_error <-  comp_samples[comp_samples$test == 'ERROR',]

### study profile for today (como no..)

#remove NA records:
drs_data_clean <- drs_data_clean %>% drop_na(consent)

profile <- table(drs_data_clean$hf)
print(profile)

#export con blood samples de cada variable

study_samples <- drs_data_clean %>% select(study_number_manual,sample1,sample2)
table(study_samples$sample2)
study_samples$sample1 <- ifelse(study_samples$sample1 == 1, 'Yes', 'No')
study_samples$sample2 <- ifelse(study_samples$sample2 == 1, 'Yes', 'No')
write.csv(study_samples,"study_samples_togo_20220704.csv",row.names = FALSE)


#num of people for each hf
#hahomegbe fixed
nrow(drs_data_clean[drs_data_clean$hf == 1 & drs_data_clean$hf_survey_where == 1,])

#notse
nrow(drs_data_clean[drs_data_clean$hf == 2 & drs_data_clean$hf_survey_where == 1,])

#kpove
nrow(drs_data_clean[drs_data_clean$hf == 3 & drs_data_clean$hf_survey_where == 1,])

#wahala
nrow(drs_data_clean[drs_data_clean$hf == 4 & drs_data_clean$hf_survey_where == 1,])


nrow(drs_data_clean[drs_data_clean$hf == 5 & drs_data_clean$hf_survey_where == 1,])

nrow(drs_data_clean[drs_data_clean$hf == 6 & drs_data_clean$hf_survey_where == 1,])

####TOTAL####
# chldren between 24-59 month attending health facility

month_hf <- drs_data_clean[drs_data_clean$screening_age_months >= 24 & drs_data_clean$screening_age_months<=59,]

## children 24-59 months screened with RDT
rdt_month <- drs_data_clean[drs_data_clean$screening_age_months >= 24 & drs_data_clean$screening_age_months<=59 & drs_data_clean$rdt == 1,]

# children 24- 59 months with RDT (+) 
rdt_month_pos <- drs_data_clean[drs_data_clean$screening_age_months >= 24 & drs_data_clean$screening_age_months<=59 & drs_data_clean$rdt_result == 1,]

# children 24- 59 months with RDT (+), approached & eligible
rdt_month_pos_elig <- drs_data_clean[drs_data_clean$screening_age_months >= 24 & drs_data_clean$screening_age_months<=59 &
                              drs_data_clean$rdt_result == 1 & drs_data_clean$elegible == 1,]

# ineligible children
inelig <- drs_data_clean[drs_data_clean$elegible == 0,]

# ineligible children
elig <- drs_data_clean[drs_data_clean$elegible == 1,]

# eligible children NOT recruited
elig <- drs_data_clean[drs_data_clean$elegible == 1 & drs_data_clean$consent == 0,]

# children whose parent refused to consent
elig <- drs_data_clean[drs_data_clean$elegible == 1 & drs_data_clean$consent == 0,]

# children with 0 samples collected
nosamp <- drs_data_clean[drs_data_clean$sample1 != 1 & drs_data_clean$sample2 != 1,]

# children eligible AND recruited?? same as previous one

# children with ONE sample collected
onesamp <- drs_data_clean[(drs_data_clean$sample1 == '' & drs_data_clean$sample2 == 1) | 
                            (drs_data_clean$sample == 1 & drs_data_clean$sample2 == ''),]

# children with TWO samples collected
####all####

###separated by HF:
haho <- drs_data_clean[drs_data_clean$hf == 1,]
notse <- drs_data_clean[drs_data_clean$hf == 2,]
kpove <- drs_data_clean[drs_data_clean$hf == 3,]
wahala <- drs_data_clean[drs_data_clean$hf == 4,]
tetetou <- drs_data_clean[drs_data_clean$hf == 5,]
avassikpe <- drs_data_clean[drs_data_clean$hf == 6,]

#### HAHO ####
# chldren between 24-59 month attending health facility

month_hf <- haho[haho$screening_age_months >= 24 & haho$screening_age_months<=59,]

## children 24-59 months screened with RDT
rdt_month <- haho[haho$screening_age_months >= 24 & haho$screening_age_months<=59 & haho$rdt == 1,]

# children 24- 59 months with RDT (+) 
rdt_month_pos <- haho[haho$screening_age_months >= 24 & haho$screening_age_months<=59 & haho$rdt_result == 1,]

# children 24- 59 months with RDT (+), approached & eligible
rdt_month_pos_elig <- haho[haho$screening_age_months >= 24 & haho$screening_age_months<=59 &
                                       haho$rdt_result == 1 & haho$elegible == 1,]

# ineligible children
inelig <- haho[haho$elegible == 0,]

# ineligible children
elig <- haho[haho$elegible == 1,]

# eligible children NOT recruited
elig <- haho[haho$elegible == 1 & haho$consent == 0,]

# children whose parent refused to consent
elig <- haho[haho$elegible == 1 & haho$consent == 0,]

# children with 0 samples collected
nosamp <- haho[haho$sample1 != 1 & haho$sample2 != 1,]

# children eligible AND recruited?? same as previous one

# children with ONE sample collected
onesamp <- haho[(haho$sample1 == '' & haho$sample2 == 1) | 
                            (haho$sample == 1 & haho$sample2 == ''),]


#### NOTSE ####
# chldren between 24-59 month attending health facility

month_hf <- notse[notse$screening_age_months >= 24 & notse$screening_age_months<=59,]

## children 24-59 months screened with RDT
rdt_month <- notse[notse$screening_age_months >= 24 & notse$screening_age_months<=59 & notse$rdt == 1,]

# children 24- 59 months with RDT (+) 
rdt_month_pos <- notse[notse$screening_age_months >= 24 & notse$screening_age_months<=59 & notse$rdt_result == 1,]

# children 24- 59 months with RDT (+), approached & eligible
rdt_month_pos_elig <- notse[notse$screening_age_months >= 24 & notse$screening_age_months<=59 &
                             notse$rdt_result == 1 & notse$elegible == 1,]

# ineligible children
inelig <- notse[notse$elegible == 0,]

# ineligible children
elig <- notse[notse$elegible == 1,]

# eligible children NOT recruited
elig <- notse[notse$elegible == 1 & notse$consent == 0,]

# children whose parent refused to consent
elig <- notse[notse$elegible == 1 & notse$consent == 0,]

# children with 0 samples collected
nosamp <- notse[notse$sample1 != 1 & notse$sample2 != 1,]

# children eligible AND recruited?? same as previous one

# children with ONE sample collected
onesamp <- notse[(notse$sample1 == '' & notse$sample2 == 1) | 
                  (notse$sample == 1 & notse$sample2 == ''),]

#### kpove ####
# chldren between 24-59 month attending health facility

month_hf <- kpove[kpove$screening_age_months >= 24 & kpove$screening_age_months<=59,]

## children 24-59 months screened with RDT
rdt_month <- kpove[kpove$screening_age_months >= 24 & kpove$screening_age_months<=59 & kpove$rdt == 1,]

# children 24- 59 months with RDT (+) 
rdt_month_pos <- kpove[kpove$screening_age_months >= 24 & kpove$screening_age_months<=59 & kpove$rdt_result == 1,]

# children 24- 59 months with RDT (+), approached & eligible
rdt_month_pos_elig <- kpove[kpove$screening_age_months >= 24 & kpove$screening_age_months<=59 &
                              kpove$rdt_result == 1 & kpove$elegible == 1,]

# ineligible children
inelig <- kpove[kpove$elegible == 0,]

# ineligible children
elig <- kpove[kpove$elegible == 1,]

# eligible children NOT recruited
elig <- kpove[kpove$elegible == 1 & kpove$consent == 0,]

# children whose parent refused to consent
elig <- kpove[kpove$elegible == 1 & kpove$consent == 0,]

# children with 0 samples collected
nosamp <- kpove[kpove$sample1 != 1 & kpove$sample2 != 1,]

# children eligible AND recruited?? same as previous one

# children with ONE sample collected
onesamp <- kpove[(kpove$sample1 == '' & kpove$sample2 == 1) | 
                   (kpove$sample == 1 & kpove$sample2 == ''),]

#### wahala ####
# chldren between 24-59 month attending health facility

month_hf <- wahala[wahala$screening_age_months >= 24 & wahala$screening_age_months<=59,]

## children 24-59 months screened with RDT
rdt_month <- wahala[wahala$screening_age_months >= 24 & wahala$screening_age_months<=59 & wahala$rdt == 1,]

# children 24- 59 months with RDT (+) 
rdt_month_pos <- wahala[wahala$screening_age_months >= 24 & wahala$screening_age_months<=59 & wahala$rdt_result == 1,]

# children 24- 59 months with RDT (+), approached & eligible
rdt_month_pos_elig <- wahala[wahala$screening_age_months >= 24 & wahala$screening_age_months<=59 &
                              wahala$rdt_result == 1 & wahala$elegible == 1,]

# ineligible children
inelig <- wahala[wahala$elegible == 0,]

# ineligible children
elig <- wahala[wahala$elegible == 1,]

# eligible children NOT recruited
elig <- wahala[wahala$elegible == 1 & wahala$consent == 0,]

# children whose parent refused to consent
elig <- wahala[wahala$elegible == 1 & wahala$consent == 0,]

# children with 0 samples collected
nosamp <- wahala[wahala$sample1 != 1 & wahala$sample2 != 1,]

# children eligible AND recruited?? same as previous one

# children with ONE sample collected
onesamp <- wahala[(wahala$sample1 == '' & wahala$sample2 == 1) | 
                   (wahala$sample == 1 & wahala$sample2 == ''),]

#### tetetou ####
# chldren between 24-59 month attending health facility

month_hf <- tetetou[tetetou$screening_age_months >= 24 & tetetou$screening_age_months<=59,]

## children 24-59 months screened with RDT
rdt_month <- tetetou[tetetou$screening_age_months >= 24 & tetetou$screening_age_months<=59 & tetetou$rdt == 1,]

# children 24- 59 months with RDT (+) 
rdt_month_pos <- tetetou[tetetou$screening_age_months >= 24 & tetetou$screening_age_months<=59 & tetetou$rdt_result == 1,]

# children 24- 59 months with RDT (+), approached & eligible
rdt_month_pos_elig <- tetetou[tetetou$screening_age_months >= 24 & tetetou$screening_age_months<=59 &
                              tetetou$rdt_result == 1 & tetetou$elegible == 1,]

# ineligible children
inelig <- tetetou[tetetou$elegible == 0,]

# ineligible children
elig <- tetetou[tetetou$elegible == 1,]

# eligible children NOT recruited
elig <- tetetou[tetetou$elegible == 1 & tetetou$consent == 0,]

# children whose parent refused to consent
elig <- tetetou[tetetou$elegible == 1 & tetetou$consent == 0,]

# children with 0 samples collected
nosamp <- tetetou[tetetou$sample1 != 1 & tetetou$sample2 != 1,]

# children eligible AND recruited?? same as previous one

# children with ONE sample collected
onesamp <- tetetou[(tetetou$sample1 == '' & tetetou$sample2 == 1) | 
                   (tetetou$sample == 1 & tetetou$sample2 == ''),]

#### avassikpe ####
# chldren between 24-59 month attending health facility

month_hf <- avassikpe[avassikpe$screening_age_months >= 24 & avassikpe$screening_age_months<=59,]

## children 24-59 months screened with RDT
rdt_month <- avassikpe[avassikpe$screening_age_months >= 24 & avassikpe$screening_age_months<=59 & avassikpe$rdt == 1,]

# children 24- 59 months with RDT (+) 
rdt_month_pos <- avassikpe[avassikpe$screening_age_months >= 24 & avassikpe$screening_age_months<=59 & avassikpe$rdt_result == 1,]

# children 24- 59 months with RDT (+), approached & eligible
rdt_month_pos_elig <- avassikpe[avassikpe$screening_age_months >= 24 & avassikpe$screening_age_months<=59 &
                              avassikpe$rdt_result == 1 & avassikpe$elegible == 1,]

# ineligible children
inelig <- avassikpe[avassikpe$elegible == 0,]

# ineligible children
elig <- avassikpe[avassikpe$elegible == 1,]

# eligible children NOT recruited
elig <- avassikpe[avassikpe$elegible == 1 & avassikpe$consent == 0,]

# children whose parent refused to consent
elig <- avassikpe[avassikpe$elegible == 1 & avassikpe$consent == 0,]

# children with 0 samples collected
nosamp <- avassikpe[avassikpe$sample1 != 1 & avassikpe$sample2 != 1,]

# children eligible AND recruited?? same as previous one

# children with ONE sample collected
onesamp <- avassikpe[(avassikpe$sample1 == '' & avassikpe$sample2 == 1) | 
                   (avassikpe$sample == 1 & avassikpe$sample2 == ''),]




#### notse cosas####
haho_filter <- haho %>% select(record_id,study_number_manual, elegible, sample1, sample2, consent,child_dob,screening_age_months,rdt,rdt_result,sp_recent,consent)
notse_filter <- notse %>% select(record_id,study_number_manual, elegible, sample1, sample2, consent,child_dob,screening_age_months,rdt,rdt_result,sp_recent,consent)
kpove_filter <- kpove %>% select(record_id,study_number_manual, elegible, sample1, sample2, consent,child_dob,screening_age_months,rdt,rdt_result,sp_recent,consent)
wahala_filter <- wahala %>% select(record_id,study_number_manual, elegible, sample1, sample2, consent,child_dob,screening_age_months,rdt,rdt_result,sp_recent,consent)
tetetou_filter <- tetetou %>% select(record_id,study_number_manual, elegible, sample1, sample2, consent,child_dob,screening_age_months,rdt,rdt_result,sp_recent,consent)
avassikpe_filter <- avassikpe %>% select(record_id,study_number_manual, elegible, sample1, sample2, consent,child_dob,screening_age_months,rdt,rdt_result,sp_recent,consent)

write.csv(haho_filter,"haho_drs.csv", row.names = F)
write.csv(notse_filter,"notse_drs.csv", row.names = F)
write.csv(kpove_filter,"kpove_drs.csv", row.names = F)
write.csv(wahala_filter,"wahala_drs.csv", row.names = F)
write.csv(tetetou_filter,"tetetou_drs.csv", row.names = F)
write.csv(avassikpe_filter,"avassikpe_drs.csv", row.names = F)



