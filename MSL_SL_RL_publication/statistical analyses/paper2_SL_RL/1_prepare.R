##########################################################
##                   get Data                   ##
##########################################################
## Description :: loads subjects 
## Input :::::::: 
## Libraries :::: 
## Output ::::::: 
##########################################################

# get covariates for template creation ----
finalSampleV1 <- read_csv(file = file.path(inputFolder, 'outputfMRIv1AVeTV.csv'))  %>% 
  filter(ID != "CBC_1089")

finalSampleV3 <- read_csv(file = file.path(inputFolder, 'outputfMRIv3AVeTVolderShort.csv'))  %>% 
  filter(ID != "CBC_1089")

finalSample <- finalSampleV1 %>% 
  bind_rows(finalSampleV3) %>% 
  select(-excludeOms20, -excludeBS15, -excludeOms25, -excludeBS20)

subjects <- finalSample %>% 
  select(ID, version, klasse.factor, auswahl_vp.factor, geschlecht, age) %>% 
  unique() %>% 
  rename(gender = geschlecht,
         grade = klasse.factor,
         group = auswahl_vp.factor)

finalSampleAdults <- read_csv(file = file.path(inputFolder, 'outputfMRIAdultsAVeTV.csv'))

subjectsAdults <- finalSampleAdults %>% 
  select(ID, version, auswahl_vp.factor, geschlecht, age) %>% 
  unique() %>% 
  rename(gender = geschlecht,
         group = auswahl_vp.factor)

diff <- (nrow(subjectsAdults) - nrow(subjects[subjects$version == "v1",]))

femaleAdults <- finalSampleAdults %>% 
  select(ID, geschlecht) %>% 
  filter(geschlecht == "f") %>% 
  distinct()

set.seed(27) # making it reproducible

excludedFemales <- femaleAdults %>% 
  slice_sample(n = diff) %>% 
  select(ID)
excludedFemales 
# to exclude: P008, P025, P032, P012, P011, P028, P021, CBC_P003

finalSampleAdults <- finalSampleAdults %>% 
  filter(!(ID %in% excludedFemales$ID))

subjectsAdults <- subjectsAdults %>% 
  filter(!(ID %in% excludedFemales$ID))

remove(femaleAdults, diff)

## add handedness to adults
adultsEHI <- read_xlsx(file.path(inputFolder, "adultsHandedness.xlsx"))

adultsEHI <- adultsEHI %>% 
  filter(ID %in% subjectsAdults$ID)

subjectsAdults <- subjectsAdults %>% 
  left_join(.,adultsEHI[, c("ID", "handedness")], by = join_by(ID))

write_csv(subjectsAdults, file.path(outputFolder, 'finalSubjectsAdults.csv'))

## days between VT2 and MR
daysVTMR <- finalSample %>% 
  select(ID, version, timePreMR) %>% 
  unique()

mean(daysVTMR$timePreMR)
round(sd(daysVTMR$timePreMR),2)

mean(daysVTMR$timePreMR[daysVTMR$version == "v1"])
round(sd(daysVTMR$timePreMR[daysVTMR$version == "v1"]),2)

mean(daysVTMR$timePreMR[daysVTMR$version == "v3"])
round(sd(daysVTMR$timePreMR[daysVTMR$version == "v3"]),2)

## look at age differences
# group and age 
ttAgeGroup <- t.test(subjects$age[subjects$version=="v1"], subjects$age[subjects$version=="v3"], alternative = "two.sided")
# p = 0.882
print(ttAgeGroup)
report(ttAgeGroup)
mean(subjects$age[subjects$version=="v1"], na.rm = T)
round(sd(subjects$age[subjects$version=="v1"], na.rm = T),2)
mean(subjects$age[subjects$version=="v3"], na.rm = T)
sd(subjects$age[subjects$version=="v3"], na.rm = T)

# mean age adults
mean(subjectsAdults$age) # 25.13
sd(subjectsAdults$age) # 2.98
min(subjectsAdults$age) # 19.02
max(subjectsAdults$age) # 30.90

table(subjectsAdults$gender)

## check gender distribution in groups
table_sex_group <- table(subjects$version, subjects$gender)

chisq_test <- chisq.test(table_sex_group)
print(chisq_test) # p-value = 0.04352

fisher_test <- fisher.test(table_sex_group)
print(fisher_test) # p-value = 0.03295

################################################################################
# prepare demographics for analyses --------------------------------------------
demographics <- read_xlsx(file.path(inputFolder, 'demographics.xlsx'))
medicalHist <- read_xlsx(file.path(inputFolder, 'medicalHist.xlsx'))

#filter only the desired subjects
numVariables <- c("EHI_latQuot", "CBCL_tot")
facVariables <- c("ID", "auswahl_vp", "klasse", "geschlecht", "EHI_handedness")

demographics <- demographics %>% 
  rename(ID = vp_nr) %>% 
  mutate(across(all_of(numVariables), as.numeric)) %>% 
  mutate(across(all_of(facVariables), as.factor)) %>% 
  filter(ID %in% subjects$ID)

setdiff(subjects$ID, demographics$ID)

medicalHist <- medicalHist %>% 
  rename(ID = vp_nr) %>% 
  mutate(across(all_of(c("ID", "auswahl_vp", "klasse", "geschlecht")), as.factor)) %>% 
  filter(ID %in% subjects$ID)

## check for missing values ----
demographics %>% 
  select(ID, klasse, where(is.numeric)) %>%   # Identify numeric columns
  mutate(has_na = rowSums(is.na(.)) > 0) %>%   # Check for NA values 
  filter(has_na) %>%   # Select the rows from the original data frame where any numeric column contains NA
  select(-has_na) %>%  # Remove the logical index column
  select(ID, where(~ any(is.na(.))))  # Keep only columns with at least one NA

medicalHist %>% 
  select(ID, klasse, where(is.numeric)) %>%   # Identify numeric columns
  mutate(has_na = rowSums(is.na(.)) > 0) %>%   # Check for NA values 
  filter(has_na) %>%  # Remove the logical index column
  select(ID, where(~ any(is.na(.))))  # Keep only columns with at least one NA

remove(demoMissing, medicalMissing)

## check for subjects with low IQ or high scores on CBCL
demographics %>% 
  select(ID, klasse, PPVT_IQ, FSIQ, meanIQ) %>% 
  filter(PPVT_IQ < 80 | FSIQ < 80)

demographics %>% 
  select(ID, klasse, CBCL_tot) %>% 
  filter(CBCL_tot > 63 | is.na(CBCL_tot) ) %>% 
  mutate(across(!c(ID, klasse), as.numeric)) %>% 
  arrange(CBCL_tot) %>% 
  rename('CBCL_tot [>63]' = CBCL_tot)

## only select desired variables
demo <- demographics %>% 
  select(-c(speechDisorder, speechDisorderType, LEAPS_languages)) %>% 
  left_join(., subjects %>% select(ID, age))

## combine in subjects table 
subjects <- subjects %>% 
  left_join(. , demo %>% 
              select(ID, EHI_handedness, PPVT_IQ, FSIQ, meanIQ),
            by = join_by(ID)) %>% 
  rename(handedness = EHI_handedness) %>% 
  mutate(group = "children")

write_csv(subjects, file.path(outputFolder, 'finalSubjects.csv'))

#######################
# behavioural data ----
# load MR data ----
finalSample$logfile[finalSample$logfile==
                      "CBC_1o24_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv"] <-
  "CBC_1024_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv"

finalSample %>% 
  group_by(ID, modality) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) # none with more than one run per modality

files <- lapply(paste(logFolder, finalSample$ID, "beh", finalSample$logfile, sep = "/"), data.table::fread)

behData <- data.frame()
expInfo <- data.frame()

for (i in 1:length(files)) {
  remove(dataTemp, expInfoTemp)
  
  # select only trials and change non-responses
  dataTemp <- data.frame(files[i]) %>% 
    mutate(trials_runs.correct_answer = ifelse(trials_runs.feedback_given == 3, NA, trials_runs.correct_answer),
           response_runs.keys = ifelse(trials_runs.feedback_given == 3, "None", response_runs.keys),
           participant = finalSample$ID[i],
           tsDifficulty = ts_difficulty[nrow(.)]) %>% 
    filter(!is.na(trials_runs.thisN)) 
  
  if (grepl("v3", finalSample$logfile[i], fixed = TRUE)){
    if (grepl("AV", finalSample$logfile[i], fixed = TRUE)){
      dataTemp <- dataTemp %>%
        select(c('participant', 'trials_runs.thisN', 'trials_runs.auditory_stim',
                 'trials_runs.visual_stim_left', 'trials_runs.visual_stim_right',
                 'trials_runs.correctKey', 'trials_runs.presentationFrequncy',
                 'response_runs.keys', 'response_runs.rt',
                 'trials_runs.correct_answer','trials_runs.feedback_given', 
                 'session', 'tsDifficulty', 'prob.Feedback')) %>% 
        rename('secondStim' = 'trials_runs.auditory_stim') %>% 
        mutate(version = "v3")
    } else {
      dataTemp <- dataTemp %>%
        select(c('participant', 'trials_runs.thisN', 'trials_runs.tactile_stim',
                 'trials_runs.visual_stim_left', 'trials_runs.visual_stim_right',
                 'trials_runs.correctKey', 'trials_runs.presentationFrequncy',
                 'response_runs.keys', 'response_runs.rt',
                 'trials_runs.correct_answer','trials_runs.feedback_given', 
                 'session', 'tsDifficulty', 'prob.Feedback')) %>% 
        rename('secondStim' = 'trials_runs.tactile_stim') %>% 
        mutate(version = "v3")
    }
  } else if (grepl("v1", finalSample$logfile[i], fixed = TRUE)){
    if (grepl("AV", finalSample$logfile[i], fixed = TRUE)){
      dataTemp <- dataTemp %>%
        select(c('participant', 'trials_runs.thisN', 'trials_runs.auditory_stim',
                 'trials_runs.visual_stim',
                 'trials_runs.presentationFrequncy',
                 'response_runs.keys', 'response_runs.rt',
                 'trials_runs.correct_answer','trials_runs.feedback_given', 
                 'session', 'tsDifficulty', 'prob.Feedback')) %>% 
        rename('secondStim' = 'trials_runs.auditory_stim',
               'trials_runs.visual_stim_left' = 'trials_runs.visual_stim') %>% 
        mutate(version = "v1")
    } else {
      dataTemp <- dataTemp %>%
        select(c('participant', 'trials_runs.thisN', 'trials_runs.tactile_stim',
                 'trials_runs.visual_stim',
                 'trials_runs.presentationFrequncy',
                 'response_runs.keys', 'response_runs.rt',
                 'trials_runs.correct_answer','trials_runs.feedback_given', 
                 'session', 'tsDifficulty', 'prob.Feedback')) %>% 
        rename('secondStim' = 'trials_runs.tactile_stim',
               'trials_runs.visual_stim_left' = 'trials_runs.visual_stim') %>% 
        mutate(version = "v1")
    }
  }
  
  if (finalSample$logfile[i] == "CBC_1019_MSI_TV_v3_Vset5_Tset2_2023-06-28_16h17.24.850.csv" ||
      finalSample$logfile[i] == "CBC_1019_MSI_TV_v3_Vset1_Tset3_2023-06-28_18h00.13.129.csv" ||
      finalSample$logfile[i] == "CBC_1020_MSI_TV_v3_Vset1_Tset2_2023-06-28_15h03.37.595.csv" ||
      finalSample$logfile[i] == "CBC_1020_MSI_TV_v3_Vset5_Tset1_2023-06-28_16h55.43.253.csv") {
    dataTemp$trials_runs.feedback_given <-  as.numeric(unlist(strsplit(
      gsub("\\[|\\]", "", dataTemp$trials_runs.feedback_given[44]), ", ")))
  }
  
  # calculate accuracy
  for (j in 1:(nrow(dataTemp))) {
    dataTemp$totalAccuracy[j] <- mean(dataTemp$trials_runs.correct_answer[1:j], na.rm = TRUE)
    dataTemp$totalError[j] <- (1 - dataTemp$totalAccuracy[j])
  }
  
  # clean up data
  dataTemp$response_runs.rt <- as.numeric(dataTemp$response_runs.rt)
  dataTemp$session <- as.numeric(dataTemp$session)
  
  dataTemp <- dataTemp %>% 
    mutate(outlier200ms = if_else(response_runs.rt < 0.2, 1, 0),
           outlierSD = if_else(response_runs.rt > (mean(response_runs.rt, na.rm = T)+3*sd(response_runs.rt, na.rm = T)),
                               1,0),
           omission = if_else(trials_runs.feedback_given==3, 1, 0),
           validTrials = if_else(outlier200ms==1 | outlierSD == 1 | omission == 1,0,1))
  
  expInfoTemp <- dataTemp %>% 
    #slice_head(n=1) %>% 
    group_by(participant, session, tsDifficulty, prob.Feedback) %>% 
    summarise(ACC = mean(trials_runs.correct_answer, na.rm = T),
              minACC = min(totalAccuracy, na.rm = T),
              maxACC = max(totalAccuracy, na.rm =T),
              RT = mean(response_runs.rt, na.rm = T),
              minRT = min(response_runs.rt, na.rm = T),
              maxRT = max(response_runs.rt, na.rm = T),
              omissions = mean(omission, na.rm = T),
              outliers = mean(outlierSD, na.rm = T),
              idxOmission = list(which(omission==1)),
              idxOutlier = list(which(outlier200ms==1 | outlierSD ==1))) 
  
  # add modality and stimType
  if (grepl("AV", finalSample$logfile[i])) {
    dataTemp$modality <- "av"
    expInfoTemp$modality <- "av"
    
    dataTemp$stimType <- "env"
    expInfoTemp$stimType <- "env"
    
    if (grepl("Aset4",finalSample$logfile[i]) | grepl("Aset5",finalSample$logfile[i]) | grepl("Aset6",finalSample$logfile[i])) {
      dataTemp$stimType <- "syll"
      expInfoTemp$stimType <- "syll"
    }  
  } else {
    dataTemp$modality <- "tv"
    dataTemp$stimType <- "vib"
    expInfoTemp$modality <- "tv"
    expInfoTemp$stimType <- "vib"
  }
  
  # add logfileName
  dataTemp$logfile <- finalSample$logfile[i]
  expInfoTemp$logfile <- finalSample$logfile[i]
  
  dataTemp$group <- "children"
  expInfoTemp$group <- "children"
  
  dataTemp$trials_runs.presentationFrequncy <- as.factor(dataTemp$trials_runs.presentationFrequncy)
  
  # combine data 
  behData <- behData %>% 
    bind_rows(., dataTemp)
  expInfo <- rbind(expInfo, expInfoTemp)
}

behData$trials_runs.presentationFrequncy <- as.factor(behData$trials_runs.presentationFrequncy)
expInfo$participant <- as.factor(expInfo$participant)
behData$participant <- as.factor(behData$participant)

behData <- behData  %>% 
  rename(ID = participant) %>% 
  arrange(version, ID, session)

expInfo <- expInfo  %>% 
  rename(ID = participant) %>% 
  mutate(version = ifelse(grepl("v1", logfile), "v1", "v3")) %>% 
  arrange(version, ID, session)

behData %>% 
  group_by(ID, modality) %>% 
  summarise(count = n()) %>% 
  filter(count > 44) # none with more than one run per modality

expInfo %>% 
  group_by(ID, modality) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) # none with more than one run per modality

MRexpInfo <- expInfo
MRbehData <- behData

################################################################################
# load data adults -------------------------------------------------------------
filePathsAdults <- lapply(paste(logFolder, "adults", finalSampleAdults$ID, "beh", 
                                finalSampleAdults$logfile, sep = "/"), data.table::fread)

length(filePathsAdults)   # 56 -> two for 28 subjects

behData <- data.frame()
expInfo <- data.frame()

for (i in 1:length(filePathsAdults)) {
  remove(dataTemp, expInfoTemp)
  
  # select only trials and change non-responses
  dataTemp <- data.frame(filePathsAdults[i]) %>% 
    mutate(trials_runs.correct_answer = ifelse(trials_runs.feedback_given == 3, NA, trials_runs.correct_answer),
           response_runs.keys = ifelse(trials_runs.feedback_given == 3, "None", response_runs.keys),
           tsDifficulty = "hard") %>% 
    filter(!is.na(trials_runs.thisN)) 
  
  if (grepl("AV", finalSampleAdults$logfile[i], fixed = TRUE)){
    dataTemp <- dataTemp %>%
      select(c('participant', 'trials_runs.thisN', 'trials_runs.audio_stim',
               'trials_runs.visual_stim',
               'trials_runs.presentationFrequency',
               'response_runs.keys', 'response_runs.rt',
               'trials_runs.correct_answer','trials_runs.feedback_given', 
               'tsDifficulty', 'FB.probability', 'session')) %>% 
      rename('secondStim' = 'trials_runs.audio_stim',
             'trials_runs.visual_stim_left' = 'trials_runs.visual_stim',
             'trials_runs.presentationFrequncy' = 'trials_runs.presentationFrequency',
             'prob.Feedback' = 'FB.probability') %>% 
      mutate(version = "v1")
  } else {
    dataTemp <- dataTemp %>%
      select(c('participant', 'trials_runs.thisN', 'trials_runs.tactile_stim',
               'trials_runs.visual_stim',
               'trials_runs.presentationFrequency',
               'response_runs.keys', 'response_runs.rt',
               'trials_runs.correct_answer','trials_runs.feedback_given', 
               'tsDifficulty', 'FB.probability', 'session')) %>% 
      rename('secondStim' = 'trials_runs.tactile_stim',
             'trials_runs.visual_stim_left' = 'trials_runs.visual_stim',
             'trials_runs.presentationFrequncy' = 'trials_runs.presentationFrequency',
             'prob.Feedback' = 'FB.probability') %>% 
      mutate(version = "v1")
  }
  
  # calculate accuracy
  for (j in 1:(nrow(dataTemp))) {
    dataTemp$totalAccuracy[j] <- mean(dataTemp$trials_runs.correct_answer[1:j], na.rm = TRUE)
    dataTemp$totalError[j] <- (1 - dataTemp$totalAccuracy[j])
  }
  
  # clean up data
  dataTemp$response_runs.rt <- as.numeric(dataTemp$response_runs.rt)
  dataTemp$session <- as.numeric(dataTemp$session)
  
  dataTemp <- dataTemp %>% 
    mutate(outlier200ms = if_else(response_runs.rt < 0.2, 1, 0),
           outlierSD = if_else(response_runs.rt > (mean(response_runs.rt, na.rm = T)+3*sd(response_runs.rt, na.rm = T)),
                               1,0),
           omission = if_else(trials_runs.feedback_given==3, 1, 0),
           validTrials = if_else(outlier200ms==1 | outlierSD == 1 | omission == 1,0,1))
  
  expInfoTemp <- dataTemp %>% 
    #slice_head(n=1) %>% 
    group_by(participant, session, tsDifficulty, prob.Feedback, version) %>% 
    summarise(ACC = mean(trials_runs.correct_answer, na.rm = T),
              minACC = min(totalAccuracy, na.rm = T),
              maxACC = max(totalAccuracy, na.rm =T),
              RT = mean(response_runs.rt, na.rm = T),
              minRT = min(response_runs.rt, na.rm = T),
              maxRT = max(response_runs.rt, na.rm = T),
              omissions = mean(omission, na.rm = T),
              outliers = mean(outlierSD, na.rm = T),
              idxOmission = list(which(omission==1)),
              idxOutlier = list(which(outlier200ms==1 | outlierSD ==1))) 
  
  # add modality and stimType
  if (grepl("AV", finalSampleAdults$logfile[i])) {
    dataTemp$modality <- "av"
    expInfoTemp$modality <- "av"
    
    dataTemp$stimType <- "env"
    expInfoTemp$stimType <- "env"
  } else {
    dataTemp$modality <- "tv"
    dataTemp$stimType <- "vib"
    expInfoTemp$modality <- "tv"
    expInfoTemp$stimType <- "vib"
  }
  
  dataTemp$group <- "adults"
  expInfoTemp$group <- "adults"
  
  # add logfileName
  dataTemp$logfile <- finalSampleAdults$logfile[i]
  expInfoTemp$logfile <- finalSampleAdults$logfile[i]
  
  dataTemp$trials_runs.presentationFrequncy <- as.factor(dataTemp$trials_runs.presentationFrequncy)
  
  # combine data 
  behData <- behData %>% 
    bind_rows(., dataTemp)
  expInfo <- rbind(expInfo, expInfoTemp)
}

behData$trials_runs.presentationFrequncy <- as.factor(behData$trials_runs.presentationFrequncy)

behData$participant[behData$participant == "CBC_p=30"] <- "CBC_P030"
behData$participant[behData$participant == "CBC_P01"] <- "CBC_P041"
behData$participant[behData$logfile == "CBC_P022_TV_prob0.9_Vset4_Tset1_2023-04-02_15h00.38.402.csv"] <- "CBC_P022"
behData$participant[behData$logfile == "CBC_P022_AV_prob0.9_Vset2_Aset2_2023-04-02_15h08.21.873.csv"] <- "CBC_P022"

expInfo$participant[expInfo$participant == "CBC_p=30"] <- "CBC_P030"
expInfo$participant[expInfo$participant == "CBC_P01"] <- "CBC_P041"
expInfo$participant[expInfo$logfile == "CBC_P022_TV_prob0.9_Vset4_Tset1_2023-04-02_15h00.38.402.csv"] <- "CBC_P022"
expInfo$participant[expInfo$logfile == "CBC_P022_AV_prob0.9_Vset2_Aset2_2023-04-02_15h08.21.873.csv"] <- "CBC_P022"

behData1 <- behData  %>% 
  rename(ID = participant) %>% 
  arrange(version, ID, session) %>% 
  mutate(logfile = basename(logfile))

ids <- sub("^([^_]+_[^_]+)_.*", "\\1", behData1$logfile)

which(ids != as.character(behData1$ID))

expInfo1 <- expInfo  %>% 
  rename(ID = participant) %>% 
  mutate(logfile = basename(logfile)) %>% 
  arrange(version, ID, session)

ids <- sub("^([^_]+_[^_]+)_.*", "\\1", expInfo1$logfile)

which(ids != as.character(expInfo1$ID))

behData1 %>% 
  group_by(ID, modality) %>% 
  summarise(count = n()) %>% 
  filter(count != 42 & count != 44) # none with more than one run per modality

expInfo1 %>% 
  group_by(ID, modality) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) # none with more than one run per modality

expInfo1$participant <- as.factor(expInfo$participant)
behData1$participant <- as.factor(behData$participant)

MRexpInfoAd <- expInfo1
MRbehDataAd <- behData1

## Save data ----
save(MRbehData, file = file.path(outputFolder, "MRdata_childrenMR.RData"))
save(MRexpInfo, file = file.path(outputFolder, "MRexpInfo_childrenMR.RData"))

save(MRexpInfoAd, file = file.path(outputFolder, "MRexpInfoAd_childrenMR.RData"))
save(MRbehDataAd, file = file.path(outputFolder, "MRdataAd_childrenMR.RData"))

save(demo, file = file.path(outputFolder, "demo.RData"))
save(finalSample, file = file.path(outputFolder, "finalSample.RData"))
save(finalSampleAdults, file = file.path(outputFolder, "finalSampleAdults.RData"))

write_excel_csv2(MRbehData, file = file.path(outputFolder, "MRdata_childrenMR.csv"))
write_excel_csv2(MRexpInfo, file = file.path(outputFolder, "MRexpInfo_childrenMR.csv"))

write_excel_csv2(MRexpInfoAd, file = file.path(outputFolder, "MRexpInfoAd_childrenMR.csv"))
write_excel_csv2(MRbehDataAd, file = file.path(outputFolder, "MRdataAd_childrenMR.csv"))

write_excel_csv2(finalSample, file = file.path(outputFolder, "finalSample.csv"))
write_excel_csv2(finalSampleAdults, file = file.path(outputFolder, "finalSampleAdults.csv"))

# report system ----
report(sessionInfo())

## Clean up workspace ----
remove(dataTemp, expInfoTemp, i, j, k, files, nrTrials, logfile, logfilename, timestamp,
       idxDiff, artOutput, ageGroupPlot, demographics, lowIQ, medicalHist, SDQ_CBCL,
       ttAge, ttAgeGroup, ageGenderPlot, ageGenderPlotsig, ageGroupPlotsig, behData,
       behData1, chisq_test, df_plot, expInfo, expInfo1, fisher_test, genderDistPlot,
       genderDistPlotsig, facVariables, filePaths, filePathsV1, filePathsV3, ids,
       numVariables, table_sex_group, filePathsAdults)
