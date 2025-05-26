##########################################################
##                      PREPARE                         ##
##########################################################

##########################################################
# get covariates for template creation ----
finalSample <- read_csv(file = file.path(inputFolder, 'outputfMRIv3AVeTV.csv'))  %>% 
  rename(logfile = "logfile.x")

finalSample %>% 
  group_by(ID) %>% 
  count(ID) %>% 
  group_by(n) %>% 
  count(n)

finalSample %>% 
  filter(ID %in% c("CBC_1022", "CBC_1028", "CBC_1030", "CBC_1031", "CBC_1033", 
                   "CBC_1036", "CBC_1051", "CBC_1064", "CBC_1087", "CBC_1143")) %>% 
  group_by(ID, modality) %>% 
  count(ID) %>% 
  filter(modality == "av" & n == 2) %>% 
  ungroup() %>% 
  count(n)

subjects <- finalSample %>% 
  select(ID, klasse.factor, auswahl_vp.factor, geschlecht, age) %>% 
  unique() %>% 
  rename(gender = geschlecht,
         grade = klasse.factor,
         group = auswahl_vp.factor)%>% 
  mutate(gender = as.factor(gender)) 

write_csv(subjects, file.path(outputFolder, 'finalSubjects.csv'))

## age differences based on gender ----
mu <- plyr::ddply(subjects, "gender", summarise, grp.mean=mean(age))
ttAge <- t.test(subjects$age[subjects$gender=="f"], subjects$age[subjects$gender=="m"], alternative = "two.sided")
# no age diffs, p = 0.5088
print(ttAge)
report(ttAge)
report_statistics(ttAge)
mean(subjects$age[subjects$gender=="f"])
round(sd(subjects$age[subjects$gender=="f"]),1)
mean(subjects$age[subjects$gender=="m"])
sd(subjects$age[subjects$gender=="m"])

## time between pre-run and mr run ----
outputComments <- read_csv(file = file.path(inputFolder, 'outputComments.csv')) %>% 
  select(-1)

subjects <- subjects %>% 
  left_join(outputComments %>% 
              select(ID, timePreMR) %>% 
              distinct(ID, .keep_all = TRUE), by = "ID")

summary(subjects$timePreMR)
sd(subjects$timePreMR)

#######################################
# prepare demographics for analyses ----
demographics <- read_csv(file.path(inputFolder, 'demographics.csv'))
medicalHist <- read_csv(file.path(inputFolder, 'medicalHist.csv'))

#filter only the desired subjects
numVariables <- c("EHI_latQuot","CBCL_tot")
facVariables <- c("ID", "auswahl_vp", "klasse", "geschlecht", "EHI_handedness")

demographics <- demographics %>% 
  mutate(across(all_of(numVariables), as.numeric)) %>% 
  mutate(across(all_of(facVariables), as.factor)) %>% 
  filter(ID %in% subjects$ID)

medicalHist <- medicalHist %>% 
  mutate(across(all_of(c("ID", "auswahl_vp", "klasse", "geschlecht")), as.factor)) %>% 
  filter(ID %in% subjects$ID)

## check for missing values ----
demoMissing <- demographics %>% 
  select(ID, klasse, where(is.numeric)) %>%   # Identify numeric columns
  mutate(has_na = rowSums(is.na(.)) > 0) %>%   # Check for NA values 
  filter(has_na) %>%   # Select the rows from the original data frame where any numeric column contains NA
  select(-has_na) %>%  # Remove the logical index column
  select(ID, where(~ any(is.na(.))))  # Keep only columns with at least one NA

medicalMissing <- medicalHist %>% 
  select(ID, klasse, where(is.numeric)) %>%   # Identify numeric columns
  mutate(has_na = rowSums(is.na(.)) > 0) %>%   # Check for NA values 
  filter(has_na) %>%  # Remove the logical index column
  select(ID, where(~ any(is.na(.))))  # Keep only columns with at least one NA

remove(demoMissing, medicalMissing)

## check for subjects with low IQ or high scores on CBCL
lowIQ <- demographics %>% 
  select(ID, klasse, PPVT_IQ, FSIQ, meanIQ) %>% 
  filter(FSIQ < 80 | PPVT_IQ < 80)

SDQ_CBCL <- demographics %>% 
  select(ID, klasse, CBCL_tot) %>% 
  filter(CBCL_tot > 63) %>% 
  mutate(across(!c(ID, klasse), as.numeric)) %>% 
  mutate(CBCL_tot = if_else(CBCL_tot <64, NA, CBCL_tot)) %>% 
  rename('CBCL_tot [>63]' = CBCL_tot)

## only select desired variables
demo <- demographics %>% 
  select(-EHI_latQuot, -CBCL_tot, -PPVT_IQ, -FSIQ, -klasse) %>% 
  left_join(., subjects[, c('ID', 'age')])
  
save(demo, file=file.path(outputFolder, "demo.RData"))

#######################
# behavioural data ----
# load data ----
finalSample$logfile[finalSample$logfile==
                      "CBC_1o24_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv"] <-
  "CBC_1024_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv"
files <- lapply(paste(logFolder, finalSample$ID, "beh", finalSample$logfile, sep = "/"), data.table::fread)

behData <- data.frame()
expInfo <- data.frame()

nrTrials <- 44

for (i in 1:length(files)) {
  remove(dataTemp, expInfoTemp)
  
  # select only trials and change non-responses
  dataTemp <- data.frame(files[i]) %>% 
    mutate(trials_runs.correct_answer = ifelse(trials_runs.feedback_given == 3, NA, trials_runs.correct_answer),
           response_runs.keys = ifelse(trials_runs.feedback_given == 3, "None", response_runs.keys),
           participant = finalSample$ID[i],
           tsDifficulty = ts_difficulty[nrow(.)]) %>% 
    filter(!is.na(trials_runs.thisN)) 
  
  
  if (grepl("AV", finalSample$logfile[i], fixed = TRUE)){
    dataTemp <- dataTemp %>%
      select(c('participant', 'trials_runs.thisN', 'trials_runs.auditory_stim',
               'trials_runs.visual_stim_left', 'trials_runs.visual_stim_right',
               'trials_runs.correctKey', 'trials_runs.presentationFrequncy',
               'response_runs.keys', 'response_runs.rt',
               'trials_runs.correct_answer','trials_runs.feedback_given', 
               'session', 'tsDifficulty', 'prob.Feedback')) %>% 
      rename('secondStim' = 'trials_runs.auditory_stim')
  } else {
    dataTemp <- dataTemp %>%
      select(c('participant', 'trials_runs.thisN', 'trials_runs.tactile_stim',
               'trials_runs.visual_stim_left', 'trials_runs.visual_stim_right',
               'trials_runs.correctKey', 'trials_runs.presentationFrequncy',
               'response_runs.keys', 'response_runs.rt',
               'trials_runs.correct_answer','trials_runs.feedback_given', 
               'session', 'tsDifficulty', 'prob.Feedback')) %>% 
      rename('secondStim' = 'trials_runs.tactile_stim')
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
           outlierSD = if_else(response_runs.rt < (mean(response_runs.rt, na.rm = T)-3*sd(response_runs.rt, na.rm = T)) |
                                 response_runs.rt > (mean(response_runs.rt, na.rm = T)+3*sd(response_runs.rt, na.rm = T)),
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
  
  # combine data 
  behData <- rbind(behData, dataTemp)
  expInfo <- rbind(expInfo, expInfoTemp)
}

behData$trials_runs.presentationFrequncy <- as.factor(behData$trials_runs.presentationFrequncy)
expInfo$participant <- as.factor(expInfo$participant)
behData$participant <- as.factor(behData$participant)

behData <- behData  %>% 
  rename(ID = participant)

expInfo <- expInfo  %>% 
  rename(ID = participant)

## Save data ----
save(behData, file = file.path(outputFolder, "data_childrenMR.RData"))
save(expInfo, file = file.path(outputFolder, "expInfo_childrenMR.RData"))
save(demo, file = file.path(outputFolder, "demo.RData"))

write_excel_csv2(behData, file = file.path(outputFolder, "data_childrenMR.csv"))
write_excel_csv2(expInfo, file = file.path(outputFolder, "expInfo_childrenMR.csv"))

# report system ----
report(sessionInfo())

## Clean up workspace ----
remove(dataTemp, expInfoTemp, i, j, k, files, nrTrials, logfile, logfilename, timestamp,
       idxDiff, artOutput)


  