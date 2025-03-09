##########################################################
##                      PREPARE                         ##
##########################################################
## Description :: 
## Input :::::::: 
## Libraries :::: 
## Output ::::::: 
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
agePlot <- ggplot(subjects, aes(x = age)) +
  geom_histogram(bins = 40, alpha = .8) +
  geom_density(alpha=0.6)+
  theme_bw() +
  ggtitle("Age")
agePlot

ggsave(file.path(outputFolder, 'plots', 'agePlot.png'),agePlot)

mu <- plyr::ddply(subjects, "gender", summarise, grp.mean=mean(age))

ttAge <- t.test(subjects$age[subjects$gender=="f"], subjects$age[subjects$gender=="m"], alternative = "two.sided")
# females are sig older, p = 0.5088
print(ttAge)
report(ttAge)
report_statistics(ttAge)
mean(subjects$age[subjects$gender=="f"])
round(sd(subjects$age[subjects$gender=="f"]),1)
mean(subjects$age[subjects$gender=="m"])
sd(subjects$age[subjects$gender=="m"])

ageGenderPlot <- ggplot(subjects, aes(y = age, x = gender, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  #ggtitle("Age by Gender") +
  scale_fill_viridis_d(option = "viridis", name = "gender") +
  jtools::theme_apa(remove.y.gridlines = F) +
  scale_y_continuous(expand = c(0, 0), limits = c(5, 15)) +
  theme(text = element_text(size = 20), legend.position = "bottom")
ageGenderPlot

ageGenderPlotsig <- ageGenderPlot +
  geom_signif(
    comparisons = list(c("f", "m")),
    map_signif_level = TRUE,
    y_position = c(13.75), # Adjust y positions for the lines
    annotations = c(round(ttAge$p.value,3)), # Corresponding significance levels
    textsize = 5,
    tip_length = 0,
    vjust = 0,
    color = "black"
  )
ageGenderPlotsig

ggsave(file.path(outputFolder, 'figures', 'ageGenderPlot.png'), ageGenderPlotsig,
       width = 20, height = 14, units = "cm")

## grade ----
unique(subjects$grade)
subjects$grade <- factor(subjects$grade, 
                            levels = c("1. KG", "2. KG", "1. Kl", "2. Kl", "3. Kl", "4. Kl", "5. Kl", "6. Kl"),
                            ordered = T)

gradePlot <- ggplot(subjects, aes(x = grade)) +
  geom_bar(alpha = .8) +
  theme_bw() +
  ggtitle("Grade")
gradePlot

ggsave(file.path(outputFolder, 'plots', 'gradePlot.png'), gradePlot)


gradeGenderPlot <- ggplot(subjects, aes(x = grade, color = gender, fill = gender)) +
  geom_bar(alpha = .8, position = "dodge") +
  theme_bw() +
  ggtitle("Grade")
gradeGenderPlot

ggsave(file.path(outputFolder, 'plots', 'gradeGenderPlot.png'), gradeGenderPlot)

lmGenderGrade <- MASS::polr(grade ~ gender, data= subjects)
summary(lmGenderGrade)

(lmGenderGradeTable <- coef(summary(lmGenderGrade)))
p <- pnorm(abs(lmGenderGradeTable[, "t value"]), lower.tail = FALSE) * 2
(lmGenderGradeTable <- cbind(lmGenderGradeTable, "p value" = p))

remove(p,mu)

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
demographics <- read_xlsx(file.path(inputFolder, 'demographics.xlsx'))
medicalHist <- read_xlsx(file.path(inputFolder, 'medicalHist.xlsx'))

#filter only the desired subjects
numVariables <- c("EHI_latQuot", "HISTLANGDEV_diffRisk", "HISTLANGDEV_totRisk", 
                  "SDQ_hyperactivity", "SDQ_total", "CBCL_ADH", "CBCL_tot", "SLRT_words",
                  "SLRT_words_pr", "SLRT_nonwords", "SLRT_nonwords_pr")
facVariables <- c("ID", "auswahl_vp", "klasse", "geschlecht", "EHI_handedness")

demographics <- demographics %>% 
  rename(ID = vp_nr) %>% 
  mutate(across(all_of(numVariables), as.numeric)) %>% 
  mutate(across(all_of(facVariables), as.factor)) %>% 
  filter(ID %in% subjects$ID)

medicalHist <- medicalHist %>% 
  rename(ID = vp_nr) %>% 
  mutate(across(all_of(c("ID", "auswahl_vp", "klasse", "geschlecht")), as.factor)) %>% 
  filter(ID %in% subjects$ID)

## check for missing values ----
demoMissing <- demographics %>% 
  select(ID, klasse, where(is.numeric), -LEAPS_languagesN, -SLRT_mean_pr) %>%   # Identify numeric columns
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
  select(ID, klasse, SDQ_hyperactivity, SDQ_total, CBCL_ADH, CBCL_tot) %>% 
  filter(CBCL_tot > 63) %>% 
  mutate(across(!c(ID, klasse), as.numeric)) %>% 
  mutate(SDQ_hyperactivity = if_else(SDQ_hyperactivity <7, NA, SDQ_hyperactivity),
         SDQ_total = if_else(SDQ_total <17, NA, SDQ_total),
         CBCL_ADH = if_else(CBCL_ADH <70, NA, CBCL_ADH),
         CBCL_tot = if_else(CBCL_tot <64, NA, CBCL_tot)) %>% 
  rename('SDQ_hyperactivity [>6]' = SDQ_hyperactivity,
         'SDQ_total [>16]' = SDQ_total,
         'CBCL_ADH [>69]' = CBCL_ADH,
         'CBCL_tot [>63]' = CBCL_tot)


## only select desired variables
demo <- demographics %>% 
  select(-c(speechDisorder, speechDisorderType, LEAPS_languages, LEAPS_languagesN,
            HISTLANGDEV_diffRisk, HISTLANGDEV_totRisk, SLRT_words, SLRT_nonwords,
            SLRT_mean_pr, SLRT_words_pr, SLRT_nonwords_pr)) %>% 
  mutate(geburtsdatum = as.Date(demographics$geburtsdatum, format="%Y-%m-%d"),
         mri_children_date = as.Date(demographics$mri_children_date, format="%Y-%m-%d"),
         age = round(as.numeric(difftime(demographics$mri_children_date,demographics$geburtsdatum, units="weeks"))/52.25, digits = 1))
  
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

# double check bad scans after rerunning preprocessing
artOutput <- read_xlsx(file.path(inputFolder, "badscans_allBad_v2.xlsx")) %>% 
  filter(logfile %in% finalSample$logfile)

(idxDiff <- which(artOutput$badScans_pr != finalSample$badScans_pr))

finalSample$badScans_pr[88] <- artOutput$badScans_pr[88]
finalSample$badScans_pr[130] <- artOutput$badScans_pr[130]

all.equal(artOutput$badScans_pr, finalSample$badScans_pr)


# report system ----
report(sessionInfo())

## Clean up workspace ----
remove(dataTemp, expInfoTemp, i, j, k, files, nrTrials, logfile, logfilename, timestamp,
       idxDiff, artOutput)


  