##########################################################
##                     Modelling                        ##
##########################################################

# Load and merge data ------------------------------------
##########################################################

## get the file names ####
subjects <- read_csv(file.path(outputFolder, 'finalSubjects.csv'))
subjects <- subjects$ID

fileNames <- list.files(paste(modelingFolder, sep = "/"), full.names = T, recursive = T)
fileNames <- fileNames %>% 
  keep(str_detect(.,'4_csv')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  keep(str_detect(.,'CBCsimpleRW_CBCdriftDiffusionLR.csv'))

fileNames <- fileNames[sapply(fileNames, function(a) any(str_detect(a, paste(subjects,"_", sep = ""))))]

## read files ####
modelingDataList <- lapply(fileNames, data.table::fread)

## merge list into 1 data frame ####
# creating a new empty data frame
modelingData <- data.frame()

#looping to the data frames, changing variable names so that they can be bound 
#to one data frame
#adding new variables indicating the perception and response model
#converting variables to the correct type to be able to bind data frames
for (i in 1:length(modelingDataList)) {
  
  rm(tempData)
  
  tempData <- modelingDataList[[i]] %>% 
    rename_with(~ "fit_startBelief", matches("^fit.*startBelief$")) %>% 
    rename_with(~ "fit_alpha1", matches("^fit.*alpha1$"))  %>% 
    rename_with(~ "fit_NLL", matches("^fit.*NLL$"))
  
  if (grepl("CBCdriftDiffusion", fileNames[i])) {
    tempData <- tempData %>% 
      rename_with(~ "fit_startingPoint", matches("^fit.*startingPoint$")) %>% 
      rename_with(~ "fit_startingBoundary", matches("^fit.*startingBoundary$")) %>% 
      rename_with(~ "fit_weight", matches("^fit.*weight$")) %>% 
      rename_with(~ "fit_nonDecisionTime", matches("^fit.*nonDecisionTime$"))
  }
  
  #find the name of the perceptual model in the file name and add it as a variable to the data frame
  nPercModel <- which(str_detect(strsplit(fileNames[i],"_")[[1]], "RW") | 
                        str_detect(strsplit(fileNames[i],"_")[[1]], "pearce"))  
  tempData$percModel <- strsplit(fileNames[i],"_")[[1]][nPercModel]
  
  #determine the response model and add it as a variable to the data frame
  if (grepl("pwBelief", fileNames[i])) {
    respModel <- paste(strsplit(fileNames[i],"_")[[1]][nPercModel+1], 
                       strsplit(fileNames[i],"_")[[1]][nPercModel+2],
                       sep = "_")
  } else {
    respModel <- strsplit(fileNames[i],"_")[[1]][nPercModel+1]
  }
  
  if (grepl("csv", respModel)){
    respModel <- strsplit(respModel,"\\.")[[1]][1]
  }
  tempData$respModel <- respModel
  
  tempData <- as.data.frame(tempData)
  
  # converts varaibles to characters to be able to bind the data frames
  columns_to_convert <- c("stimPairLeft", "stimPairRight", "chosenPair", "otherPair")
  tempData[columns_to_convert] <- lapply(tempData[columns_to_convert], as.character)
  
  # binding all data frames in one data frame
  modelingData <- bind_rows(modelingData, tempData)
  
}

## write data ####
write_csv(modelingData, file = paste(outputFolder, "modelling", "modelingData.csv", sep = "/"))
modelingData <- read_csv(paste(outputFolder, "modelling", "modelingData.csv", sep = "/"))

# recalculate NLL for DDM ----
modelingData <- modelingData %>% 
  mutate(corrLik = if_else(is.na(choiceLeft), NA, corrLik))

newNLL <- modelingData %>% 
  group_by(ID, session, percModel, respModel) %>% 
  summarise(CL = mean(corrLik, na.rm=T),
            NLL = if_else(is.na(CL), -sum(log(lik), na.rm = TRUE), -sum(log(corrLik), na.rm = TRUE)),
            .groups = 'drop') %>% 
  select(-CL)

modelingData <- modelingData %>% 
  left_join(., newNLL)
  
remove(newNLL)

# create a summary table ####
# selecting the needed variables, grouping the data and then only taking the first entry for each group
modelingSummary <- modelingData %>% 
  dplyr::select(ID, session, mod2Type, percModel, respModel,
                starts_with("fit"), NLL) %>% 
  group_by(ID, session, percModel, respModel) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(model = paste(percModel, respModel, sep = "_"))

# calculating the mean accuracy for each session
modelingAccuracy <- modelingData %>% 
  group_by(ID, session, percModel, respModel) %>% 
  summarise(accuracy = mean(choiceAccurate, na.rm=T),
            probFB = round(mean(rewardAccurate, na.rm=T),2)) %>% 
  ungroup()

# adding the mean accuracy to the summary table
nparms <- modelingSummary %>% 
  select(ID, session, model, starts_with("fit"), -ends_with("startBelief"),
         -ends_with("startingPoint"), -ends_with("NLL")) %>% 
  mutate(nparms = rowSums(!is.na(select(., -ID, -session, -model))),
         nparms2 = nparms+2) %>% 
  select(ID, session, model, nparms, nparms2)

modelingSummary <- modelingSummary %>% 
  full_join(., modelingAccuracy, by = c("ID", "session", "percModel", "respModel")) %>% 
  left_join(., nparms, by = join_by(ID, session, model)) %>% 
  arrange(ID, session)

modelingSummary <- modelingSummary %>% 
  mutate(AIC = 2*NLL + 2*nparms,
         BIC = 2*NLL + log(44)*nparms,
         AIC2 = 2*NLL + 2*nparms2,
         BIC2 = 2*NLL + log(44)*nparms2)

## write data ####
write_csv(modelingSummary, file = paste(outputFolder, "modelling", "modelingSummary.csv", sep = "/"))

# analyses Modeling ----
# correlation alpha with accuracy
load(file.path(outputFolder, "data_childrenMR.RData"))
load(file.path(outputFolder, "expInfo_childrenMR.RData"))
load(file.path(outputFolder, "demo.RData"))
modelingSummary <- read_csv(file = paste(outputFolder, "modelling", "modelingSummary.csv", sep = "/"))

modelingData$bin[modelingData$trial >=1 & modelingData$trial < 12] <- 1
modelingData$bin[modelingData$trial >11 & modelingData$trial < 23] <- 2
modelingData$bin[modelingData$trial >22 & modelingData$trial < 34] <- 3
modelingData$bin[modelingData$trial >33 & modelingData$trial < 45] <- 4

behData$bin[behData$trials_runs.thisN >=0 & behData$trials_runs.thisN < 11] <- 1
behData$bin[behData$trials_runs.thisN >10 & behData$trials_runs.thisN < 22] <- 2
behData$bin[behData$trials_runs.thisN >21 & behData$trials_runs.thisN < 33] <- 3
behData$bin[behData$trials_runs.thisN >32 & behData$trials_runs.thisN < 44] <- 4

modelingData$bin <- as.factor(modelingData$bin)
behData$bin <- as.factor(behData$bin)

modelSelection <- modelingSummary %>% 
  filter(model == "CBCsimpleRW_CBCdriftDiffusionLR") %>% 
  group_by(ID, mod2Type) %>% 
  summarise(alpha = mean(fit_alpha1),
            nonDecisionTime = mean(fit_nonDecisionTime),
            boundary= mean(fit_startingBoundary),
            mAcc = mean(accuracy)) %>% 
  left_join(.,demo[,c("ID", "age")], by = join_by(ID))

save(modelSelection, file = file.path(outputFolder, "modelSelection.RData"))

# learning rate and modality
alphaLM <- lmer(alpha ~ mod2Type * age + (1|ID), data=modelSelection)

summary(alphaLM)
anova(alphaLM)
report(anova(alphaLM))

lmTable <- nice_table(as.data.frame(report_table(alphaLM)),
                      title = "Linear Mixed Model for Learning Rate and Modality", note = "ABC", 
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

ggplot(modelSelection, aes(mod2Type, alpha, color=mod2Type)) +
  geom_violin() +
  ggtitle("Learning Rate and Modality")

ggsave(file.path(outputFolder, "modelling", "LearningRateModality.png"))

# non-decision time and modality
TerLM <- lmer(nonDecisionTime ~ mod2Type + age + (1|ID), data=modelSelection)
TerLM2 <- lmer(nonDecisionTime ~ mod2Type * age + (1|ID), data=modelSelection)

anova(TerLM, TerLM2)
anova(TerLM)
report(anova(TerLM))
summary(TerLM)

lmTable <- nice_table(as.data.frame(report_table(TerLM)),
                      title = "Linear Mixed Model for non-decision time and Modality", note = "ABC", 
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

modelSelection <- modelSelection %>% 
  mutate(modality = if_else(mod2Type=="aud", "AV","TV"))

ggplot(modelSelection, aes(modality, nonDecisionTime, group=modality, fill = modality)) +
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0, width =0.2) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle("") + 
  ylab("Non-Decision Time") + xlab("Modality") +
  geom_signif(
    comparisons = list(c("AV", "TV")),
    map_signif_level = TRUE,
    y_position = c(1.08), # Adjust y positions for the lines
    annotations = c(".004**"), # Corresponding significance levels
    textsize = 8,
    tip_length = 0,
    vjust = 0,
    color = "black") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits=c(0.2,1.2)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22)  # Legend title
  )

ggsave(file.path(outputFolder, "figures", "nonDecisionTimeModality.png"),
       width = 24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "nonDecisionTimeModality.svg"),
       width = 24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "nonDecisionTimeModality.tif"),
       width = 24, height = 15, units = "cm")

# boundary and modality
boundLM <- lmer(boundary ~ mod2Type + age + (1|ID), data=modelSelection)
boundLM2 <- lmer(boundary ~ mod2Type * age + (1|ID), data=modelSelection)

anova(boundLM, boundLM2)
anova(boundLM)
report(anova(boundLM))
summary(boundLM)

lmTable <- nice_table(as.data.frame(report_table(boundLM)),
                      title = "Linear Mixed Model for Boundary and Modality", note = "ABC", 
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

ggplot(modelSelection, aes(modality, boundary, group=modality, fill = modality)) +
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0, width =0.2) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #geom_jitter(width = 0.1, color = "darkgrey") +
  ggtitle("") +
  ylab("Boundary Separation") + xlab("Modality") +
  geom_signif(
    comparisons = list(c("AV", "TV")),
    map_signif_level = TRUE,
    y_position = c(4.58), # Adjust y positions for the lines
    annotations = c("<.001***"), # Corresponding significance levels
    textsize = 8,
    tip_length = 0,
    vjust = 0,
    color = "black") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits=c(1,5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22)  # Legend title
  )

ggsave(file.path(outputFolder, "figures", "boundaryModality.png"),
       width = 24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "boundaryModality.svg"),
       width = 24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "boundaryModality.tif"),
       width = 24, height = 15, units = "cm")

# drift rate
selectedData <- modelingData %>% 
  filter(respModel == "CBCdriftDiffusionLR") %>% 
  mutate(trial = trial-1) %>%
  left_join(.,behData[,c("ID", "logfile", "trials_runs.thisN", "bin", "outlier200ms", "outlierSD", 
                         "omission", "validTrials", "modality", "trials_runs.correct_answer")],
            by = join_by("ID", "filename"=="logfile", "trial"=="trials_runs.thisN", "bin")) %>% 
  filter(validTrials==1) %>% 
  left_join(.,demo[,c("ID", "age")], by = join_by(ID))

modDataSumm <- selectedData %>% 
  mutate(V1 = choiceLeft*beliefPairNorm + choiceRight*beliefOtherPairNorm,
         V2 = choiceLeft*beliefOtherPairNorm + choiceRight*beliefPairNorm,
         driftRate = (V1-V2)*fit_weight) %>% 
  group_by(ID, modality, age, bin) %>% 
  summarise(mDriftRate = mean(driftRate, na.rm=T),
            absMDriftRate = mean(abs(driftRate), na.rm=T),
            mAcc = mean(trials_runs.correct_answer)) %>% 
  mutate(modality = if_else(modality =="av", "AV", "TV"))

meanDriftFourthsPlot <- selectedData %>% 
  mutate(V1 = choiceLeft*beliefPairNorm + choiceRight*beliefOtherPairNorm,
         V2 = choiceLeft*beliefOtherPairNorm + choiceRight*beliefPairNorm,
         driftRate = (V1-V2)*fit_weight,
         modality = if_else(modality=="av", "AV", "TV")) %>%  
  group_by(modality, bin) %>% 
  summarise(mDriftRate = mean(driftRate, na.rm=T),
            absMDriftRate = mean(abs(driftRate), na.rm=T),
            mAcc = mean(trials_runs.correct_answer))

driftLM <- lmer(absMDriftRate ~ modality + bin + (1|ID), data=modDataSumm)
driftLM2 <- lmer(absMDriftRate ~ modality + bin + age + (1|ID), data=modDataSumm)
driftLM3 <- lmer(absMDriftRate ~ modality + bin + age + modality:age + (1|ID), data=modDataSumm)
driftLM4 <- lmer(absMDriftRate ~ modality + bin + age + modality:age + bin:age + (1|ID), data=modDataSumm)
driftLM5 <- lmer(absMDriftRate ~ modality * bin * age + (1|ID), data=modDataSumm)
lmerTest::step(driftLM5)
anova(driftLM, driftLM2, driftLM3, driftLM4, driftLM5)
summary(driftLM4)  
anova(driftLM4)
report(anova(driftLM4))

lmTable <- nice_table(as.data.frame(report_table(driftLM4)),
                      title = "Linear Mixed Model for Drift Rate and Modality", note = "ABC", 
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

driftLM2PH <- emmeans::emmeans(driftLM4, pairwise ~ bin, data=modDataSumm)
pairs(driftLM2PH)

lmPHTable <- nice_table(as.data.frame(pairs(driftLM2PH)),
                        title = "Post-hoc tests for Drift Rates and Fourths", note = "ABC", 
                        col.format.custom = 2:5, format.custom = "fun2",
                        #col.format.custom = 6, format.custom = "fun3",
                        highlight = T)
lmPHTable
print(lmPHTable, preview = "docx")

ggplot(modDataSumm, aes(bin, absMDriftRate, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8, width = 1.2) +
  geom_boxplot(width = 0.3, alpha = 0) +
  geom_line(data= meanDriftFourthsPlot, aes(x=bin, y = absMDriftRate, group = modality, 
                                            linetype = modality, color = modality), 
            linewidth = 1.2)  +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle("") +
  geom_signif(
    comparisons = list(c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"),
                       c("2", "4"), c("3", "4")),
    map_signif_level = TRUE,
    y_position = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2), # Adjust y positions for the lines
    annotations = c("<.001***", "<.001***", "<.001***", "<.001***", "<.001***",".001**"), # Corresponding significance levels
    textsize = 7,
    tip_length = 0,
    vjust = 0,
    color = "black") +
  ylab("absolute Drift Rate") + 
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22)  # Legend title
  ) 

ggsave(file.path(outputFolder, "figures", "driftRateModalityFourths.png"),
       width=24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "driftRateModalityFourths.svg"),
       width=24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "driftRateModalityFourths.tif"),
       width=24, height = 15, units = "cm")

DRmodAge <- selectedData %>% 
  mutate(V1 = choiceLeft*beliefPairNorm + choiceRight*beliefOtherPairNorm,
         V2 = choiceLeft*beliefOtherPairNorm + choiceRight*beliefPairNorm,
         driftRate = (V1-V2)*fit_weight,
         modality = if_else(modality =="av", "AV", "TV")) %>% 
  group_by(ID, modality, bin, age) %>% 
  summarise(mDriftRate = mean(driftRate, na.rm=T),
            absMDriftRate = mean(abs(driftRate), na.rm=T),
            mAcc = mean(trials_runs.correct_answer))

ggplot(DRmodAge, aes(age, absMDriftRate, color=modality)) +
  geom_point() +
  geom_smooth(method = "lm", linewidth = 1.2, aes(linetype = modality))+
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  facet_grid(~bin) +
  ggtitle("") +
  ylab("absoulte Drift Rate") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  )   

ggsave(file.path(outputFolder, "figures", "driftRateModalityAge.png"),
       width=24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "driftRateModalityAge.svg"),
       width=24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "driftRateModalityAge.eps"),
       width=159, height = 105, units = "mm")

ggsave(file.path(outputFolder, "figures", "driftRateModalityAge.tif"),
       width=159, height = 105, units = "mm")

DRmodAge2 <- selectedData %>% 
  mutate(V1 = choiceLeft*beliefPairNorm + choiceRight*beliefOtherPairNorm,
         V2 = choiceLeft*beliefOtherPairNorm + choiceRight*beliefPairNorm,
         driftRate = (V1-V2)*fit_weight,
         modality = if_else(modality =="av", "AV", "TV")) %>% 
  group_by(ID, modality, age) %>% 
  summarise(mDriftRate = mean(driftRate, na.rm=T),
            absMDriftRate = mean(abs(driftRate), na.rm=T),
            mAcc = mean(trials_runs.correct_answer))

ggplot(DRmodAge2, aes(age, absMDriftRate, color=modality)) +
  geom_point() +
  geom_smooth(method = "lm", linewidth = 1.2, aes(linetype = modality))+
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle("") +
  ylab("absoulte Drift Rate") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  )    

ggsave(file.path(outputFolder, "figures", "driftRateModalityAgeRun.eps"),
       width=159, height = 105, units = "mm")

