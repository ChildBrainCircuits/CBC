##########################################################
##                        PREPARE                       ##
##########################################################
## Description :: 
## Input :::::::: 
## Libraries :::: 
## Output ::::::: 
##########################################################

# Load and merge data ------------------------------------
##########################################################

## get the file names ####
subjects <- list.files(path = modelingFolder, pattern = "CBC_")
subjects <- subjects[nchar(subjects)==8 & subjects!="CBC_555" & subjects!="CBC_ϩ"] #simulated subjects have shorter names

fileNames <- list.files(paste(modelingFolder, sep = "/"), full.names = T, recursive = T)
fileNames <- fileNames %>% 
  keep(str_detect(.,'4_csv')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  keep(str_detect(.,'CBCsimpleRW_CBCdriftDiffusionLR.'))

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
    as.data.frame()
  
  if (grepl("v1", fileNames[i])) {
    tempData$version <- "v1"
    
  } else if (grepl("v3", fileNames[i])) {
    tempData$version <- "v3"
    # converts variables to characters to be able to bind the data frames
    columns_to_convert <- c("stimPairLeft", "stimPairRight", "chosenPair", "otherPair")
    tempData[columns_to_convert] <- lapply(tempData[columns_to_convert], as.character)
  }
  
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
  group_by(ID, session, fit_percModel, fit_fit_respModel) %>% 
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
  dplyr::select(ID, session, mod2Type, fit_percModel, fit_respModel,
                starts_with("fit"), NLL) %>% 
  group_by(ID, session, fit_percModel, fit_respModel) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(model = paste(fit_percModel, fit_respModel, sep = "_"))

# calculating the mean accuracy for each session
modelingAccuracy <- modelingData %>% 
  group_by(ID, session, fit_percModel, fit_respModel) %>% 
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
  full_join(., modelingAccuracy, by = c("ID", "session", "fit_percModel", "fit_respModel")) %>% 
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
modelSelection %>% 
  group_by(mod2Type) %>% 
  summarise(mAlpha = mean(alpha),
            sdAlpha = sd(alpha))

step(lmer(alpha ~ mod2Type * age + (1|ID), data=modelSelection))
summary(lmer(alpha ~ 1 + (1|ID), data=modelSelection))
report(lmer(alpha ~ 1 + (1|ID), data=modelSelection))

alphaLM <- lmer(alpha ~ mod2Type * age + (1|ID), data=modelSelection)

summary(alphaLM)
anova(alphaLM)
report(anova(alphaLM))

lmTable <- nice_table(as.data.frame(report_table(alphaLM)),
                      title = "Linear Mixed Model for Learning Rate and Modality", note = "ABC", 
                      #col.format.custom = c(2:6, 11:13), format.custom = "fun",
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

ggplot(modelSelection, aes(mod2Type, alpha, color=mod2Type)) +
  geom_violin() +
  ggtitle("Learning Rate and Modality")

ggsave(file.path(outputFolder, "modelling", "LearningRateModality.png"))

# non-decision time and modality
modelSelection %>% 
  group_by(mod2Type) %>% 
  summarise(mTer = mean(nonDecisionTime),
            sdTer = sd(nonDecisionTime))
step(lmer(nonDecisionTime ~ mod2Type * age + (1|ID), data=modelSelection))

TerLM <- lmer(nonDecisionTime ~ mod2Type + age + (1|ID), data=modelSelection)
TerLM2 <- lmer(nonDecisionTime ~ mod2Type * age + (1|ID), data=modelSelection)

anova(TerLM, TerLM2)
anova(TerLM)
report(anova(TerLM))
summary(TerLM)

lmTable <- nice_table(as.data.frame(report_table(TerLM)),
                      title = "Linear Mixed Model for non-decision time and Modality", note = "ABC", 
                      #col.format.custom = c(2:6, 11:13), format.custom = "fun",
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

modelSelection <- modelSelection %>% 
  mutate(modality = if_else(mod2Type=="aud", "AV","TV"))

ggplot(modelSelection, aes(modality, nonDecisionTime, group=modality, fill = modality)) +
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0, width =0.2) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #geom_jitter(width = 0.1) +
  ggtitle("") + 
  ylab("Non-Decision Time [s]") + xlab("Modality") +
  geom_signif(
    comparisons = list(c("AV", "TV")),
    map_signif_level = TRUE,
    y_position = c(2.5), # Adjust y positions for the lines
    annotations = c(".003**"), # Corresponding significance levels
    textsize = 8,
    tip_length = 0,
    vjust = 0,
    color = "black") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits=c(0,3.0)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22)  # Legend title
  )

ggsave(file.path(outputFolder, "figures", "nonDecisionTimeModality.svg"),
       width = 24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "nonDecisionTimeModality.tif"),
       width = 24, height = 15, units = "cm")

ggplot(modelSelection, aes(age, nonDecisionTime, color=modality)) +
  geom_point() +
  geom_smooth(method = "lm", aes(linetype = modality), se = T, linewidth = 1.2) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  #scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle("") + 
  ylab("Non-Decision Time [s]") + xlab("Age") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0,3)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  ) 

ggsave(file.path(outputFolder, "figures", "nonDecisionTimeModalityAge.svg"),
       width = 24, height = 15, units = "cm")

ggsave(file.path(outputFolder, "figures", "nonDecisionTimeModalityAge.tif"),
       width = 24, height = 15, units = "cm")

# boundary and modality
modelSelection %>% 
  group_by(mod2Type) %>% 
  summarise(mBS = mean(boundary),
            sdBS = sd(boundary))

step(lmer(boundary ~ mod2Type * age + (1|ID), data=modelSelection)) 

boundLM <- lmer(boundary ~ 1 + (1|ID), data=modelSelection)

anova(boundLM)
report(anova(boundLM))
report(boundLM)

lmTable <- nice_table(as.data.frame(report_table(boundLM)),
                      title = "Linear Mixed Model for Boundary and Modality", note = "ABC", 
                      #col.format.custom = c(2:6, 11:13), format.custom = "fun",
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
  # geom_signif(
  #   comparisons = list(c("AV", "TV")),
  #   map_signif_level = TRUE,
  #   y_position = c(4.58), # Adjust y positions for the lines
  #   annotations = c("<.001***"), # Corresponding significance levels
  #   textsize = 8,
  #   tip_length = 0,
  #   vjust = 0,
  #   color = "black") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits=c(1,5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22)  # Legend title
  )

# ggsave(file.path(outputFolder, "figures", "boundaryModality.svg"),
#        width = 24, height = 15, units = "cm")
# 
# ggsave(file.path(outputFolder, "figures", "boundaryModality.tif"),
#        width = 24, height = 15, units = "cm")

# drift rate
selectedData <- modelingData %>% 
  filter(fit_respModel == "CBCdriftDiffusionLR") %>% 
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

modDataSumm %>% 
  group_by(modality, bin) %>% 
  summarise(mDR = mean(absMDriftRate),
            sdDR = sd(absMDriftRate))

step(lmer(absMDriftRate ~ modality * bin * age + (1|ID), data=modDataSumm)) 

driftLM <- lmer(absMDriftRate ~ modality + bin + age + modality:age + bin:age + (1|ID), data=modDataSumm)

summary(driftLM)  
anova(driftLM)
report(anova(driftLM))

driftLM2PH <- emmeans::emmeans(driftLM, pairwise ~ bin, data=modDataSumm)
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
    annotations = c("<.001***", "<.001***", "<.001***", "<.001***", "<.001***",".002**"), # Corresponding significance levels
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
  #geom_violin() +
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
  #geom_violin() +
  geom_point() +
  geom_smooth(method = "lm", linewidth = 1.2, aes(linetype = modality))+
  #stat_cor(digits = 2, p.accuracy = .001)+
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_grid(~bin) +
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

