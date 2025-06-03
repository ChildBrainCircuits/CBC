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
initialVars <- ls()

## get the file names ####
load(file.path(outputFolder, "ALLbehDataMR.RData"))
load(file.path(outputFolder, "ALLexpInfoMR.RData"))

behData %>% 
  group_by(ID, modality, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, modality)

expInfo %>% 
  count(run, modality)

subjects <- unique(behData$ID)

################################################################################
# load data --------------------------------------------------------------------
fileNames <- list.files(paste(modelingFolder, sep = "/"), full.names = T, recursive = T)
fileNames <- fileNames %>% 
  keep(str_detect(.,'4_csv')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  discard(str_detect(.,'uniATsimpleRW')) %>% 
  discard(str_detect(.,'uniVsimpleRW')) %>% 
  discard(str_detect(., 'CBCsimpleSurprise'))

fileNames <- fileNames[sapply(fileNames, function(a) any(str_detect(a, paste(subjects,"_", sep = ""))))]

## read files ####
modelingDataList <- lapply(fileNames, data.table::fread)

## merge list into 1 data frame ####
# creating a new empty data frame
modelingData <- data.frame()

#looping to the data frames
#converting variables to the correct type to be able to bind data frames
for (i in 1:length(modelingDataList)) { #length(modelingDataList)
  
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
  
  if (grepl("P2_Pre", fileNames[i])) {
    tempData$run <- "pre"
  } else if (grepl("P2_MR", fileNames[i])) {
    tempData$run <- "MR"
  }
  
  # binding all data frames in one data frame
  modelingData <- bind_rows(modelingData, tempData)
  
}

################################################################################
# only select logfiles from fMRI output
modelingData <- modelingData %>% 
  semi_join(expInfo, by = c("filename" = "logfile"))

modelingData %>% 
  group_by(ID, run, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, mod2Type)

setdiff(unique(expInfo$logfile), unique(modelingData$filename))

# save and read data -----------------------------------------------------------
write_csv(modelingData, file = paste(outputFolder, "modelling", "modellingData.csv", sep = "/"))
modelingData <- read_csv(file = paste(outputFolder, "modelling", "modellingData.csv", sep = "/"))

modelingData <- modelingData %>% 
  filter(fit_percModel != 'CBCpearceHall')

modelingData <- modelingData %>% 
  semi_join(expInfo, by = c("filename" = "logfile"))

modelingData %>% 
  group_by(ID, run, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, mod2Type)

setdiff(unique(expInfo$logfile), unique(modelingData$filename))

unique(modelingData$fit_percModel)

# create a summary table ####
# selecting the needed variables, grouping the data and then only taking the first entry for each group
unique(modelingData$fit_respModel)

modelingSummary <- modelingData %>% 
  dplyr::select(ID, filename, run, version, session, mod2Type, fit_percModel, fit_respModel,
                starts_with("fit"), fit_NLL) %>% 
  filter(version == "v1" | (version == "v3" & fit_respModel == "CBCdriftDiffusionLR_pwBelief")) %>% 
  group_by(ID, run, version, session, fit_percModel, fit_respModel) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(model = paste(fit_percModel, fit_respModel, sep = "_"))

modelingSummary %>% 
  group_by(ID, run, mod2Type) %>% 
  summarise(n = n()) %>% 
  filter(n != 4)

# calculating the mean accuracy for each session
modelingAccuracy <- modelingData %>% 
  group_by(ID, run, version, session, fit_percModel, fit_respModel) %>% 
  filter(version == "v1" | (version == "v3" & fit_respModel == "CBCdriftDiffusionLR_pwBelief")) %>% 
  summarise(accuracy = mean(choiceAccurate, na.rm=T),
            probFB = round(mean(rewardAccurate, na.rm=T),2)) %>% 
  ungroup()

# adding the mean accuracy to the summary table
nparms <- modelingSummary %>% 
  select(ID, run, version, session, model, starts_with("fit"), -ends_with("startBelief"),
         -ends_with("startingPoint"), -ends_with("NLL"), -ends_with("percModel"), 
         -ends_with("respModel")) %>% 
  mutate(nparms = rowSums(!is.na(select(., -ID, -session, -model, -run, -version))),
         nparms2 = nparms+2) %>% 
  select(ID, run, version, session, model, nparms, nparms2)

modelingSummary <- modelingSummary %>% 
  full_join(., modelingAccuracy, by = c("ID", "run", "version", "session", "fit_percModel", "fit_respModel")) %>% 
  left_join(., nparms, by = join_by(ID, run, version, session, model)) %>% 
  arrange(ID, run, session)

modelingSummary <- modelingSummary %>% 
  mutate(AIC = 2*fit_NLL + 2*nparms,
         BIC = 2*fit_NLL + log(44)*nparms,
         AIC2 = 2*fit_NLL + 2*nparms2,
         BIC2 = 2*fit_NLL + log(44)*nparms2)

#################################################################################
# best models per runs and modality
MRmeanBICSubMod <- modelingSummary %>% 
  filter(run == "MR") %>% 
  group_by(ID, run, version, model, mod2Type) %>% 
  summarise(mBIC = mean(BIC))

MRbestModSubMod <- MRmeanBICSubMod %>% 
  group_by(ID, run, version, mod2Type) %>% 
  slice_min(mBIC) %>% 
  ungroup()

MRmodelCountsMod <- MRbestModSubMod %>%
  group_by(version, mod2Type) %>% 
  count(model, name = "count") %>%
  arrange(version, mod2Type, desc(count))

################################################################################
# get best fitting model for MR analyses ---------------------------------------
MRbestModSubMod

bestModRun <- MRbestModSubMod %>% 
  mutate(run = "MR") 

# get the modeling data for best model per run
MRmodelingOutput <- modelingData %>% 
  select(ID, filename, session, trial, mod2Type, stimPair, triplet, frequency, rewardAccurate, reactionTime, 
         choice, choiceAccurate, choiceLeft, choiceRight, reward, beliefPair, beliefPairNorm, beliefOtherPair, 
         beliefOtherPairNorm, rewardPE, starts_with("fit_"), version, run, alphaT) %>% 
  mutate(model = paste(fit_percModel, fit_respModel, sep = "_")) %>% 
  filter(run == "MR") %>% 
  inner_join(MRbestModSubMod,
             by = c("ID", "run", "model", "mod2Type", "version"))

MRmodelingOutput %>% 
  group_by(ID, run, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, mod2Type)

MRmodelingOutput %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(model, version, mod2Type) %>% 
  table

modelingOutput <- modelingData %>% 
  select(ID, filename, session, trial, mod2Type, stimPair, triplet, frequency, rewardAccurate, reactionTime, 
         choice, choiceAccurate, choiceLeft, choiceRight, reward, beliefPair, beliefPairNorm, beliefOtherPair, 
         beliefOtherPairNorm, rewardPE, starts_with("fit_"), version, run, alphaT) %>% 
  mutate(model = paste(fit_percModel, fit_respModel, sep = "_")) %>% 
  filter(run == "MR") %>% 
  inner_join(bestModRun,
             by = c("ID", "run", "model", "mod2Type", "version"))

modelingOutput %>% 
  group_by(ID, run, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, mod2Type)

modelingOutput %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(model, mod2Type, run) %>% 
  table

write_csv(modelingOutput, file = paste(outputFolder, "modelling", "modellingOuputfMRI.csv", sep = "/"))

################################################################################
# look at PE distribution per modality and run ---------------------------------
modelingOutput$version <- as.factor(modelingOutput$version)
modelingOutput$mod2Type <- as.factor(modelingOutput$mod2Type)
modelingOutput$ID <- as.factor(modelingOutput$ID)
modelingOutput$run <- as.factor(modelingOutput$run)

modelingOutput %>% 
  group_by(ID, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(model, mod2Type, version) %>% 
  table

# simple vs 2-step discriminative choice, independent of modality
prop.test(c(27+25, 2+4), c(2*29,2*29))

# simple vs 2-step match recognition choice, independent of modality
prop.test(c(20+23, 8+5), c(2*28,2*28))

# simple vs 2-step across tasks and modality
prop.test(c(27+25+20+23, 2+4+8+5), c(2*57,2*57))

# simple vs 2-step match recognition choice, av
prop.test(c(15+5, 8+0), c(28,28))

# simple vs 2-step match recognition choice, tv
prop.test(c(21+2, 3+2), c(28,28))

################################################################################
modelingOutput

modelingSummary %>% 
  select(model, nparms) %>%
  distinct()

modelingSummaryBestModel <- modelingSummary %>% 
  inner_join(bestModRun,
             by = c("ID", "run", "model", "mod2Type", "version")) %>% 
  mutate(bestModel = str_remove(str_extract(model, "^CBC[^_]+"), "^CBC")) %>% 
  mutate(bestModel = if_else(bestModel %in% c("bothPairsRW", "2StepRW"), 
                             "bothPairs/2StepRW", bestModel), 
         bestModel = if_else(bestModel %in% c("bothPairsAsymRW", "2StepAsymRW"), 
                             "bothPairs/2StepAsymRW", bestModel))

modelingOutput <- modelingOutput %>% 
  mutate(bestModel = str_remove(str_extract(model, "^CBC[^_]+"), "^CBC")) %>% 
  mutate(bestModel = if_else(bestModel %in% c("bothPairsRW", "2StepRW"), 
                             "bothPairs/2StepRW",bestModel), 
         bestModel = if_else(bestModel %in% c("bothPairsAsymRW", "2StepAsymRW"), 
                             "bothPairs/2StepAsymRW", bestModel))

unique(modelingOutput$bestModel)

ggplot(modelingSummaryBestModel, aes(x = mod2Type, y = bestModel, group = ID, color = ID)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +       # Points for AV and TV
  geom_line(aes(group = ID, color = ID), size = 0.8, position = position_dodge(width = 0.2)) + # Connect points per subject/run
  facet_grid(version ~ run) +          # Separate plots by run
  labs(x = "Modality", y = "Model", title = "Model Changes Across Runs and Modalities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

modelingSummaryBestModel <- modelingSummaryBestModel %>%
  mutate(run_modality = paste(run, mod2Type, sep = "_"),
         run_modality = factor(run_modality, 
                               levels = c("pre_aud", "MR_aud",  "pre_tac", "MR_tac")),
         run = factor(run, levels= c("pre", "MR")),
         bestModel = factor(bestModel,
                            levels = c("bothPairs/2StepAsymRW", "bothPairs/2StepRW", 
                                       "pearceHall", "simpleAsymRW","simpleRW"))) %>% # Combine run and modality
  arrange(ID, desc(run), mod2Type)

################################################################################
# statistical analyses ---------------------------------------------------------
modelingOutput <- modelingOutput %>% 
  group_by(ID, version, mod2Type) %>% 
  mutate(alphaStart = if_else(!is.na(fit_delta1), fit_alpha1, NA),
         fit_alpha1 = if_else(!is.na(fit_delta1), mean(alphaT), fit_alpha1)) %>% 
  mutate(outlier200ms = if_else(reactionTime < 0.2, 1, 0),
         outlierSD = if_else(reactionTime > (mean(reactionTime, na.rm = T)+3*sd(reactionTime, na.rm = T)),
                             1,0),
         omission = if_else(is.na(reward), 1, 0),
         validTrials = if_else(outlier200ms==1 | outlierSD == 1 | omission == 1,0,1)) %>% 
  mutate(
    # For v1: split into 3 bins of 14 trials each
    bin = case_when(
      version == "v1" & trial <= 14 ~ 1,
      version == "v1" & trial <= 28 ~ 2,
      version == "v1" & trial <= 42 ~ 3,
      # For v3: split into 3 bins with 15, 15, and 14 trials
      version == "v3" & trial <= 15 ~ 1,
      version == "v3" & trial <= 30 ~ 2,
      version == "v3" & trial <= 44 ~ 3),
    bin = as.factor(bin)) %>% 
  ungroup() %>% 
  arrange(ID, mod2Type) %>% 
  mutate(
    V1 = case_when(
      version == "v3" ~ choiceLeft*beliefPairNorm + choiceRight*beliefOtherPairNorm,
      version == "v1" ~ beliefPair),
    V2 = case_when(
      version == "v3" ~ choiceLeft*beliefOtherPairNorm + choiceRight*beliefPairNorm,
      version == "v1" ~ 1-beliefPair),
    driftRate = (V1-V2)*fit_weight) 

behData <- behData %>% 
  arrange(ID, modality) %>% 
  mutate(omission = if_else(is.na(response_runs.rt), 1, omission),
         validTrials = if_else(outlier200ms==1 | outlierSD == 1 | omission == 1,0,1))

head(modelingOutput$run)
head(behData$run)

all(modelingOutput$ID == behData$ID)
which(round(modelingOutput$reactionTime,3) != round(behData$response_runs.rt,3))

head(modelingOutput$reactionTime)
head(behData$response_runs.rt)

all(which(modelingOutput$outlier200ms==1) == which(behData$outlier200ms==1))
all(which(modelingOutput$outlierSD==1) == which(behData$outlierSD==1))
all(which(modelingOutput$omission==1) == which(behData$omission==1))

modelingDataTrials <- modelingOutput %>% 
  filter(validTrials == 1) 

modelingDataMean <- modelingDataTrials %>% 
  group_by(ID, version, mod2Type) %>% 
  slice(1) %>% 
  select(-c(trial, choiceAccurate, reward, beliefPair, rewardPE, alphaT, 
            reactionTime, outlier200ms, outlierSD, omission, validTrials,
            bin, V1, V2, driftRate)) %>% 
  ungroup()

modelingDataBins <- modelingDataTrials %>% 
  group_by(ID, version, mod2Type, bin) %>% 
  summarise(mDR = mean(driftRate, na.rm = T),
            mAbsDR = mean(abs(driftRate), na.rm = T),
            mRT = mean(reactionTime, na.rm = T),
            mACC = mean(choiceAccurate, na.rm = T)) %>% 
  ungroup()

modelingDataMean$ID <- as.factor(modelingDataMean$ID)
modelingDataMean$mod2Type <- as.factor(modelingDataMean$mod2Type)
modelingDataMean$version <- as.factor(modelingDataMean$version)

# 
subjects <- read_csv(file.path(outputFolder, 'finalSubjects.csv'))

modelingDataMean <- modelingDataMean %>% 
  left_join(., subjects[,c("ID", "gender")], by = join_by(ID)) %>% 
  distinct() %>% 
  mutate(gender = as.factor(gender)) %>% 
  mutate(modality = if_else(mod2Type=="aud", "AV", "TV"),
         version = if_else(version == "v3", "discChoice", "matchRecog"))

modelingDataBins <- modelingDataBins %>% 
  left_join(., subjects[,c("ID", "gender")], by = join_by(ID)) %>% 
  mutate(gender = as.factor(gender)) %>% 
  mutate(modality = if_else(mod2Type=="aud", "AV", "TV"),
         version = if_else(version == "v3", "discChoice", "matchRecog"))

modelingDataMean %>% 
  group_by(version, modality) %>% 
  summarise(meanAlpha = round(mean(fit_alpha1),2),
            sdAlpha = round(sd(fit_alpha1),2),
            meanTer = round(mean(fit_nonDecisionTime),2),
            sdTer = round(sd(fit_nonDecisionTime),2),
            meanWeight = round(mean(fit_weight),2),
            sdWeight = round(sd(fit_weight),2),
            meanBound = round(mean(fit_startingBoundary),2),
            sdBound = round(sd(fit_startingBoundary),2))

modelingDataBins %>% 
  group_by(bin, version, modality) %>% 
  summarise(meanDrift = round(mean(mAbsDR),2),
            sdDrift = round(sd(mAbsDR),2))
  

## learning rate ----
step(lmer(fit_alpha1 ~ modality * version + gender+
            (1 | ID),
          data = modelingDataMean),
     keep = "gender")

lmAlpha <- lmer(fit_alpha1 ~ gender + 
                  (1 | ID),
              data = modelingDataMean)
summary(lmAlpha)
anova(lmAlpha)

ggplot(modelingDataMean, aes(x = run, y = fit_alpha1, fill=modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  facet_grid(~ version) +
  jtools::theme_apa()

## non-decision time ----
step(lmer(fit_nonDecisionTime ~ modality * version +gender +
            (1 | ID),
          data = modelingDataMean),
     keep = "gender")

lmTer <- lmer(fit_nonDecisionTime ~ modality + gender + 
                (1 | ID),
                data = modelingDataMean)
summary(lmTer)
anova(lmTer)
report(anova(lmTer))

ggplot(modelingDataMean, aes(x = version, y = fit_nonDecisionTime, fill=modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_wrap(~ version) + 
  ggtitle("") +
  ylab("Non-Decision Time") + xlab ("Task Version") +
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  # theme(#text = element_text(size = 25),  # Increases all text
  #   axis.title.y = element_text(size = 22), # Axis titles
  #   axis.title.x = element_text(size = 22), # Axis titles
  #   axis.text.y = element_text(size = 20), # Axis titles
  #   axis.text.x = element_text(size = 20), # Axis titles
  #   legend.text = element_text(size = 22),  # Legend text
  #   legend.title = element_text(size = 22),  # Legend title
  #   strip.text.x = element_text(size=22)
  # ) 
  geom_signif(
    y_position = c(2.3, 2.3),
    xmin = c(0.95, 1.95),
    xmax = c(1.05, 2.05),
    annotation = c("<.001", "<.001"),
    # textsize = 7,
    tip_length = 0.015,
    vjust = 0,
    size = 0.5
  )

ggsave(file.path(outputFolder, 'plots', 'NonDecisionTime.png'),
       width = 1.75*7.5, height = 1.75*3.75, units = "cm")

ggsave(file.path(outputFolder, "plots", "NonDecisionTime.tif"),
       width = 1.75*7.5, height = 1.75*3.75, units = "cm")
 
## drift weight ----
step(lmer(fit_weight ~ modality * version + gender +
            (1 | ID),
          data = modelingDataMean),
     keep = "gender")

lmWeight <- lmer(fit_weight ~ gender + 
                   (1 | ID),
                 data = modelingDataMean)
summary(lmWeight)
anova(lmWeight)

ggplot(modelingDataMean, aes(x = run, y = fit_weight, fill=mod2Type)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  facet_wrap(~ version)

## boundary separation ----
step(lmer(fit_startingBoundary ~ modality * version +gender +
            (1 | ID),
          data = modelingDataMean),
     keep = "gender")
lmBoundary<- lmer(fit_startingBoundary ~ gender + 
                    (1 | ID),
                 data = modelingDataMean)
summary(lmBoundary)
anova(lmBoundary)
report(anova(lmBoundary))

ggplot(modelingDataMean, aes(x = version, y = fit_startingBoundary, fill=modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_wrap(~version) +
  ggtitle("") +
  ylab("Boundary Separation") + xlab ("Run") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(1, 5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  )  

ggsave(file.path(outputFolder, 'plots', 'BoundarySeparation.png'),
       width = 15, height = 6, units = "cm")

ggsave(file.path(outputFolder, "plots", "BoundarySeparation.tif"),
       width = 30, height = 12, units = "cm")

## drift rate ----
step(lmer(mAbsDR ~ modality * version * bin + gender +
            (1 | ID),
          data = modelingDataBins),
     keep = "gender")

lmDrift <- lmer(mAbsDR ~ modality + bin + gender +
                  (1 | ID),
                data = modelingDataBins)
summary(lmDrift)
anova(lmDrift)
report(anova(lmDrift))

PH_bin <- emmeans::emmeans(lmDrift, pairwise ~ bin, adjust = "tukey")

nice_table(PH_bin$contrasts) %>% 
  print(., preview = "docx")

annotationsMod <- data.frame(
  group = c("discChoice", "discChoice", "discChoice", "matchRecog", "matchRecog", "matchRecog"),
  start = c(0.95, 1.95, 2.95, 0.95, 1.95, 2.95),
  end = c(1.05, 2.05, 3.05, 1.05, 2.05, 3.05),
  y = c(1.1, 1.8, 2.2, 1.0, 2.1, 2.2),
  label = c("<.001***", "<.001***", "<.001***", "<.001***", "<.001***", "<.001***")
)

annotationsMod

meanAbsDr <- modelingDataBins %>% 
  group_by(version, modality, bin) %>% 
  summarise(meanDR = mean(mAbsDR))

ggplot(modelingDataBins, aes(factor(bin), mAbsDR, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  geom_line(data = meanAbsDr, aes(x = bin, y = meanDR, group = modality, 
                                  color=modality, linetype = modality),
            linewidth = 1) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  facet_grid( ~ version,
              labeller=labeller(version=c("discChoice" = "discriminative choice", 
                                       "matchRecog" = "match recognition"))) +
  #ggtitle("") +
  ylab("Absolute Drift Rate") + xlab ("Bin") +
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + 
  scale_y_continuous(expand = c(0, 0), , limits = c(-0, 3)) + 
  geom_signif(
    comparisons = list(c("1", "3"), c("1", "2"), c("2", "3")),
    map_signif_level = TRUE,
    y_position = c(2.2, 2.4, 2.6), # Adjust y positions for the lines
    annotations = c("<.001***", "<.001***", "<.002**"), # Corresponding significance levels
    # textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.5)

ggsave(file.path(outputFolder, "plots", "absDriftRate.tif"),
       width = 1.5*15, height = 1.5*6, units = "cm")

## Clean up workspace ----
finalVars <- ls()
newVars <- setdiff(finalVars, initialVars)
vars2keep <- c("behData", "expInfo")
newVars <- setdiff(newVars, vars2keep)

remove(list = newVars)
