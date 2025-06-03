##########################################################
##                      SURPRISE                        ##
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
load(file.path(outputFolder, "ALLbehData.RData"))
load(file.path(outputFolder, "ALLexpInfo.RData"))
load(file.path(outputFolder, "demo.RData"))

load(file.path(outputFolder, "MRdataAd_childrenMR.RData"))
load(file.path(outputFolder, "MRexpInfoAd_childrenMR.RData"))

behData <- behData %>% 
  bind_rows(MRbehDataAd %>% 
              mutate(run = "MR")) %>% 
  filter(run == "MR")
expInfo <- expInfo %>%
  bind_rows(MRexpInfoAd %>% 
              mutate(run = "MR"))%>% 
  filter(run == "MR")
behData %>% 
  group_by(ID, run, modality, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(group, run, modality)

expInfo %>% 
  count(group, run, modality)

subjects <- unique(behData$ID)

################################################################################
# load data --------------------------------------------------------------------
fileNames <- list.files(paste(modelingFolder, sep = "/"), full.names = T, recursive = T)
fileNames <- fileNames %>% 
  keep(str_detect(.,'4_csv')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  discard(str_detect(.,'uniATsimpleRW')) %>% 
  discard(str_detect(.,'uniVsimpleRW')) %>% 
  keep(str_detect(., 'CBCsimpleSurprise'))

fileNames <- fileNames[sapply(fileNames, function(a) any(str_detect(a, paste(subjects,"_", sep = ""))))]

## read files ####
surpriseDataList <- lapply(fileNames, data.table::fread)

## merge list into 1 data frame ####
# creating a new empty data frame
surpriseData <- data.frame()

#looping to the data frames
#converting variables to the correct type to be able to bind data frames
for (i in 1:length(surpriseDataList)) { #length(modelingDataList)
  
  rm(tempData)
  
  tempData <- surpriseDataList[[i]] %>% 
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
  
  if (grepl("CBC_P", fileNames[i])) {
    tempData$group <- "adults"
  } else if (grepl("CBC_1", fileNames[i])) {
    tempData$group <- "children"
  }
  
  # binding all data frames in one data frame
  surpriseData <- bind_rows(surpriseData, tempData)
  
}

################################################################################
# save and read data -----------------------------------------------------------
write_csv(surpriseData, file = paste(outputFolder, "modelling", "surpriseData.csv", sep = "/"))
surpriseData <- read_csv(file = paste(outputFolder, "modelling", "surpriseData.csv", sep = "/"))

# only select logfiles from fMRI output
surpriseData2 <- surpriseData %>% 
  semi_join(expInfo, by = c("filename" = "logfile"))

surpriseData2 %>% 
  group_by(group, ID, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(group, run, mod2Type)

setdiff(unique(expInfo$logfile), unique(surpriseData2$filename))

## save surprise output for fMRI analyses --------------------------------------
surpriseOutputC <- surpriseData2 %>% 
  select(ID, filename, session, trial, mod2Type, stimPair, triplet, matchingPair,
         nonMatchingPair, frequency, reactionTime, version, run, simpleSurprise, 
         nonMatchPairSurprise, group) %>% 
  filter(run == "MR", group == "children")

surpriseOutputC %>% 
  group_by(ID, run, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, mod2Type)

write_csv(surpriseOutputC, file = paste(outputFolder, "modelling", "surpriseOuputfMRI.csv", sep = "/"))

# output for adults
surpriseOutputA <- surpriseData2 %>% 
  select(ID, filename, session, trial, mod2Type, stimPair, triplet, matchingPair,
         nonMatchingPair, frequency, reactionTime, version, run, simpleSurprise, 
         nonMatchPairSurprise, group) %>% 
  filter(run == "MR", group == "adults")

surpriseOutputA %>% 
  group_by(ID, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(mod2Type)

write_csv(surpriseOutputA, file = paste(outputFolder, "modelling", "surpriseOuputfMRI_adults.csv", sep = "/"))

# select important variables
surpriseData <- surpriseData2 %>% 
  select(ID, group, filename, version, run, session, trial, stimPair, triplet, stimPairLeft, stimPairRight,
         mod2Type, frequency, reactionTime, choice, choiceAccurate, reward,
         simpleSurprise, nonMatchPairSurprise) %>% 
  mutate(stimulus = ifelse(!is.na(stimPair), stimPair, triplet),
         surpriseProb = exp(-simpleSurprise),
         surpriseNMProb = exp(-nonMatchPairSurprise),
         frequency = as.factor(frequency))

################################################################################
# plotting surprise against RT - CHILDREN --------------------------------------
surpriseDataC <- surpriseData %>% 
  filter(group == "children") %>% 
  group_by(ID, mod2Type) %>% 
  mutate(outlier200ms = if_else(reactionTime < 0.2, 1, 0),
         outlierSD = if_else(reactionTime < (mean(reactionTime, na.rm = T)-3*sd(reactionTime, na.rm = T)) |
                               reactionTime > (mean(reactionTime, na.rm = T)+3*sd(reactionTime, na.rm = T)),
                             1,0),
         validTrials = if_else(outlier200ms == 1 | outlierSD == 1,0,1),
         validTrials = if_else(is.na(reward),0, validTrials)) %>% 
  ungroup()

sum(surpriseDataC$outlier200ms, na.rm = T)
sum(surpriseDataC$outlierSD, na.rm = T)

surpriseDataAnalyses <- surpriseDataC %>% 
  filter(validTrials == 1) %>% 
  group_by(ID) %>% 
  mutate(meanRT = mean(reactionTime, na.rm = T),
         modality = if_else(mod2Type == "aud", "AV", "TV"),
         version = if_else(version == "v3", "discChoice", "matchRecog")) %>% 
  ungroup()

################################################################################
# statistics -------------------------------------------------------------------
finalSampleC <- read_csv(file.path(outputFolder, 'finalSubjects.csv'))
finalSampleA <- read_csv(file.path(outputFolder, 'finalSubjectsAdults.csv'))

finalSample <- finalSampleC %>% 
  bind_rows(finalSampleA)

surpriseDataAnalyses <- surpriseDataAnalyses %>% 
  left_join(., finalSample[,c("ID", "gender")], by = join_by(ID)) %>% 
  mutate(modality = if_else(mod2Type == "aud", "AV", "TV"))

step(lmer(reactionTime ~ simpleSurprise * modality * version +gender + 
            (1|ID), 
          data = surpriseDataAnalyses),
     keep = "gender")

lmShannon <- lmer(reactionTime  ~ simpleSurprise + modality + version + gender +# main effects
                    (1 | ID) + # random intercept for ID
                    modality:version, # interaction effects
                  data = surpriseDataAnalyses)
summary(lmShannon)
anova(lmShannon)
report(anova(lmShannon))
# SPE = 0.081
# TV = 0.212
# pre = 0.180
# TV*v3 = 0.121
# run*v3 = -0.270

ggplot(surpriseDataAnalyses, aes(x = simpleSurprise, y = reactionTime, group = modality, colour = modality)) +
  geom_point(alpha = 0.5, color = "lightgrey", size = 2, aes(shape = modality)) +
  stat_smooth(method = "lm", fullrange = T, aes(linetype = modality), linewidth = 1.2) +
  facet_grid( ~ version,
              labeller=labeller(version=c("discChoice" = "discriminative choice", 
                                          "matchRecog" = "match recognition"))) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  #ggtitle("") +
  ylab("Reaction Time [s]") + xlab ("Shannon Surprise") +
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6.5)) 

ggsave(file.path(outputFolder, "plots", "Group_RTvsSPE.tif"),
       width = 1.5*15, height = 1.5*6, units = "cm")

## NonMatching surprise
surpriseDataAnalysesV3 <- surpriseDataAnalyses %>% 
  filter(version == "discChoice")

step(lmer(reactionTime ~ nonMatchPairSurprise * modality + gender +
            (1|ID), 
          data = surpriseDataAnalysesV3),
     keep = "gender")

lmNMShannon <- lmer(reactionTime  ~ nonMatchPairSurprise + modality + gender +# main effects
                    (1 | ID), # random intercept for ID
                  data = surpriseDataAnalysesV3)

summary(lmNMShannon)
anova(lmNMShannon)
report(anova(lmNMShannon))
# TV = 0.333

################################################################################
# plotting surprise against RT - Adults --------------------------------------
surpriseDataAvC <- surpriseData %>% 
  filter(version == "v1", run == "MR") %>% 
  group_by(ID, run, mod2Type) %>% 
  mutate(outlier200ms = if_else(reactionTime < 0.2, 1, 0),
         outlierSD = if_else(reactionTime < (mean(reactionTime, na.rm = T)-3*sd(reactionTime, na.rm = T)) |
                               reactionTime > (mean(reactionTime, na.rm = T)+3*sd(reactionTime, na.rm = T)),
                             1,0),
         validTrials = if_else(outlier200ms == 1 | outlierSD == 1,0,1),
         validTrials = if_else(is.na(reward),0, validTrials)) %>% 
  ungroup()

sum(surpriseDataC$outlier200ms, na.rm = T)
sum(surpriseDataC$outlierSD, na.rm = T)

surpriseDataAnalyses2 <- surpriseDataAvC %>% 
  filter(validTrials == 1) %>% 
  group_by(ID) %>% 
  mutate(meanRT = mean(reactionTime, na.rm = T)) %>% 
  ungroup()

################################################################################
# statistics -------------------------------------------------------------------
surpriseDataAnalyses2 <- surpriseDataAnalyses2 %>% 
  left_join(., finalSample[,c("ID", "gender")], by = join_by(ID)) %>% 
  mutate(modality = if_else(mod2Type == "aud", "AV", "TV"))

step(lmer(reactionTime ~ simpleSurprise * modality * group + gender +
            (1|ID), 
          data = surpriseDataAnalyses2),
     keep = "gender")

lmShannon2 <- lmer(reactionTime  ~ simpleSurprise + modality + group + gender +# main effects
                    (1 | ID) + # random intercept for ID
                    modality:group, # interaction effects
                  data = surpriseDataAnalyses2)
summary(lmShannon2)
anova(lmShannon2)
report(anova(lmShannon2))
# SPE = 0.067
# TV = 0.310

max(surpriseDataAnalyses2$reactionTime)
ggplot(surpriseDataAnalyses2, aes(x = simpleSurprise, y = reactionTime, group = modality, colour = modality)) +
  geom_point(alpha = 0.5, color = "grey", size = 2, aes(shape = modality)) +
  #scale_shape_manual(values = c("AV" = 21, "TV" = 24)) +
  stat_smooth(method = "lm", fullrange = T, aes(linetype = modality), linewidth = 1.2) +
  facet_grid(~ group) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  #ggtitle("") +
  ylab("Reaction Time [s]") + xlab ("Shannon Surprise") +
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6.5)) #+

ggsave(file.path(outputFolder, "plots", "Group_RTvsSPE_AvC.tif"),
       width = 1.5*15, height = 1.5*6, units = "cm")

## Clean up workspace ----
finalVars <- ls()
newVars <- setdiff(finalVars, initialVars)
vars2keep <- c("behData", "expInfo")
newVars <- setdiff(newVars, vars2keep)

remove(list = newVars)
