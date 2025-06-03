##########################################################
##           Analyse Adults vs Kids Modelling           ##
##########################################################
## Description :: 
## Input :::::::: 
## Libraries :::: 
## Output ::::::: 
##########################################################

initialVars <- ls()

################################################################################
# read data ----
modelingOutputAd <- read_csv(paste(outputFolder, "modelling", "modellingOuputfMRI_adults.csv", sep = "/"))
allModelingDataAd <- read_csv(paste(outputFolder, "modelling", "modellingDataAdults.csv", sep = "/"))

modelingOutputAd$group <- "adults"
allModelingDataAd$group <- "adults"

modelingOutputCh <- read_csv(paste(outputFolder, "modelling", "modellingOuputfMRI.csv", sep = "/"))
allModelingDataCh <- read_csv(paste(outputFolder, "modelling", "modellingData.csv", sep = "/"))

modelingOutputChV1 <- modelingOutputCh %>% 
  mutate(group = "children") %>% 
  filter(run == "MR", version == "v1")

modelingOutputChV1 %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(mod2Type) %>% 
  table

modelingOutputChV3 <- modelingOutputCh %>% 
  mutate(group = "children") %>% 
  filter(run == "MR", version == "v3")

modelingOutputChV3 %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(mod2Type) %>% 
  table

allModelingDataCh <- allModelingDataCh %>% 
  mutate(group = "children") %>% 
  filter(run == "MR", version == "v1")

modelingOutputAd %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(mod2Type) %>% 
  table

# model fitting ----
## adults ----
modelingOutputAd %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(model, mod2Type) %>% 
  table

# is the 2-step model more likely in auditory than tactile?
prop.test(c(15, 8), c(28, 28)) # p = 0.1032

# is the 2-step model more likely than the simple model across both modalities
prop.test(c(15+8, 13+20), c(2*28,2*28)) # p = 0.08897

# is the 2-step model more likely for just auditory runs
prop.test(c(15, 13), c(28, 28))  # p = 0.7893

# is the 2-step model more likely for just tactile runs
prop.test(c(8, 20), c(28, 28))  # p = 0.003283

## match recog children ----
modelingOutputChV1 %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(model, mod2Type) %>% 
  table

# simple vs 2-step match recognition choice, independent of modality
prop.test(c(20+23, 8+5), c(2*28,2*28))

# simple vs 2-step match recognition choice, av
prop.test(c(15+5, 8+0), c(28,28))

# simple vs 2-step match recognition choice, tv
prop.test(c(21+2, 3+2), c(28,28))

## disc choice ---- 
modelingOutputChV3 %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(model, mod2Type) %>% 
  table

# simple vs 2-step discriminative choice, independent of modality
prop.test(c(27+25, 2+4), c(2*29,2*29))

# simple vs 2-step across tasks and modality
prop.test(c(27+25+20+23, 2+4+8+5), c(2*57,2*57))

###############################################################################
# load data ----
load(file = file.path(outputFolder, "MRdata_childrenMR.RData"))

MRbehDataChV1 <- MRbehData %>% 
  filter(version == "v1")

MRbehDataChV3 <- MRbehData %>% 
  filter(version == "v3")

load(file = file.path(outputFolder, "MRdataAd_childrenMR.RData"))

remove(MRbehData)

behDataAvC <- MRbehDataChV1 %>% 
  bind_rows(MRbehDataAd) %>% 
  mutate(trial = trials_runs.thisN+1) %>% 
  select(-participant, -trials_runs.correctKey, -trials_runs.visual_stim_right) %>% 
  arrange(ID, modality)

behDataV1vV3 <- MRbehDataChV1 %>% 
  bind_rows(MRbehDataChV3) %>% 
  mutate(trial = trials_runs.thisN+1) %>% 
  select(-trials_runs.correctKey, -trials_runs.visual_stim_right) %>% 
  arrange(ID, modality)
  
################################################################################
# prepare data Adults vs. children ----
modelingDataAvC <- modelingOutputChV1 %>% 
  bind_rows(modelingOutputAd) %>% 
  select(ID, filename, run, group, version, mod2Type, model, trial, choiceAccurate, 
         reward, beliefPair, rewardPE, fit_alpha1, fit_nonDecisionTime, fit_weight,
         fit_startingBoundary, fit_delta1, alphaT, reactionTime) %>%
  rename(logfile = filename) %>% 
  group_by(ID, mod2Type) %>% 
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
    V1 = beliefPair,
    V2 = 1-beliefPair,
    driftRate = (V1-V2)*fit_weight) 

all(which(modelingDataAvC$outlier200ms==1) == which(behDataAvC$outlier200ms==1))
all(which(modelingDataAvC$outlierSD==1) == which(behDataAvC$outlierSD==1))
all(which(modelingDataAvC$omission==1) == which(behDataAvC$omission==1))

data.frame(rtModeling = modelingDataAvC$reactionTime,
           rtBehData = behDataAvC$response_runs.rt) %>% 
  mutate(diff = round(rtModeling, 4) - round(rtBehData,4)) %>% 
  filter(diff != 0)

modelingDataAvCTrials <- modelingDataAvC %>% 
  filter(validTrials == 1) 

modelingDataAvCMean <- modelingDataAvCTrials %>% 
  group_by(ID, mod2Type) %>% 
  slice(1) %>% 
  select(-c(trial, choiceAccurate, reward, beliefPair, rewardPE, alphaT, 
            reactionTime, outlier200ms, outlierSD, omission, validTrials,
            bin, V1, V2, driftRate)) %>% 
  ungroup()

modelingDataAvCBins <- modelingDataAvCTrials %>% 
  group_by(ID, mod2Type, group, bin) %>% 
  summarise(mDR = mean(driftRate, na.rm = T),
            mAbsDR = mean(abs(driftRate), na.rm = T),
            mRT = mean(reactionTime, na.rm = T),
            mACC = mean(choiceAccurate, na.rm = T)) %>% 
  ungroup()

modelingDataAvCMean$ID <- as.factor(modelingDataAvCMean$ID)
modelingDataAvCMean$mod2Type <- as.factor(modelingDataAvCMean$mod2Type)
modelingDataAvCMean$group <- as.factor(modelingDataAvCMean$group)

subjects <- read_csv(file.path(outputFolder, 'finalSubjects.csv'))
subjectsAdults <- read_csv(file.path(outputFolder, 'finalSubjectsAdults.csv'))

subsV1 <- subjectsAdults %>% 
  bind_rows(subjects) %>% 
  filter(version == "v1")

modelingDataAvCMean <- modelingDataAvCMean %>% 
  left_join(., subsV1[,c("ID", "gender")], by = join_by(ID)) %>% 
  distinct() %>% 
  mutate(gender = as.factor(gender)) %>% 
  mutate(modality = if_else(mod2Type=="aud", "AV", "TV"))

modelingDataAvCBins <- modelingDataAvCBins %>% 
  left_join(., subsV1[,c("ID", "gender")], by = join_by(ID)) %>% 
  mutate(gender = as.factor(gender)) %>% 
  mutate(modality = if_else(mod2Type=="aud", "AV", "TV"))

modelingDataAvCMean %>% 
  group_by(group, version, modality) %>% 
  summarise(meanAlpha = round(mean(fit_alpha1),2),
            sdAlpha = round(sd(fit_alpha1),2),
            meanTer = round(mean(fit_nonDecisionTime),2),
            sdTer = round(sd(fit_nonDecisionTime),2),
            meanWeight = round(mean(fit_weight),2),
            sdWeight = round(sd(fit_weight),2),
            meanBound = round(mean(fit_startingBoundary),2),
            sdBound = round(sd(fit_startingBoundary),2))

modelingDataAvCBins %>% 
  group_by(bin, group, modality) %>%
  summarise(meanDrift = round(mean(mAbsDR),2),
            sdDrift = round(sd(mAbsDR),2))

## alpha 1 lea## alpha 1 learning rate ----
modelingDataMean <- modelingDataAvCMean

step(lmer(fit_alpha1 ~ modality * group + gender+
            (1 | ID),
          data = modelingDataMean),
     keep = c("gender"))

lmAlpha <- lmer(fit_alpha1 ~ gender + group +
                  (1 | ID),
                data = modelingDataMean)
summary(lmAlpha)
anova(lmAlpha)
report(anova(lmAlpha))

ggplot(modelingDataMean, aes(x = group, y = fit_alpha1, fill=modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_grid(cols = vars(group)) +
  jtools::theme_apa()

## non-decision time ---
step(lmer(fit_nonDecisionTime ~ modality * group + gender +
            (1 | ID),
          data = modelingDataMean),
     keep = "gender")

lmTer <- lmer(fit_nonDecisionTime ~ modality + group + gender + 
                  (1 | ID),
                data = modelingDataMean)
summary(lmTer)
anova(lmTer)
report(anova(lmTer))

ggplot(modelingDataMean, aes(x = group, y = fit_nonDecisionTime, fill=modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_wrap(~ version) + 
  #ggtitle("") +
  ylab("Non-Decision Time") + xlab ("Age Group") +
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  theme(legend.position = "None") +
  geom_signif(
    comparisons = list(c("adults", "children")),
    y_position = 2.15,
    tip_length = 0,
    size = 0.5,
    annotation = c(".004**")
  ) 

ggsave(file.path(outputFolder, "plots", "NonDecisionTime_AvC.tif"),
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")

## drift weight ---
step(lmer(fit_weight ~ modality * group + gender +
            (1 | ID),
          data = modelingDataMean),
     keep = "gender")

lmWeight <- lmer(fit_weight ~ group + gender +
                (1 | ID),
              data = modelingDataMean)
summary(lmWeight)
anova(lmWeight)
report(anova(lmWeight))

ggplot(modelingDataMean, aes(x = group, y = fit_weight, fill=modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_wrap(~ version) + 
  #ggtitle("") +
  ylab("Drift Weight") + xlab ("Age Group") +
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + scale_y_continuous(expand = c(0, 0), limits = c(0, 16)) +
  geom_signif(
    comparisons = list(c("adults", "children")),
    y_position = 13.5,
    tip_length = 0,
    size = 0.5,
    annotation = c("<.001***")
  )

ggsave(file.path(outputFolder, "plots", "DriftWeight_AvC.tif"),
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")

## boundary ---
step(lmer(fit_startingBoundary ~ modality * group +gender +
            (1 | ID),
          data = modelingDataMean),
     keep = "gender")
lmBound <- lmer(fit_startingBoundary ~ gender +
                   (1 | ID),
                 data = modelingDataMean)
summary(lmBound)
anova(lmBound)
report(anova(lmBound))

## drift rate ----
modelingDataBins <- modelingDataAvCBins

step(lmer(mAbsDR ~ modality * group * bin + gender +
            (1 | ID),
          data = modelingDataBins),
     keep = "gender")

lmDR <- lmer(mAbsDR ~ modality + group + bin + gender +
               modality:group + group:bin +
                  (1 | ID),
                data = modelingDataBins)
summary(lmDR)
anova(lmDR)
report(anova(lmDR))

PH_bin <- emmeans::emmeans(lmDR, pairwise ~ bin | group, adjust = "tukey")
PH_group <- emmeans::emmeans(lmDR, pairwise ~ group | bin, adjust = "tukey")
PH_mod <- emmeans::emmeans(lmDR, pairwise ~ modality | group, adjust = "tukey")
PH_mod2 <- emmeans::emmeans(lmDR, pairwise ~ group | modality, adjust = "tukey")

PH_bin
PH_group
PH_mod
PH_mod2

nice_table(PH_bin$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_group$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_mod$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_mod2$contrasts) %>% 
  print(., preview = "docx")

annotationsMod <- data.frame(
  group = c("adults", "adults", "adults", "children", "children", "children"),
  start = c(1, 1, 2, 1, 1, 2),
  end =   c(2, 3, 3, 2, 3, 3),
  y =     c(3.2, 3.7, 4.2, 2.0, 2.5, 3.0),
  label = c("<.001***", "<.001*** ", "<.001***  ", ".001***", "<.001***", ".285")
)

annotationsMod

meanAbsDr <- modelingDataBins %>% 
  group_by(group, modality, bin) %>% 
  summarise(meanDR = mean(mAbsDR))

ggplot(modelingDataBins, aes(x = bin, y = mAbsDR, fill=modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  geom_line(data = meanAbsDr, aes(x = bin, y = meanDR, group = modality, 
                                  color=modality, linetype = modality),
            linewidth = 1) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  facet_wrap(~group) +
  #ggtitle("") +
  ylab("Absolute Drift Rate") + xlab ("Bin") +
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + scale_y_continuous(expand = c(0, 0), , limits = c(-0, 4.5)) + 
  geom_signif(
    data = annotationsMod,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    inherit.aes = FALSE,
    # textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.5,
    manual = T
  )

ggsave(file.path(outputFolder, "plots", "meanDrift_AvC.tif"),
       width = 1.5*15, height = 1.5*6, units = "cm")

## Clean up workspace ----
finalVars <- ls()
newVars <- setdiff(finalVars, initialVars)
vars2keep <- c("behData", "expInfo")
newVars <- setdiff(newVars, vars2keep)

remove(list = newVars)

