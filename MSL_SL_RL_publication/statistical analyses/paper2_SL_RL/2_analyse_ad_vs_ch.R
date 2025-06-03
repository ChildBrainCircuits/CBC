##########################################################
##               Analyse Adults vs Kids                 ##
##########################################################
## Description :: 
## Input :::::::: 
## Libraries :::: 
## Output ::::::: 
##########################################################

initialVars <- ls()

# load data ----
load(file.path(outputFolder, "MRdata_childrenMR.RData"))
load(file.path(outputFolder, "MRexpInfo_childrenMR.RData"))

load(file.path(outputFolder, "MRexpInfoAd_childrenMR.RData"))
load(file.path(outputFolder, "PREdataAd_childrenMR.RData"))

load(file.path(outputFolder, "demo.RData"))
load(file.path(outputFolder, "finalSample.RData"))
load(file.path(outputFolder, "finalSampleAdults.RData"))


behDataC <- MRbehData %>% 
  filter(ID != "CBC_1089", 
         version == "v1") %>% 
  left_join(., demo[,c("ID", "age", "geschlecht")], by = join_by(ID)) 

expInfoC <- MRexpInfo %>% 
  filter(ID != "CBC_1089", 
         version == "v1") %>% 
  left_join(., demo[,c("ID", "age", "geschlecht")], by = join_by(ID)) 

demoC <- demo %>% 
  filter(ID != "CBC_1089", 
         mri_children_msi_version == "1 Rakete/V1")

length(unique(behDataC$ID))

finalSampleAdults <- finalSampleAdults %>% 
  mutate(geschlecht = if_else(geschlecht=="f", "Weiblich", "Männlich"))

behDataA <- MRbehDataAd %>% 
  select(-participant) %>% 
  left_join(., finalSampleAdults[,c("ID", "geschlecht", "age")], 
            by = join_by(ID), relationship = "many-to-many")
expInfoA <- MRexpInfoAd %>% 
  select(-participant) %>% 
  left_join(., finalSampleAdults[,c("ID", "geschlecht", "age")], 
            by = join_by(ID), relationship = "many-to-many")

behData <- behDataC %>% 
  bind_rows(behDataA)

expInfo <- expInfoC %>% 
  bind_rows(expInfoA)

# add bins
behData <- behData %>%
  group_by(ID, session) %>%
  rename(trial = trials_runs.thisN) %>% 
  mutate(
    # For v1: split into 3 bins of 14 trials each
    bin = case_when(
      version == "v1" & trial <= 13 ~ 1,
      version == "v1" & trial <= 27 ~ 2,
      version == "v1" & trial <= 41 ~ 3,
      # For v3: split into 3 bins with 15, 15, and 14 trials
      version == "v3" & trial <= 14 ~ 1,
      version == "v3" & trial <= 29 ~ 2,
      version == "v3" & trial <= 43 ~ 3)) %>% 
  ungroup() %>% 
  mutate(run = "MR")

meanOmOutl <- expInfo %>% 
  group_by(group, ID, version, modality) %>% 
  summarise(meanOm = mean(omissions, na.rm=T)*100,
            meanOutl = mean(outliers, na.rm=T)*100)

meanPerformance <- behData %>% 
  group_by(group, ID, version, modality) %>% 
  summarise(meanAcc = mean(trials_runs.correct_answer, na.rm=T),
            # sdAcc = sd(trials_runs.correct_answer, na.rm=T),
            # iqrAcc = IQR(trials_runs.correct_answer, na.rm=T),
            meanRT = mean(response_runs.rt, na.rm = T),
            # sdRT = sd(response_runs.rt, na.rm = T),
            meanRTCorr = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T)) %>% 
  # sdRTCorr = sd(response_runs.rt, na.rm = T)) 
  left_join(., meanOmOutl, by = join_by(group, ID, version, modality))

performanceTable <- meanPerformance %>% 
  group_by(version, group, modality) %>% 
  summarise(mAcc = mean(meanAcc)*100,
            sdAcc = sd(meanAcc)*100,
            medianAcc = median(meanAcc)*100,
            iqrAcc = IQR(meanAcc)*100,
            mRT = mean(meanRT),
            sdRT = sd(meanRT),
            mRTcorr = mean(meanRTCorr),
            sdRTcorr = sd(meanRTCorr),
            mOm = mean(meanOm),
            sdOm = sd(meanOm),
            mOutl = mean(meanOutl),
            sd = sd(meanOutl)) %>% 
  ungroup() %>%
  pivot_longer(cols = -c(version, group, modality), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = c(version, group, modality), values_from = value) %>% 
  nice_table(title = "MR Task Performance", note = "ABC", 
             col.format.custom = 2:3, format.custom = "fun")
performanceTable

# prepare for analyses ---------------------------------------------------------
behDataCorrTrials <- behData %>% 
  filter(validTrials == 1,
         trials_runs.correct_answer == 1) %>% 
  mutate(RT = response_runs.rt) %>% 
  mutate(ID = as.factor(ID),
         version = as.factor(version),
         modality = as.factor(modality),
         bin = as.factor(bin),
         modality = if_else(modality == "av", "AV", "TV"),
         task = if_else(version == "v1", "hard", "easy"))

behDataBinsModality <- behData %>% 
  filter(validTrials == 1) %>% 
  group_by(ID, geschlecht, group, version, modality, bin) %>% 
  summarise (RT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
             ACC = mean(trials_runs.correct_answer, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ID = as.factor(ID),
         version = as.factor(version),
         bin = as.factor(bin),
         modality = if_else(modality == "av", "AV", "TV"),
         task = if_else(version == "v1", "hard", "easy"),
         group = as.factor(group),
         modality = as.factor(modality),
         task = as.factor(task),
         geschlecht = as.factor(geschlecht))

meanRTBinsPlot <- behData %>% 
  group_by(version, group, modality, bin) %>% 
  summarise(mRT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
            mACC = mean(trials_runs.correct_answer, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(modality = if_else(modality == "av", "AV", "TV"),
         task = if_else(version == "v1", "hard", "easy"))

################################################################################
# analyse 
# RT depending on task version, group, modality, bin
step(lmer(RT ~ group * modality * bin +
            (1|ID), 
          data = behDataBinsModality))

lmRT <- lmer(RT ~ group + modality + bin + geschlecht +# main effects
               (1 | ID) + # random intercept for ID
               group:modality + group:bin, # interaction effects
             data = behDataBinsModality)
summ <- summary(lmRT)
summ
anova(lmRT)
report(anova(lmRT))

effectsize::effectsize(anova(lmRT), partial = F, type="eta")

(PH_group <- emmeans::emmeans(lmRT, pairwise ~ bin | group, adjust = "tukey"))
(PH_group2 <- emmeans::emmeans(lmRT, pairwise ~ group | bin, adjust = "tukey"))
(PH_mod <- emmeans::emmeans(lmRT, pairwise ~ modality | group, adjust = "tukey"))
(PH_mod2 <- emmeans::emmeans(lmRT, pairwise ~  group | modality, adjust = "tukey"))

nice_table(PH_group$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_group2$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_mod$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_mod2$contrasts) %>% 
  print(., preview = "docx")

RTplot <- ggplot(behDataBinsModality, aes(x = bin, y = RT, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0) +
  geom_line(data= meanRTBinsPlot, aes(x=bin, y = mRT, group = modality, 
                                      linetype = modality, color = modality), 
            linewidth = 1)  +
  facet_grid( ~ group) +  # 2x4 grid: run (rows), version (columns)
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ylab("Reaction Time [s]") + xlab("Bin") + 
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + scale_y_continuous(expand = c(0, 0), limits = c(0, 4.5)) +
  theme(legend.position = "None",
        strip.text.x = element_text(size = 10.5))
RTplot

annotations <- data.frame(
  group = c("adults", "adults", "adults", "children"),
  start = c("1", "1", "2", "1"),
  end = c("2", "3", "3", "3"),
  y = c(3.1, 3.6, 4.1, 3.6),
  label = c("<.001***", "<.001***", ".022*", ".029*")
)

annotations

RTplotsig <- RTplot +
  geom_signif(
    data = annotations,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    inherit.aes = FALSE,
    # textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.5,
    manual = T
  )
RTplotsig


ggsave(file.path(outputFolder, 'plots', 'RTPlot_AvC.tif'), RTplotsig,
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")

# ACC depending on group, run, modality, bin
step(lmer(ACC ~ group * modality * bin +
            (1|ID), 
          data = behDataBinsModality))

lmACC <- lmer(ACC ~ group + modality + bin + geschlecht +# main effects
                (1 | ID) + # random intercept for ID
                group:modality + group:bin, # interaction effects
              data = behDataBinsModality)
summary(lmACC)
anova(lmACC)
report(anova(lmACC))

(PH_bin_ACC <- emmeans::emmeans(lmACC, pairwise ~ bin | group, adjust = "tukey"))
(PH_group_ACC <- emmeans::emmeans(lmACC, pairwise ~ group | bin, adjust = "tukey"))
(PH_mod_ACC <- emmeans::emmeans(lmACC, pairwise ~ modality | group, adjust = "tukey"))
(PH_mod_ACC2 <- emmeans::emmeans(lmACC, pairwise ~ group | modality, adjust = "tukey"))

nice_table(PH_bin_ACC$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_group_ACC$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_mod_ACC$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_mod_ACC2$contrasts) %>% 
  print(., preview = "docx")


ACCplot <- ggplot(data = behDataBinsModality, aes(x = bin, y = ACC, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0) +
  geom_line(data= meanRTBinsPlot, aes(x=bin, y = mACC, group = modality, 
                                      linetype = modality, color = modality), 
            linewidth = 1)  +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  facet_grid(~ group) +  # 2x4 grid: run (rows), version (columns)
  ylab("Accuracy") + xlab("Bin") + 
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(strip.text.x = element_text(size = 10.5))
ACCplot

annotationsACC <- data.frame(
  group = c("adults", "adults", "adults"),
  start = c("1", "1", "2"),
  end = c("2", "3", "3"),
  y = c(0.05, 0.15, 0.25),
  label = c(".002**", "<.001***", ".008*")
)

annotationsACC

ACCplotsig <- ACCplot +
  geom_signif(
    data = annotationsACC,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    inherit.aes = FALSE,
    # textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.5,
    manual = T
  )
ACCplotsig

ggsave(file.path(outputFolder, 'plots', 'ACCPlot_AvC.tiff'), ACCplotsig,
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")


## Clean up workspace ----
finalVars <- ls()
newVars <- setdiff(finalVars, initialVars)
vars2keep <- c("behData", "expInfo")
newVars <- setdiff(newVars, vars2keep)

remove(list = newVars)
