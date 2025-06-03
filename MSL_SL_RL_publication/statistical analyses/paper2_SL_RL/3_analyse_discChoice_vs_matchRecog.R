##########################################################
##                      Analyse                         ##
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
load(file.path(outputFolder, "demo.RData"))
load(file.path(outputFolder, "finalSample.RData"))
subjectsAdults <- read_csv(file.path(outputFolder, 'finalSubjectsAdults.csv'))
subjects <- read_csv(file.path(outputFolder, 'finalSubjects.csv'))

MRbehData <- MRbehData %>% 
  filter(ID != "CBC_1089")

MRexpInfo <- MRexpInfo %>% 
  filter(ID != "CBC_1089")

demo <- demo %>% 
  filter(ID != "CBC_1089")

length(unique(MRbehData$ID))

# demographics ----
## handedness ----
table(subjectsAdults$handedness)

table(demo$EHI_handedness)

allSubs <- subjects %>% 
  bind_rows(subjectsAdults)
  
## age ----
shapiro.test(allSubs$age) # not normal
summary(aov(age ~ interaction(group, version), data = allSubs))
kruskal.test(age ~ interaction(group, version), data = allSubs)

FSA::dunnTest(age ~ interaction(group, version), data = allSubs, method = "bonferroni")

## IQ ----
# Check normality for IQ in each group
shapiro.test(subjects$meanIQ[subjects$version == "v1"]) # normal
shapiro.test(subjects$meanIQ[subjects$version == "v3"]) # normal

t.test(meanIQ ~ version, data = subjects) # p = .401

## handedness ----
tableEHI <- allSubs %>% 
  mutate(group_v = interaction(group, version)) %>% 
  select(group_v, handedness) %>% 
  table()

tableEHI <- tableEHI[1:3,]
tableEHI

fisher.test(tableEHI)

## sex ----
tableSex <- allSubs %>% 
  mutate(group_v = interaction(group, version)) %>% 
  select(group_v, gender) %>% 
  table()

tableSex <- tableSex[1:3,]
tableSex

fisher.test(tableSex)

rcompanion::pairwiseNominalIndependence(tableSex, fisher = TRUE, gtest = FALSE, chisq = FALSE, method = "bonferroni")
  
## table with demographics test ----
demo <- demo %>% 
  left_join(., distinct(finalSample[,c("ID", "version")])) %>% 
  mutate(EHI_latQuot = as.numeric(EHI_latQuot),
         CBCL_tot = as.numeric(CBCL_tot))

demoTable <- creatDemoTable(demo, c("version"))

results <- demo %>%
  select(-version, -ID, -auswahl_vp, -klasse, - geburtsdatum, -mri_children_date, 
         -mri_children_msi_version, -EHI_handedness, -geschlecht) %>%  # Exclude the group variable
  map_df(~ broom::tidy(t.test(.x ~ demo$version)), .id = "variable") # Perform t-test for each variable and tidy results

print(results)

chisq.test(table(demo$geschlecht, demo$version))
# p-value = 0.04352
fisher.test(table(demo$geschlecht, demo$version))
# p-value = 0.03295
# significant association between geschlecht and version
# difference is likely to be due to random chance.

chi_sq_result <- tibble(
  variable = "geschlecht",
  statistic = chisq.test(table(demo$geschlecht, demo$version))$statistic,
  p.value = chisq.test(table(demo$geschlecht, demo$version))$p.value,
  method = "Chi-squared test"
)

combined_results <- results %>% 
  bind_rows(., chi_sq_result) %>% 
  nice_table(title = "p-values", note = "ABC", highlight = TRUE,
             stars = TRUE,)
print(combined_results, preview = "docx")

remove(chi_sq_result, combined_results, demoTable, results)

# ------------------------------------------------------------------------------
# analysing behavioural data ----
# ------------------------------------------------------------------------------

MRbehData <- MRbehData %>%
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

behData <- MRbehData %>% 
  mutate(ID = as.factor(ID),
         version = if_else(version == "v3", "discChoice", "matchRecog"))

table(behData$stimType)

expInfo <- MRexpInfo %>% 
  mutate(version = ifelse(grepl("v1", logfile), "v1", "v3"),
         run = "MR") %>% 
  ungroup() %>% 
  mutate(version = if_else(version == "v3", "discChoice", "matchRecog"))  

behData %>% 
  group_by(ID, modality, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(modality)

expInfo %>% 
  count(modality)

save(behData, file = file.path(outputFolder, "ALLbehDataMR.RData"))
save(expInfo, file = file.path(outputFolder, "ALLexpInfoMR.RData"))

## mean performance
meanOmOutl <- expInfo %>% 
  group_by(ID, version, modality) %>% 
  summarise(meanOm = mean(omissions, na.rm=T)*100,
            meanOutl = mean(outliers, na.rm=T)*100)

meanPerformance <- behData %>% 
  group_by(ID, version, modality) %>% 
  summarise(meanAcc = mean(trials_runs.correct_answer, na.rm=T),
            meanRT = mean(response_runs.rt, na.rm = T),
            meanRTCorr = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T)) %>% 
  left_join(., meanOmOutl, by = join_by(ID, version, modality))

performanceTable <- meanPerformance %>% 
  group_by(version, modality) %>% 
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
  pivot_longer(cols = -c(version, modality), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = c(version, modality), values_from = value) %>% 
  nice_table(title = "MR Task Performance", note = "ABC", 
             col.format.custom = 2:3, format.custom = "fun")

print(performanceTable, preview = "docx")


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
         task = version)

behDataBinsModality <- behData %>% 
  filter(validTrials == 1) %>% 
  group_by(ID, version, modality, bin) %>% 
  summarise (RT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
             ACC = mean(trials_runs.correct_answer, na.rm = T)) %>% 
  left_join(., demo[,c("ID", "age", "geschlecht")], by = join_by(ID)) %>% 
  ungroup() %>% 
  mutate(ID = as.factor(ID),
         version = as.factor(version),
         modality = as.factor(modality),
         bin = as.factor(bin),
         modality = if_else(modality == "av", "AV", "TV"),
         task = version)

meanRTBinsPlot <- behData %>% 
  group_by(version, modality, bin) %>% 
  summarise(mRT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
            mACC = mean(trials_runs.correct_answer, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(modality = if_else(modality == "av", "AV", "TV"),
         task = version)

## statistical analyses ----
# RT depending on task version, run, modality, bin
step(lmer(RT ~ version * modality * bin +
            (1|ID), 
          data = behDataBinsModality))

lmRT <- lmer(RT ~ modality + bin + geschlecht +# main effects
               (1 | ID), # random intercept for ID
             data = behDataBinsModality)
summary(lmRT)
anova(lmRT)
report(anova(lmRT))

PH_bin <- emmeans::emmeans(lmRT, pairwise ~ bin, adjust = "tukey")
PH_mod <- emmeans::emmeans(lmRT, pairwise ~ modality, adjust = "tukey")

PH_bin
PH_mod

nice_table(PH_bin$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_mod$contrasts) %>% 
  print(., preview = "docx")

RTplot <- ggplot(behDataBinsModality, aes(x = bin, y = RT, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0) +
  geom_line(data= meanRTBinsPlot, aes(x=bin, y = mRT, group = modality, 
                                         linetype = modality, color = modality), 
            linewidth = 1)  +
  facet_grid( ~ task, 
              labeller=labeller(task=c("discChoice" = "discriminative choice", 
                                       "matchRecog" = "match recognition"))) +  # 2x4 grid: run (rows), version (columns)
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ylab("Reaction Time [s]") + xlab("Bin") + 
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + scale_y_continuous(expand = c(0, 0), limits = c(0, 4.5)) +
  theme(legend.position = "None",
        strip.text.x = element_text(size = 10.5))
RTplot

RTplotsig <- RTplot +
  geom_signif(
    comparisons = list(c("1", "3"), c("2", "3")),
    map_signif_level = TRUE,
    y_position = c(3.4, 3.9), # Adjust y positions for the lines
    annotations = c("<.001***", ".032*"), # Corresponding significance levels
    # textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.5,
  )
RTplotsig

ggsave(file.path(outputFolder, 'plots', 'RTPlot.tif'), RTplotsig,
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")

# ACC depending on task version, modality, bin
step(lmer(ACC ~ version * modality * bin +
            (1|ID), 
          data = behDataBinsModality))

lmACC <- lmer(ACC ~ modality + bin + geschlecht +# main effects
               (1 | ID),  # random intercept for ID
             data = behDataBinsModality)
summary(lmACC)
anova(lmACC)
report(anova(lmACC))

PH_ACC_bin <- emmeans::emmeans(lmACC, pairwise ~ bin, adjust = "tukey")
PH_ACC_Mod <- emmeans::emmeans(lmACC, pairwise ~ modality, adjust = "tukey")

PH_ACC_bin
PH_ACC_Mod

nice_table(PH_ACC_bin$contrasts) %>% 
  print(., preview = "docx")

nice_table(PH_ACC_Mod$contrasts) %>% 
  print(., preview = "docx")


ACCplot <- ggplot(data = behDataBinsModality, aes(x = bin, y = ACC, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0) +
  geom_line(data= meanRTBinsPlot, aes(x=bin, y = mACC, group = modality, 
                                      linetype = modality, color = modality), 
            linewidth = 1.2)  +
  facet_grid( ~ task, 
              labeller=labeller(task=c("discChoice" = "discriminative choice", 
                                       "matchRecog" = "match recognition"))) +  # 2x4 grid: run (rows), version (columns)
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ylab("Accuracy") + xlab("Bin") + 
  jtools::theme_apa(remove.y.gridlines = F, legend.use.title = T) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(strip.text.x = element_text(size = 10.5))
ACCplot

ACCplotsig <- ACCplot +
  geom_signif(
    comparisons = list(c("1", "3")),
    map_signif_level = TRUE,
    y_position = c(0.1), # Adjust y positions for the lines
    annotations = c("<.001***"), # Corresponding significance levels
    # textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.5,
    hjust = -0.05
  ) +
  geom_signif(
    comparisons = list(c("1", "2")),
    map_signif_level = TRUE,
    y_position = c(0.0), # Adjust y positions for the lines
    annotations = c(".024*"), # Corresponding significance levels
    # textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.5
  )
ACCplotsig

ggsave(file.path(outputFolder, 'plots', 'ACCPlot.tif'), ACCplotsig,
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")

## Clean up workspace ----
finalVars <- ls()
newVars <- setdiff(finalVars, initialVars)
vars2keep <- c("behData", "expInfo")
newVars <- setdiff(newVars, vars2keep)

remove(list = newVars)
  