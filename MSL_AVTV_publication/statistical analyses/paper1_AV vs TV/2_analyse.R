##########################################################
##                      Analyse                         ##
##########################################################

##########################################################

# load data ----
load(file.path(outputFolder, "data_childrenMR.RData"))
load(file.path(outputFolder, "expInfo_childrenMR.RData"))
load(file.path(outputFolder, "demo.RData"))

# demographics ----
## handedness ----
#replace missing values
table(demo$EHI_handedness)

## table with demographics test ----
demo <- demo %>% 
  mutate(ageGroup = case_when(
    age > 5.5  & age <= 8.0   ~ '5.7-8.0',
    age > 8.0  & age <= 10.5  ~ '8.1-10.5',
    age > 10.5  & age <= 13.0 ~ '10.6-13.0'),
    ageGroup = factor(ageGroup, 
                      levels = c('5.7-8.0', '8.1-10.5', '10.6-13.0'),
                      ordered = T)
  )

demoTable <- creatDemoTable(demo, c("ageGroup"))

demoTable <- creatDemoTable(demo, c("auswahl_vp"))

demoTableShort <- demoTable %>% 
  nice_table(title = "Demographics", note = "ABC")
print(demoTableShort, preview = "docx")

# add the mean reaction times, accuracies and omissions per AV and TV
nrTrials <- 44

meanOmOutl <- expInfo %>% 
  group_by(ID, modality) %>% 
  summarise(meanOm = mean(omissions, na.rm=T)*100,
            meanOutl = mean(outliers, na.rm=T)*100)

meanPerformance <- behData %>% 
  group_by(ID, modality) %>% 
  summarise(meanAcc = mean(trials_runs.correct_answer, na.rm=T),
            meanRT = mean(response_runs.rt, na.rm = T),
            meanRTCorr = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T)) %>% 
  left_join(., meanOmOutl, by = join_by(ID, modality))

fun <- function(x) {
  formatC(x, format = "f", digits = 2)
}

performanceTable <- meanPerformance %>% 
  group_by(modality) %>% 
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
  pivot_longer(cols = -c(modality), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = modality, values_from = value) %>% 
  nice_table(title = "MR Task Performance", note = "ABC", 
             col.format.custom = 2:3, format.custom = "fun")

print(performanceTable, preview = "docx")

table(behData$trials_runs.feedback_given)/7920*100
table(behData$outlier200ms)/7920*100
table(behData$outlierSD)/7920*100
table(behData$validTrials)

# ------------------------------------------------------------------------------
# analysing behavioural data ----
# ------------------------------------------------------------------------------

# add fourths to data
behData$bin[behData$trials_runs.thisN >=0 & behData$trials_runs.thisN < 11] <- 1
behData$bin[behData$trials_runs.thisN >10 & behData$trials_runs.thisN < 22] <- 2
behData$bin[behData$trials_runs.thisN >21 & behData$trials_runs.thisN < 33] <- 3
behData$bin[behData$trials_runs.thisN >32 & behData$trials_runs.thisN < 44] <- 4
behData$bin <- as.factor(behData$bin)

## overview trial structure ----
tsTable <- expInfo %>% 
  left_join(., demo[, c("ID", "age")]) %>% 
  group_by(modality, tsDifficulty, prob.Feedback) %>% 
  summarise(n = n(),
            meanAcc = round(mean(ACC)*100,2),
            sdACC = round(sd(ACC)*100,1),
            mAge = mean(age)) %>% 
  arrange(modality, tsDifficulty, desc(prob.Feedback)) %>% 
  mutate(perc = round(n/90*100,1))
table(expInfo$tsDifficulty, expInfo$prob.Feedback)

taskDiff <- behData %>% 
  mutate(FBaccuracy = as.numeric(trials_runs.correct_answer==trials_runs.feedback_given)) %>% 
  group_by(ID, session, modality, prob.Feedback, tsDifficulty) %>% 
  mutate(
    # compare each trial to the one before it
    repeat_consec = (secondStim == lag(secondStim)),
    # if any TRUE in the session, mark “easy”, else “hard”
    difficulty    = if_else(any(repeat_consec, na.rm = TRUE),
                            "easy", "hard"),
    #prob.Feedback = as.factor(prob.Feedback),
  ) %>%
  select(-repeat_consec) %>% 
  summarise(probFB = mean(FBaccuracy, na.rm = T),
            probTrials = 1-probFB,
            diff = as.factor(unique(difficulty)),
            nPosFB  = sum(trials_runs.feedback_given == 1, na.rm = TRUE),   # count of positive FB
            nNegFB  = sum(trials_runs.feedback_given == 0, na.rm = TRUE),   # count of negative FB
            acc = mean(trials_runs.correct_answer, na.rm = TRUE),
            .groups = "drop") %>% 
  left_join(., demo[,c("ID", "age")], by = join_by(ID))

head(taskDiff)

taskDiff %>%
  group_by(prob.Feedback, diff) %>%
  summarize(
    n_sessions = n(),                                        # count of rows = sessions
    nSubs      = length(unique(ID)),
    meanNegFb  = mean(nNegFB),
    meanPosFB  = mean(nPosFB),
    mean_age   = mean(age, na.rm = TRUE),                    # average age
    sd_age     = sd(age, na.rm = TRUE),                      # age standard deviation
    min_age    = min(age, na.rm = TRUE),                     # youngest
    max_age    = max(age, na.rm = TRUE),                     # oldest
    age_range  = paste0(min_age, "–", max_age)               # e.g. “10–11”
  ) %>%
  ungroup() %>% 
  arrange(desc(prob.Feedback), diff) %>% 
  select(-min_age, -max_age) %>% 
  nice_table() %>% 
  print(preview = "docx")

## task performance ----
# add age to data
behData <- behData %>% 
  left_join(., demo[,c("ID", "age")], by = join_by(ID))

behData$session[behData$logfile == "CBC_1136_MSI_AV_v3_Vset3_Aset3_2024-09-11_17h04.04.997.csv"] <- 5

behDataFourths <- behData %>% 
  filter(validTrials == 1) %>% 
  group_by(ID, bin) %>% 
  summarise (RT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
             ACC = mean(trials_runs.correct_answer, na.rm = T)) %>% 
  left_join(., demo[,c("ID", "age")], by = join_by(ID))

behDataFourthsSesison <- behData %>% 
  filter(validTrials == 1) %>% 
  group_by(ID, session, bin) %>% 
  summarise (RT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
             ACC = mean(trials_runs.correct_answer, na.rm = T)) %>% 
  left_join(., demo[,c("ID", "age")], by = join_by(ID))

behData4thsModalitySession <- behData %>% 
  filter(validTrials == 1) %>% 
  group_by(ID, session, modality, bin) %>% 
  summarise (RT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
             ACC = mean(trials_runs.correct_answer, na.rm = T)) %>% 
  left_join(., demo[,c("ID", "age")], by = join_by(ID))

behData4thsModality <- behData %>% 
  filter(validTrials == 1) %>% 
  group_by(ID, modality, bin) %>% 
  summarise (RT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
             ACC = mean(trials_runs.correct_answer, na.rm = T)) %>% 
  left_join(., demo[,c("ID", "age")], by = join_by(ID))

## statistical analyses ----
# performance above chance
qbinom(0.95, size = 44, prob = 0.5) + 1 # 28/44 significant above chance

ggplot(behData4thsModality, aes(x = bin, y = ACC, group = modality, color = modality)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.6) +
  facet_wrap(~ID)

ACCaboveThr <- behData4thsModalitySession %>% 
  group_by(ID) %>% 
  reframe(binAboveThr = sum(ACC >= 28/44))

ACCaboveThr %>% 
  filter(binAboveThr < 2)

## combine fourths and modality
step(lmer(RT ~ modality * bin * ageC +(1|ID), data = behData4thsModality))
step(lmer(log(RT) ~ modality * bin * age +(1|ID), data = behData4thsModality))

lmRT5 <- lmer(RT ~ modality + bin + age + modality:age + (1|ID), data = behData4thsModality)
summary(lmRT5)

res <- residuals(lmRT5)
qqnorm(res); qqline(res, col = "blue")
shapiro.test(res) # non-normality
hist(res,
     breaks = 30,
     main   = "Histogram of LMM residuals",
     xlab   = "Residual",
     border = "gray")
plot(DHARMa::simulateResiduals(lmRT5))

lmRT5log <- lmer(log(RT) ~ modality + bin + age + modality:age + (1|ID), data = behData4thsModality)
res2 <- residuals(lmRT5log)
qqnorm(res2); qqline(res2, col = "blue")
shapiro.test(res2) # non-normality
hist(res2,
     breaks = 30,
     main   = "Histogram of LMM residuals (log transformed)",
     xlab   = "Residual",
     border = "gray")
plot(DHARMa::simulateResiduals(lmRT5log))

summary(lmRT5)
summary(lmRT5log)

anova(lmRT5)
report(anova(lmRT5))

report_text(lmRT5)
lmTable <- nice_table(as.data.frame(report_table(lmRT5)),
                      title = "Linear Mixed Model for Accuracies and Modality", note = "ABC", 
                      #col.format.custom = c(2:6, 11:13), format.custom = "fun",
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

lmRT5PH <- emmeans::emmeans(lmRT5, c("bin"), data=behData4thsModalitySession)
pairs(lmRT5PH)

lmPHTable <- nice_table(as.data.frame(pairs(lmRT5PH)),
                        title = "Post-hoc tests for Reaction Times and Fourths", note = "ABC", 
                        #col.format.custom = 2:12, format.custom = "fun",
                        highlight = T)
lmPHTable
print(lmPHTable, preview = "docx")

## Accuracies
lmACC8 <- lmer(ACC ~ modality + bin + age + modality:age + bin:age + modality:bin + modality:bin: age + (1|ID), data = behData4thsModality)

lmerTest::step(lmACC8)

lmACC5 <- lmer(ACC ~ modality + bin + age + modality:age +(1|ID) , data = behData4thsModality)

behData4thsModality$corrAge <- behData4thsModality$age - mean(behData4thsModality$age)

lmACC5.1 <- lmer(ACC ~ modality * corrAge + bin +(1|ID) , data = behData4thsModality)

summary(lmACC5)
summary(lmACC5.1)
anova(lmACC5)
report(anova(lmACC5))
as.report_text(report(anova(lmACC5)), summary = T)

lmTable <- nice_table(as.data.frame(report_table(lmACC5)),
                      title = "Linear Mixed Model for Accuracies and Modality", note = "ABC", 
                      col.format.custom = c(6), format.custom = "fun3",
                      highlight = T)
lmTable
print(lmTable, preview = "docx")

lmACC5PH <- emmeans::emmeans(lmACC5, c("bin"), data=behData4thsModality)
pairs(lmACC5PH)

lmPHTable <- nice_table(as.data.frame(pairs(lmACC5PH)),
                        title = "Post-hoc tests for Accuracies and Fourths", note = "ABC", 
                        #col.format.custom = 2:12, format.custom = "fun",
                        highlight = T)
lmPHTable
print(lmPHTable, preview = "docx")

# plotting results
meanRTFourthsPlot <- behData %>% 
  group_by(modality, bin) %>% 
  summarise(mRT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
            mACC = mean(trials_runs.correct_answer, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(modality = if_else(modality == "av", "AV", "TV"))

medAcc4thbin <- behData4thsModality %>% 
  filter(bin == 4) %>% 
  group_by(modality) %>% 
  summarise(mACC = mean(ACC)*100,
            mdACC = round(median(ACC)*100,2))

behData %>% 
  group_by(modality, bin) %>% 
  summarise(mRT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
            mACC = mean(trials_runs.correct_answer, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(modality = if_else(modality == "av", "AV", "TV"))

meanPerformance <- behData %>% 
  group_by(ID, modality) %>% 
  summarise(meanAcc = mean(trials_runs.correct_answer, na.rm=T),
            # sdAcc = sd(trials_runs.correct_answer, na.rm=T),
            # iqrAcc = IQR(trials_runs.correct_answer, na.rm=T),
            meanRT = mean(response_runs.rt, na.rm = T),
            # sdRT = sd(response_runs.rt, na.rm = T),
            meanRTCorr = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T)) %>% 
  # sdRTCorr = sd(response_runs.rt, na.rm = T)) 
  left_join(., meanOmOutl, by = join_by(ID, modality))

behData4thsModality <- behData4thsModality %>% 
  mutate(modality = if_else(modality == "av", "AV", "TV"))

RTplot <- ggplot(data = behData4thsModality, aes(x = bin, y = RT, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0) +
  geom_line(data= meanRTFourthsPlot, aes(x=bin, y = mRT, group = modality, 
                                         linetype = modality, color = modality), 
            linewidth = 1.2)  +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_grid(~stimType, switch="y") +
  ylab("Reaction Time [s]") + xlab("bin")  + ggtitle("") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 4)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  )    
RTplot

RTplotsig <- RTplot +
  geom_signif(
    comparisons = list(c("1", "3"), c("1", "4"), c("2", "3"), c("2", "4")),
    map_signif_level = TRUE,
    y_position = c(0.01, 0.27, 0.53, 0.8), # Adjust y positions for the lines
    annotations = c("<.001***", ".001***", ".006**", ".035*"), # Corresponding significance levels
    textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.75
  )
RTplotsig

ggsave(plot = RTplotsig, filename = file.path(outputFolder, "figures", "RTFourthsModality.png"),
       width = 24, height = 15, units = "cm")

ggsave(plot = RTplotsig, filename = file.path(outputFolder, "figures", "RTFourthsModality.svg"),
       width = 24, height = 15, units = "cm")

ggsave(plot = RTplotsig, filename = file.path(outputFolder, "figures", "RTFourthsModality.tif"),
       width = 24, height = 15, units = "cm")

ACCplot <- ggplot(data = behData4thsModality, aes(x = bin, y = ACC, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0) +
  geom_line(data= meanRTFourthsPlot, aes(x=bin, y = mACC, group = modality, 
                                         linetype = modality, color = modality), 
            linewidth = 1.2)  +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #facet_grid(~stimType, switch="y") +
  ylab("Accuracy") + xlab("bin") + ggtitle("") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  )  
ACCplot

ACCplotsig <- ACCplot +
  geom_signif(
    comparisons = list(c("1", "3"), c("1", "4"), c("2", "3"), c("2", "4")),
    map_signif_level = TRUE,
    y_position = c(0, 0.07, 0.14, 0.21), # Adjust y positions for the lines
    annotations = c("<.001***", "<.001***", ".001***", "<.001***"), # Corresponding significance levels
    textsize = 7,
    tip_length = 0,
    vjust = 0,
    size = 0.75
  )
ACCplotsig

ggsave(plot = ACCplotsig, filename = file.path(outputFolder, "figures", "ACCFourthsModality.png"),
       width = 24, height = 15, units = "cm")

ggsave(plot = ACCplotsig, filename = file.path(outputFolder, "figures", "ACCFourthsModality.svg"),
       width = 24, height = 15, units = "cm")

ggsave(plot = ACCplotsig, filename = file.path(outputFolder, "figures", "ACCFourthsModality.tif"),
       width = 24, height = 15, units = "cm")

behDataModality <- behData %>% 
  filter(validTrials == 1) %>% 
  group_by(ID, modality) %>% 
  summarise (RT = mean(response_runs.rt[trials_runs.correct_answer==1], na.rm = T),
             ACC = mean(trials_runs.correct_answer, na.rm = T)) %>% 
  left_join(., demo[,c("ID", "age")], by = join_by(ID))

RTplotInterac <- ggplot(behData4thsModality, aes(age, RT, group = modality, color =modality)) +
  geom_point() +
  geom_smooth(method = "lm", aes(linetype = modality), se = T, linewidth = 1.2) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle("") + 
  ylab("Reaction Time [s]") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0,4)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  ) 
RTplotInterac

ggsave(plot = RTplotInterac, filename = file.path(outputFolder, "figures", "RTAgeModality.png"),
       width = 24, height = 15, units = "cm")

ggsave(plot = RTplotInterac, filename = file.path(outputFolder, "figures", "RTAgeModality.svg"),
       width = 24, height = 15, units = "cm")

ggsave(plot = RTplotInterac, filename = file.path(outputFolder, "figures", "RTAgeModality.tif"),
       width = 24, height = 15, units = "cm")

ACCplotInterac <- ggplot(behData4thsModality, aes(age, ACC, group = modality, color =modality)) +
  geom_point() +
  geom_smooth(method = "lm", aes(linetype = modality), se = T, linewidth = 1.2)  +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle("") + 
  ylab("Accuracy") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    legend.title = element_text(size = 22),  # Legend title
    strip.text.x = element_text(size=22)
  )
ACCplotInterac

ggsave(plot = ACCplotInterac, filename = file.path(outputFolder, "figures", "ACCAgeModality.png"),
       width = 24, height = 15, units = "cm")

ggsave(plot = ACCplotInterac, filename = file.path(outputFolder, "figures", "ACCAgeModality.svg"),
       width = 24, height = 15, units = "cm")

ggsave(plot = ACCplotInterac, filename = file.path(outputFolder, "figures", "ACCAgeModality.tif"),
       width = 24, height = 15, units = "cm")

## Clean up workspace ----
remove(p_values, ttAge, tsTable, SDQ_CBCL, RTplot, RTplotsig, performanceTable,
       p, medicalHist, meanRTFourthsPlot, meanPerformance, meanOmOutl, lowIQ,
       lmTable, lmRT6, lmRT5PH, lmRT5, lmRT4, lmRT3, lmRT2, lmRT, lmPHTable,
       lmGenderGrade, lmGenderGradeTable, lmACC7, lmACC6, lmACC5PH, lmACC5,
       lmACC4, lmACC3, lmACC2, lmACC, LKplot, IQplot, IDSplot, gradePlot,
       gradeGenderPlot, demoTableShort, demoTableGender, demoTable, behData4thsModality,
       behData4thsModalitySession, behDataFourths, behDataFourthsSesison, 
       behDataModality, behDataModalitySession, behDataRT, agePlot, ACCplot, ACCplotsig,
       ageGenderPlot, KiTAPplot, p, lmACC5.1, lmACC6.1, lmACC8, lmRT7, lmRT8,
       ACCplotInterac, RTplotInterac)

              