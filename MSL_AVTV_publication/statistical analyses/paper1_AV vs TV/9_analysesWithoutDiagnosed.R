##########################################################
##           Re-Analyse without diagnosed               ##
##########################################################

##########################################################
# load data ----------------------------------------------
load(file.path(outputFolder, "data_childrenMR.RData"))
load(file.path(outputFolder, "expInfo_childrenMR.RData"))
load(file.path(outputFolder, "demo.RData"))
modelingData <- read_csv(paste(outputFolder, "modelling", "modelingData.csv", sep = "/"))
load(file.path(outputFolder, "extractedBetaslong.RData"))

##########################################################
# remove subs --------------------------------------------
subsDiagnosed <- c('CBC_1051', 'CBC_1108', 'CBC_1003', 'CBC_1014', 'CBC_1019', 
                   'CBC_1072', 'CBC_1090', 'CBC_1093', 'CBC_1119', 'CBC_1146')

behDataShort <- behData %>% 
  filter(!(ID %in% subsDiagnosed))

modelingDataShort <- modelingData %>% 
  filter(!(ID %in% subsDiagnosed))

expInfoShort <- expInfo %>% 
  filter(!(ID %in% subsDiagnosed))

demoShort <- demo %>% 
  filter(!(ID %in% subsDiagnosed))

demoExcluded <- demo %>% 
  filter(ID %in% subsDiagnosed)

extractedBetaslongShort <- extractedBetaslong %>% 
  filter(!(ID %in% subsDiagnosed))

# N and age after exclusion
nrow(demoShort)
mean(demoShort$age)
sd(demoShort$age)
min(demoShort$age)
max(demoShort$age)
table(demoShort$geschlecht)

# N and age for excluded
nrow(demoExcluded)
mean(demoExcluded$age)
sd(demoExcluded$age)
min(demoExcluded$age)
max(demoExcluded$age)
table(demoExcluded$geschlecht)

##########################################################
# analyse modeling ---------------------------------------
behDataShort$bin[behDataShort$trials_runs.thisN >=0 & behDataShort$trials_runs.thisN < 11] <- 1
behDataShort$bin[behDataShort$trials_runs.thisN >10 & behDataShort$trials_runs.thisN < 22] <- 2
behDataShort$bin[behDataShort$trials_runs.thisN >21 & behDataShort$trials_runs.thisN < 33] <- 3
behDataShort$bin[behDataShort$trials_runs.thisN >32 & behDataShort$trials_runs.thisN < 44] <- 4
behDataShort$bin <- as.factor(behDataShort$bin)

modelingDataShort$bin[modelingDataShort$trial >=1 & modelingDataShort$trial < 12] <- 1
modelingDataShort$bin[modelingDataShort$trial >11 & modelingDataShort$trial < 23] <- 2
modelingDataShort$bin[modelingDataShort$trial >22 & modelingDataShort$trial < 34] <- 3
modelingDataShort$bin[modelingDataShort$trial >33 & modelingDataShort$trial < 45] <- 4
modelingDataShort$bin <- as.factor(modelingDataShort$bin)

selectedData <- modelingDataShort %>% 
  filter(respModel == "CBCdriftDiffusionLR") %>% 
  mutate(trial = trial-1) %>%
  left_join(.,behDataShort[,c("ID", "logfile", "trials_runs.thisN", "bin", "outlier200ms", "outlierSD", 
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

meanDriftbinPlot <- selectedData %>% 
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

lmTable <- nice_table(as.data.frame(report_table(driftLM4)),
                      title = "Linear Mixed Model for Drift Rate and Modality", note = "ABC", 
                      highlight = T)
lmTable

driftLM2PH <- emmeans::emmeans(driftLM4, "bin", data=modDataSumm)
pairs(driftLM2PH)

lmPHTable <- nice_table(as.data.frame(pairs(driftLM2PH)),
                        title = "Post-hoc tests for Drift Rates and bin", note = "ABC", 
                        col.format.custom = 2:5, format.custom = "fun2",
                        highlight = T)
lmPHTable

ggplot(modDataSumm, aes(bin, absMDriftRate, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8, width = 1.2) +
  geom_boxplot(width = 0.3, alpha = 0) +
  geom_line(data= meanDriftbinPlot, aes(x=bin, y = absMDriftRate, group = modality, 
                                            linetype = modality, color = modality), 
            linewidth = 1.2)  +
  scale_linetype_manual(values=c("longdash", "dotted")) +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle("Drift Rate and Modality") +
  geom_signif(
    comparisons = list(c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"),
                       c("2", "4"), c("3", "4")),
    map_signif_level = TRUE,
    y_position = c(1.3, 1.4, 1.5, 1.6, 1.7, 1.8), # Adjust y positions for the lines
    annotations = c("<.001***", "<.001***", "<.001***", "<.001***", "<.001***",".002**"), # Corresponding significance levels
    textsize = 3,
    tip_length = 0,
    vjust = 0.2,
    color = "black") +
  ylab("absoulte drift rate") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  theme(text = element_text(size = 20)) 

ggsave(file.path(outputFolder, "modelling", "driftRateModalitybin.png"),
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
  geom_smooth(method = "lm")+
  facet_grid(~bin) +
  ggtitle("Drift Rate and Modality") +
  ylab("absoulte drift rate") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  theme(text = element_text(size = 20))  

ggsave(file.path(outputFolder, "modelling", "driftRateModalityAge.png"),
       width=24, height = 15, units = "cm")


##########################################################
# analyse PE ROI -----------------------------------------
# beta values PE
betasSubPE <- extractedBetaslongShort %>% 
  filter(mask == "PE_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues))

min(betasSubPE$betaValues) # -8.121
max(betasSubPE$betaValues) # 5.689

betasPE <- extractedBetaslongShort %>% 
  filter(mask == "PE_uniform",
         hemisphere == "wholeBrain")

p_values_PE <- c()
combined_anovas_PE <- tibble()

PELM2 <- lmer(betaValues ~ modality * age + (1|ID), betasPE)

summary(PELM2)
report(PELM2)

anova(PELM2)
combined_anovas_PE <-  anova(PELM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI")

p_values_PE <- c(p_values_PE, summary(PELM2)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(PELM2)),
                      title = 'ABC', note = 'ABC', 
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

ggplot(betasPE, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 25))

ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta_Subgroup.png'),
       height = 15, width = 24, units = "cm")

# effects in subclusters ------------------------------------------
levels(betasSubPE$label)

# anterior insula left
betasSubPE1 <- betasSubPE %>% 
  filter(label == "anterior insula", hemisphere == "left")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubPE1)
summary(SubLM)
anova(SubLM)
combined_anovas_PE <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Anterior Insula left") %>% 
  bind_rows(combined_anovas_PE, .)

p_values_PE <- c(p_values_PE, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Left Anterior Insula', note = 'ABC', 
                      highlight = T)
lmTable

p1 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Anterior Insula') + ylim(-9,3) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p1

# anterior insula right
betasSubPE1 <- betasSubPE %>% 
  filter(label == "anterior insula", hemisphere == "right")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubPE1)
summary(SubLM)
anova(SubLM)
combined_anovas_PE <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Anterior Insula right") %>% 
  bind_rows(combined_anovas_PE, .)

p_values_PE <- c(p_values_PE, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Right Anterior Insula', note = 'ABC', 
                      highlight = T)
lmTable

p2 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Anterior Insula') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p2

# precentral gyrus left
betasSubPE1 <- betasSubPE %>% 
  filter(label == "precentral gyrus", hemisphere == "left")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubPE1)
summary(SubLM)
anova(SubLM)
combined_anovas_PE <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Precentral Gyrus left") %>% 
  bind_rows(combined_anovas_PE, .)

p_values_PE <- c(p_values_PE, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Left Precentral Gyrus', note = 'ABC', 
                      highlight = T)
lmTable

p3 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Precentral Gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p3

# right
betasSubPE1 <- betasSubPE %>% 
  filter(label == "precentral gyrus", hemisphere == "right")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubPE1)
summary(SubLM)
anova(SubLM)
combined_anovas_PE <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Precentral Gyrus right") %>% 
  bind_rows(combined_anovas_PE, .)

p_values_PE <- c(p_values_PE, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Right Precentral Gyrus', note = 'ABC', 
                      highlight = T)
lmTable

p4 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Precentral Gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p4

# bilateral supplementary motor cortex
betasSubPE1 <- betasSubPE %>% 
  filter(label == "supplementary motor cortex")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubPE1)
summary(SubLM)
anova(SubLM)
combined_anovas_PE <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Supplementary Motor Cortex bilateral") %>% 
  bind_rows(combined_anovas_PE, .)

p_values_PE <- c(p_values_PE, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Bilateral Supplementary Motor Cortex', note = 'ABC', 
                      highlight = T)
lmTable

p5 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Bilateral Supplementary Motor Cortex') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p5

# ventral striatum left
betasSubPE1 <- betasSubPE %>% 
  filter(label == "ventral striatum", hemisphere == "left")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubPE1)
summary(SubLM)
anova(SubLM)
combined_anovas_PE <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Ventral Striatum left") %>% 
  bind_rows(combined_anovas_PE, .)

p_values_PE <- c(p_values_PE, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Left Ventral Striatum', note = 'ABC', 
                      highlight = T)
lmTable

p6 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Ventral Striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p6

# right
betasSubPE1 <- betasSubPE %>% 
  filter(label == "ventral striatum", hemisphere == "right")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubPE1)
summary(SubLM)
anova(SubLM)
combined_anovas_PE <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Ventral Striatum right") %>% 
  bind_rows(combined_anovas_PE, .)

p_values_PE <- c(p_values_PE, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Right Ventral Striatum', note = 'ABC', 
                      highlight = T)
lmTable

p7 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Ventral Striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p7

# adjust p-values
adjusted_p_values_PE <- p.adjust(p_values_PE, method = "BH")

msTable <- data.frame(p_uncorr = p_values_PE, p = adjusted_p_values_PE)

# get effects of anovas
combined_anovas_PE_final <- combined_anovas_PE %>% 
  select(Model, term, NumDF, statistic, p.value) %>% 
  rename(DF = NumDF,
         F.Value = statistic,
         p_uncorrected = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorrected, method = "BH")) 

lmTable <- nice_table(combined_anovas_PE_final,
                      title = 'adjusted p-values', note = 'ABC', 
                      col.format.custom = c(5), format.custom = 'fun3',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')


