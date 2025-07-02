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
  summarise(betaValues = mean(betaValues)) %>% 
  ungroup()

PEmin <- floor(min(betasSubPE$betaValues)) # -4.245
PEmax <- ceiling(max(betasSubPE$betaValues)) # 2.295

betasPE <- extractedBetaslongShort %>% 
  filter(mask == "PE_uniform",
         hemisphere == "wholeBrain") %>% 
  ungroup()

## analyes ----
combined_anovas_PE <- tibble()

betasPE <- betasPE %>%
  mutate(age_c = age - mean(age))

PELM2 <- lmer(betaValues ~ modality * age_c + (1|ID), betasPE,
              contrasts = list(modality = contr.sum))

summary(PELM2)
anova(PELM2)

effSizes <- effectsize::eta_squared(anova(PELM2, type = 3), partial = TRUE) %>% 
  as_tibble() %>% 
  select(Parameter, Eta2_partial, CI_low, CI_high) 

combined_anovas_PE <-  anova(PELM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI") %>%
  left_join(effSizes, by = c("term" = "Parameter"))

ggplot(betasPE, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm') +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('RPE network ROI') +
  ylab("beta values") +
  annotate("text", label = "paste('modality ', italic(p), ' = .005**')",
           parse = TRUE, x = 5.8, y = -2.6,
           hjust = 0, vjust = 0, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age ', italic(p), ' = .002**')",
           parse = TRUE, x = 5.8, y = -3.7,
           hjust = 0, vjust = 0, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .046*')",
           parse = TRUE, x = 5.8, y = -4.8,
           hjust = 0, vjust = 0, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + 
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    strip.text.x = element_text(size=22),
    plot.title = element_text(size=22),
    legend.position = "none"
  ) 

# ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta_rev1_ICA.svg'),
#        height = 8, width = 12.8, units = "cm")
# ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta_rev1_ICA.tif'),
#        height = 8, width = 12.8, units = "cm")

## effects in subclusters ------------------------------------------
clusters <- unique(betasSubPE$label)
hemispheres <- unique(betasSubPE$hemisphere)

plots_PE <- list()

for (cluster in clusters) {
  for (hem in hemispheres) {
    # Filter the data for the current cluster and hemisphere
    betasSubPE1 <- betasSubPE %>% 
      filter(label == cluster, hemisphere == hem) %>% 
      mutate(age_c = age - mean(age))
    
    if (nrow(betasSubPE1) == 0) {
      next
    }
    
    SubLM <- lmer(betaValues ~ modality * age_c + (1|ID), betasSubPE1,
                  contrasts = list(modality = contr.sum))
    
    effSizesPE <- effectsize::eta_squared(anova(SubLM, type = 3), partial = TRUE) %>% 
      as_tibble() %>% 
      select(Parameter, Eta2_partial, CI_low, CI_high) 
    
    combined_anovas_PE <-  anova(SubLM) %>% 
      broom.mixed::tidy(effects = "fixed") %>% 
      mutate(Model = paste(cluster, hem)) %>% 
      left_join(effSizesPE, by = c("term" = "Parameter")) %>% 
      bind_rows(combined_anovas_PE, .)
    
    
    plot  <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
      geom_point(alpha=0.6) +
      geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
      ggtitle(paste(cluster, hem)) +
      scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
      jtools::theme_apa(remove.y.gridlines = F) + 
      scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
      theme(text = element_text(size = 20))
    
    # Add plot to the list
    plots_PE[[paste(cluster, hem)]] <- plot
  }
}

combined_anovas_PE_corr <- combined_anovas_PE %>% 
  #filter(Model != 'Whole ROI') %>% 
  rename(p_uncorr = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorr, method = "BH"),
         p_uncorr = sprintf("%.3f", p_uncorr))  %>% 
  select(Model, term, sumsq, meansq, NumDF, DenDF, statistic, p_uncorr, p.value, Eta2_partial, CI_low, CI_high)

nice_table(combined_anovas_PE_corr, highlight = TRUE,
           title = "RPE: Anova Main and Interaction effects") %>% 
  print(., preview = 'docx')

ggarrange(plotlist = plots_PE, ncol = 3, nrow = 3,
          common.legend = TRUE)


betasSubPE1 <- betasSubPE %>% 
  filter(label == "anterior insula", hemisphere == "left") %>% 
  mutate(age_c = age - mean(age))

SubLM <- lmer(betaValues ~ age_c * modality + (1|ID), betasSubPE1,
              contrasts = list(modality = contr.sum))

lmerTest::step()
summary(SubLM)

mean(betasSubPE$age)


