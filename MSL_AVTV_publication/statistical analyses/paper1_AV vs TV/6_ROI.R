##########################################################
##                    ROI ANAlyses                      ##
##########################################################

# Load and merge data ------------------------------------
##########################################################
load(file.path(outputFolder, "demo.RData"))
load(file.path(outputFolder, "modelSelection.RData"))

files <- list.files(file.path(inputFolder, 'ROI')) %>% 
  discard(str_detect(.,'extracedBetas.csv'))

extractedBetasList <- lapply(file.path(inputFolder, 'ROI', files), data.table::fread)

extractedBetas <- data.frame()

for (i in 1:length(extractedBetasList)) {
  temp <- extractedBetasList[[i]]
  
  if (grepl("_adults", files[i])) {
    temp$group <- "adults"
  } else {
    temp$group <- "children"
  }
  
  extractedBetas <- extractedBetas %>% 
    bind_rows(., temp)
}

extractedBetas <- extractedBetas %>% 
  left_join(.,demo[,c('ID', 'age')],by = join_by(ID)) 

extractedBetas$hemisphere[extractedBetas$hemisphere=='bilater'] <- 'bilateral'

extractedBetaslong <- extractedBetas %>% 
  rename(modality = task,
         betaValues = beta) %>% 
  mutate(mask = as.factor(mask),
         modality = as.factor(modality),
         hemisphere = as.factor(hemisphere),
         label = as.factor(label),
         ageMinC = age-min(age),
         ageMeanC = age-(mean(age)),
         ageZ = (age - mean(age, na.rm = TRUE)) / sd(age, na.rm = TRUE))

levels(extractedBetaslong$label)

# combine with learning rate
head(modelSelection)
head(extractedBetaslong)

modelSelectionROI <- modelSelection %>% 
  rename(learningRate = alpha) %>% 
  mutate(modality = if_else(mod2Type == "aud", "AV", "TV"))

extractedBetaslong <- extractedBetaslong %>% 
  left_join(., modelSelectionROI[, c("ID", "learningRate", "modality")],
            by = join_by(ID, modality))

save(extractedBetaslong, file = file.path(outputFolder, "extractedBetaslong.RData"))
load(file.path(outputFolder, "extractedBetaslong.RData"))

# beta values MS
betasSubMS <- extractedBetaslong %>% 
  filter(mask == "MSTACT_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues))

MSImin <- floor(min(betasSubMS$betaValues)) # -2.849
MSImax <- ceiling(max(betasSubMS$betaValues)) # 9.624

betasMS <- extractedBetaslong %>% 
  filter(mask == "MSTACT_uniform",
         hemisphere == "wholeBrain")

# beta values PE
betasSubPE <- extractedBetaslong %>% 
  filter(mask == "PE_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues))

PEmin <- floor(min(betasSubPE$betaValues)) # -8.121
PEmax <- ceiling(max(betasSubPE$betaValues)) # 5.689

betasPE <- extractedBetaslong %>% 
  filter(mask == "PE_uniform",
         hemisphere == "wholeBrain")

# beta values Val
betasSubVAL <- extractedBetaslong %>% 
  filter(mask == "VAL_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues))

VALmin <- floor(min(betasSubVAL$betaValues)) # -180.406
VALmax <- ceiling(max(betasSubVAL$betaValues)) # 236.328

betasVAL <- extractedBetaslong %>% 
  filter(mask == "VAL_uniform",
         hemisphere == "wholeBrain")
# analyses ---------------------------------------------------------------
#######################################################################
p_values <- c()
combined_anovas <- tibble()

stimLM2 <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasMS)

summary(stimLM2)
anova(stimLM2)
combined_anovas <-  anova(stimLM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI")
    
report(stimLM2)

p_values <- c(p_values, summary(stimLM2)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimLM2)),
                      title = 'Linear Mixed Model for Learning Rate and Modality', note = 'ABC', 
                      highlight = T)
lmTable

ggplot(betasMS, aes(age, betaValues, colour=modality, group=modality, linetype = modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', linewidth = 1.2) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Multisensory network ROI') +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' = .006**')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .039*')",
           parse = TRUE, x = 5.8, y = 8.0,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI.png'),
       height = 15, width = 24, units = "cm")
ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI.svg'),
       height = 8, width = 12.8, units = "cm")
ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI.tif'),
       height = 8, width = 12.8, units = "cm")

## effects in subclusters ---
# -----------------------------
# anterior insula left 
betasSubMS1 <- betasSubMS %>% 
  filter(label == "anterior insula", hemisphere == "left")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Anterior Insula left") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Anterior Insula left', note = 'ABC', 
                      highlight = T)
lmTable

lAI <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  ggtitle('Left anterior insula') +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) 
lAI

#right
betasSubMS1 <- betasSubMS %>% 
  filter(label == "anterior insula", hemisphere == "right")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Anterior Insula right") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Anterior Insula right', note = 'ABC', 
                      highlight = T)
lmTable

rAI <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right anterior insula') + 
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
rAI

# inferior occipital cortex left 
betasSubMS1 <- betasSubMS %>% 
  filter(label == "inferior occipital cortex", hemisphere == "left")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Inferior Occipital Cortex left") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Inferior Occipital Cortex left', note = 'ABC', 
                      highlight = T)
lmTable

lIOC <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left inf. cccipital cortex') + 
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
lIOC 

#right
betasSubMS1 <- betasSubMS %>% 
  filter(label == "inferior occipital cortex", hemisphere == "right")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Inferior Occipital Cortex right") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Inferior Occipital Cortex right', note = 'ABC', 
                      highlight = T)
lmTable

rIOC <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right inf. occipital cortex') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
rIOC

# planum temporale left 
betasSubMS1 <- betasSubMS %>% 
  filter(label == "planum temporale", hemisphere == "left")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Planum Temporale left") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Planum Temporale left', note = 'ABC', 
                      highlight = T)
lmTable

lPT <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left planum temporale') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
lPT

#right
betasSubMS1 <- betasSubMS %>% 
  filter(label == "planum temporale", hemisphere == "right")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Planum Temporale right") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Planum Temporale Right', note = 'ABC', 
                      highlight = T)
lmTable

rPT <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right planum temporale') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
rPT 

# postcentral gyrus left 
betasSubMS1 <- betasSubMS %>% 
  filter(label == "postcentral gyrus", hemisphere == "left")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Postcentral Gyrus left") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Postcentral Gyrus left', note = 'ABC', 
                      highlight = T)
lmTable

lPCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left postcentral gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
lPCG

#right
betasSubMS1 <- betasSubMS %>% 
  filter(label == "postcentral gyrus", hemisphere == "right")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Postcentral Gyrus right") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Postcentral Gyrus right', note = 'ABC', 
                      highlight = T)
lmTable

rPCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right postcentral gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
rPCG

# precentral gyrus left 
betasSubMS1 <- betasSubMS %>% 
  filter(label == "precentral gyrus", hemisphere == "left")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Precentral Gyrus left") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Precentral Gyrus left', note = 'ABC', 
                      highlight = T)
lmTable

lPrCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left precentral gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
lPrCG

#right
betasSubMS1 <- betasSubMS %>% 
  filter(label == "precentral gyrus", hemisphere == "right")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Precentral Gyrus right") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Precentral Gyrus right', note = 'ABC', 
                      highlight = T)
lmTable

rPrCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right precentral gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
rPrCG

# superior parietal lobe left 
betasSubMS1 <- betasSubMS %>% 
  filter(label == "superior parietal lobe", hemisphere == "left")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Superior Parietal Lobe left") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Superior Parietal Lobe left', note = 'ABC', 
                      highlight = T)
lmTable

lSPL <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left sup. parietal lobe') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
lSPL

#right
betasSubMS1 <- betasSubMS %>% 
  filter(label == "superior parietal lobe", hemisphere == "right")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Superior Parietal Lobe right") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Superior Parietal Lobe right', note = 'ABC', 
                      highlight = T)
lmTable

rSPL <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right sup. parietal lobe') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
rSPL

# thalamus
betasSubMS1 <- betasSubMS %>% 
  filter(label == "thalamus")

stimSubLM <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasSubMS1)
summary(stimSubLM)
anova(stimSubLM)
combined_anovas <-  anova(stimSubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Thalamus") %>% 
  bind_rows(combined_anovas, .)

p_values <- c(p_values, summary(stimSubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(stimSubLM)),
                      title = 'Thalamus', note = 'ABC', 
                      highlight = T)
lmTable

thal <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Thalamus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
thal


## adjust p-values
adjusted_p_values <- p.adjust(p_values, method = "BH")

msTable <- data.frame(p_uncorr = round(p_values,3), p = round(adjusted_p_values,3))

lmTable <- nice_table(msTable,
                      title = 'adjusted p-values', note = 'ABC', 
                      col.format.custom = c(1), format.custom = 'fun3',
                      highlight = T)
lmTable

# get effects of anovas
combined_anovas_final <- combined_anovas %>% 
  select(Model, term, NumDF, statistic, p.value) %>% 
  rename(DF = NumDF,
         F.Value = statistic,
         p_uncorrected = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorrected, method = "BH")) 

lmTable <- nice_table(combined_anovas_final,
                      title = 'adjusted p-values', note = 'ABC', 
                      col.format.custom = c(5), format.custom = 'fun3',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

## annotate plots
lAI <- lAI +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' = .013*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_lAI.tif'),
       lAI,
       height = 8, width = 12.8, units = "cm")

rAI <- rAI +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' = .007**')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_rAI.tif'),
       rAI,
       height = 8, width = 12.8, units = "cm")

lIOC <- lIOC +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('modality ', italic(p), ' = .021*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age ', italic(p), ' = .002**')",
           parse = TRUE, x = 5.8, y = 8.0,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .010**')",
           parse = TRUE, x = 5.8, y = 6.2,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_lIOC.tif'),
       lIOC,
       height = 8, width = 12.8, units = "cm")

rIOC <- rIOC +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('modality ', italic(p), ' = .020*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .011*')",
           parse = TRUE, x = 5.8, y = 8.0,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_rIOC.tif'),
       rIOC,
       height = 8, width = 12.8, units = "cm")

lPCG <- lPCG +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' = .007**')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_lPCG.tif'),
       lPCG,
       height = 8, width = 12.8, units = "cm")

rPCG <- rPCG +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_rPCG.tif'),
       rPCG,
       height = 8, width = 12.8, units = "cm")

lPrCG <- lPrCG +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_lPrCG.tif'),
       lPrCG,
       height = 8, width = 12.8, units = "cm")

rPrCG <- rPrCG +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' = .020*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_rPrCG.tif'),
       rPrCG,
       height = 8, width = 12.8, units = "cm")

lPT <- lPT +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' = .015*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .001***')",
           parse = TRUE, x = 5.8, y = 8.0,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_lPT.tif'),
       lPT,
       height = 8, width = 12.8, units = "cm")

rPT <- rPT +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' = .020*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .001***')",
           parse = TRUE, x = 5.8, y = 8.0,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_rPT.tif'),
       rPT,
       height = 8, width = 12.8, units = "cm")

lSPL <- lSPL +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_lSPL.tif'),
       lSPL,
       height = 8, width = 12.8, units = "cm")

rSPL <- rSPL +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_rSPL.tif'),
       rSPL,
       height = 8, width = 12.8, units = "cm")

thal <- thal +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .042*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_thal.tif'),
       thal,
       height = 8, width = 12.8, units = "cm")

thal <- thal +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .042*')",
           parse = TRUE, x = 5.8, y = 9.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    strip.text.x = element_text(size=22),
    plot.title = element_text(size=22)
  ) 
legend <- get_legend(thal)
cowplot::ggdraw() + cowplot::draw_grob(legend)

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_Legend.tif'),
       height = 4, width = 6.4, units = "cm")

#---------------------------------------------------------------------------------------
# prediction Error ----
#------------------------------------------------------------------------------------------
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
                      highlight = T)
lmTable

ggplot(betasPE, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm') +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('RPE network ROI') +
  ylab("beta values") +
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

ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta.png'),
       height = 15, width = 24, units = "cm")
ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta.svg'),
       height = 8, width = 12.8, units = "cm")
ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta.tif'),
       height = 8, width = 12.8, units = "cm")

# effects in subclusters ------------------------------------------

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
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left anterior insula') + 
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), c(PEmin, PEmax)) +
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
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right anterior insula') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), c(PEmin, PEmax)) +
  theme(text = element_text(size = 20))
p2

ggsave(file.path(outputFolder, "figures", "PEanteriorInsula.eps"),
       width=159, height = 105, units = "mm")

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
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left precentral gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), c(PEmin, PEmax)) +
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
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right precentral gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), c(PEmin, PEmax)) +
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
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('L/R suppl. motor cortex') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), c(PEmin, PEmax)) +
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
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left ventral striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), c(PEmin, PEmax)) +
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
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right ventral striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), c(PEmin, PEmax)) +
  theme(text = element_text(size = 20))
p7

## adjust p-values
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

## annotate plots
p1 <- p1 +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_p1.tif'),
       p1,
       height = 8, width = 12.8, units = "cm")

p2 <- p2 +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  annotate("text", label = "paste('age ', italic(p), ' < .002**')",
           parse = TRUE, x = 5.7, y = 5.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits =  c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_p2.tif'),
       p2,
       height = 8, width = 12.8, units = "cm")

p3 <- p3 +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits =  c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_p3.tif'),
       p3,
       height = 8, width = 12.8, units = "cm")

p4 <- p4 +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits =  c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_p4.tif'),
       p4,
       height = 8, width = 12.8, units = "cm")

p5 <- p5 +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits =  c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_p5.tif'),
       p5,
       height = 8, width = 12.8, units = "cm")

p6 <- p6 +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_p6.tif'),
       p6,
       height = 8, width = 12.8, units = "cm")

p7 <- p7 +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_p7.tif'),
       p7,
       height = 8, width = 12.8, units = "cm")

# ------------------------------------------------------------------------------------
# value ----
# ---------------------------------------------------------------------------------------
p_values_VAL <- c()
combined_anovas_VAL <- tibble()

valLM2 <- lmer(betaValues ~ modality * age + (1|ID), betasVAL)

summary(valLM2)
anova(valLM2)
combined_anovas_VAL <-  anova(valLM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI")

p_values_VAL <- c(p_values_VAL, summary(valLM2)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(valLM2)),
                      title = 'ABC', note = 'ABC', 
                      highlight = T)
lmTable

ggplot(betasVAL, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Activation in VAL Regions during VAL Processing') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 20))

ggsave(file.path(outputFolder, 'plots', 'ROIVALBeta.png'),
       height = 15, width = 24, units = "cm")

# effects in subclusters ---
#------------------------------------------------------------------------------------

# amygdala right
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "amygdala", hemisphere == "right")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Amygdala right") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Right Amygdala', note = 'ABC', 
                      highlight = T)
lmTable

p10 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Amygdala') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p10

# anterior insula left
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "anterior insula", hemisphere == "left")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Anterior Insula left") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Left Anterior Insula', note = 'ABC', 
                      highlight = T)
lmTable

p20 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Anterior Insula') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p20

# anterior insula right
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "anterior insula", hemisphere == "right")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Anterior Insula right") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'right Anterior Insula', note = 'ABC', 
                      highlight = T)
lmTable

p30 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('right Anterior Insula') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p30

# medial frontal cortex
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "medial frontal cortex", hemisphere == "bilateral")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Medial Frontal Cortex bilateral") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Bilateral Medial Frontal Cortex', note = 'ABC', 
                      highlight = T)
lmTable

p40 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Bilateral Medial Frontal Cortex') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p40

# precentral gyrus right
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "precentral gyrus", hemisphere == "right")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Precentral Gyrus right") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Right Precentral Gyrus', note = 'ABC', 
                      highlight = T)
lmTable

p50 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Precentral Gyrus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p50

# supplementary motor cortex bilateral
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "supplementary motor cortex", hemisphere == "bilateral")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Supplementary Motor Cortex bilateral") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Bilateral Supplementary Motor Cortex', note = 'ABC', 
                      highlight = T)
lmTable

p60 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Bilateral Supplementary Motor Cortex') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p60

# ventral striatum left
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "ventral striatum", hemisphere == "left")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Ventral Striatum left") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Left Ventral Striatum', note = 'ABC', 
                      highlight = T)
lmTable

p70 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Ventral Striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p70

# ventral striatum right
betasSubVAL1 <- betasSubVAL %>% 
  filter(label == "ventral striatum", hemisphere == "right")

SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
summary(SubLM)
anova(SubLM)
combined_anovas_VAL <-  anova(SubLM) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Ventral Striatum right") %>% 
  bind_rows(combined_anovas_VAL, .)

p_values_VAL <- c(p_values_VAL, summary(SubLM)$coefficients[,"Pr(>|t|)"]) 

lmTable <- nice_table(as.data.frame(report_table(SubLM)),
                      title = 'Right Ventral Striatum', note = 'ABC', 
                      highlight = T)
lmTable

p80 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Ventral Striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p80

## adjust p-values
adjusted_p_values_VAL <- p.adjust(p_values_VAL, method = "BH")

msTable <- data.frame(p_uncorr = p_values_VAL, p = adjusted_p_values_VAL)

# get effects of anovas
combined_anovas_VAL_final <- combined_anovas_VAL %>% 
  select(Model, term, NumDF, statistic, p.value) %>% 
  rename(DF = NumDF,
         F.Value = statistic,
         p_uncorrected = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorrected, method = "BH")) 

lmTable <- nice_table(combined_anovas_VAL_final,
                      title = 'adjusted p-values', note = 'ABC', 
                      col.format.custom = c(5), format.custom = 'fun3',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

