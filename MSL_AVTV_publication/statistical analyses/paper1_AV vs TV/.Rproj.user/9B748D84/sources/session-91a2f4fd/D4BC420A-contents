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
load(file.path(outputFolder, "demo.RData"))
#load(file.path(outputFolder, "modelSelection.RData"))
modellingOuput <- read_csv(file.path(outputFolder, "modelling", "modellingOuputfMRI_adults.csv"))
# -> need the summary
modellingOuputAdults <- read_csv(file.path(outputFolder, "modelling", "modellingOuputfMRI.csv"))
# -> need the summary


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

min(betasSubMS$betaValues) # -2.849
max(betasSubMS$betaValues) # 9.624

betasMS <- extractedBetaslong %>% 
  filter(mask == "MSTACT_uniform",
         hemisphere == "wholeBrain")

# beta values PE
betasSubPE <- extractedBetaslong %>% 
  filter(mask == "PE_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues))

min(betasSubPE$betaValues) # -8.121
max(betasSubPE$betaValues) # 5.689

betasPE <- extractedBetaslong %>% 
  filter(mask == "PE_uniform",
         hemisphere == "wholeBrain")

# beta values Val
betasSubVAL <- extractedBetaslong %>% 
  filter(mask == "VAL_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues))

min(betasSubVAL$betaValues) # -180.406
max(betasSubVAL$betaValues) # 236.328

betasVAL <- extractedBetaslong %>% 
  filter(mask == "VAL_uniform",
         hemisphere == "wholeBrain")
# analyses ---------------------------------------------------------------
#######################################################################
p_values <- c()
combined_anovas <- tibble()

stimLM2 <- lmer(betaValues ~ modality * age + learningRate + (1|ID), betasMS)
#lmerTest::step(lmer(betaValues ~ modality * age * learningRate + (1|ID), betasMS))

summary(stimLM2)
anova(stimLM2)
combined_anovas <-  anova(stimLM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI")
    
report(stimLM2)

p_values <- c(p_values, summary(stimLM2)$coefficients[,"Pr(>|t|)"]) 

# fitted vs residual with smooth line added
# plot(stimLM2, type=c("p","smooth"), col.line=1)
# scale-location plot
# plot(stimLM2,
#      sqrt(abs(resid(.)))~fitted(.),
#      type=c("p","smooth"), col.line=1)
# Q-Q plot
# lattice::qqmath(stimLM2)
# residuals vs leverage
# plot(stimLM2, rstudent(.) ~ hatvalues(.))
# car::influencePlot(stimLM2)

lmTable <- nice_table(as.data.frame(report_table(stimLM2)),
                      title = 'Linear Mixed Model for Learning Rate and Modality', note = 'ABC', 
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

ggplot(betasMS, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point() +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #ggtitle('Multisensory Regions during Stimulus Presentation') +
  ylab("Beta Values") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 25))

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI.png'),
       height = 15, width = 24, units = "cm")

ggplot(betasSubMS, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~label) +
  ggtitle('Multisensory Regions during Stimulus Presentation') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 20))

## effects in subclusters ---
# -----------------------------
# p = 0.05/13 = 0.003846154
# p = 0.05/14 = 0.003571429
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

lAI <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Anterior Insula') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

rAI <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Anterior Insula') + ylim(-2.5,12) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

lIOC <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Inferior Occipital Cortex') + ylim(-2.5,12) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

rIOC <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Inferior Occipital Cortex') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

lPT <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Planum Temporale') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

rPT <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Planum Temporale') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

lPCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Postcentral Gyrus') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

rPCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Postcentral Gyrus') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

lPrCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Precentral Gyrus') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

rPrCG <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Precentral Gyrus') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

lSPL <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Left Superior Parietal Lobe') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

rSPL <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Superior Parietal Lobe') +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

thal <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Thalamus') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-3, 10)) +
  theme(text = element_text(size = 20))
thal

ggarrange(lAI, rAI, lIOC, rIOC, lPCG, rPCG, lPrCG, rPrCG, lPT, rPT, lSPL, rSPL, thal,
             common.legend = TRUE, legend = "bottom")

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_sub.png'),
       height = 30, width = 48, units = "cm")

# adjust p-values
adjusted_p_values <- p.adjust(p_values, method = "BH")

msTable <- data.frame(p_uncorr = round(p_values,3), p = round(adjusted_p_values,3))

lmTable <- nice_table(msTable,
                      title = 'adjusted p-values', note = 'ABC', 
                      col.format.custom = c(1), format.custom = 'fun3',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

# get effects of anovas
anova(stimLM2)
report(anova(stimLM2))
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

ggplot(betasPE, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #ggtitle('Activation in PE Regions during PE Processing') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 25))

ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta.png'),
       height = 15, width = 24, units = "cm")

# effects in subclusters ------------------------------------------
# p = 0.05/7 = 0.007142857
# p = 0.05/8 = 0.00625
levels(betasSubPE$label)
ggplot(betasSubPE, aes(age, betaValues, colour=modality, group=label)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(label~hemisphere)

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p2 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Anterior Insula') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p7 <- ggplot(betasSubPE1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Ventral Striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-9, 7)) +
  theme(text = element_text(size = 20))
p7

ggarrange(p1, p2, p3, p4, p5, p6, p7,
          common.legend = TRUE, legend = "bottom")

ggsave(file.path(outputFolder, 'figures', 'ROIPEBetaPE_sub.png'),
       height = 30, width = 48, units = "cm")

# adjust p-values
adjusted_p_values_PE <- p.adjust(p_values_PE, method = "BH")

msTable <- data.frame(p_uncorr = p_values_PE, p = adjusted_p_values_PE)

lmTable <- nice_table(msTable,
                      title = 'adjusted p-values', note = 'ABC', 
                      col.format.custom = c(1), format.custom = 'fun3',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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
# p = 0.05/8 = 0.00625
# p = 0.05/9 = 0.0055555556
levels(betasSubVAL$label)
ggplot(betasSubVAL, aes(age, betaValues, colour=modality, group=label)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(label~hemisphere)

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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p10 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p20 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p30 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p40 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p50 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p60 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p70 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
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
                      #col.format.custom = c(2:6, 11:13), format.custom = 'fun',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

p80 <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', aes(linetype = modality)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Right Ventral Striatum') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-181, 237)) +
  theme(text = element_text(size = 20))
p80

ggarrange(p10, p20, p30, p40, p50, p60, p70, p80,
          common.legend = TRUE, legend = "bottom")

ggsave(file.path(outputFolder, 'plots', 'ROIValBetaVAL_sub.png'),
       height = 30, width = 48, units = "cm")

# adjust p-values
adjusted_p_values_VAL <- p.adjust(p_values_VAL, method = "BH")

msTable <- data.frame(p_uncorr = p_values_VAL, p = adjusted_p_values_VAL)

lmTable <- nice_table(msTable,
                      title = 'adjusted p-values', note = 'ABC', 
                      col.format.custom = c(1), format.custom = 'fun3',
                      highlight = T)
lmTable
print(lmTable, preview = 'docx')

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