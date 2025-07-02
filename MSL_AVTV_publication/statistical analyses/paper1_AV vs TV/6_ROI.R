##########################################################
##                    ROI ANAlyses                      ##
##########################################################

# Load and merge data ------------------------------------
##########################################################
load(file.path(outputFolder, "demo.RData"))
load(file.path(outputFolder, "modelSelection_rev1.RData"))

files <- list.files(file.path(inputFolder, 'ROI')) %>% 
  discard(str_detect(.,'extracedBetas.csv'))

extractedBetasList <- lapply(file.path(inputFolder, 'ROI', files), data.table::fread)

extractedBetas <- data.frame()

for (i in 1:length(extractedBetasList)) {
  temp <- extractedBetasList[[i]]
  
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
  summarise(betaValues = mean(betaValues)) %>% 
  ungroup()

MSImin <- floor(min(betasSubMS$betaValues)) # -2.870
MSImax <- ceiling(max(betasSubMS$betaValues)) # 8.719

betasMS <- extractedBetaslong %>% 
  filter(mask == "MSTACT_uniform",
         hemisphere == "wholeBrain")

# beta values PE
betasSubPE <- extractedBetaslong %>% 
  filter(mask == "PE_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues))  %>% 
  ungroup()

PEmin <- floor(min(betasSubPE$betaValues)) # -4.245
PEmax <- ceiling(max(betasSubPE$betaValues)) # 2.295

betasPE <- extractedBetaslong %>% 
  filter(mask == "PE_uniform",
         hemisphere == "wholeBrain")

# beta values Val
betasSubVAL <- extractedBetaslong %>% 
  filter(mask == "VAL_uniform",
         hemisphere != "wholeBrain") %>% 
  group_by(ID, mask, modality, label, hemisphere, age, learningRate) %>% 
  summarise(betaValues = mean(betaValues)) %>% 
  ungroup()

VALmin <- floor(min(betasSubVAL$betaValues)) # -1017.277
VALmax <- ceiling(max(betasSubVAL$betaValues)) # 1323.536

betasVAL <- extractedBetaslong %>% 
  filter(mask == "VAL_uniform",
         hemisphere == "wholeBrain")
# analyses ---------------------------------------------------------------
#######################################################################
combined_anovas <- tibble()

betasMS <- betasMS %>%
  mutate(
    age_c           = age - mean(age),
    learningRate_c  = learningRate - mean(learningRate),
  )

stimLM2 <- lmer(betaValues ~ modality * age_c + learningRate_c + (1|ID), betasMS,
                contrasts = list(modality = contr.sum))

summary(stimLM2)
anova(stimLM2, type = 3)
report(anova(stimLM2, type = 3))

effSizes <- effectsize::eta_squared(anova(stimLM2, type = 3), partial = TRUE) %>% 
  as_tibble() %>% 
  select(Parameter, Eta2_partial, CI_low, CI_high) 

combined_anovas <-  anova(stimLM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI") %>%
  left_join(effSizes, by = c("term" = "Parameter"))

ggplot(betasMS, aes(age, betaValues, colour=modality, group=modality, linetype = modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', linewidth = 1.2) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Multisensory network ROI') +
  ylab("beta values") +
  annotate("text", label = "paste('modality ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 7.3,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('learning rate ', italic(p), ' = .010**')",
           parse = TRUE, x = 5.8, y = -1.29,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI.svg'),
       height = 8, width = 12.8, units = "cm")
ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI.tif'),
       height = 8, width = 12.8, units = "cm")

ggplot(betasMS, aes(betaValues, learningRate, colour=modality, group=modality, linetype = modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', linewidth = 1.2, fullrange = T) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Multisensory network ROI') +
  xlab("beta values") + ylab("Learning Rate") +
  annotate("text", label = "paste('learning rate ', italic(p), ' = .010**')",
           parse = TRUE, x = -0.5, y = 0.9,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_LR.svg'),
       height = 8, width = 12.8, units = "cm")
ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_LR.tif'),
       height = 8, width = 12.8, units = "cm")

betasSubMS1 <- betasSubMS %>% 
  filter(label == "planum temporale", hemisphere == "right")

ggplot(betasSubMS1, aes(betaValues, learningRate, colour=modality, group=modality, linetype = modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', linewidth = 1.2, fullrange = T) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('R planum temporale') +
  xlab("beta values") + ylab("Learning Rate") +
  annotate("text", label = "paste('learning rate ', italic(p), ' = .010**')",
           parse = TRUE, x = -0.5, y = 0.9,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
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

betasSubMS11 <- betasSubMS %>% 
  filter(label == "postcentral gyrus", hemisphere == "right")

ggplot(betasSubMS11, aes(betaValues, learningRate, colour=modality, group=modality, linetype = modality)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='lm', linewidth = 1.2, fullrange = T) +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('R postcentral gyrus') +
  xlab("beta values") + ylab("Learning Rate") +
  annotate("text", label = "paste('learning rate ', italic(p), ' = .010**')",
           parse = TRUE, x = -0.5, y = 0.9,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
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

ggplot(betasMS, aes(x = learningRate_c)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of centered learningRate",
       x     = "learningRate (centered)",
       y     = "Count")

car::influencePlot(stimLM2)

ggplot(betasSubMS, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(label~hemisphere) +
  ggtitle('Multisensory Regions during Stimulus Presentation') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 20))

## effects in subclusters ---
clusters <- unique(betasSubMS$label)
hemispheres <- unique(betasSubMS$hemisphere)

plots_MSI <- list()

for (cluster in clusters) {
  for (hem in hemispheres) {
    # Filter the data for the current cluster and hemisphere
    betasSubMS1 <- betasSubMS %>% 
      filter(label == cluster, hemisphere == hem) %>% 
      mutate(
        age_c           = age - mean(age),
        learningRate_c  = learningRate - mean(learningRate),
      )
    
    if (nrow(betasSubMS1) == 0) {
      next
    }
    
    SubLM <- lmer(betaValues ~ modality * age_c + learningRate_c + (1|ID), betasSubMS1,
                  contrasts = list(modality = contr.sum))
    
    effSizes1 <- effectsize::eta_squared(anova(SubLM, type = 3), partial = TRUE) %>% 
      as_tibble() %>% 
      select(Parameter, Eta2_partial, CI_low, CI_high) 
    
    combined_anovas <-  anova(SubLM) %>% 
      broom.mixed::tidy(effects = "fixed") %>% 
      mutate(Model = paste(cluster, hem)) %>%
      left_join(effSizes1, by = c("term" = "Parameter")) %>% 
      bind_rows(combined_anovas, .)
    
    plot  <- ggplot(betasSubMS1, aes(age, betaValues, colour=modality, group=modality)) +
      geom_point(alpha=0.6) +
      geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
      ggtitle(paste(cluster, hem)) +
      scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
      jtools::theme_apa(remove.y.gridlines = F) + 
      scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
      theme(text = element_text(size = 20))
    
    # Add plot to the list
    plots_MSI[[paste(cluster, hem)]] <- plot
  }
}

combined_anovas_MSI_corr <- combined_anovas %>% 
  #filter(Model != 'Whole ROI') %>% 
  rename(p_uncorr = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorr, method = "BH"),
         p_uncorr = sprintf("%.3f", p_uncorr))  %>% 
  select(Model, term, sumsq, meansq, NumDF, DenDF, statistic, p_uncorr, p.value, Eta2_partial, CI_low, CI_high)

nice_table(combined_anovas_MSI_corr, highlight = TRUE,
           title = "MSI: Anova Main and Interaction effects") %>% 
  print(., preview = 'docx')

ggarrange(plotlist = plots_MSI, ncol = 3, nrow = 5,
          common.legend = TRUE)

plots_MSI[[1]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L anterior insula") +
  annotate("text", label = "paste('age ', italic(p), ' = .006**')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  jtools::theme_apa(remove.y.gridlines = F) + 
  scale_y_continuous(expand = c(0, 0), limits = c(MSImin, MSImax)) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[2]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R anterior insula") +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[3]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L inf. occipital cortex") +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[4]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R inf. occipital cortex") +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[5]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L planum temporale") +
  annotate("text", label = "paste('modality ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age ', italic(p), ' = .005*')",
           parse = TRUE, x = 5.8, y = 7.3,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .006**')",
           parse = TRUE, x = 5.8, y = 5.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[6]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R planum temporale") +
  annotate("text", label = "paste('modality ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age ', italic(p), ' = .003**')",
           parse = TRUE, x = 5.8, y = 7.3,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .001**')",
           parse = TRUE, x = 5.8, y = 5.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('learning rate ', italic(p), ' = .017*')",
           parse = TRUE, x = 5.8, y = -1.29,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[7]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L postcentral gyrus") +
  annotate("text", label = "paste('modality ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 7.3,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .006**')",
           parse = TRUE, x = 5.8, y = 5.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_lPoCG.tif'),
       height = 8, width = 12.8, units = "cm")

plots_MSI[[8]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R postcentral gyrus") +
  annotate("text", label = "paste('age ', italic(p), ' = .046*')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('learning rate ', italic(p), ' = .006**')",
           parse = TRUE, x = 5.8, y = -1.29,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_rPoCG.tif'),
       height = 8, width = 12.8, units = "cm")

plots_MSI[[9]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L precentral gyrus") +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[10]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R precentral gyrus") +
  annotate("text", label = "paste('modality ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age ', italic(p), ' = .001**')",
           parse = TRUE, x = 5.8, y = 7.3,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .003**')",
           parse = TRUE, x = 5.8, y = 5.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[11]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L superior parietal lobe") +
  annotate("text", label = "paste('modality ', italic(p), ' = .041*')",
           parse = TRUE, x = 5.8, y = 8.8,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = 7.3,
           hjust = 0, vjust = 1, color = "blue", size = 6) +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[12]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R superior parietal lobe") +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[13]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L thalamus") +
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
       height = 8, width = 12.8, units = "cm")

plots_MSI[[13]] +
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
    plot.title = element_text(size=22)
  ) 
legend <- get_legend(plots_MSI[[13]])
cowplot::ggdraw() + cowplot::draw_grob(legend)

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaMSI_Legend.tif'),
       height = 4, width = 6.4, units = "cm")

#---------------------------------------------------------------------------------------
# prediction Error ----
#------------------------------------------------------------------------------------------
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
           hjust = 0, vjust = 0, color = "blue", size = 6) +
  annotate("text", label = "paste('age ', italic(p), ' = .002**')",
           parse = TRUE, x = 5.8, y = -3.7,
           hjust = 0, vjust = 0, color = "blue", size = 6) +
  annotate("text", label = "paste('age × modality ', italic(p), ' = .046*')",
           parse = TRUE, x = 5.8, y = -4.8,
           hjust = 0, vjust = 0, color = "blue", size = 6) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta.svg'),
       height = 8, width = 12.8, units = "cm")
ggsave(file.path(outputFolder, 'figures', 'ROIPEBeta.tif'),
       height = 8, width = 12.8, units = "cm")

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

ggarrange(plotlist = plots_PE, ncol = 3, nrow = 5,
          common.legend = TRUE)

plots_PE[[1]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L anterior insula") +
  jtools::theme_apa(remove.y.gridlines = F) + 
  annotate("text", label = "paste('age ', italic(p), ' = .001***')",
           parse = TRUE, x = 5.8, y = -4.8,
           hjust = 0, vjust = 0, color = "blue", size = 6) +
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_lAI.tif'),
       height = 8, width = 12.8, units = "cm")

plots_PE[[1]] +
  scale_linetype_manual(values=c("solid", "solid")) +
  ylab("beta values") +
  ggtitle("L anterior insula") +
  jtools::theme_apa(remove.y.gridlines = F) + 
  # annotate("text", label = "paste('age ', italic(p), ' = .001***')",
  #          parse = TRUE, x = 5.8, y = -4.8,
  #          hjust = 0, vjust = 0, color = "blue", size = 6) +
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_lAI.eps'),
       height = 104, width = 155, units = "mm")

plots_PE[[2]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R anterior insula") +
  annotate("text", label = "paste('age ', italic(p), ' < .001***')",
           parse = TRUE, x = 5.8, y = -4.8,
           hjust = 0, vjust = 0, color = "blue", size = 6) +
  jtools::theme_apa(remove.y.gridlines = F) + 
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_rAI.tif'),
       height = 8, width = 12.8, units = "cm")

plots_PE[[3]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L precentral gyrus") +
  jtools::theme_apa(remove.y.gridlines = F) + 
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_lPrCG.tif'),
       height = 8, width = 12.8, units = "cm")

plots_PE[[4]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R precentral gyrus") +
  jtools::theme_apa(remove.y.gridlines = F) + 
  annotate("text", label = "paste('age ', italic(p), ' = .004**')",
           parse = TRUE, x = 5.8, y = -4.8,
           hjust = 0, vjust = 0, color = "blue", size = 6) +
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_rPrCG.tif'),
       height = 8, width = 12.8, units = "cm")

plots_PE[[5]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L/R suppl. motor cortex") +
  jtools::theme_apa(remove.y.gridlines = F) + 
  annotate("text", label = "paste('age ', italic(p), ' = .001***')",
           parse = TRUE, x = 5.8, y = -4.8,
           hjust = 0, vjust = 0, color = "blue", size = 6) +
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_lrSMC.tif'),
       height = 8, width = 12.8, units = "cm")

plots_PE[[6]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("L striatum") +
  jtools::theme_apa(remove.y.gridlines = F) + 
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_lStri.tif'),
       height = 8, width = 12.8, units = "cm")

plots_PE[[7]] +
  scale_linetype_manual(values=c("longdash", "dotdash")) +
  ylab("beta values") +
  ggtitle("R striatum") +
  jtools::theme_apa(remove.y.gridlines = F) + 
  annotate("text", label = "paste('modality ', italic(p), ' = .014*')",
           parse = TRUE, x = 5.8, y = -4.8,
           hjust = 0, vjust = 0, color = "blue", size = 6) +
  scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
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

ggsave(file.path(outputFolder, 'figures', 'ROIstimBetaPE_rStri.tif'),
       height = 8, width = 12.8, units = "cm")

# ------------------------------------------------------------------------------------
# value ----
# ---------------------------------------------------------------------------------------
combined_anovas_VAL <- tibble()

valLM2 <- lmer(betaValues ~ modality * age + (1|ID), betasVAL)

summary(valLM2)
anova(valLM2)
combined_anovas_VAL <-  anova(valLM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI")

ggplot(betasVAL, aes(age, betaValues, colour=modality, group=modality)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Activation in VAL Regions during VAL Processing') +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 20))

# ggsave(file.path(outputFolder, 'plots', 'ROIVALBeta.png'),
#        height = 15, width = 24, units = "cm")

# effects in subclusters ---
clusters <- unique(betasSubVAL$label)
hemispheres <- unique(betasSubVAL$hemisphere)

plots_VAL <- list()

for (cluster in clusters) {
  for (hem in hemispheres) {
    # Filter the data for the current cluster and hemisphere
    betasSubVAL1 <- betasSubVAL %>% 
      filter(label == cluster, hemisphere == hem)
    
    if (nrow(betasSubVAL1) == 0) {
      next
    }
    
    SubLM <- lmer(betaValues ~ modality * age + (1|ID), betasSubVAL1)
    
    combined_anovas_VAL <-  anova(SubLM) %>% 
      broom.mixed::tidy(effects = "fixed") %>% 
      mutate(Model = paste(cluster, hem)) %>% 
      bind_rows(combined_anovas_VAL, .)
    
    plot  <- ggplot(betasSubVAL1, aes(age, betaValues, colour=modality, group=modality)) +
      geom_point(alpha=0.6) +
      geom_smooth(method='lm', aes(linetype = modality), linewidth = 1.2) +
      ggtitle(paste(cluster, hem)) +
      scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
      jtools::theme_apa(remove.y.gridlines = F) + 
      scale_y_continuous(expand = c(0, 0), limits = c(PEmin, PEmax)) +
      theme(text = element_text(size = 20))
    
    # Add plot to the list
    plots_VAL[[paste(cluster, hem)]] <- plot
  }
}

combined_anovas_VAL_corr <- combined_anovas_VAL %>% 
  #filter(Model != 'Whole ROI') %>% 
  rename(p_uncorr = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorr, method = "BH"),
         p_uncorr = sprintf("%.3f", p_uncorr))  %>% 
  select(Model, term, sumsq, meansq, NumDF, DenDF, statistic, p_uncorr, p.value)

nice_table(combined_anovas_VAL_corr, highlight = TRUE,
           title = "VAL: Anova Main and Interaction effects") %>% 
  print(., preview = 'docx')

ggplot(betasSubVAL, aes(age, betaValues, colour=modality, group=label)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(label~hemisphere)

