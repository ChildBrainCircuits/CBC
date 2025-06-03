##########################################################
##                        PREPARE                       ##
##########################################################
## Description :: 
## Input :::::::: 
## Libraries :::: 
## Output ::::::: 
##########################################################

initialVars <- ls()

# Load and merge data ------------------------------------
##########################################################
load(file.path(outputFolder, "demo.RData"))
load(file.path(outputFolder, "finalSample.RData"))
load(file.path(outputFolder, "finalSampleAdults.RData"))

finalSample$group <- "children"
finalSampleAdults$group <- "adults"

finalSampleAll <- finalSample %>% 
  select(-geburtsdatum) %>% 
  bind_rows(finalSampleAdults %>% 
              select(-geburtsdatum)) %>% 
  ungroup() %>% 
  mutate(Task = if_else(Task == "av11", "av1", Task),
         Task = if_else(Task == "tv11", "tv1", Task))

files <- list.files(file.path(inputFolder, 'ROI', 'marsbar_adults_vs_children')) %>% 
  discard(str_detect(.,'extracedBetas.csv')) %>% 
  discard(str_detect(.,'as_overview.csv'))

extractedBetasList <- lapply(file.path(inputFolder, 'ROI', 'marsbar_adults_vs_children', files), data.table::fread)

extractedBetas <- data.frame()

for (i in 1:length(extractedBetasList)) {
  temp <- extractedBetasList[[i]]
  
  extractedBetas <- extractedBetas %>% 
    bind_rows(., temp)
}

extractedBetas <- extractedBetas %>% 
  filter(ID != "CBC_1089") %>% 
  left_join(.,finalSampleAll[,c('ID', 'age', 'version', 'group')],
            by = join_by(ID)) 

extractedBetas$hemisphere[extractedBetas$hemisphere=='bilater'] <- 'bilateral'


extractedBetaslong <- extractedBetas %>% 
  rename(modality = task,
         betaValues = beta) %>% 
  mutate(mask = as.factor(mask),
         modality = as.factor(modality),
         hemisphere = as.factor(hemisphere),
         label = as.factor(label),
         ) 

levels(extractedBetaslong$label)

# get the groups
extractedBetasAvC <- extractedBetaslong %>% 
  filter(version == 'v1')

###############################################################################
# adults vs children ----

# beta values unexpected mask
betasUNEX <- extractedBetasAvC %>% 
  filter(mask == "unexpected",
         hemisphere == "wholeBrain") %>% 
  distinct()

betasSubUNEX <- extractedBetasAvC %>%
  filter(mask == "unexpected",
         hemisphere != "wholeBrain") %>%
  group_by(ID, mask, modality, group, label, hemisphere, age) %>%
  summarise(betaValues = mean(betaValues)) %>% 
  distinct()

table(betasSubUNEX$ID, betasSubUNEX$modality)

(minUNEX <- floor(min(betasSubUNEX$betaValues))) # -1.66
(maxUNEX <- ceiling(max(betasSubUNEX$betaValues))) # 2.33

################################################################################
## Unexpected ROI ---------------------------------------------------------------

# Initialize empty data frames and lists for results
combined_anovas_UNEX_AvC <- tibble()

stimLM2 <- lmer(betaValues ~ modality * group + (1|ID), betasUNEX)
summary(stimLM2)
anova(stimLM2)

combined_anovas_UNEX_AvC <-  anova(stimLM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI")

UNEXwm <- ggplot(betasUNEX, aes(group, betaValues, fill = modality)) +
  #geom_violin() +
  #geom_boxplot(color = "black", width = 0.5, fill = "white", alpha = 0.5) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0.2, color = "black") +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  #ggtitle('Multisensory Regions during Stimulus Presentation') +
  #facet_grid(~version) +
  ylab("Beta Values") + #ylim(-1,4) +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(-4,3)) +
  theme(text = element_text(size = 25)) 
UNEXwm

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_AvC.png'), 
       UNEXwm,
       #width = 24, height = 15, units = "cm")
       width = 18, height = 16, units = 'cm')

### subcluster -------------------------------------------------------------------

clusters <- unique(betasSubUNEX$label)
hemispheres <- c("left", "right", "bilateral, left", "bilateral")

plots_UNEX_AvC <- list()

for (cluster in clusters) {
  for (hem in hemispheres) {
    # Filter the data for the current cluster and hemisphere
    betasSubUNEX1 <- betasSubUNEX %>% 
      filter(label == cluster, hemisphere == hem)
    
    if (nrow(betasSubUNEX1) == 0) {
      next
    }
    
    SubLM <- lmer(betaValues ~ modality * group + (1|ID), betasSubUNEX1)
    
    combined_anovas_UNEX_AvC <-  anova(SubLM) %>% 
      broom.mixed::tidy(effects = "fixed") %>% 
      mutate(Model = paste(cluster, hem)) %>% 
      bind_rows(combined_anovas_UNEX_AvC, .)
    
    plot  <- ggplot(betasSubUNEX1, aes(group, betaValues, fill=modality)) +
      introdataviz::geom_split_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.2, color = "black") +
      scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
      scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
      ggtitle(paste(cluster, hem)) +
      jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits = c(minUNEX, maxUNEX)) +
      theme(text = element_text(size = 20))
    
    # Add plot to the list
    plots_UNEX_AvC[[paste(cluster, hem)]] <- plot
  }
}

ggarrange(plotlist = plots_UNEX_AvC, ncol = 4, nrow = 4,
          common.legend = TRUE)

combined_anovas_UNEX_AvC_corr <- combined_anovas_UNEX_AvC %>% 
  filter(Model != 'Whole ROI') %>% 
  rename(p_uncorr = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorr, method = "BH"),
         p_uncorr = sprintf("%.3f", p_uncorr))  %>% 
  select(Model, term, sumsq, meansq, NumDF, DenDF, statistic, p_uncorr, p.value)

nice_table(combined_anovas_UNEX_AvC_corr, highlight = TRUE,
           title = "UNEX: Anova Main and Interaction effects A v C") %>% 
  print(., preview = 'docx')

combined_anovas_UNEX_AvC_corr2 <- combined_anovas_UNEX_AvC %>% 
  filter(!(Model %in% c('Whole ROI', 
                        'superior temporal gyrus right', 'superior temporal gyrus left',
                        'planum temporale left',
                        'precentral gyrus left',
                        'thalamus right'
  ))) %>% 
  rename(p_uncorr = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorr, method = "BH"),
         p_uncorr = sprintf("%.3f", p_uncorr))  %>% 
  select(Model, term, sumsq, meansq, NumDF, DenDF, statistic, p_uncorr, p.value)

nice_table(combined_anovas_UNEX_AvC_corr2, highlight = TRUE,
           title = "UNEX: Anova Main and Interaction effects A v C") %>% 
  print(., preview = 'docx')

plots_UNEX_AvC[[1]] +
  ylab("beta values") +
  ggtitle('R anterior insula') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_1.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[2]] +
  ylab("beta values") +
  ggtitle('L mid. frontal gyrus') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_2.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[3]] +
  ylab("beta values") +
  ggtitle('R mid. frontal gyrus') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_3.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[4]] +
  ylab("beta values") +
  ggtitle('L planum temporale') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_4.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[5]] +
  ylab("beta values") +
  ggtitle('L precentral gyrus') +
  annotate("text", label = "paste('age group ', italic(p), ' = .05*')",
           parse = TRUE, x = 0.5, y = 2.8,
           hjust = 0, vjust = 1, color = "blue", size = 7.5) +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_5.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[6]] +
  ylab("beta values") +
  ggtitle('R striatum') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_6.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[7]] +
  ylab("beta values") +
  ggtitle('L striatum, thal., ant. insula') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_7.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[8]] +
  ylab("beta values") +
  ggtitle('L sup. parietal lobe') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_8.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[9]] +
  ylab("beta values") +
  ggtitle('R sup. parietal lobe') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_9.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[10]] +
  ylab("beta values") +
  ggtitle('L sup. temporal gyrus') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_10.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[11]] +
  ylab("beta values") +
  ggtitle('R sup. temporal gyrus') +
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

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_11.tif'),
       height = 8, width = 12.8, units = "cm")

plots_UNEX_AvC[[12]] +
  ylab("beta values") +
  ggtitle('R thalamus') +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 22), # Axis titles
    axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 22),  # Legend text
    strip.text.x = element_text(size=22),
    plot.title = element_text(size=22),
    legend.position = "bottom"
  )

ggsave(file.path(outputFolder, 'plots', 'ROI_UNEX_12.tif'),
       height = 8, width = 12.8, units = "cm")

###############################################################################
# v1 vs v3 ----

files <- list.files(file.path(inputFolder, 'ROI', 'marsbar_discChoice_vs_matchRecog')) %>% 
  discard(str_detect(.,'extracedBetas.csv')) %>% 
  discard(str_detect(.,'as_overview.csv'))

extractedBetasList <- lapply(file.path(inputFolder, 'ROI', 'marsbar_discChoice_vs_matchRecog', files), data.table::fread)

extractedBetas2 <- data.frame()

for (i in 1:length(extractedBetasList)) {
  temp <- extractedBetasList[[i]]
  
  extractedBetas2 <- extractedBetas2 %>% 
    bind_rows(., temp)
}

extractedBetas2 <- extractedBetas2 %>% 
  filter(ID != "CBC_1089") %>% 
  left_join(.,finalSampleAll[,c('ID', 'age', 'version', 'group')],
            by = join_by(ID)) 


extractedBetaslong2 <- extractedBetas2 %>% 
  rename(modality = task,
         betaValues = beta) %>% 
  mutate(mask = as.factor(mask),
         modality = as.factor(modality),
         hemisphere = as.factor(hemisphere),
         label = as.factor(label),
         task = if_else(version=="v3", "discChoice", "matchRecog"),
  ) 

levels(extractedBetaslong2$label)
levels(extractedBetaslong2$mask)

# beta values PE
betasAC <- extractedBetaslong2 %>% 
  filter(mask == "AC",
         hemisphere == "wholeBrain") %>% 
  distinct()

betasSubAC <- extractedBetaslong2 %>%
  filter(mask == "AC",
         hemisphere != "wholeBrain") %>%
  group_by(ID, mask, modality, version, task, label, hemisphere, age) %>%
  summarise(betaValues = mean(betaValues)) %>% 
  distinct()

table(betasSubAC$ID, betasSubAC$modality)

(minAC <- floor(min(betasSubAC$betaValues))) # -1.90
(maxAC <- ceiling(max(betasSubAC$betaValues))) # 1.47

## analyses whole mask -------------------------------------------------
## sPE AC -----------------------------------------------------------------------
# Initialize empty data frames and lists for results
combined_anovas_NOV <- tibble()

stimLM2 <- lmer(betaValues ~ modality * version + (1|ID), betasAC)
summary(stimLM2)
anova(stimLM2)

combined_anovas_NOV <-  anova(stimLM2) %>% 
  broom.mixed::tidy(effects = "fixed") %>% 
  mutate(Model = "Whole ROI")

NOVwm <- ggplot(betasAC, aes(task, betaValues, fill = modality)) +
  introdataviz::geom_split_violin(alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0.2, color = "black") +
  scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
  ggtitle('Surprise ROI') +
  ylab("beta values") + xlab("task version") +
  # annotate("text", label = "paste('modality ', italic(p), ' = .001***')",
  #          parse = TRUE, x = 0.7, y = 2.8,
  #          hjust = 0, vjust = 1, color = "blue", size = 7.5) +
  jtools::theme_apa(remove.y.gridlines = F) + 
  scale_y_continuous(expand = c(0, 0), limits = c(minAC, maxAC)) +
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
NOVwm

ggsave(file.path(outputFolder, 'plots', 'ROI_AC_WM.tif'),
       NOVwm,
       height = 8, width = 12.8, units = "cm")

### subcluster -------------------------------------------------------------------
#unique(interaction(betasSubNOV$label, betasSubNOV$hemisphere))
#unique(betasSubNOV$hemisphere)

clusters <- unique(betasSubAC$label)
hemispheres <- c("left", "right", "bilateral")

plots_AC <- list()

for (cluster in clusters) {
  for (hem in hemispheres) {
    # Filter the data for the current cluster and hemisphere
    betasSubAC1 <- betasSubAC %>% 
      filter(label == cluster, hemisphere == hem)
    
    if (nrow(betasSubAC1) == 0) {
      next
    }
    
    SubLM <- lmer(betaValues ~ modality * version + (1|ID), betasSubAC1)
    
    combined_anovas_NOV <-  anova(SubLM) %>% 
      broom.mixed::tidy(effects = "fixed") %>% 
      mutate(Model = paste(cluster, hem)) %>% 
      bind_rows(combined_anovas_NOV, .)
    
    plot  <- ggplot(betasSubAC1, aes(task, betaValues, fill=modality)) +
      introdataviz::geom_split_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.2, color = "black") +
      scale_color_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
      scale_fill_manual(values = viridis(n=2, begin = 0.2, end = 0.8)) +
      ggtitle(paste(cluster, hem)) +
      jtools::theme_apa(remove.y.gridlines = F) + 
      scale_y_continuous(expand = c(0, 0), limits = c(minAC, maxAC)) +
      theme(text = element_text(size = 20))
    
    # Add plot to the list
    plots_AC[[paste(cluster, hem)]] <- plot
  }
}

combined_anovas_NOV_corr <- combined_anovas_NOV %>% 
  filter(Model != 'Whole ROI') %>% 
  rename(p_uncorr = p.value) %>% 
  mutate(p.value = p.adjust(p_uncorr, method = "BH"),
         p_uncorr = sprintf("%.3f", p_uncorr))  %>%  
  select(Model, term, sumsq, meansq, NumDF, DenDF, statistic, p_uncorr, p.value)

nice_table(combined_anovas_NOV_corr, highlight = TRUE,
           title = "Surprise AC: Anova Main and Interaction effects v1 vs v3") %>% 
  print(., preview = 'docx')


plots_AC[[1]] +
  ylab("beta values") +
  ggtitle('L ant. cingulate cortex') +
  xlab("task version") +
  theme(#text = element_text(size = 25),  # Increases all text
    # axis.title.y = element_text(size = 22), # Axis titles
    # axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 12), # Axis titles
    axis.text.x = element_text(size = 12), # Axis titles
    # legend.text = element_text(size = 22),  # Legend text
    # strip.text.x = element_text(size=22),
    # plot.title = element_text(size=22),
    legend.position = "none"
  )

ggsave(file.path(outputFolder, 'plots', 'ROI_SPE_AC_1.tif'),
       height = 1.5*3.12, width = 1.5*5, units = "cm")

plots_AC[[2]] +
  ylab("beta values") +
  ggtitle('L inf. frontal gyrus') +
  xlab("task version") +
  theme(#text = element_text(size = 25),  # Increases all text
    # axis.title.y = element_text(size = 22), # Axis titles
    # axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 12), # Axis titles
    axis.text.x = element_text(size = 12), # Axis titles
    # legend.text = element_text(size = 22),  # Legend text
    # strip.text.x = element_text(size=22),
    # plot.title = element_text(size=22),
    legend.position = "none"
  )

ggsave(file.path(outputFolder, 'plots', 'ROI_SPE_AC_2.tif'),
       height = 1.5*3.12, width = 1.5*5, units = "cm")

plots_AC[[3]] +
  ylab("beta values") +
  ggtitle('R inf. frontal gyrus') +
  xlab("task version") +
  theme(#text = element_text(size = 25),  # Increases all text
    # axis.title.y = element_text(size = 22), # Axis titles
    # axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 12), # Axis titles
    axis.text.x = element_text(size = 12), # Axis titles
    # legend.text = element_text(size = 22),  # Legend text
    # strip.text.x = element_text(size=22),
    # plot.title = element_text(size=22),
    legend.position = "none"
  )

ggsave(file.path(outputFolder, 'plots', 'ROI_SPE_AC_3.tif'),
       height = 1.5*3.12, width = 1.5*5, units = "cm")

plots_AC[[4]] +
  ylab("beta values") +
  ggtitle('L/R mid. cingulate cortex') +
  geom_signif(
    comparisons = list(c("discChoice", "matchRecog")),
    y_position = 1.25,
    tip_length = 0,
    size = 0.5,
    annotation = c("<.001***")
  ) +
  xlab("task version") +
  theme(#text = element_text(size = 25),  # Increases all text
    # axis.title.y = element_text(size = 22), # Axis titles
    # axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 12), # Axis titles
    axis.text.x = element_text(size = 12), # Axis titles
    # legend.text = element_text(size = 22),  # Legend text
    # strip.text.x = element_text(size=22),
    # plot.title = element_text(size=22),
    legend.position = "none"
  )

ggsave(file.path(outputFolder, 'plots', 'ROI_SPE_AC_4.tif'),
       height = 1.5*3.12, width = 1.5*5, units = "cm")

plots_AC[[5]] +
  ylab("beta values") +
  ggtitle('R mid. frontal gyrus') +
  geom_signif(
    comparisons = list(c("discChoice", "matchRecog")),
    y_position = 1.25,
    tip_length = 0,
    size = 0.5,
    annotation = c(".032*")
  ) +
  xlab("task version") +
  theme(#text = element_text(size = 25),  # Increases all text
    # axis.title.y = element_text(size = 22), # Axis titles
    # axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 12), # Axis titles
    axis.text.x = element_text(size = 12), # Axis titles
    # legend.text = element_text(size = 22),  # Legend text
    # strip.text.x = element_text(size=22),
    # plot.title = element_text(size=22),
    legend.position = "none"
  )

ggsave(file.path(outputFolder, 'plots', 'ROI_SPE_AC_5.tif'),
       height = 1.5*3.12, width = 1.5*5, units = "cm")

plots_AC[[6]] +
  ylab("beta values") +
  ggtitle('L/R sup. parietal lobe') +
  xlab("task version") +
  theme(#text = element_text(size = 25),  # Increases all text
    # axis.title.y = element_text(size = 22), # Axis titles
    # axis.title.x = element_text(size = 22), # Axis titles
    axis.text.y = element_text(size = 12), # Axis titles
    axis.text.x = element_text(size = 12), # Axis titles
    # legend.text = element_text(size = 22),  # Legend text
    # strip.text.x = element_text(size=22),
    # plot.title = element_text(size=22),
    legend.position = "bottom",
    legend.key.height = unit(0.5, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-8,0,0,0)
  )

ggsave(file.path(outputFolder, 'plots', 'ROI_SPE_AC_6.tif'),
       height = 1.5*3.12, width = 1.5*5, units = "cm")


