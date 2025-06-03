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
initialVars <- ls()

## get the file names ####
# v3 pre Run
subjects <- list.files(path = 
                         file.path(modelingFolder, 'CBC_Modeling_v3_P2_Pre', 'analysed'),
                       pattern = "CBC_")
subjects <- subjects[nchar(subjects)<8 & subjects!="CBC_555" & subjects!="CBC_ϩ"] #simulated subjects have shorter names

fileNames <- list.files(paste(file.path(modelingFolder, 'CBC_Modeling_v3_P2_Pre', 'analysed'), sep = "/"), full.names = T, recursive = T)
fileNames <- fileNames %>% 
  keep(str_detect(.,'4_csv')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  keep(str_detect(.,'driftDiffusion'))

fileNames <- fileNames[sapply(fileNames, function(a) any(str_detect(a, paste(subjects,"_", sep = ""))))]

# v1 preRun
fileNamesV1 <- list.files(paste(file.path(modelingFolder, 'CBC_Modeling_v1_P2_Pre', 'analysed'), sep = "/"), full.names = T, recursive = T)
fileNamesV1 <- fileNamesV1 %>% 
  keep(str_detect(.,'4_csv')) %>% 
  #discard(str_detect(.,'2Step')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  #discard(str_detect(.,'uniATsimpleRW'))
  keep(str_detect(.,'driftDiffusion'))

fileNamesV1 <- fileNamesV1[sapply(fileNamesV1, function(a) any(str_detect(a, paste(subjects,"_", sep = ""))))]


# combine both fileNames
fileNames <- c(fileNames, fileNamesV1)

## read files ####
simulationDataList <- lapply(fileNames, data.table::fread)

## merge list into 1 data frame ####
# creating a new empty data frame
simulationData <- data.frame()

#looping to the data frames
#converting variables to the correct type to be able to bind data frames
for (i in 1:length(simulationDataList)) { #length(simulationDataList)
 
  rm(tempData)
  
  tempData <- simulationDataList[[i]] %>% 
    as.data.frame()
  
  if (grepl("v1", fileNames[i])) {
    tempData$version <- "v1"
    
  } else if (grepl("v3", fileNames[i])) {
    tempData$version <- "v3"
    # converts variables to characters to be able to bind the data frames
    columns_to_convert <- c("stimPairLeft", "stimPairRight", "chosenPair", "otherPair")
    tempData[columns_to_convert] <- lapply(tempData[columns_to_convert], as.character)
  }
  
  # binding all data frames in one data frame
  simulationData <- bind_rows(simulationData, tempData)
  
}

## write data ####
write_csv(simulationData, file = paste(outputFolder, "modelling", "simulationData.csv", sep = "/"))

simulationData <- read_csv(file = paste(outputFolder, "modelling", "simulationData.csv", sep = "/"))

# create a summary table ####
# selecting the needed variables, grouping the data and then only taking the first entry for each group
simulationSummary <- simulationData %>% 
  select(ID, session, version,
         starts_with("sim"), starts_with("fit")) %>% 
  group_by(ID, version, session, fit_percModel, fit_respModel) %>% 
  slice(1) %>% 
  ungroup()

# calculating the mean accuracy for each session
simulationAccuracy <- simulationData %>% 
  group_by(ID, version, session, fit_percModel, fit_respModel) %>% 
  summarise(accuracy = mean(choiceAccurate, na.rm=T),
            probFB = round(mean(rewardAccurate, na.rm=T),2)) %>% 
  ungroup()

# adding the mean accuracy to the summary table
simulationSummary <- simulationSummary %>% 
  full_join(., simulationAccuracy, by = c("ID", "version", "session", "fit_percModel", "fit_respModel")) %>% 
  arrange(ID, session) %>% 
  mutate(model = paste(fit_percModel, fit_respModel, sep = "_"))

## write data ####
write_csv(simulationSummary, file = paste(outputFolder, "modelling", "simulationSummary.csv", sep = "/"))

# Compare simulation vs fit ----
simVars <- simulationSummary %>% 
  select(starts_with("sim") & -contains("percModel") & -contains("respModel")) %>% 
  names()
fitVars <- simulationSummary %>% 
  select(starts_with("fit") & -contains("percModel") & -contains("respModel") &
           -contains("NLL")) %>% 
  names()

output_filename <- paste0(outputFolder, "/recovery/recovery_", gsub("sim_", "", simVars), ".png")


for (i in 1:length(simVars)) {
  ggplot(simulationSummary, aes_string(simVars[i], fitVars[i])) +
    geom_point() +
    geom_abline(intercept =0, color = "grey")+
    #stat_smooth(method = "lm", se = FALSE, color = "light blue") +
    sm_statCorr(text_size = 3) +
    ggtitle(paste(simVars[i], "vs", fitVars[i])) +
    facet_grid(version~model) +
    theme_bw() +
    theme(text=element_text(size=8)) 
  
  ggsave(output_filename[i],
         width = 60, height = 10,  units = "cm")
}


for (k in unique(simulationSummary$version)) {
  models <- unique(simulationSummary$model[simulationSummary$version==k])
  for (i in models){
    tempSum <- simulationSummary %>% 
      filter(model == i, version == k) %>% 
      select_if(~ !all(is.na(.)))
    
    simVars <- tempSum %>% 
      select(starts_with("sim") & !ends_with("startBelief") & -contains("startingPoint") &
               -contains("percModel") & -contains("respModel")) %>% 
      names()
    fitVars <- tempSum %>% 
      select(starts_with("fit") & !ends_with("startBelief") & -contains("startingPoint") &
               !ends_with("NLL") & -contains("percModel") & -contains("respModel")) %>% 
      names()
    
    rm(list = ls(pattern = "^plot"), g, corr_matrix, p_matrix)    
    
    for (j in 1:length(simVars)){
      eval(parse(text=paste0("plot", j, " <- ggplot(tempSum, aes(x = ", simVars[j],
                             ", y = ", fitVars[j], ")) + geom_point(color = 'grey') + ",
                             "geom_abline(intercept =0, color = 'grey') +",
                             "sm_statCorr(text_size = 3) +",
                             "ggtitle('", simVars[j], " vs. ", fitVars[j], "')")))
    }
    
    
    if (length(simVars) == 8) {
      g <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 2, nrow = 4)
      g <- annotate_figure(g, top = text_grob(paste(k, i, sep = "_"),
                                              face = "bold"))
    } else if (length(simVars) == 7) {
      g <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol = 2, nrow = 4)
      g <- annotate_figure(g, top = text_grob(paste(k, i, sep = "_"),
                                              face = "bold"))
    } else if (length(simVars) == 6) {
      g <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2, nrow = 3)
      g <- annotate_figure(g, top = text_grob(paste(k, i, sep = "_"),
                                              face = "bold"))
    } else if (length(simVars) == 5) {
      g <- ggarrange(plot1, plot2, plot3, plot4, plot5, ncol = 2, nrow = 3)
      g <- annotate_figure(g, top = text_grob(paste(k, i, sep = "_"),
                                              face = "bold"))
    } else if (length(simVars) == 4) {
      g <- ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
      g <- annotate_figure(g, top = text_grob(paste(k, i, sep = "_"),
                                              face = "bold"))
    }
    
    g
    ggsave(paste0(outputFolder, "/recovery/", k, "_", i, "_SP.png"),g,
           width = 2000, height = 3000,  units = "px")
    
    corr_matrix <- cor(tempSum[, c(simVars, fitVars)])
    corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA
    p_matrix <- ggcorrplot::cor_pmat(tempSum[, c(simVars, fitVars)])
    p_matrix[lower.tri(p_matrix, diag = TRUE)] <- NA
    
    ggcorrplot::ggcorrplot(corr_matrix,
                           p.mat = p_matrix,
                           insig = "pch",
                           #pch = 11,
                           lab = TRUE,  outline.color = "light grey", 
                           colors = RColorBrewer::brewer.pal(n = 3, name = "RdYlBu"),
                           title = i)
    
    ggsave(paste0(outputFolder, "/recovery/", k, "_", i, "_CP.png"),
           width = 2700, height = 1765,  units = "px")
    
  }
}


# recovery for paper 2 ----
simSum <- simulationSummary %>% 
  filter(!grepl("pearceHall", model),
         version != "v3" | str_ends(model, "pwBelief")) %>% 
  mutate(
    modelGroup = case_when(
      str_detect(model, "simpleRW") ~ "simpleRW",
      str_detect(model, "simpleAsymRW") ~ "simpleAsymRW",
      str_detect(model, "2StepRW") | str_detect(model, "bothPairsRW") ~ "2StepRW",
      str_detect(model, "2StepAsymRW") | str_detect(model, "bothPairsAsymRW") ~ "2StepAsymRW",
      TRUE ~ NA_character_
    )
  )

unique(simSum$model)
unique(simSum$modelGroup)

## simple RW ----
tempSum <- simSum %>% 
  filter(modelGroup == 'simpleRW') %>% 
  select(where(~ !all(is.na(.))),
         -sim_percModel, -sim_respModel,
         -fit_percModel, -fit_respModel) %>% 
  mutate(task = if_else(version == "v1", "matchRecog", "discChoice"))

simVars <- tempSum %>% 
  select(starts_with("sim") & !ends_with("startBelief")
         & !ends_with("startingPoint")) %>% 
  names()
fitVars <- tempSum %>% 
  select(starts_with("fit") & !ends_with("startBelief") &
           !ends_with("NLL") & !ends_with("startingPoint")) %>% 
  names()

plotList <- list()

for (j in seq_along(simVars)) {
  plotList[[j]] <- ggplot(tempSum, aes(x = .data[[simVars[j]]], y = .data[[fitVars[j]]])) +
    geom_point(color = 'grey') +
    geom_abline(intercept = 0, color = 'grey') +
    sm_statCorr(text_size = 3) +
    facet_wrap(~task) +
    ggtitle(paste0(simVars[j], " vs. ", fitVars[j]))
}

plotList[[1]] <- plotList[[1]] +
  ggtitle(expression("Learning Rate " * italic(eta))) +
  xlab(expression("simulated " * italic(eta))) +
  ylab(expression("recovered " * italic(eta))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList[[2]] <- plotList[[2]] +
  ggtitle(expression("Non-decision Time " * italic(T[er]))) +
  xlab(expression("simulated " * italic(T[er]))) +
  ylab(expression("recovered " * italic(T[er]))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 3.1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList[[3]] <- plotList[[3]] +
  ggtitle(expression("Drift Weight " * italic(m))) +
  xlab(expression("simulated " * italic(m))) +
  ylab(expression("recovered " * italic(m))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList[[4]] <- plotList[[4]] +
  ggtitle(expression("Boundary " * italic(a))) +
  xlab(expression("simulated " * italic(a))) +
  ylab(expression("recovered " * italic(a))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

ggsave(plot = plotList[[1]],
       file.path(outputFolder, "figures/", "/Recovery_sRW_LR.tif"),
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList[[2]],
       file.path(outputFolder, "figures/", "/Recovery_sRW_Ter.tif"),
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList[[3]],
       file.path(outputFolder, "figures/", "/Recovery_sRW_DW.tif"),
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList[[4]],
       file.path(outputFolder, "figures/", "/Recovery_sRW_BS.tif"),
       width = 1.5*7.5, height = 1.5*3.75, units = "cm")

# correlation plot
corr_matrix <- cor(tempSum %>% 
                     filter(version == "v1") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr1 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white"
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Match Recognition") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 10), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt 

corr_matrix <- cor(tempSum %>% 
                     filter(version == "v3") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr2 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white"
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Discriminative Choice") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 10), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt  

final_plot_sRW <- ggarrange(corr2, corr1, ncol = 2, common.legend = TRUE, legend = "right")

ggsave(paste0(outputFolder, "/figures/Correlation_simpleRW.tif"),
       plot = final_plot_sRW,
       width = 1.5*15, height = 1.5*7.5,  units = "cm")

## simple Asym RW ----
tempSum <- simSum %>% 
  filter(modelGroup == 'simpleAsymRW') %>% 
  select(where(~ !all(is.na(.))),
         -sim_percModel, -sim_respModel,
         -fit_percModel, -fit_respModel) %>% 
  mutate(task = if_else(version == "v1", "matchRecog", "discChoice"))

simVars <- tempSum %>% 
  select(starts_with("sim") & !ends_with("startBelief")
         & !ends_with("startingPoint")) %>% 
  names()
fitVars <- tempSum %>% 
  select(starts_with("fit") & !ends_with("startBelief") &
           !ends_with("NLL") & !ends_with("startingPoint")) %>% 
  names()

plotList_saRW <- list()

for (j in seq_along(simVars)) {
  plotList_saRW[[j]] <- ggplot(tempSum, aes(x = .data[[simVars[j]]], y = .data[[fitVars[j]]])) +
    geom_point(color = 'grey') +
    geom_abline(intercept = 0, color = 'grey') +
    sm_statCorr(text_size = 3) +
    facet_wrap(~task) +
    ggtitle(paste0(simVars[j], " vs. ", fitVars[j]))
}

plotList_saRW[[1]] <- plotList_saRW[[1]] +
  ggtitle(expression("Learning Rate " * italic(eta)['+'])) +
  xlab(expression("simulated " * italic(eta)['+'])) +
  ylab(expression("recovered " * italic(eta)['+'])) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_saRW[[2]] <- plotList_saRW[[2]] +
  ggtitle(expression("Learning Rate " * italic(eta)['-'])) +
  xlab(expression("simulated " * italic(eta)['-'])) +
  ylab(expression("recovered " * italic(eta)['-'])) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_saRW[[3]] <- plotList_saRW[[3]] +
  ggtitle(expression("Non-decision Time " * italic(T[er]))) +
  xlab(expression("simulated " * italic(T[er]))) +
  ylab(expression("recovered " * italic(T[er]))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 3.1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_saRW[[4]] <- plotList_saRW[[4]] +
  ggtitle(expression("Drift Weight " * italic(m))) +
  xlab(expression("simulated " * italic(m))) +
  ylab(expression("recovered " * italic(m))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15.1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_saRW[[5]] <- plotList_saRW[[5]] +
  ggtitle(expression("Boundary " * italic(a))) +
  xlab(expression("simulated " * italic(a))) +
  ylab(expression("recovered " * italic(a))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

ggsave(plot = plotList_saRW[[1]],
       file.path(outputFolder, "figures/", "/Recovery_sAsRW_LR1.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_saRW[[2]],
       file.path(outputFolder, "figures/", "/Recovery_sAsRW_LR2.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_saRW[[3]],
       file.path(outputFolder, "figures/", "/Recovery_sAsRW_Ter.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_saRW[[4]],
       file.path(outputFolder, "figures/", "/Recovery_sAsRW_DW.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_saRW[[5]],
       file.path(outputFolder, "figures/", "/Recovery_sAsRW_BS.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")

# correlation plots
corr_matrix <- cor(tempSum %>% 
                     filter(version == "v1") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr1 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white",
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Match Recognition") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 15), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt   

corr_matrix <- cor(tempSum %>% 
                     filter(version == "v3") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr2 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white",
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Discriminative Choice") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 10), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt   

final_plot_saRW <- ggarrange(corr2, corr1, ncol = 2, common.legend = TRUE, legend = "right")

ggsave(paste0(outputFolder, "/figures/Correlation_simpleAsymRW.tif"),
       plot = final_plot_saRW,
       width = 1.5*15, height = 1.5*7.5,  units = "cm")

## 2Step RW ----
tempSum <- simSum %>% 
  filter(modelGroup == '2StepRW') %>% 
  select(where(~ !all(is.na(.))),
         -sim_percModel, -sim_respModel,
         -fit_percModel, -fit_respModel) %>% 
  mutate(task = if_else(version == "v1", "matchRecog", "discChoice"))

simVars <- tempSum %>% 
  select(starts_with("sim") & !ends_with("startBelief")
         & !ends_with("startingPoint")) %>% 
  names()
fitVars <- tempSum %>% 
  select(starts_with("fit") & !ends_with("startBelief") &
           !ends_with("NLL") & !ends_with("startingPoint")) %>% 
  names()

plotList_tRW <- list()

for (j in seq_along(simVars)) {
  plotList_tRW[[j]] <- ggplot(tempSum, aes(x = .data[[simVars[j]]], y = .data[[fitVars[j]]])) +
    geom_point(color = 'grey') +
    geom_abline(intercept = 0, color = 'grey') +
    sm_statCorr(text_size = 3) +
    facet_wrap(~task) +
    ggtitle(paste0(simVars[j], " vs. ", fitVars[j]))
}

plotList_tRW[[1]] <- plotList_tRW[[1]] +
  ggtitle(expression("Learning Rate " * italic(eta)^'C')) +
  xlab(expression("simulated " * italic(eta)^'C')) +
  ylab(expression("recovered " * italic(eta)^'C')) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_tRW[[2]] <- plotList_tRW[[2]] +
  ggtitle(expression("Learning Rate " * italic(eta)^'O')) +
  xlab(expression("simulated " * italic(eta)^'O')) +
  ylab(expression("recovered " * italic(eta)^'O')) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_tRW[[3]] <- plotList_tRW[[3]] +
  ggtitle(expression("Non-decision Time " * italic(T[er]))) +
  xlab(expression("simulated " * italic(T[er]))) +
  ylab(expression("recovered " * italic(T[er]))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.1, 3)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.1, 3)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_tRW[[4]] <- plotList_tRW[[4]] +
  ggtitle(expression("Drift Weight " * italic(m))) +
  xlab(expression("simulated " * italic(m))) +
  ylab(expression("recovered " * italic(m))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_tRW[[5]] <- plotList_tRW[[5]] +
  ggtitle(expression("Boundary " * italic(a))) +
  xlab(expression("simulated " * italic(a))) +
  ylab(expression("recovered " * italic(a))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

ggsave(plot = plotList_tRW[[1]],
       file.path(outputFolder, "figures/", "/Recovery_tRW_LR1.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_tRW[[2]],
       file.path(outputFolder, "figures/", "/Recovery_tRW_LR2.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_tRW[[3]],
       file.path(outputFolder, "figures/", "/Recovery_tRW_Ter.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_tRW[[4]],
       file.path(outputFolder, "figures/", "/Recovery_tRW_DW.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_tRW[[5]],
       file.path(outputFolder, "figures/", "/Recovery_tRW_BS.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")

# Correlation Plot
corr_matrix <- cor(tempSum %>% 
                     filter(version == "v1") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr1 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white",
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Match Recognition") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 10), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt  

corr_matrix <- cor(tempSum %>% 
                     filter(version == "v3") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr2 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white",
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Discriminative Choice") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 10), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt  

final_plot_tRW <- ggarrange(corr2, corr1, ncol = 2, common.legend = TRUE, legend = "right")

ggsave(paste0(outputFolder, "/figures/Correlation_transferRW.tif"),
       plot = final_plot_tRW,
       width = 1.5*15, height = 1.5*7.5,  units = "cm")

## 2Step Asym RW ----
tempSum <- simSum %>% 
  filter(modelGroup == '2StepAsymRW') %>% 
  select(where(~ !all(is.na(.))),
         -sim_percModel, -sim_respModel,
         -fit_percModel, -fit_respModel) %>% 
  mutate(task = if_else(version == "v1", "matchRecog", "discChoice"))

simVars <- tempSum %>% 
  select(starts_with("sim") & !ends_with("startBelief")
         & !ends_with("startingPoint")) %>% 
  names()
fitVars <- tempSum %>% 
  select(starts_with("fit") & !ends_with("startBelief") &
           !ends_with("NLL") & !ends_with("startingPoint")) %>% 
  names()

plotList_taRW <- list()

for (j in seq_along(simVars)) {
  plotList_taRW[[j]] <- ggplot(tempSum, aes(x = .data[[simVars[j]]], y = .data[[fitVars[j]]])) +
    geom_point(color = 'grey') +
    geom_abline(intercept = 0, color = 'grey') +
    sm_statCorr(text_size = 3) +
    facet_wrap(~task) +
    ggtitle(paste0(simVars[j], " vs. ", fitVars[j]))
}

plotList_taRW[[1]] <- plotList_taRW[[1]] +
  ggtitle(expression("Learning Rate " * italic(eta)['+']^'C')) +
  xlab(expression("simulated " * italic(eta)['+']^'C')) +
  ylab(expression("recovered " * italic(eta)['+']^'C')) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_taRW[[2]] <- plotList_taRW[[2]] +
  ggtitle(expression("Learning Rate " * italic(eta)['-']^'C')) +
  xlab(expression("simulated " * italic(eta)['-']^'C')) +
  ylab(expression("recovered " * italic(eta)['-']^'C')) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_taRW[[3]] <- plotList_taRW[[3]] +
  ggtitle(expression("Learning Rate " * italic(eta)['+']^'O')) +
  xlab(expression("simulated " * italic(eta)['+']^'O')) +
  ylab(expression("recovered " * italic(eta)['+']^'O')) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_taRW[[4]] <- plotList_taRW[[4]] +
  ggtitle(expression("Learning Rate " * italic(eta)['-']^'O')) +
  xlab(expression("simulated " * italic(eta)['-']^'O')) +
  ylab(expression("recovered " * italic(eta)['-']^'O')) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_taRW[[5]] <- plotList_taRW[[5]] +
  ggtitle(expression("Non-decision Time " * italic(T[er]))) +
  xlab(expression("simulated " * italic(T[er]))) +
  ylab(expression("recovered " * italic(T[er]))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.1, 3)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.1, 3)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_taRW[[6]] <- plotList_taRW[[6]] +
  ggtitle(expression("Drift Weight " * italic(m))) +
  xlab(expression("simulated " * italic(m))) +
  ylab(expression("recovered " * italic(m))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

plotList_taRW[[7]] <- plotList_taRW[[7]] +
  ggtitle(expression("Boundary " * italic(a))) +
  xlab(expression("simulated " * italic(a))) +
  ylab(expression("recovered " * italic(a))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    plot.title = element_text(margin=margin(0,0,-6,0)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.background = element_blank())

ggsave(plot = plotList_taRW[[1]],
       file.path(outputFolder, "figures/", "/Recovery_taRW_LR1.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_taRW[[2]],
       file.path(outputFolder, "figures/", "/Recovery_taRW_LR2.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_taRW[[3]],
       file.path(outputFolder, "figures/", "/Recovery_taRW_LR3.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_taRW[[4]],
       file.path(outputFolder, "figures/", "/Recovery_taRW_LR4.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_taRW[[5]],
       file.path(outputFolder, "figures/", "/Recovery_taRW_Ter.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_taRW[[6]],
       file.path(outputFolder, "figures/", "/Recovery_taRW_DW.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")
ggsave(plot = plotList_taRW[[7]],
       file.path(outputFolder, "figures/", "/Recovery_taRW_BS.tif"),
       width = 1.5*5, height = 1.5*3.75, units = "cm")

# correlation plot
corr_matrix <- cor(tempSum %>% 
                     filter(version == "v1") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr1 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white"
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Match Recognition") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 10), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt  

corr_matrix <- cor(tempSum %>% 
                     filter(version == "v3") %>% 
                     select(all_of(simVars), all_of(fitVars)))
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

corr2 <- ggcorrplot::ggcorrplot(corr_matrix,
                                lab = TRUE,  outline.color = "light grey", lab_col = "white"
                                ) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  ggtitle("Discriminative Choice") +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        #text = element_text(size = 10), 
        plot.margin = margin(t = 0, r = -15, b = 0, l = -15))  # top, right, bottom, left in pt  

final_plot_taRW <- ggarrange(corr2, corr1, ncol = 2, common.legend = TRUE, legend = "right")

ggsave(paste0(outputFolder, "/figures/Correlation_transferAsymmRW.tif"),
       plot = final_plot_taRW,
       width = 1.5*17, height = 1.5*8.5,  units = "cm")

# Clean up work space -------------------------------------
finalVars <- ls()
newVars <- setdiff(finalVars, initialVars)
vars2keep <- c("behData", "expInfo")
newVars <- setdiff(newVars, vars2keep)

remove(list = newVars)
