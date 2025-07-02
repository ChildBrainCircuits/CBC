##########################################################
##                     SIMULATION                       ##
##########################################################

# Load and merge data ------------------------------------
##########################################################

## get the file names ####
subjects <- list.files(path = modelingFolder, pattern = "CBC_")
subjects <- subjects[nchar(subjects)<8 & subjects!="CBC_555" & subjects!="CBC_ϩ"] #simulated subjects have shorter names

fileNames <- list.files(paste(modelingFolder, sep = "/"), full.names = T, recursive = T)
fileNames <- fileNames %>% 
  keep(str_detect(.,'4_csv')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  keep(str_detect(.,'driftDiffusion'))

fileNames <- fileNames[sapply(fileNames, function(a) any(str_detect(a, paste(subjects,"_", sep = ""))))]

## read files ####
simulationDataList <- lapply(fileNames, data.table::fread)

## merge list into 1 data frame ####
# creating a new empty data frame
simulationData <- data.frame()

#looping to the data frames, changing variable names so that they can be bound 
#to one data frame
#adding new variables indicating the perception and response model
#converting variables to the correct type to be able to bind data frames
for (i in 1:length(simulationDataList)) {
 
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

simulationData <- read_csv(file = paste(outputFolder, "simulationData.csv", sep = "/"))

# create a summary table ####
# selecting the needed variables, grouping the data and then only taking the first entry for each group
simulationSummary <- simulationData %>% 
  dplyr::select(ID, session, percModel, respModel,
         starts_with("sim"), starts_with("fit")) %>% 
  group_by(ID, session, percModel, respModel) %>% 
  slice(1) %>% 
  ungroup()

# calculating the mean accuracy for each session
simulationAccuracy <- simulationData %>% 
  group_by(ID, session, percModel, respModel) %>% 
  summarise(accuracy = mean(choiceAccurate, na.rm=T),
            probFB = round(mean(rewardAccurate, na.rm=T),2)) %>% 
  ungroup()

# adding the mean accuracy to the summary table
simulationSummary <- simulationSummary %>% 
  full_join(., simulationAccuracy, by = c("ID", "session", "percModel", "respModel")) %>% 
  arrange(ID, session) %>% 
  mutate(model = paste(percModel, respModel, sep = "_"))

## write data ####
write_csv(simulationSummary, file = paste(outputFolder, "modelling", "simulationSummary.csv", sep = "/"))

## recovery for paper 1 ----
simulationSummary <- read_csv(file = paste(outputFolder, "modelling", "simulationSummary.csv", sep = "/"))

tempSum <- simulationSummary %>% 
  filter(model == "CBCsimpleRW_CBCdriftDiffusionLR") %>% 
  select(-sim_percModel, -sim_respModel,
         -fit_percModel, -fit_respModel)

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
    #sm_statCorr() +
    ggtitle(paste0(simVars[j], " vs. ", fitVars[j]))
}

plotList[[1]] <- plotList[[1]] +
  sm_statCorr(label_x = 0.005, label_y = 0.95, text_size = 6) +
  ggtitle(expression("learning Rate " * italic(eta))) +
  xlab(expression("simulated " * italic(eta))) +
  ylab(expression("recovered " * italic(eta))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 20), # Axis titles
    axis.title.x = element_text(size = 20), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 20),  # Legend text
    strip.text.x = element_text(size=20),
    plot.title = element_text(size=20),
    legend.position = "none",
    plot.background = element_blank())

plotList[[2]] <- plotList[[2]] +
  sm_statCorr(label_x = 0.3135, label_y = 2.865, text_size = 6) +
  ggtitle(expression("Non-decision time " * italic(tau))) +
  xlab(expression("simulated " * italic(tau))) +
  ylab(expression("recovered " * italic(tau))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.1, 3.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.1, 3.1)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 20), # Axis titles
    axis.title.x = element_text(size = 20), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 20),  # Legend text
    strip.text.x = element_text(size=20),
    plot.title = element_text(size=20),
    legend.position = "none",
    plot.background = element_blank()
  )

plotList[[3]] <- plotList[[3]] +
  sm_statCorr(label_x = 0.075, label_y = 14.25, text_size = 6) +
  ggtitle(expression("Drift weight " * italic(v[mod]))) +
  xlab(expression("simulated " * italic(v[mod]))) +
  ylab(expression("recovered " * italic(v[mod]))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 20), # Axis titles
    axis.title.x = element_text(size = 20), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 20),  # Legend text
    strip.text.x = element_text(size=20),
    plot.title = element_text(size=20),
    legend.position = "none",
    plot.background = element_blank()
  )

plotList[[4]] <- plotList[[4]] +
  sm_statCorr(label_x = 1.02, label_y = 4.8, text_size = 6) +
  ggtitle(expression("Boundary " * italic(a))) +
  xlab(expression("simulated " * italic(a))) +
  ylab(expression("recovered " * italic(a))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 5)) +
  theme(#text = element_text(size = 25),  # Increases all text
    axis.title.y = element_text(size = 20), # Axis titles
    axis.title.x = element_text(size = 20), # Axis titles
    axis.text.y = element_text(size = 20), # Axis titles
    axis.text.x = element_text(size = 20), # Axis titles
    legend.text = element_text(size = 20),  # Legend text
    strip.text.x = element_text(size=20),
    plot.title = element_text(size=20),
    legend.position = "none",
    plot.background = element_blank()
  ) 

g <- ggarrange(plotlist = plotList, 
               ncol = 2, nrow = 2, align = "hv",
               widths = c(0.9, 0.9), heights = c(1, 1)) + 
  theme(plot.background = element_rect(fill = "white", color = NA))

g

finalPlot <- g + 
  theme(plot.background = element_rect(fill = "white", color = NA))
finalPlot

ggsave(filename=paste0(outputFolder, "/figures/ParameterRecoveryDDM.svg"),
       finalPlot, 
       width = 1.2*19, height = 1.2*19,  units = "cm")

ggsave(filename=paste0(outputFolder, "/figures/ParameterRecoveryDDMn.tif"),
       finalPlot, 
       width = 1.2*19, height = 1.2*19,  units = "cm")

corr_matrix <- cor(tempSum[, c(simVars, fitVars)])
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

ggcorrplot::ggcorrplot(corr_matrix,
                       lab = TRUE,  outline.color = "light grey", lab_col = "white",
                       #colors = viridis(2, option = "viridis"),
                       #title = "Correlation Plot for Simulated and Recovered Parameter Values"
) +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        text = element_text(size = 15))   # Remove gridlines


ggsave(paste0(outputFolder, "/figures/CorrelationSimFitDDM.svg"),
       width = 0.85*19, height = 0.6*19,  units = "cm")

ggsave(paste0(outputFolder, "/figures/CorrelationSimFitDDMn.tif"),
       width = 0.85*19, height = 0.6*19,  units = "cm")


# Clean up work space -------------------------------------
##########################################################
remove(fileNames, simulationDataList, tempData, columns_to_convert, i, PercModel,
       respModel, modelingAccuracy, g, plot1, plot2, plot3, plot4, plot5, simulationAccuracy,
       fitVars,j, models, simVars)
