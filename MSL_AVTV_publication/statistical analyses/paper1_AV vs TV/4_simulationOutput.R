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
  keep(str_detect(.,'simpleRW')) %>% 
  discard(str_detect(.,'pwBelief'))

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
    rename_with(~ "sim_startBelief", matches("^sim.*startBelief$")) %>% 
    rename_with(~ "sim_alpha1", matches("^sim.*alpha1$")) %>% 
    rename_with(~ "fit_startBelief", matches("^fit.*startBelief$")) %>% 
    rename_with(~ "fit_alpha1", matches("^fit.*alpha1$"))  %>% 
    rename_with(~ "fit_NLL", matches("^fit.*NLL$"))
  
  if (grepl("sigmoid", fileNames[i], ignore.case = T)) {
    tempData <- tempData%>% 
      rename_with(~ "sim_beta", matches("^sim.*beta$")) %>%
      rename_with(~ "fit_beta", matches("^fit.*beta$"))
  }
  
  # rename variables so that they are uniform for each modeling output
  if (grepl("both", fileNames[i], ignore.case = T)) {
    tempData <- tempData %>% 
      rename_with(~ "sim_alpha2", matches("^sim.*alpha2$")) %>% 
      rename_with(~ "fit_alpha2", matches("^fit.*alpha2$"))
  }
  
  if (grepl("CBCpearceHall_", fileNames[i])) {
    tempData <- tempData %>% 
      rename_with(~ "sim_delta1", matches("^sim.*delta$")) %>% 
      rename_with(~ "fit_delta1", matches("^fit.*delta$"))
  }
  
  if (grepl("CBCpearceHallBoth", fileNames[i])) {
    tempData <- tempData %>% 
      rename_with(~ "sim_delta1", matches("^sim.*delta$")) %>% 
      rename_with(~ "fit_delta1", matches("^fit.*delta1$")) %>% 
      rename_with(~ "sim_delta2", matches("^sim.*delta2$")) %>% 
      rename_with(~ "fit_delta2", matches("^fit.*delta2$"))
  }
  
  if (grepl("CBCdriftDiffusion", fileNames[i])) {
    tempData <- tempData %>% 
      rename_with(~ "sim_startingPoint", matches("^sim.*startingPoint$")) %>% 
      rename_with(~ "fit_startingPoint", matches("^fit.*startingPoint$")) %>% 
      rename_with(~ "sim_startingBoundary", matches("^sim.*startingBoundary$")) %>% 
      rename_with(~ "fit_startingBoundary", matches("^fit.*startingBoundary$")) %>% 
      rename_with(~ "sim_weight", matches("^sim.*weight$")) %>% 
      rename_with(~ "fit_weight", matches("^fit.*weight$")) %>% 
      rename_with(~ "sim_nonDecisionTime", matches("^sim.*nonDecisionTime$")) %>% 
      rename_with(~ "fit_nonDecisionTime", matches("^fit.*nonDecisionTime$"))
  }
  
  #find the name of the perceptual model in the file name and add it as a variable to the data frame
  nPercModel <- which(str_detect(strsplit(fileNames[i],"_")[[1]], "RW") | 
                        str_detect(strsplit(fileNames[i],"_")[[1]], "pearce"))  
  tempData$percModel <- strsplit(fileNames[i],"_")[[1]][nPercModel]
  
  #determine the response model and add it as a variable to the data frame
  if (grepl("pwBelief", fileNames[i])) {
    respModel <- paste(strsplit(fileNames[i],"_")[[1]][nPercModel+1], 
                       strsplit(fileNames[i],"_")[[1]][nPercModel+2],
                       sep = "_")
  } else {
    respModel <- strsplit(fileNames[i],"_")[[1]][nPercModel+1]
  }
  
  if (grepl("csv", respModel)){
    respModel <- strsplit(respModel,"\\.")[[1]][1]
  }
  tempData$respModel <- respModel

  tempData <- as.data.frame(tempData)
  
  # converts varaibles to characters to be able to bind the data frames
  columns_to_convert <- c("stimPairLeft", "stimPairRight", "chosenPair", "otherPair")
  tempData[columns_to_convert] <- lapply(tempData[columns_to_convert], as.character)
  
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
  filter(model == "CBCsimpleRW_CBCdriftDiffusionLR")

simVars <- tempSum %>% 
  select(starts_with("sim") & !ends_with("startBelief")
         & !ends_with("startingPoint")) %>% 
  names()
fitVars <- tempSum %>% 
  select(starts_with("fit") & !ends_with("startBelief") &
           !ends_with("NLL") & !ends_with("startingPoint")) %>% 
  names()

for (j in 1:length(simVars)){
  eval(parse(text=paste0("plot", j, " <- ggplot(tempSum, aes(x = ", simVars[j],
                         ", y = ", fitVars[j], ")) + geom_point(color = 'grey') + ",
                         "geom_abline(intercept =0, color = 'grey') +",
                         "ggtitle('", simVars[j], " vs. ", fitVars[j], "')")))
}

plot1 <- plot1 +
  sm_statCorr(label_x = 0.01, label_y = 0.95, text_size = 5) +
  ggtitle(expression("Recovery of Learning Rate " * italic(eta))) +
  xlab(expression("simulated " * italic(eta))) +
  ylab(expression("recovered " * italic(eta))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(text = element_text(size = 8), 
        plot.background = element_blank())

plot2 <- plot2 +
  sm_statCorr(label_x = 0.31, label_y = 1.05, text_size = 5) +
  ggtitle(expression("Recovery of Non-decision Time " * italic(tau))) +
  xlab(expression("simulated " * italic(tau))) +
  ylab(expression("recovered " * italic(tau))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.3, 1.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.3, 1.1)) +
  theme(text = element_text(size = 8), 
        plot.background = element_blank())

plot3 <- plot3 +
  sm_statCorr(label_x = 0.1, label_y = 13.5, text_size = 5) +
  ggtitle(expression("Recovery of Drift Weight " * italic(v[mod]))) +
  xlab(expression("simulated " * italic(v[mod]))) +
  ylab(expression("recovered " * italic(v[mod]))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(text = element_text(size = 8), 
        plot.background = element_blank())

plot4 <- plot4 +
  sm_statCorr(label_x = 1.1, label_y = 4.75, text_size = 5) +
  ggtitle(expression("Recovery of Boundary " * italic(a))) +
  xlab(expression("simulated " * italic(a))) +
  ylab(expression("recovered " * italic(a))) +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 5)) +
  theme(text = element_text(size = 8), 
        plot.background = element_blank())

g <- ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, align = "hv",
               widths = c(0.9, 0.9), heights = c(1, 1)) 

finalPlot <- g + 
  theme(plot.background = element_rect(fill = "white", color = NA))
finalPlot
ggsave(filename=paste0(outputFolder, "/figures/ParameterRecoveryDDMn.png"),
       finalPlot, 
       width = 19, height = 19,  units = "cm")

corr_matrix <- cor(tempSum[, c(simVars, fitVars)])
corr_matrix[lower.tri(corr_matrix, diag = T)] <- NA

ggcorrplot::ggcorrplot(corr_matrix,
                       lab = TRUE,  outline.color = "light grey", lab_col = "white") +
  scale_fill_gradientn(colors = viridis(256, option = 'viridis', direction = -1)) +
  theme_minimal() +  # Minimal theme for clean background
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.ticks = element_blank(),    # Remove axis ticks
        panel.grid = element_blank(),
        legend.position = "right",
        text = element_text(size = 15))   # Remove gridlines

ggsave(paste0(outputFolder, "/figures/CorrelationSimFitDDMn.png"),
       width = 19, height = 19,  units = "cm")

# check NLL for min/max ----
simulationSummaryShort <- simulationSummary %>% 
  filter(!is.na(fit_delta1), !is.na(fit_NLL), !is.na(fit_alpha1))

plot(simulationSummary$fit_delta1, simulationSummary$fit_NLL)
plot(simulationSummary$fit_delta2, simulationSummary$fit_NLL)

ggplot(simulationSummary, aes(fit_delta1, fit_beta)) +
  geom_point(aes(color = fit_NLL, size = fit_NLL))

lattice::cloud(fit_NLL ~ fit_alpha1 + fit_delta1, data = simulationSummaryShort)
plot_ly(data= simulationSummary, x = simulationSummary$fit_alpha1, y = simulationSummary$fit_delta1, 
        z = simulationSummary$fit_NLL, type = "surface") %>% 
  add_surface()

# Clean up work space -------------------------------------
##########################################################
remove(fileNames, simulationDataList, tempData, columns_to_convert, i, PercModel,
       respModel, modelingAccuracy, g, plot1, plot2, plot3, plot4, plot5, simulationAccuracy,
       fitVars,j, models, simVars)
