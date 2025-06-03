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
load(file.path(outputFolder, "MRdataAd_childrenMR.RData"))
load(file.path(outputFolder, "MRexpInfoAd_childrenMR.RData"))

behData <- MRbehDataAd
expInfo <- MRexpInfoAd

behData %>% 
  group_by(ID, modality, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(modality)

expInfo %>% 
  count(ID, modality)

subjects <- unique(behData$ID)

################################################################################
# load data --------------------------------------------------------------------
fileNames <- list.files(paste(modelingFolder, 'CBC_Modeling_v1_P2_MR_adults', sep = "/"), full.names = T, recursive = T)
fileNames <- fileNames %>% 
  keep(str_detect(.,'4_csv')) %>% 
  keep(str_detect(.,'Dfit')) %>% 
  keep(str_detect(., 'CBC_P')) %>% 
  discard(str_detect(.,'uniATsimpleRW')) %>% 
  discard(str_detect(.,'uniVsimpleRW')) %>% 
  discard(str_detect(., 'CBCsimpleSurprise'))

fileNames <- fileNames[sapply(fileNames, function(a) any(str_detect(a, paste(subjects,"_", sep = ""))))]

## read files ####
modelingDataList <- lapply(fileNames, data.table::fread)

## merge list into 1 data frame ####
# creating a new empty data frame
modelingData <- data.frame()

#looping to the data frames
#converting variables to the correct type to be able to bind data frames
for (i in 1:length(modelingDataList)) { #length(modelingDataList)
  
  rm(tempData)
  
  tempData <- modelingDataList[[i]] %>% 
    as.data.frame()
  
  if (grepl("v1", fileNames[i])) {
    tempData$version <- "v1"
    
  } else if (grepl("v3", fileNames[i])) {
    tempData$version <- "v3"
    # converts variables to characters to be able to bind the data frames
    columns_to_convert <- c("stimPairLeft", "stimPairRight", "chosenPair", "otherPair")
    tempData[columns_to_convert] <- lapply(tempData[columns_to_convert], as.character)
  }
  
  if (grepl("P2_Pre", fileNames[i])) {
    tempData$run <- "pre"
  } else if (grepl("P2_MR", fileNames[i])) {
    tempData$run <- "MR"
  }
  
  # binding all data frames in one data frame
  modelingData <- bind_rows(modelingData, tempData)
  
}

################################################################################
# save and read data -----------------------------------------------------------

# only select logfiles from fMRI output
modelingData <- modelingData %>% 
  semi_join(expInfo, by = c("filename" = "logfile"))

modelingData %>% 
  group_by(ID, run, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, mod2Type)

setdiff(unique(expInfo$logfile), unique(modelingData$filename))

write_csv(modelingData, file = paste(outputFolder, "modelling", "modellingDataAdults.csv", sep = "/"))
modelingData <- read_csv(file = paste(outputFolder, "modelling", "modellingDataAdults.csv", sep = "/"))

modelingData <- modelingData %>% 
  filter(fit_percModel != 'CBCpearceHall')

unique(modelingData$fit_percModel)

# create a summary table ####
# selecting the needed variables, grouping the data and then only taking the first entry for each group
unique(modelingData$fit_respModel)

modelingSummary <- modelingData %>% 
  dplyr::select(ID, filename, run, version, session, mod2Type, fit_percModel, fit_respModel,
                starts_with("fit"), fit_NLL) %>% 
  filter(version == "v1" | (version == "v3" & fit_respModel == "CBCdriftDiffusionLR_pwBelief")) %>% 
  group_by(ID, run, version, session, fit_percModel, fit_respModel) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(model = paste(fit_percModel, fit_respModel, sep = "_"))

modelingSummary %>% 
  group_by(ID, mod2Type) %>% 
  summarise(n = n()) %>% 
  filter(n != 4)

# calculating the mean accuracy for each session
modelingAccuracy <- modelingData %>% 
  group_by(ID, run, version, session, fit_percModel, fit_respModel) %>% 
  filter(version == "v1" | (version == "v3" & fit_respModel == "CBCdriftDiffusionLR_pwBelief")) %>% 
  summarise(accuracy = mean(choiceAccurate, na.rm=T),
            probFB = round(mean(rewardAccurate, na.rm=T),2)) %>% 
  ungroup()

# adding the mean accuracy to the summary table
nparms <- modelingSummary %>% 
  select(ID, run, version, session, model, starts_with("fit"), -ends_with("startBelief"),
         -ends_with("startingPoint"), -ends_with("NLL"), -ends_with("percModel"), 
         -ends_with("respModel")) %>% 
  mutate(nparms = rowSums(!is.na(select(., -ID, -session, -model, -run, -version))),
         nparms2 = nparms+2) %>% 
  select(ID, run, version, session, model, nparms, nparms2)

modelingSummary <- modelingSummary %>% 
  full_join(., modelingAccuracy, by = c("ID", "run", "version", "session", "fit_percModel", "fit_respModel")) %>% 
  left_join(., nparms, by = join_by(ID, run, version, session, model)) %>% 
  arrange(ID, run, session)

modelingSummary <- modelingSummary %>% 
  mutate(AIC = 2*fit_NLL + 2*nparms,
         BIC = 2*fit_NLL + log(44)*nparms,
         AIC2 = 2*fit_NLL + 2*nparms2,
         BIC2 = 2*fit_NLL + log(44)*nparms2)

#################################################################################
# best models per runs and modality
MRmeanBICSubMod <- modelingSummary %>% 
  filter(run == "MR") %>% 
  group_by(ID, run, version, model, mod2Type) %>% 
  summarise(mBIC = mean(BIC))

MRbestModSubMod <- MRmeanBICSubMod %>% 
  group_by(ID, run, version, mod2Type) %>% 
  slice_min(mBIC) %>% 
  ungroup()

MRmodelCountsMod <- MRbestModSubMod %>%
  group_by(version, mod2Type) %>% 
  count(model, name = "count") %>%
  arrange(version, mod2Type, desc(count))

################################################################################
# get best fitting model for MR analyses ---------------------------------------
bestModRun <- MRbestModSubMod %>% 
  mutate(run = "MR") 

# get the modeling data for best model per run
modelingOutput <- modelingData %>% 
  select(ID, filename, session, trial, mod2Type, stimPair, frequency, rewardAccurate, reactionTime, 
         choice, choiceAccurate,  reward, beliefPair,  
         rewardPE, starts_with("fit_"), version, run, alphaT) %>% 
  mutate(model = paste(fit_percModel, fit_respModel, sep = "_")) %>% 
  #filter(run == "MR") %>% 
  inner_join(bestModRun,
             by = c("ID", "run", "model", "mod2Type", "version"))

modelingOutput %>% 
  group_by(ID, run, mod2Type, session) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(run, mod2Type)

modelingOutput %>% 
  group_by(ID, run, mod2Type, model) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(model, mod2Type) %>% 
  table

write_csv(modelingOutput, file = paste(outputFolder, "modelling", "modellingOuputfMRI_adults.csv", sep = "/"))

## Clean up workspace ----
finalVars <- ls()
newVars <- setdiff(finalVars, initialVars)
vars2keep <- c("behData", "expInfo")
newVars <- setdiff(newVars, vars2keep)

remove(list = newVars)
