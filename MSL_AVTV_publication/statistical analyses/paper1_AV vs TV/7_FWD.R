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
# meanFWDrun <- read_excel('B:/Data/CBC_Data/analyses/multisensory_nr/analyses/MR_children/paper1NEW/meanFWD.xlsx')
meanFWDrun <- read_excel("B:/Data/CBC_Data/analyses/multisensory_nr/outputs/children_MR/badScans/badscans_allBad_v2_FWD_Art.xlsx")
load(file.path(outputFolder, "demo.RData"))

# meanFWD <- meanFWDrun %>% 
#   mutate(meanFWD = rowMeans(meanFWDrun[,2:6], na.rm = TRUE)) %>% 
#   select(ID, meanFWD) %>% 
#   left_join(., demo[,c('ID', 'age')], by = join_by(ID))

meanFWD <- meanFWDrun %>% 
  select(ID, meanFWDValue) %>% 
  group_by(ID) %>% 
  summarise(meanFWD = mean(meanFWDValue, na.rm = TRUE)) %>% 
  left_join(., demo[,c('ID', 'age')], by = join_by(ID)) %>% 
  filter(!is.na(age))

cor.test(meanFWD$meanFWD, meanFWD$age)
report(cor.test(meanFWD$meanFWD, meanFWD$age))

summary(lm(meanFWD ~ age, meanFWD))
anova(lm(meanFWD ~ age, meanFWD))
report(anova(lm(meanFWD ~ age, meanFWD)))

ggplot(meanFWD, aes(age, meanFWD)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle("Mean Framewise Displacement Values and Age") +
  ylab("mean FWD")

ggsave(file.path(outputFolder, "plots", "FWDcorrAge.png"),
       width = 16, height = 10, units = "cm")

## look at dif between modalities
meanFWDrun$modality <- "av"
meanFWDrun$stimType <- "env"

meanFWDrun$modality[grepl("TV",meanFWDrun$logfile)] <- "tv"
meanFWDrun$stimType[grepl("TV",meanFWDrun$logfile)] <- "tact"

meanFWDrun$stimType[grepl("Aset4",meanFWDrun$logfile) | 
                      grepl("Aset5",meanFWDrun$logfile) |
                      grepl("Aset6",meanFWDrun$logfile)] <- "syll"


meanFWDMod <- meanFWDrun %>% 
  select(ID, stimType, meanFWDValue) %>% 
  group_by(ID, stimType) %>% 
  summarise(meanFWD = mean(meanFWDValue, na.rm = TRUE)) %>% 
  left_join(., demo[,c('ID', 'age')], by = join_by(ID)) %>% 
  filter(!is.na(age), stimType != "syll")

summary(lm(meanFWD ~ age + stimType, meanFWDMod))
anova(lm(meanFWD ~ age + stimType, meanFWDMod))
report(anova(lm(meanFWD ~ age + stimType, meanFWDMod)))

t.test(meanFWDMod$meanFWD[meanFWDMod$stimType=="env"], 
       meanFWDMod$meanFWD[meanFWDMod$stimType=="tact"])

report(t.test(meanFWDMod$meanFWD[meanFWDMod$stimType=="env"], 
              meanFWDMod$meanFWD[meanFWDMod$stimType=="tact"]))

ggplot(meanFWDMod, aes(stimType, meanFWD, group = stimType)) +
  # geom_point() +
  # stat_smooth(method = "lm") +
  # facet_wrap(~stimType) +
  geom_boxplot() +
  ggtitle("Mean Framewise Displacement Values and Age") 

ggsave(file.path(outputFolder, "plots", "FWDcorrAge.png"),
       width = 16, height = 10, units = "cm")

