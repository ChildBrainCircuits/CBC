##########################################################
##             Framewise Displacement                   ##
##########################################################

# Load and merge data ------------------------------------
##########################################################
meanFWDrun <- read_excel(file.path(inputFolder, "badscans_allBad_v2_FWD09.xlsx"))
load(file.path(outputFolder, "demo.RData"))

# prepare data
meanFWD <- meanFWDrun %>% 
  select(ID, meanFWDValue) %>%
  filter(ID %in% demo$ID) %>% 
  group_by(ID) %>% 
  summarise(meanFWD = mean(meanFWDValue, na.rm = TRUE)) %>% 
  left_join(., demo[,c('ID', 'age')], by = join_by(ID))

cor.test(meanFWD$meanFWD, meanFWD$age)
report(cor.test(meanFWD$meanFWD, meanFWD$age))

ggplot(meanFWD, aes(age, meanFWD)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle("") +
  ylab("mean FWD") +
  jtools::theme_apa(remove.y.gridlines = F) + scale_y_continuous(expand = c(0, 0), limits=c(0,1.6)) + 
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

ggsave(file.path(outputFolder, "figures", "FWDcorrAge.tif"),
       width = 24, height = 15, units = "cm")
