##########################################################
##                         PLOTS                        ##
##########################################################

##########################################################
# Load and merge data ------------------------------------

##########################################################
# plot tactile frequency patterns ------------------------

frequencies <- data.frame(
  set = c("set1", "set2", "set3")) %>% 
  mutate(set1_1 = NA, set1_2 = NA, set1_3 = NA,
         set2_1 = NA, set2_2 = NA, set2_3 = NA,
         set3_1 = NA, set3_2 = NA, set3_3 = NA,
         set4_1 = NA, set4_2 = NA, set4_3 = NA)
frequencies[1,2:13] <- c(5,5,5,10,30,70,90,10,90,70,70,70)
frequencies[2,2:13] <- c(10,10,10,70,30,5,5,70,5,90,90,90)
frequencies[3,2:13] <- c(15,15,15,7,50,100,40,5,100,100,100,100)

frequencies_long <- frequencies %>%
  pivot_longer(cols = -set, names_to = c("pattern", "time"), names_sep = "_") %>%
  mutate(time = as.numeric(time)-1,
         pattern = as.factor(as.numeric(factor(pattern))),
         set = factor(set, labels = c("set 1", "set 2", "set 3")))  # Convert time to numeric for plotting

ggplot(frequencies_long, aes(x = time, y = value, group = pattern, color = pattern)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), linewidth = 2) +
  facet_wrap(~set) +
  scale_y_log10(expand = c(0, 0.05),
                breaks = sort(unique(frequencies_long$value)),
                minor_breaks = NULL) +  
  labs(x = "Time [s]", y = "Frequency [Hz] (log scale)", color = "Pattern") +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") + scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
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

ggsave(file.path(outputFolder, 'figures', 'tactileFrequencies.svg'),
       height = 12.6, width = 32.4, units = "cm")

ggsave(file.path(outputFolder, 'figures', 'tactileFrequencies.jpg'),
       width = 40, height = 15, units = 'cm')

ggsave(file.path(outputFolder, 'figures', 'tactileFrequencies.tif'),
       width = 40, height = 15, units = 'cm')

##########################################################
# remove unused variables --------------------------------
remove(frequencies, frequencies_long)

