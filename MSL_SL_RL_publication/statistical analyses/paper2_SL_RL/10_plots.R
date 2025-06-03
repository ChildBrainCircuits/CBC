##########################################################
##                         PLOTS                        ##
##########################################################
## Description :: 
## Input :::::::: 
## Libraries :::: 
## Output ::::::: 
##########################################################

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
frequencies[3,2:13] <- c(15,15,15,7,30,100,40,5,100,100,100,100)

frequencies_long <- frequencies %>%
  pivot_longer(cols = -set, names_to = c("pattern", "time"), names_sep = "_") %>%
  mutate(time = as.numeric(time)-1,
         pattern = as.factor(as.numeric(factor(pattern))),
         set = factor(set, labels = c("set 1", "set 2", "set 3")))  # Convert time to numeric for plotting


colors <- scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9)$palette(4)

ggplot(frequencies_long, aes(x = time, y = value, group = pattern, color = pattern)) +
  #geom_line(size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), linewidth = 2) +
  facet_wrap(~set) +
  scale_y_log10(expand = c(0, 0.05),
                breaks = sort(unique(frequencies_long$value)),
                minor_breaks = NULL) +  
  #ggtitle("Frequency Patterns for Tactile Stimulation") +
  labs(x = "Time [s]", y = "Frequency [Hz] (log scale)", color = "Pattern") +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") + 
  #scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  scale_color_manual(values = colors) 

ggsave(file.path(outputFolder, 'figures', 'tactileFrequencies1A.tif'),
       width = 1.5*15, height = 1.5*4.5, units = 'cm')

frequencies2 <- data.frame(
  set = c("set1", "set2", "set3")) %>% 
  mutate(set1_1 = NA, set1_2 = NA, set1_3 = NA,
         set2_1 = NA, set2_2 = NA, set2_3 = NA,
         set3_1 = NA, set3_2 = NA, set3_3 = NA)
frequencies2[1,2:10] <- c(5,5,5,10,30,90,90,5,90)
frequencies2[2,2:10] <- c(10,10,10,55,15,5,5,90,5)
frequencies2[3,2:10] <- c(85,85,85,35,50,100,100,10,100)

frequencies2_long <- frequencies2 %>%
  pivot_longer(cols = -set, names_to = c("pattern", "time"), names_sep = "_") %>%
  mutate(time = as.numeric(time)-1,
         pattern = as.factor(as.numeric(factor(pattern))),
         set = factor(set, labels = c("set 1", "set 2", "set 3")))  # Convert time to numeric for plotting

ggplot(frequencies2_long, aes(x = time, y = value, group = pattern, color = pattern)) +
  #geom_line(size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), linewidth = 2) +
  facet_wrap(~set) +
  scale_y_log10(expand = c(0, 0.05),
                breaks = sort(unique(frequencies_long$value)),
                minor_breaks = NULL) +  
  #ggtitle("Frequency Patterns for Tactile Stimulation") +
  labs(x = "Time [s]", y = "Frequency [Hz] (log scale)", color = "Pattern") +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") + 
  #scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  scale_color_manual(values = colors) 

ggsave(file.path(outputFolder, 'figures', 'tactileFrequencies1B.tif'),
       width = 1.5*15, height = 1.5*4.5, units = 'cm')

frequencies3 <- data.frame(
  set = c("set1", "set2", "set3", "set4")) %>% 
  mutate(set1_1 = NA, set1_2 = NA, set1_3 = NA,
         set2_1 = NA, set2_2 = NA, set2_3 = NA,
         set3_1 = NA, set3_2 = NA, set3_3 = NA)
frequencies3[1,2:10] <- c(5,5,5,10,30,90,90,5,90)
frequencies3[2,2:10] <- c(90,90,90,55,35,5,5,90,5)
frequencies3[3,2:10] <- c(10,10,10,35,55,100,100,10,100)
frequencies3[4,2:10] <- c(100,100,100,80,60,10,10,100,10)

frequencies3_long <- frequencies3 %>%
  pivot_longer(cols = -set, names_to = c("pattern", "time"), names_sep = "_") %>%
  mutate(time = as.numeric(time)-1,
         pattern = as.factor(as.numeric(factor(pattern))),
         set = factor(set, labels = c("set 1", "set 2", "set 3", "set4")))  # Convert time to numeric for plotting

ggplot(frequencies3_long, aes(x = time, y = value, group = pattern, color = pattern)) +
  #geom_line(size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), linewidth = 2) +
  facet_grid(~set) +
  scale_y_log10(expand = c(0, 0.05),
                breaks = sort(unique(frequencies_long$value)),
                minor_breaks = NULL) +  
  #ggtitle("Frequency Patterns for Tactile Stimulation") +
  labs(x = "Time [s]", y = "Frequency [Hz] (log scale)", color = "Pattern") +
  jtools::theme_apa(remove.y.gridlines = F, legend.pos = "none") + 
  #scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  scale_color_manual(values = colors)

ggsave(file.path(outputFolder, 'figures', 'tactileFrequencies1C.tif'),
       width = 1.5*15, height = 1.5*4.5, units = 'cm')

##########################################################
# remove unused variables --------------------------------
remove(frequencies, frequencies_long)

