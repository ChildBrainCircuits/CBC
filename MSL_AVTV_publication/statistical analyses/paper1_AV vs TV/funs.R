#### functions used in this project ####

# function for overview table ####
creatDemoTable <- function(data, vars2group){
  demo_grouped <- data %>%
    group_by_at(vars(!!!vars2group)) %>%
    summarise(
      "Age [y]"= paste(round(mean(age, na.rm = T), 2), " (", 
                       round(sd(age, na.rm = T), 2), ") [", 
                       min(age, na.rm=T), "-", max(age, na.rm=T),
                       "]; N=", sum(!is.na(age)),
                       sep = ""),
      "Sex [f:m]"=paste(sum(geschlecht=="Weiblich"),":",sum(geschlecht=="Männlich"), sep = ""),
      "Handedness [l:a:r]" = paste(sum(EHI_handedness=="left", na.rm = T),":",sum(EHI_handedness=="ambidexterous", na.rm = T),":", sum(EHI_handedness=="right", na.rm = T), sep = ""),
      IQ = NA,
      "mean IQ" = paste(round(mean(meanIQ, na.rm = T),1), " (",round(sd(meanIQ, na.rm = T),1),
                        ") [",round(min(meanIQ, na.rm = T),1),"-",round(max(meanIQ, na.rm = T),1), "]", sep = ""),
      "IDS-2" =NA,
      "RAN [t value]" = paste(round(mean(IDS_ran_tval, na.rm = T), 2), " (", round(sd(IDS_ran_tval, na.rm = T), 2), 
                              ") N=", sum(!is.na(IDS_ran_tval)),sep = ""),
      "processing speed [t-value]" = paste(round(mean(IDS_procSpeed_tval, na.rm = T), 2), " (", round(sd(IDS_procSpeed_tval, na.rm = T), 2), 
                                           ") N=", sum(!is.na(IDS_procSpeed_tval)),sep = ""),
      "KITAP" = NA,
      "sustained attention median RT [PR]" = paste(round(mean(KITAP_susAtt_RT_pr, na.rm = T), 2), " (", round(sd(KITAP_susAtt_RT_pr, na.rm = T), 2), 
                                                   ") N=", sum(!is.na(KITAP_susAtt_RT_pr)),sep = ""),
    ) %>%
    ungroup() %>%
    pivot_longer(cols = -c(!!!vars2group), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = vars2group, values_from = value)
  
  return(demo_grouped)
}

# function to save tables with factor labels ----
saveFunction <- function(df, filename){
  data_factor_levels_preserved <- df
  factor_columns <- sapply(data_factor_levels_preserved, is.factor)
  data_factor_levels_preserved[factor_columns] <- lapply(data_factor_levels_preserved[factor_columns], as.character)
  openxlsx::write.xlsx(data_factor_levels_preserved, filename, rowNames=F, overwrite = T)
}

# ----
fun2 <- function(x) {
  formatC(x, format = "f", digits = 2)
}


fun3 <- function(x) {
  formatC(x, format = "f", digits = 3)
}