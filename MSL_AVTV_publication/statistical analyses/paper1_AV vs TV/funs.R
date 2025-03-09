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
      "CBCL" = NA,
      "ADH [#above threshold]" = paste(sum(CBCL_ADH > 69, na.rm = T)),
      "Total [#above threshold]" = paste(sum(CBCL_tot > 63, na.rm = T)),
      IQ = NA,
      "verbal IQ (PPVT)" = paste(round(mean(PPVT_IQ, na.rm = T),1), " (",round(sd(PPVT_IQ, na.rm = T),1),
                                 ") [",round(min(PPVT_IQ, na.rm = T),1),"-",round(max(PPVT_IQ, na.rm = T),1),
                                 "]; N=", sum(!is.na(PPVT_IQ)), sep = ""),
      "fluid reasoning IQ (WISC-V, WPPSI)" = paste(round(mean(FSIQ, na.rm = T),1), " (",round(sd(FSIQ, na.rm = T),1),
                                                   ") [",round(min(FSIQ, na.rm = T),1),"-",round(max(FSIQ, na.rm = T),1),
                                                   "]; N=", sum(!is.na(FSIQ)), sep = ""),
      "mean IQ" = paste(round(mean(meanIQ, na.rm = T),1), " (",round(sd(meanIQ, na.rm = T),1),
                        ") [",round(min(meanIQ, na.rm = T),1),"-",round(max(meanIQ, na.rm = T),1), "]", sep = ""),
      "Multisensory Gain" = NA,
      "rel. unisensory gain [%]" = paste(round(mean(relative_unisensory_gain, na.rm = T), 2), " (", 
                                         round(sd(relative_unisensory_gain, na.rm = T), 2), ") [", 
                                         round(min(relative_unisensory_gain, na.rm=T),2), "-", round(max(relative_unisensory_gain, na.rm=T),2),
                                         "]; N=", sum(!is.na(relative_unisensory_gain)),
                                         sep = ""),
      "rel. multisensory gain [%]" = paste(round(mean(relative_multisensory_gain, na.rm = T), 2), " (", 
                                           round(sd(relative_multisensory_gain, na.rm = T), 2), ") [", 
                                           round(min(relative_multisensory_gain, na.rm=T),2), "-", round(max(relative_multisensory_gain, na.rm=T),2),
                                           "]; N=", sum(!is.na(relative_multisensory_gain)),
                                           sep = ""),
      # "SLRT-ii reading fluency" = NA,
      # word = paste(round(mean(SLRT_words, na.rm = T), 2), " (", round(sd(SLRT_words, na.rm = T), 2), 
      #              ") N=", sum(!is.na(SLRT_words)),sep = ""),
      # pseudoword = paste(round(mean(SLRT_nonwords, na.rm = T), 2), " (", round(sd(SLRT_nonwords, na.rm = T), 2), 
      #                    ") N=", sum(!is.na(SLRT_nonwords)),sep = ""),
      # "word [PR]" = paste(round(mean(SLRT_words_pr, na.rm = T), 2), " (", round(sd(SLRT_words_pr, na.rm = T), 2), 
      #                     ") N=", sum(!is.na(SLRT_words_pr)),sep = ""),
      # "pseudoword [PR]" = paste(round(mean(SLRT_nonwords_pr, na.rm = T), 2), " (", round(sd(SLRT_nonwords_pr, na.rm = T), 2), 
      #                           ") N=", sum(!is.na(SLRT_nonwords_pr)),sep = ""),
      # "average [PR]" = paste(round(mean(SLRT_mean_pr, na.rm = T), 2), " (", round(sd(SLRT_mean_pr, na.rm = T), 2), 
      #                        ") N=", sum(!is.na(SLRT_mean_pr)),sep = ""),
      "Letter Knowledge" = NA,
      "Sounds (lower case letters)" = paste(round(mean(LETKNOW_soundLC, na.rm = T), 2), " (", round(sd(LETKNOW_soundLC, na.rm = T), 2), 
                                            ") N=", sum(!is.na(LETKNOW_soundLC)),sep = ""),
      "Complex sounds" = paste(round(mean(LETKNOW_soundComplex, na.rm = T), 2), " (", round(sd(LETKNOW_soundComplex, na.rm = T), 2), 
                               ") N=", sum(!is.na(LETKNOW_soundComplex)),sep = ""),
      "Working Memory" = NA,
      "VIS correct items FW" = paste(round(mean(CORSI_sumSeqFW, na.rm = T), 2), " (", round(sd(CORSI_sumSeqFW, na.rm = T), 2), 
                                     ") N=", sum(!is.na(CORSI_sumSeqFW)),sep = ""),
      "VIS longest sequence FW" = paste(round(mean(CORSI_longSeqFW, na.rm = T), 2), " (", round(sd(CORSI_longSeqFW, na.rm = T), 2), 
                                        ") N=", sum(!is.na(CORSI_longSeqFW)),sep = ""),
      "VIS correct items BW" = paste(round(mean(CORSI_sumSeqBW, na.rm = T), 2), " (", round(sd(CORSI_sumSeqBW, na.rm = T), 2), 
                                     ") N=", sum(!is.na(CORSI_sumSeqBW)),sep = ""),
      "VIS longest sequence BW" = paste(round(mean(CORSI_longSeqBW, na.rm = T), 2), " (", round(sd(CORSI_longSeqBW, na.rm = T), 2), 
                                        ") N=", sum(!is.na(CORSI_longSeqBW)),sep = ""),
      "VERB correct items FW" = paste(round(mean(DIGSPAN_sumSeqFW, na.rm = T), 2), " (", round(sd(DIGSPAN_sumSeqFW, na.rm = T), 2), 
                                      ") N=", sum(!is.na(DIGSPAN_sumSeqFW)),sep = ""),
      "VERB longest sequence FW" = paste(round(mean(DIGSPAN_longSeqFW, na.rm = T), 2), " (", round(sd(DIGSPAN_longSeqFW, na.rm = T), 2), 
                                         ") N=", sum(!is.na(DIGSPAN_longSeqFW)),sep = ""),
      "VERB correct items BW" = paste(round(mean(DIGSPAN_sumSeqBW, na.rm = T), 2), " (", round(sd(DIGSPAN_sumSeqBW, na.rm = T), 2), 
                                      ") N=", sum(!is.na(DIGSPAN_sumSeqBW)),sep = ""),
      "VERB longest sequence BW" = paste(round(mean(DIGSPAN_longSeqBW, na.rm = T), 2), " (", round(sd(DIGSPAN_longSeqBW, na.rm = T), 2), 
                                         ") N=", sum(!is.na(DIGSPAN_longSeqBW)),sep = ""),
      "IDS-2" =NA,
      "RAN [t value]" = paste(round(mean(IDS_ran_tval, na.rm = T), 2), " (", round(sd(IDS_ran_tval, na.rm = T), 2), 
                              ") N=", sum(!is.na(IDS_ran_tval)),sep = ""),
      "RAN normal [s]" = paste(round(mean(IDS_ran_t, na.rm = T), 2), " (", round(sd(IDS_ran_t, na.rm = T), 2), 
                               ") N=", sum(!is.na(IDS_ran_t)),sep = ""),
      "RAN bw [s]" = paste(round(mean(IDS_ranBW_t, na.rm = T), 2), " (", round(sd(IDS_ranBW_t, na.rm = T), 2), 
                           ") N=", sum(!is.na(IDS_ranBW_t)),sep = ""),
      "RAN stroop [s]" = paste(round(mean(IDS_ranStroop_t, na.rm = T), 2), " (", round(sd(IDS_ranStroop_t, na.rm = T), 2), 
                               ") N=", sum(!is.na(IDS_ranStroop_t)),sep = ""),
      "processing speed [t-value]" = paste(round(mean(IDS_procSpeed_tval, na.rm = T), 2), " (", round(sd(IDS_procSpeed_tval, na.rm = T), 2), 
                                           ") N=", sum(!is.na(IDS_procSpeed_tval)),sep = ""),
      "KITAP" = NA,
      "sustained attention median RT [ms]" = paste(round(mean(KITAP_susAtt_medRT, na.rm = T), 2), " (", round(sd(KITAP_susAtt_medRT, na.rm = T), 2), 
                                                   ") N=", sum(!is.na(KITAP_susAtt_medRT)),sep = ""),
      "sustained attention median RT [PR]" = paste(round(mean(KITAP_susAtt_RT_pr, na.rm = T), 2), " (", round(sd(KITAP_susAtt_RT_pr, na.rm = T), 2), 
                                                   ") N=", sum(!is.na(KITAP_susAtt_RT_pr)),sep = ""),
      "Go/NoGo median RT [ms]" = paste(round(mean(KITAP_GNG_medRT, na.rm = T), 2), " (", round(sd(KITAP_GNG_medRT, na.rm = T), 2), 
                                       ") N=", sum(!is.na(KITAP_GNG_medRT)),sep = ""),
      "Go/NoGo median RT [PR]" = paste(round(mean(KITAP_GNG_RT_pr, na.rm = T), 2), " (", round(sd(KITAP_GNG_RT_pr, na.rm = T), 2), 
                                       ") N=", sum(!is.na(KITAP_GNG_RT_pr)),sep = ""),
      "Go/NoGo speed-accuracy-tradeoff" = paste(round(mean(KITAP_GNG_SpAcc, na.rm = T), 2), " (", round(sd(KITAP_GNG_SpAcc, na.rm = T), 2), 
                                                ") N=", sum(!is.na(KITAP_GNG_SpAcc)),sep = ""),
      "divided attention median RT auditory [ms]" = paste(round(mean(KITAP_divAtt_aud_medRT, na.rm = T), 2), " (", round(sd(KITAP_divAtt_aud_medRT, na.rm = T), 2), 
                                                          ") N=", sum(!is.na(KITAP_divAtt_aud_medRT)),sep = ""),
      "divided attention median RT auditory [PR]" = paste(round(mean(KITAP_divAtt_aud_RT_pr, na.rm = T), 2), " (", round(sd(KITAP_divAtt_aud_RT_pr, na.rm = T), 2), 
                                                          ") N=", sum(!is.na(KITAP_divAtt_aud_RT_pr)),sep = ""),
      "divided attention median RT visual [ms]" = paste(round(mean(KITAP_divAtt_vis_medRT, na.rm = T), 2), " (", round(sd(KITAP_divAtt_vis_medRT, na.rm = T), 2), 
                                                        ") N=", sum(!is.na(KITAP_divAtt_vis_medRT)),sep = ""),
      "divided attention median RT visual [PR]" = paste(round(mean(KITAP_divAtt_vis_RT_pr, na.rm = T), 2), " (", round(sd(KITAP_divAtt_vis_RT_pr, na.rm = T), 2), 
                                                        ") N=", sum(!is.na(KITAP_divAtt_vis_RT_pr)),sep = "")
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