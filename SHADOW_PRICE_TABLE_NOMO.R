# Title: SHADOW_PRICE_TABLE_NOMO
#   Purposae: Aggregate  nomograme shadow prices from different market runs
#   for publication into flexDashboard
#   Structure: Read in cSV files, wrangle, and write back to CSV

# 1. Read in indivdual  CSV files for HASP, RTPD, and RTD run shadow prices

df_read_SP_HASP <- read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_HASP_NOMOGRAM.csv")
df_read_SP_RTPD <- read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_RTPD_NOMOGRAM.csv")
df_read_SP_RTD <-  read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_RTD_NOMOGRAM.csv")
df_read_SP_DAM <-  read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_DAM_NOMOGRAM.csv")

#  2. Prepate each frame for join by adding and renaming colkumn

  # A. HASP
  df_read_SP_HASP_column <- df_read_SP_HASP %>% 
    mutate(X = SHADOW_PRC)
  
  new_col_name <- paste0("HASP-", df_read_SP_HASP$HE_INT[1])
  
  colnames(df_read_SP_HASP_column )[colnames(df_read_SP_HASP_column ) == 'X'] <- new_col_name
  
  df_read_SP_HASP_prejoin <- df_read_SP_HASP_column  %>% 
    dplyr::select(-c(SHADOW_PRC, HE_INT))
  
  
  # B. RTPD
  df_read_SP_RTPD_column <- df_read_SP_RTPD %>% 
    mutate(X = SHADOW_PRC)
  
  new_col_name <- paste0("RTPD-", df_read_SP_RTPD$HE_INT[1])
  
  colnames(df_read_SP_RTPD_column )[colnames(df_read_SP_RTPD_column ) == 'X'] <- new_col_name
  
  df_read_SP_RTPD_prejoin <- df_read_SP_RTPD_column  %>% 
    dplyr::select(-c(SHADOW_PRC, HE_INT, OPR_HR))
  
  
  # C. RTD
  df_read_SP_RTD_column <- df_read_SP_RTD %>% 
    mutate(X = SHADOW_PRC)
  
  new_col_name <- paste0("RTD-", df_read_SP_RTD$HE_INT[1])
  
  colnames(df_read_SP_RTD_column )[colnames(df_read_SP_RTD_column ) == 'X'] <- new_col_name
  
  df_read_SP_RTD_prejoin <- df_read_SP_RTD_column  %>% 
    dplyr::select(-c(SHADOW_PRC, HE_INT))
  
  
  # D. DAM - filter on current OPR_HR from RTPD file
  Current_OPR_HR <- df_read_SP_RTPD$OPR_HR[1]

  df_read_SP_DAM_column <- df_read_SP_DAM %>% 
    dplyr::filter(OPR_HR == Current_OPR_HR) %>% 
    mutate(X = SHADOW_PRC)
  
  new_col_name <- paste0("DAM-",  df_read_SP_DAM_column$OPR_HR[1])
  
  colnames(df_read_SP_DAM_column )[colnames(df_read_SP_DAM_column ) == 'X'] <- new_col_name
  
  df_read_SP_DAM_prejoin <- df_read_SP_DAM_column  %>% 
    dplyr::select(-c(SHADOW_PRC, OPR_HR))
  
  
# 3. Join all run dataframes into single frame and replacer NA with 0
  df_SP_NOMO_join <-  df_read_SP_RTPD_prejoin %>% 
    dplyr::full_join(df_read_SP_HASP_prejoin , by = "NOMOGRAM_ID") %>% 
    dplyr::full_join(df_read_SP_RTD_prejoin , by = "NOMOGRAM_ID") %>% 
    dplyr::full_join(df_read_SP_DAM_prejoin , by = "NOMOGRAM_ID") 
  
  
  df_SP_NOMO_join[is.na(df_SP_NOMO_join)] <- 0

  
# 4. Sort on Diffs
  
  # (Need to create new columns cbind, and rename by index position as workaround)
  
  RTPD <- df_SP_NOMO_join[2]
  DAM <- df_SP_NOMO_join[5]
  
  df_SP_NOMO_cols<- df_SP_NOMO_join %>% 
    cbind(RTPD) %>% 
    cbind(DAM) 
  
  colnames(df_SP_NOMO_cols)[6] = "RTPD_SP"
  colnames(df_SP_NOMO_cols)[7] = "DAM_SP"
  
  df_SP_NOMO_write <-  df_SP_NOMO_cols %>% 
    dplyr::mutate(Diff = RTPD_SP - DAM_SP) %>% 
    dplyr::arrange(desc(abs(Diff))) %>% 
    select(-c(RTPD_SP, DAM_SP, Diff))
  
  
# 4. Write to CSV
  write.csv(df_SP_NOMO_write, "C:/R/Dash1/CSV/REACTIVE/DASH_JOIN_NOMOGRAM.csv", row.names = FALSE)
  
  

 
    
  
  
  
