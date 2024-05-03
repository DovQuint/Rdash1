# Title: SHADOW_PRICE_TABLE_intertie
#   Purposae: Aggregate  intertie shadow prices from different market runs
#   for publication into flexDashboard
#   Structure: Read in cSV files, wrangle, and write back to CSV

# 1. Read in indivdual  CSV files for HASP, RTPD, and RTD run shadow prices

df_read_TIE_HASP <- read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_HASP_INTERTIE.csv")
df_read_TIE_RTPD <- read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_RTPD_INTERTIE.csv")
df_read_TIE_RTD <-  read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_RTD_INTERTIE.csv")
df_read_TIE_DAM <-  read.csv("C:/R/Dash1/CSV/REACTIVE/DASH_DAM_INTERTIE.csv")

#  2. Prepate each frame for join by adding and renaming colkumn

# A. HASP
df_read_TIE_HASP_column <- df_read_TIE_HASP %>% 
  mutate(X = SHADOW_PRC)

new_col_name <- paste0("HASP-", df_read_TIE_HASP$HE_INT[1])

colnames(df_read_TIE_HASP_column )[colnames(df_read_TIE_HASP_column ) == 'X'] <- new_col_name


df_read_TIE_HASP_prejoin <- df_read_TIE_HASP_column  %>% 
  dplyr::mutate(TI_DIR_FULL = if_else(TI_DIRECTION == "I", "imp", "exp")) %>% 
  dplyr::mutate(TI_ID_DIR= paste0(TI_ID, "_", TI_DIR_FULL)) %>% 
  dplyr::select(-c(SHADOW_PRC, HE_INT, TI_DIRECTION, TI_ID, TI_DIR_FULL, OPR_HR)) 


# B. RTPD
df_read_TIE_RTPD_column <- df_read_TIE_RTPD %>% 
  mutate(X = SHADOW_PRC)

new_col_name <- paste0("RTPD-", df_read_TIE_RTPD$HE_INT[1])

colnames(df_read_TIE_RTPD_column )[colnames(df_read_TIE_RTPD_column ) == 'X'] <- new_col_name

df_read_TIE_RTPD_prejoin <- df_read_TIE_RTPD_column    %>% 
  dplyr::mutate(TI_DIR_FULL = if_else(TI_DIRECTION == "I", "imp", "exp")) %>% 
  dplyr::mutate(TI_ID_DIR= paste0(TI_ID, "_", TI_DIR_FULL)) %>% 
  dplyr::select(-c(SHADOW_PRC, HE_INT, TI_DIRECTION, TI_ID, TI_DIR_FULL,OPR_HR)) 



# C. RTD
df_read_TIE_RTD_column <- df_read_TIE_RTD %>% 
  mutate(X = SHADOW_PRC)

new_col_name <- paste0("RTD-", df_read_TIE_RTD$HE_INT[1])

colnames(df_read_TIE_RTD_column )[colnames(df_read_TIE_RTD_column ) == 'X'] <- new_col_name

df_read_TIE_RTD_prejoin <- df_read_TIE_RTD_column   %>% 
  dplyr::mutate(TI_DIR_FULL = if_else(TI_DIRECTION == "I", "imp", "exp")) %>% 
  dplyr::mutate(TI_ID_DIR= paste0(TI_ID, "_", TI_DIR_FULL)) %>% 
  dplyr::select(-c(SHADOW_PRC, HE_INT, TI_DIRECTION, TI_ID, TI_DIR_FULL,OPR_HR)) 



# D. DAM - filter on current OPR_HR from RTPD file
Current_OPR_HR <- df_read_TIE_RTPD$OPR_HR[1]

df_read_TIE_DAM_column <- df_read_TIE_DAM %>% 
  dplyr::filter(OPR_HR == Current_OPR_HR) %>% 
  mutate(X = SHADOW_PRC)

new_col_name <- paste0("DAM-",  df_read_TIE_DAM_column$OPR_HR[1])

colnames(df_read_TIE_DAM_column )[colnames(df_read_TIE_DAM_column ) == 'X'] <- new_col_name

df_read_TIE_DAM_prejoin <- df_read_TIE_DAM_column %>% 
  dplyr::mutate(TI_DIR_FULL = if_else(TI_DIRECTION == "I", "imp", "exp")) %>% 
  dplyr::mutate(TI_ID_DIR= paste0(TI_ID, "_", TI_DIR_FULL)) %>% 
  dplyr::select(-c(SHADOW_PRC, TI_DIRECTION, TI_DIR_FULL, TI_ID, OPR_HR)) 


# 3. Join all run dataframes into single frame and replacer NA with 0
df_SP_TIE_join <-  df_read_TIE_RTPD_prejoin %>% 
  dplyr::full_join(df_read_TIE_HASP_prejoin , by = "TI_ID_DIR") %>% 
  dplyr::full_join(df_read_TIE_RTD_prejoin , by = "TI_ID_DIR") %>% 
  dplyr::full_join(df_read_TIE_DAM_prejoin , by = "TI_ID_DIR") %>% 
  dplyr::relocate(TI_ID_DIR)


df_SP_TIE_join[is.na(df_SP_TIE_join)] <- 0



# 4. Write to CSV
write.csv(df_SP_TIE_join, "C:/R/Dash1/CSV/REACTIVE/DASH_JOIN_INTERTIE.csv", row.names = FALSE)








