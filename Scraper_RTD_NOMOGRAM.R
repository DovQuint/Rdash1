# Title: DASHBOARD_SCRPAER_1
# Purpose: Scrape CAISO OASIS data for use in FlexDash
# Structure: Individual Functions for each OASIS table feed into csv files and are then read to a single 
#   dynamic CSV file to be read by reactive Shiny functio within flex dashboard. The functions are 
#   aggregated andexecuted within a perpetual loop to run every 5 mins
#   See: https://www.caiso.com/Documents/OASIS-InterfaceSpecification_v5_1_1Clean_Fall2017Release.pdf

# 0. Load libray

source("C:/R/Dash1/Load_Lib.R")

# 1. Define RTD Nomogram Scraper Function
#   Function Inputs:
#   - input_query_name =  name of report being queried (i.e. PRC_NOMOGRAM)
#   - input_start_date =  start date (url takes "yyyymmdd" format)
#   - input_end_date = end date (can be created as a plus one from start date within the function)
#   - input_version_number = version number (1 through 8; normally 1 or 2 depending on report check OASIS docs)
#   - input_select_columns = collection of columns to select from dataframe (i.e. c("NODE_ID", "OPR_INTERVAL"))


FN_Scrape_PRC_RTM_NOMOGRAM <-  function(input_query_name,
                                   input_start_date,
                                   input_version_number,
                                   input_select_columns) {
  
  #  INTERNAL DEBUGGER - FUNCTION INPUTS #
  # input_query_name <- "PRC_RTM_NOMOGRAM"
  # input_start_date <- "2024-04-29"
  # input_version_number <- 1
  # input_sql_data_source <- "CAISO_PRICES"
  # input_select_columns <- c("OPR_DT", "OPR_HR", "OPR_INTERVAL", "NOMOGRAM_ID", "CONSTRAINT_CAUSE", "PRC")
  
  
  # A .Convert input_start_date from "yyyy-mm-dd" to yyyymmdd and add D+1 as end_date_input
  
  # set dates
  date_dash_format <- as.Date(input_start_date)
  date_dash_format_plus2 <- date_dash_format + 2
  
  # reformat dates
  input_start_date_yyyymmdd <- format(strptime(date_dash_format, "%Y-%m-%d"),"%Y%m%d") 
  input_end_date_yyyymmdd <- format(strptime(date_dash_format_plus2, "%Y-%m-%d"),"%Y%m%d") 
  
  # B. Define URL components
  
  url_base<- "http://oasis.caiso.com/oasisapi/SingleZip?queryname="
  url_query_name_input <- input_query_name
  url_result_format <- "&resultformat=6&"
  url_market_run <-"market_run_id="
  url_market_run_id <- "RTM"
  url_nomogram_id <- "&nomogramid=ALL&"
  url_start_time <- "startdatetime="
  url_start_date_input <- input_start_date_yyyymmdd
  url_time_T_start <- "T10:00-0000"
  url_end_time <- "&enddatetime="
  url_end_date_input <- input_end_date_yyyymmdd
  url_time_T_end <- "T10:00-0000"
  url_version <- "&version="
  url_version_input <- as.character(input_version_number)
  
  # C. Paste URL components into single string
  
  url_full_call <- paste0(  url_base,
                            url_query_name_input,
                            url_result_format,
                            url_market_run,
                            url_market_run_id,
                            url_nomogram_id,
                            url_start_time,
                            url_start_date_input,
                            url_time_T_start,
                            url_end_time,
                            url_end_date_input,
                            url_time_T_end,
                            url_version,
                            url_version_input)
  
  # D.  Create temp file for OASIS download
  
  # Create temp directory
  td  <- tempdir() 
  
  # Create tempfile in tempdir with .zip extension
  tf <-  tempfile(tmpdir=td, fileext=".zip") 
  
  # E. Download url file from OASIS
  
  download.file(url_full_call, destfile = tf, mode = "wb") #wb ensures file can be unzipped in windows
  
  # F. Set R to sleep for 6 seconds due to OASIS policy 
  
  Sys.sleep(10)
  
  # G. Define exdir string and create directory for unzipping of file
  
  # get wd
  string_WD <- getwd()
  
  # set exdir string
  exdir_string <- paste0(string_WD,
                         "/",
                         "Download/",
                         input_query_name,
                         "_",
                         input_start_date_yyyymmdd)
  # create directory
  dir.create(exdir_string) 
  
  # H. Unzip downloaded files from temp directory to exdir folder
  
  unzip(tf, exdir = exdir_string)
  
  # I.  List files in exdir folder
  
  files_csv_list <- list.files(path = exdir_string)
  
  # J.  Use purr:map_df to apply read.csv to files in exdir and bind into dataframe
  
  df_csv_bind <- map_df(paste(exdir_string, files_csv_list, sep = "/"), read.csv, 
                        stringsAsFactors = FALSE)   
  # message
  message(paste0("The binded dataframe for ",input_query_name," for ", 
                 input_start_date_yyyymmdd, " has ", nrow(df_csv_bind), " rows"))
  
  
  # K. Select columns and add month and year columns  
  
  df_write_csv <- df_csv_bind %>% 
    dplyr::select(all_of(input_select_columns)) %>% 
    mutate(MARKET_RUN_ID = "RTD") 
  
  
  # L.  Write CSV to file
  string_WD <- getwd()
  
  csv_string <- paste0(string_WD,
                       "/CSV/RTD_NOMOGRAM/",
                       input_query_name,
                       "_",
                       input_start_date_yyyymmdd,
                       ".csv")
  
  write.csv(df_write_csv, csv_string)
  
  
  
  # M.  Unlink from temp folder and delete exdir folder with unzipped csv files
  
  file_path_exdir <- exdir_string 
  unlink(file_path_exdir, recursive = TRUE) 
  unlink(td) 
  
  
  # End bracket for function
} 

# 2. Establish inputs for function 
input_start_date <- lubridate::today() - 1
input_query_name <- "PRC_RTM_NOMOGRAM"
input_version_number <- 1
input_sql_data_source <- "CAISO_PRICES"
input_select_columns <- c("OPR_DT", "OPR_HR", "OPR_INTERVAL", "NOMOGRAM_ID", "CONSTRAINT_CAUSE", "PRC")

# 3. Execute Function
FN_Scrape_PRC_RTM_NOMOGRAM(input_query_name = input_query_name,
                           input_start_date  = input_start_date ,
                           input_version_number = input_version_number,
                           input_select_columns = input_select_columns)

# 4. Read most recent CSV file and get most recent data
file_list <- file.info(list.files("C:/R/Dash1/CSV/RTD_NOMOGRAM", full.names = T))
most_recent_file <- rownames(file_list)[which.max(file_list$mtime)]
most_recent_file

df_read_RTD_NOMOGRAM <- read.csv(most_recent_file) %>% 
  mutate(OPR_DT = lubridate::as_date(OPR_DT)) 

df_recent_write_RTD_NOMOGRAM <- df_read_RTD_NOMOGRAM %>% 
  filter(OPR_DT == max(OPR_DT)) %>% 
  filter(OPR_HR == max(OPR_HR)) %>%
  filter(OPR_INTERVAL == max(OPR_INTERVAL)) %>% 
  dplyr::mutate(SHADOW_PRC = round(PRC, 2)) %>% 
  dplyr::mutate(HE_INT = paste0(OPR_HR,"-", OPR_INTERVAL)) %>% 
  dplyr::select(-c(X, CONSTRAINT_CAUSE, OPR_DT, OPR_HR, PRC, OPR_INTERVAL, MARKET_RUN_ID)) 

# 5. Select columns and write to CSV
csv_string <- "C:/R/Dash1/CSV/REACTIVE/DASH_RTD_NOMOGRAM.csv"
write.csv(df_recent_write_RTD_NOMOGRAM, csv_string, row.names = FALSE)



