
# Set directory.
setwd("...")

# Load packages.
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)

# Read the names of the files in the CCD data folder.
master <- list.files("Raw Data\\NCES CCD")
# save folder path for future use.
pth <- "Raw Data\\NCES CCD\\"



#- Membership files (1415 - 1819).
# Create empty data frame to append data.
membership_all <- data.frame()

# Loop for each file.
for (y in master[str_detect(master,"ccd_sch_052")]) {
  # Detect file name based on whether file(y) is ZIP or not.
  if(str_detect(y, ".zip")){
    folder_zip <- as.character(unzip(paste0(pth, y), list = TRUE)$Name)
    df_name <- folder_zip[str_detect(folder_zip, "csv|txt")] #select only csv or txt
  }else{
    df_name <- y
  }
  # Read data based on df_name has CSV or TXT. 
  if(str_detect(df_name, "csv")){ #csv files
    # if file(y) is ZIP or not.
    if(str_detect(y, ".zip")){
      data0 <- read.csv(unz(paste0(pth, y), df_name), header = TRUE, sep = ",")
    }else{
      data0 <- read.csv(paste0(pth, y), header = TRUE, sep = ",")
    }
  }else{ #txt files
    # if file(y) is ZIP or not.
    if(str_detect(y, ".zip")){
      data0 <- read.delim(unz(paste0(pth, y), df_name))
    }else{
      data0 <- read.delim(paste0(pth, y))
    }
  }
  
  # -Export data based on file type WIDE or LONG.
  # export data based on whether it is a wide or long file.
  if(str_detect(df_name, "_l_")){ #long files
    # Filter MA, Grade 12.
    df_ccd <- data0 %>% 
      filter(ST=="MA") %>% 
      filter(GRADE=="Grade 12")
    # Clean state school codes (then, these are the same as DESE codes).
    df_ccd$School.Code <- substr(df_ccd$ST_SCHID, nchar(df_ccd$ST_SCHID)-7, nchar(df_ccd$ST_SCHID))
    # Create new data frame with std count breakdown.
    df_ccd_final <- df_ccd %>% 
      # select variables
      select("SCHOOL_YEAR","School.Code","SCH_NAME","SEX","RACE_ETHNICITY","STUDENT_COUNT") %>% 
      # make data wider
      pivot_wider(names_from = c(RACE_ETHNICITY,SEX), values_from = STUDENT_COUNT) %>% 
      # extract student counts by group
      mutate(Female = rowSums(select(.,ends_with("_Female")), na.rm = T),
             Male = rowSums(select(.,ends_with("_Male")), na.rm = T), 
             `American Indian or Alaska Native` = rowSums(select(.,starts_with("American Indian or Alaska Native")), na.rm = T),
             Asian = rowSums(select(.,starts_with("Asian")), na.rm = T),
             `Black or African American` = rowSums(select(.,starts_with("Black or African American")), na.rm = T),
             `Hispanic/Latino` = rowSums(select(.,starts_with("Hispanic/Latino")), na.rm = T),
             `Native Hawaiian or Other Pacific Islander` = rowSums(select(.,starts_with("Native Hawaiian or Other Pacific Islander")), na.rm = T),
             `Two or more races` = rowSums(select(.,starts_with("Two or more races")), na.rm = T),
             White = rowSums(select(.,starts_with("White")), na.rm = T)) %>% 
      # rename variables for consistency 
      rename(Year=SCHOOL_YEAR, G12=`No Category Codes_No Category Codes`) %>% 
      # select variables to simplify data
      select(-(`American Indian or Alaska Native_Female`:White_Male)) %>% 
      arrange(School.Code)
    
  }else{ #wide files
    # define missing values
    data0[data0==-1] <- NA
    data0[data0==-2] <- NA
    data0[data0==-9] <- NA
    # Filter MA.
    df_ccd <- data0 %>% 
      filter(STABR=="MA") 
    # Clean state school codes (then, these are the same as DESE codes).
    df_ccd$School.Code <- substr(df_ccd$ST_SCHID, nchar(df_ccd$ST_SCHID)-7, nchar(df_ccd$ST_SCHID))
    # Create new data frame with std count breakdown.
    df_ccd_final <- df_ccd %>% 
      # filter data for schools with 12th grade students only.
      filter(is.na(G12)==F) %>% 
      # select variables
      select("SURVYEAR","School.Code","SCH_NAME", contains("12")) %>% 
      # extract student counts by group
      mutate(Female = rowSums(select(.,ends_with("F")), na.rm = T),
             Male = rowSums(select(.,ends_with("M")), na.rm = T),
             `American Indian or Alaska Native` = rowSums(select(.,starts_with("AM")), na.rm = T),
             Asian = rowSums(select(.,starts_with("AS")), na.rm = T),
             `Black or African American` = rowSums(select(.,starts_with("BL")), na.rm = T),
             `Hispanic/Latino` = rowSums(select(.,starts_with("HI")), na.rm = T),
             `Native Hawaiian or Other Pacific Islander` = rowSums(select(.,starts_with("HP")), na.rm = T),
             `Two or more races` = rowSums(select(.,starts_with("TR")), na.rm = T),
             White = rowSums(select(.,starts_with("WH")), na.rm = T)) %>% 
      # select variables to simplify data
      select(-contains("12"),"G12") %>% 
      # rename variables for consistency 
      rename(Year=SURVYEAR) %>% 
      # sort and relocate variables for consistency
      arrange(School.Code) %>% 
      relocate(G12, .after = SCH_NAME) 
  }
  # Append data
  membership_all <- rbind(membership_all, df_ccd_final)
}
# Write data.
write.xlsx(membership_all, file="Output Data\\CCD cleaned\\CCD Membership 2015-2019.xlsx", sheetName="CCD Membership", row.names=FALSE)



#- Directory files (1415 - 1819).
# Create empty data frame to append data.
directory_all <- data.frame()

# Loop for each file.
for (y in master[str_detect(master,"ccd_sch_029")]) {
  # Detect file name based on whether file(y) is ZIP or not.
  if(str_detect(y, ".zip")){
    folder_zip <- as.character(unzip(paste0(pth, y), list = TRUE)$Name)
    df_name <- folder_zip[str_detect(folder_zip, "csv|txt")] #select only csv or txt
  }else{
    df_name <- y
  }
  # Read data based on df_name has CSV or TXT. 
  if(str_detect(df_name, "csv")){ #csv files
    # if file(y) is ZIP or not.
    if(str_detect(y, ".zip")){
      data0 <- read.csv(unz(paste0(pth, y), df_name), header = TRUE, sep = ",")
    }else{
      data0 <- read.csv(paste0(pth, y), header = TRUE, sep = ",")
    }
  }else{ #txt files
    # if file(y) is ZIP or not.
    if(str_detect(y, ".zip")){
      data0 <- read.delim(unz(paste0(pth, y), df_name))
    }else{
      data0 <- read.delim(paste0(pth, y))
    }
  }
  
  # rename variables that are different across the years.
  if("STABR" %in% names(data0)){
    df_ccd <- data0 %>% 
      rename(ST=STABR, Year=SURVYEAR)
  }else{
    df_ccd <- data0 %>% 
      rename(Year=SCHOOL_YEAR)
  }
  
  # Filter MA. 
  df_ccd <- df_ccd %>% 
    filter(ST=="MA")
  # Filter G12OFFERED (detect variable name first, changing across the years).
  tt <- df_ccd %>% 
    select(.,contains("12")) %>% 
    select(.,ends_with("OFFERED")) %>% 
    names()
  df_ccd <- df_ccd[which(df_ccd[,tt] %in% c("Y","Yes")),]
  # Clean state school codes (then, these are the same as DESE codes).
  df_ccd$School.Code <- substr(df_ccd$ST_SCHID, nchar(df_ccd$ST_SCHID)-7, nchar(df_ccd$ST_SCHID))
  # Select variables.
  df_ccd <- df_ccd %>% 
    select("Year","School.Code","SCH_NAME","SCH_TYPE_TEXT","LEVEL","GSLO","CHARTER_TEXT") %>% 
    arrange(School.Code)

  # Append data
  directory_all <- rbind(directory_all, df_ccd)
}
# Rename factor values.
directory_all$SCH_TYPE_TEXT[str_detect(directory_all$SCH_TYPE_TEXT,"Alternative")] <- "Alternative Education School"
directory_all$LEVEL[directory_all$LEVEL==3] <- "High"
directory_all$LEVEL[directory_all$LEVEL==4] <- "Middle"

# Write data.
write.xlsx(directory_all, file="Output Data\\CCD cleaned\\CCD Directory 2015-2019.xlsx", sheetName="CCD Directory", row.names=FALSE)



#- School Characteristics files (1415 - 1819).
# Create empty data frame to append data.
schchar_all <- data.frame()

# Loop for each file.
for (y in master[str_detect(master,"ccd_sch_129")]) {
  # Detect file name based on whether file(y) is ZIP or not.
  if(str_detect(y, ".zip")){
    folder_zip <- as.character(unzip(paste0(pth, y), list = TRUE)$Name)
    df_name <- folder_zip[str_detect(folder_zip, "csv|txt")] #select only csv or txt
  }else{
    df_name <- y
  }
  # Read data based on df_name has CSV or TXT. 
  if(str_detect(df_name, "csv")){ #csv files
    # if file(y) is ZIP or not.
    if(str_detect(y, ".zip")){
      data0 <- read.csv(unz(paste0(pth, y), df_name), header = TRUE, sep = ",")
    }else{
      data0 <- read.csv(paste0(pth, y), header = TRUE, sep = ",")
    }
  }else{ #txt files
    # if file(y) is ZIP or not.
    if(str_detect(y, ".zip")){
      data0 <- read.delim(unz(paste0(pth, y), df_name))
    }else{
      data0 <- read.delim(paste0(pth, y))
    }
  }
  
  # rename variables that are different across the years.
  if("STABR" %in% names(data0)){
    df_ccd <- data0 %>% 
      rename(ST=STABR, Year=SURVYEAR)
  }else{
    df_ccd <- data0 %>% 
      rename(Year=SCHOOL_YEAR)
  }
  
  # Filter MA. 
  df_ccd <- df_ccd %>% 
    filter(ST=="MA")
  # Clean state school codes (then, these are the same as DESE codes).
  df_ccd$School.Code <- substr(df_ccd$ST_SCHID, nchar(df_ccd$ST_SCHID)-7, nchar(df_ccd$ST_SCHID))
  # Clean variable names before selecting.
  names(df_ccd) <- str_replace(names(df_ccd), "_STATUS", "")
  names(df_ccd) <- str_replace(names(df_ccd), "STATUS", "")
  # Select variables.
  df_ccd <- df_ccd %>% 
    select("Year","School.Code","SCH_NAME","NSLP_TEXT","TITLEI_TEXT") %>% 
    arrange(School.Code)
  
  # Append data
  schchar_all <- rbind(schchar_all, df_ccd)
}
# Rename factor values.
schchar_all$TITLEI_TEXT[schchar_all$TITLEI_TEXT %in% c("Missing","MISSING","Not reported")] <- NA
schchar_all$NSLP_TEXT[schchar_all$NSLP_TEXT %in% c("Missing","MISSING","Not reported")] <- NA
schchar_all$NSLP_TEXT <- str_replace(schchar_all$NSLP_TEXT, ",", "")

# Write data.
write.xlsx(schchar_all, file="Output Data\\CCD cleaned\\CCD School Characteristics 2015-2019.xlsx", sheetName="CCD School Characteristics", row.names=FALSE)

