#setwd("S:/PRJ-MuttamaCreek/MuttamaGW/logger data-Muttama")

require(tidyverse)
require(lubridate)

# Set the datadirectory and read in the piezometer info
piezo_info <- read_csv("2016_PiezometerDetail.csv")[3:25,]
Site_names <- piezo_info$`Site Name`
piezo_names <- piezo_info$`Site No.`
filelist <- dir(pattern="_data")



# Functions
date_match <- function(date_in) {
  if (is.na(str_match(substr(date_in[1],1,4),"/"))) {
    date_out <- ymd_hms(date_in)
  } else date_out <- dmy_hms(date_in)
  return(date_out)
}

col_rename <- function(data_in) {
  L <- F
  # check column names
  if (any(grepl("WATER LEVEL",colnames(data_in)))==T) L <- T
  
  if(L == T) {
    data_out <- data_in %>% rename(`Depth (m)` = `WATER LEVEL`,
                          `Temperature (C)` = TEMPERATURE) 
  } else data_out <- data_in %>% rename(`Depth (m)` = LEVEL,
                               `Temperature (C)` = TEMPERATURE)

  return(data_out)
}


read_EC <- function(data_in, textlines = FileLines) {
  firstdata <- grep("Date,Time",textlines)
  Out <- read_csv(data_in, skip=firstdata-1)

  Out <- Out %>%
      mutate(Date = date_match(paste(Date,Time))) 
  Out <- col_rename(Out) %>% 
      select(Date,`Temperature (C)`, `Depth (m)`)
  
  if(mean(Out$`Depth (m)`) > 50) {
    Out <- Out %>%
      # correct for cm to m
      mutate(`Depth (m)` = `Depth (m)`/100)
  }
  return(Out)
}


read_TROLL <- function(data_in, textlines = FileLines) {
  firstdata <- grep("Seconds",textlines)
  Out <- read_csv(data_in, skip=firstdata-1)
  
  Out <- Out %>% 
      mutate(Date = dmy_hms(`Date and Time`)) %>%
      select(Date,`Temperature (C)`, `Depth (m)`)
  return(Out)  
}  

read_level <- function(data_in, textlines = FileLines) {
  firstdata <- grep("Date,Time",textlines)
  Out <- read_csv(data_in, skip=firstdata-1)

  Out <- Out %>%
    mutate(Date = date_match(paste(Date,Time))) 
  Out <- col_rename(Out) %>% 
    select(Date,`Temperature (C)`, `Depth (m)`)
  
  if(mean(Out$`Depth (m)`) > 50) {
    Out <- Out %>%
      # correct for cm to m
      mutate(`Depth (m)` = `Depth (m)`/100)
  }
  return(Out)
}


# read piezo, check type of logger
read_piezo <- function(file_in, plotit=T) {
  TROLL=F; EC=F
  # check for type of logger TROLL or in-Situ
  FileLines <- readLines(file_in)
  if (grepl("Report Date:",FileLines[1])==T) TROLL=T
  if (any(grepl("CONDUCTIVITY",FileLines[1:20]))==T) EC=T

  if (TROLL==T) {
    Out <- read_TROLL(data_in = file_in, textlines = FileLines)
  } else {
    if (EC == T) {
      Out <- read_EC(data_in = file_in, textlines = FileLines)
    } else {
      Out <- read_level(data_in = file_in, textlines = FileLines)
    }
  }
  
  if (plotit == T) {
    Out %>% ggplot(aes(Date,`Depth (m)`)) + geom_line()
  }
  return(Out)
}


# summarise to daily
piezo_daily <- function(data_in) {
  Output <- data_in %>%
    group_by(Date = date(Date)) %>%
    summarise(`Depth (m)` = mean(`Depth (m)`))
  return(Output)
}

# Function to Correct logger data for intercept and cable length
correct_depth <- function(data_in, piezo_info_in) {

  # # This has to be put in manually: off sets and depths until we have a data sheet complete
  cable <- piezo_info_in$`Logger cable (m)`
  int <- piezo_info_in$`Piezometer height above surface (m)`
  
  #correct the level data for intercept and cable length
  data_out <- data_in %>%
    mutate(`Depth (m)` = int - cable + `Depth (m)`)
  
}

# Function to Correct observed data for intercept
correct_obs <- function(obs_in, piezo_info_in) {
  
  # # This has to be put in manually: off sets and depths until we have a data sheet complete
  int <- piezo_info_in$`Piezometer height above surface (m)`
  
  #correct the level data for intercept and cable length
  obs_out <- obs_in %>%
    mutate(`Depth (m)` = `Depth (m)` - int)
  
}



# function to remove anomalies caused by sampling days
correct_sampling_anom <- function(data_in) {
  # remove any remaining sampling anomalies
  data_temp <- data_in %>%
    mutate(diff_depth = c(NA,diff(`Depth (m)`))) 
  
  sd_value <- sd(data_temp$diff_depth, na.rm= T)
  
  data_out <- data_temp %>% 
    filter(diff_depth < sd_value & diff_depth > -sd_value) %>%
    select(-diff_depth) %>%
    filter(Date > Date[2])
  return(data_out)
}

# function to expand dates to the full range of dates
# put in NA values for missing observations
expand_dates <- function(data_in) {
  #browser()
  # find first and last date, create a sequence
  min_date <- data_in$Date[which.min(data_in$Date)]
  max_date <- data_in$Date[which.max(data_in$Date)]
  dates <- tibble(Date = seq(min_date,max_date, by = 1))
  # left join
  data_temp <- left_join(dates,data_in, by = "Date")
  return(data_temp)
}



join_piezo <- function(data1,data2, gap = 5) {
  end_date <- data1$Date[nrow(data1)]
  data1 <- data1 %>%
     filter(Date < (end_date - gap))
  if (nrow(data2) >0) {
    data2 <- data2 %>%
      # gap is an integer indicating how much data you want to drop
      filter(Date > (end_date + gap))
      out_data <- bind_rows(data1,data2)
  } else out_data <- data1
  return(out_data)
 
}

# ----------------------
# run a loop through the dates and the sites
all_data <- list()
rm(store_data)

for (p in 1:length(piezo_names)) {
  for (i in 1:length(filelist)) {
    # read in the list of piezo_files in the dir
    piezo_files <- dir(path = filelist[i],pattern=".csv")
    piezo_files <- piezo_files[grep("baro", piezo_files,
                                    invert =T, ignore.case=T)]
    # find the file name
    file_name <- piezo_files[grep(piezo_names[p],piezo_files)]
    if (length(file_name)>0) {
      # read the file
      data_piezo <- read_piezo(paste(filelist[i],
                                     file_name, sep="/"), plotit=F)
      data_piezo_day <- piezo_daily(data_piezo)
      data_piezo_correct <- correct_depth(data_piezo_day,
                                          piezo_info_in = piezo_info[p,])
      # remove any remaining sampling anomalies
      data_piezo_correct <- correct_sampling_anom(data_piezo_correct)
      data_piezo_correct <- expand_dates(data_piezo_correct)
      # add site and piezo name
      data_piezo_correct <- data_piezo_correct %>%
        mutate(Site = Site_names[p], Piezo = piezo_names[p])
      # store with other data
      if (!exists("store_data")) {
        store_data <- data_piezo_correct
      } else {
        data_piezo_join <- join_piezo(store_data, 
                                      data_piezo_correct)
        store_data <- data_piezo_join
      } # end else
      print(file_name)
    }
  } # end i loop
  all_data[[p]] <- store_data
  rm(store_data)
} # end p loop  


plot_df <- bind_rows(all_data)

windows()
plot_df %>% ggplot(aes(Date, `Depth (m)`, colour = Piezo)) +
  geom_line() + facet_wrap(~Piezo) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45))

# temporary output
save(plot_df, file="MuttamaGW_daily_output.Rdata")
load(file="MuttamaGW_daily_output.Rdata")

# Compare to manual observed data
# read in the two observed data files, one until 2016, one after 2016
Observed <- read_csv("Manual_SWL_measurements.csv") %>%
  mutate(Date = dmy(Date))
Observed2 <- read_csv("2017muttamagw-MonitoringDataGW.csv") %>%
  mutate(Date = format(dmy_hm(`Datetime Start`),"%Y-%m-%d")) 

# remove the SW entries from Observed 2
Observed2 <- Observed2 %>%
  filter(grepl("SW",`Site ID`)==F) %>%
  #and remove NA values
  na.omit() %>%
  mutate(Date = ymd(Date))

# reorganise columns to match plot_df
Observed <- Observed %>%
  rename(Piezo = Well_ID,
         `Depth (m)` = `Depth from surface level (m)`) %>%
  mutate(type = "Observed",
         Site = NA) %>%
  mutate(`Depth (m)` = - `Depth (m)`) %>%
  select(Date,`Depth (m)`, Site, Piezo, type)

# correct for standpipe height
Observed_test <- list()
for (p in 1:nrow(piezo_info)) {
  temp <- Observed %>%
  filter(Piezo == piezo_info$`Site No.`[p])
  
  Observed_test[[p]] <- correct_obs(obs_in = temp, piezo_info_in = piezo_info[p,])
}

Observed_test <- bind_rows(Observed_test)

Observed2 <- Observed2 %>%
  mutate(Piezo = substr(`Site ID`,1,4),
         Site = substr(`Site ID`, 6, nchar(`Site ID`)),
         type = "Observed") %>%
  rename(`Depth (m)` = `Water Level Start`) %>%
  mutate(`Depth (m)` = - `Depth (m)`) %>%
  select(Date,`Depth (m)`, Site, Piezo, type)

Observed_test2 <- list()
for (p in 1:nrow(piezo_info)) {
  temp <- Observed2 %>%
    filter(Piezo == piezo_info$`Site No.`[p])
  
  Observed_test2[[p]] <- correct_obs(obs_in = temp, piezo_info_in = piezo_info[p,])
}

Observed_test2 <- bind_rows(Observed_test2)



# add "type" to plot.df
plot_df <- plot_df %>%
  mutate(type = "Logger")

# bind_rows
plot_df_wObs <- bind_rows(plot_df,Observed_test,Observed_test2)

# remove the Goulburn sites
plot_df_wObs <- plot_df_wObs %>%
  filter(grepl("Goulburn",Site)==F)


# make a new plot
windows()
plot_df_wObs %>% ggplot(aes(Date, `Depth (m)`, colour = type)) +
  geom_point() + facet_wrap(~Piezo) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45))


# write out daily data
write_csv(plot_df_wObs,"All_dailyPiezoPlot_wObs.csv")

# # Correct the anomalies due to changes in loggers etc
# # average logger data to weekly
# Logger_weekly <- plot_df %>%
#   group_by(Piezo,Year = year(Date), Week = week(Date)) %>%
#   summarise(Depth_week = mean(`Depth (m)`, na.rm = T),
#             Date_week = min(Date))
# 
# # do the same for the observed
# Observed_weekly <- bind_rows(Observed,Observed2) %>%
#   group_by(Piezo,Year = year(Date), Week = week(Date)) %>%
#   summarise(Depth_week = mean(`Depth (m)`, na.rm = T),
#             Date_week = Date)
# 
#   
# # arrange the data differently by join
# All_data <- left_join(Logger_weekly,Observed_weekly, by = c("Week","Piezo","Year"))

#All_data %>% mutate(diff_depth = Depth_week.x - Depth_week.y) %>% ggplot(aes(Date_week.x,diff_depth)) + geom_point() + facet_wrap(~Piezo)


