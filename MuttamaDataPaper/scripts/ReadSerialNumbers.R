setwd("S:/PRJ-MuttamaCreek/MuttamaGW/logger data-Muttama")

require(tidyverse)
require(lubridate)

# Set the datadirectory and read in the piezometer info
piezo_info <- read_csv("2016_PiezometerDetail.csv")[3:25,]
Site_names <- piezo_info$`Site Name`
piezo_names <- piezo_info$`Site No.`
filelist <- dir(pattern="_data")

# read logger SN, check type of logger
read_logger_SN <- function(file_in) {
  TROLL = F
  # check for type of logger TROLL or in-Situ
  FileLines <- readLines(file_in)
  if (grepl("Report Date:",FileLines[1]) == T) TROLL = T
# find the lines with the serial number and date
  if (TROLL==T) {
#    browser()
    SN <- as.numeric(substr(FileLines[16],15,21))
    Date <- dmy(substr(FileLines[1],14,23))
  } else {
    SN <- as.numeric(substr(FileLines[2],1,7))
    test <- suppressWarnings(as.numeric(substr(FileLines[length(FileLines)],1,4)))
    if (is.na(test) == T) {
      Date <- dmy(substr(FileLines[length(FileLines)],1,10))
    } else {
      Date <- ymd(substr(FileLines[length(FileLines)],1,10))
    }
  }
  
  return(list(SN = SN,Date = Date))
}



# run a loop through the dates and the sites
all_data <- list()
#rm(store_data)

for (p in 1:length(piezo_names)) {
  for (i in 1:length(filelist)) {
    # find the right file to read in
    piezo_files <- dir(path = filelist[i],pattern=".csv")
    piezo_files <- piezo_files[grep("baro", piezo_files,
                                    invert =T, ignore.case=T)]
    # find the file name
    file_name <- piezo_files[grep(piezo_names[p],piezo_files)]
    if (length(file_name)>0) {
      all_data[[(p-1)*length(filelist) + i]] <- 
        bind_cols(read_logger_SN(paste(filelist[i],
                                       file_name, sep="/")))
      all_data[[(p-1)*length(filelist) + i]]$Piezo <- piezo_names[p]
    }  
  }
}

all_data_c <- bind_rows(all_data)

windows()
all_data_c %>%
  ggplot(aes(Date,SN)) + geom_point(size=2, colour = "red") +
  facet_wrap(~Piezo, scales="free")

write_csv(all_data_c, "SerialNumbers.csv")
