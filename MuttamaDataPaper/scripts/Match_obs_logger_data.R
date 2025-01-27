#setwd("Y:/MuttamaGW/logger data-Muttama")

require(tidyverse)
require(lubridate)

# Set the datadirectory and read in the piezometer info
piezo_info <- read_csv("../loggerdata-Muttama/2016_PiezometerDetail.csv")[3:25,]
Site_names <- piezo_info$`Site Name`
piezo_names <- piezo_info$`Site No.`

# Data corrector using serial numbers
# simple regression or substitution based on observed data matching

# read in the serial numbers in time
serials <- read_csv("../loggerdata-Muttama/SerialNumbers.csv")
# read in  logger data, this file does not have the observed data
all_data <- read_csv("../loggerdata-Muttama/All_dailyPiezoPlot_wObs.csv")

# split observed and loggers
logger_data <- all_data %>%
  filter(type == "Logger")

obs_data <- all_data %>%
  filter(type == "Observed")


# expand the dates in the serials dataset
store <- list()

for (i in unique(serials$Piezo)) {
  foo <- serials %>%
    filter(Piezo == i)
  bar <- logger_data %>%
    filter(Piezo == i)
  dates <- tibble(Date = seq.Date(min(bar$Date),max(bar$Date),by=1))
  store[[i]] <- left_join(dates,foo,by = "Date") %>%
    fill(Piezo, .direction = "downup") %>%
    fill(SN, .direction = "downup") 
}
serials_new <- bind_rows(store) 
#rm(match_data)
# match the serials with the piezo data
match_data <- left_join(logger_data,serials_new, by = c("Date", "Piezo"))

# expand the dates in the observed data
store <- list()

for (i in unique(obs_data$Piezo)) {
  foo <- obs_data %>%
    filter(Piezo == i)
  bar <- logger_data %>%
    filter(Piezo == i)
  dates <- tibble(Date = seq.Date(min(foo$Date),max(bar$Date),by=1))
  store[[i]] <- left_join(dates,foo,by = "Date")  %>%
    fill(Piezo, .direction = "downup")
}
obsdata_new <- bind_rows(store) %>%
  rename(`Obs Depth (m)` = `Depth (m)`) %>%
  select(-Site)


# merge with matched data
match_data <- full_join(obsdata_new, match_data, by = c("Date", "Piezo") )

piezo_store <- list()
store <- list()
mod_store <- list()
piezo_mod_store <- list()

# split data by piezo and serial number
# regress observed against logger and correct loggers
for (i in unique(match_data$Piezo)) {
  # split by piezo
  foo <- match_data %>%
    filter(Piezo == i) %>%
    # fill Serial number to all observed dates
    fill(SN, .direction = "downup") 
# run through all different serial number
    for (j in 1:length(unique(foo$SN))) {
      name <- unique(foo$SN)[j]
      
      # split by serial number
      bar <- foo %>%
        filter(SN == name) 
      # interpolate the depth values using na.approx() to account for the fact that no logger data exists when observed
      bar <- bar %>%
        mutate(Int_depth = `Depth (m)`) %>%
        fill(Int_depth,.direction = "downup")
    # if there are sufficient observed points (assumed to be > 3)  
      if (nrow(na.omit(bar %>% select(`Obs Depth (m)`))) > 3) {
        # run regression between interpolated depth and observed depth
        lm_mod <- lm(`Obs Depth (m)` ~ Int_depth, data = bar)
        # check p-values
        coef_data <- summary(lm_mod)$coefficients
        # if the slope is not significant (using p > 0.10)
        # but also use this if the regression cannot be fitted (due to only 0 observations)
        if (coef_data[2,4] > 0.10 & is.na(coef_data[2,4])==F) {
          equation <- coef_data[1,1] + bar$Int_depth
        } else {
          if (coef_data[1,4] > 0.10 & is.na(coef_data[1,4])==F) {
            # intercept not significant
            equation <- coef_data[2,1]*bar$Int_depth 
          } else {
            equation <- coef_data[1,1] + 
                        coef_data[2,1]*bar$Int_depth 
          }
        }
        # store regression summary
        mod_store[[j]] <- summary(lm_mod)
        # predict corrected depth values and store
        store[[j]] <- bar %>%
          mutate(`Corrected Depth (m)` = equation) %>%
          filter(type.y == "Logger") 
      } else {
        # not enough values for a regression, use mean difference between logged and observed value to correct
        if (length(na.omit(bar$`Obs Depth (m)`)) > 0 & length(na.omit(bar$`Obs Depth (m)`)) < 3 ) {
          store[[j]] <- bar %>%
            mutate(`Corrected Depth (m)` = Int_depth - 
                     mean(Int_depth - `Obs Depth (m)`,
                                             na.rm=T)) %>%
            filter(type.y == "Logger") 
        } else {
          # no observed data, no correction possible 
          store[[j]] <- bar %>%
            mutate(`Corrected Depth (m)` = `Depth (m)`) %>%
            filter(type.y == "Logger") 
        }
      }
    } # close for loop across Serial numbers
  # } else {
  # # corrected depth = measured depth
  #   store[[j]] <- foo %>%
  #     mutate(`Corrected Depth (m)` = `Depth (m)`) %>%
  #     filter(type.x == "Logger") 
  # }  # CLOSE IF ONLY 1 SN
  # unlist by piezo
  piezo_store[[i]] <- bind_rows(store)
  # store regression results
  piezo_mod_store[[i]] <- mod_store
}

  
# unlist the piezo's
all_data_corrected <- bind_rows(piezo_store) #%>% select(-type.y)
names(all_data_corrected)
nrow(all_data_corrected %>% filter(type.x == "Observed"))
all_data_corrected <- bind_rows(all_data_corrected,obsdata_new %>% filter(type == "Observed"))


# show a plot of the corrected data with the observed
windows()
all_data_corrected %>%
  pivot_longer(cols=c(`Corrected Depth (m)`, `Depth (m)`, `Obs Depth (m)`), 
               names_to = "SWL_type", values_to = "SWL (m)") %>% 
  #mutate(alpha_value = ifelse(SWL_type == `Obs Depth (m)`,1,0.2)) %>%
#  filter(Piezo == "GW03") %>%
  ggplot(aes(Date,`SWL (m)`, colour = SWL_type)) + geom_point() + facet_wrap(~Piezo, scales = "free") 

png("../Corrected_piezodepths.png", width = 960, height = 480)
all_data_corrected %>%
  mutate(`Observed Depth (m)` = `Obs Depth (m)`) %>%
  pivot_longer(cols=c(`Corrected Depth (m)`, `Depth (m)`, `Obs Depth (m)`), 
               names_to = "SWL_type", values_to = "SWL (m)") %>% 
  mutate(alpha_value = ifelse(SWL_type == "Obs Depth (m)",1,0.2)) %>%
  #  filter(Piezo == "GW03") %>%
  ggplot(aes(Date,`SWL (m)`, colour = SWL_type, alpha = alpha_value)) + 
  geom_point() + 
  #geom_point(aes(Date, `Observed Depth (m)`)) +
  facet_wrap(~Piezo) + theme_bw() + 
  #scale_linetype_manual(values=c("Corrected Depth (m)" = 1,
  #                               "Depth (m)" = 1 ,
  #                               "Obs Depth (m)" = 0)) #+
  #scale_shape_manual(values=c("Corrected Depth (m)" = "NA",
  #                             "Depth (m)" = "NA" ,
  #                             "Obs Depth (m)" = 1)) +
 scale_color_manual(values=c("Corrected Depth (m)" = "red",
                             "Depth (m)" = "blue" ,
                             "Obs Depth (m)" = "green")) 
dev.off()

write_csv(all_data_corrected,"../loggerdata-Muttama/CorrectedPiezoObservations.csv")


