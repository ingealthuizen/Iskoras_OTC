# TOMST logger processing
# load TOMSTloggerID information
library(tidyverse)
library(lubridate)
#install.packages(myClim)

#library(here)
setwd("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\")

TomstID<-read.csv2("Climate\\TOMST\\TOMSTloggerID2023.csv")

### Read in tomst logger files
files <- dir(path = "Climate\\TOMST\\TOMST2023", 
             pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
temp <- map_df(set_names(files), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = FALSE, sep = ";", dec = ","))
}, .id = "File")


data <- temp %>% 
  # rename column names
  rename("ID" = "V1", "Date_Time" = "V2", "Time_zone" = "V3", "SoilTemperature" = "V4", "GroundTemperature" = "V5", "AirTemperature" = "V6", "RawSoilmoisture" = "V7", "Shake" = "V8", "ErrorFlag" = "V9") %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, format="%Y.%m.%d %H:%M"),
         Date = as.Date(Date_Time)) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(
    LoggerID = gsub(".*data_([^_]+)[_].*", "\\1", File), 
    LoggerID = as.integer(LoggerID)) 

# bind data to tomstID location
TomstLoggerData<- left_join(TomstID, data, by= "LoggerID")%>%
  mutate( LoggerID = as.factor(LoggerID))%>%
  filter(Date > "2020-06-25") # remove all data from before installation of loggers in field
  
### SOilmoisture correction
#based on appendix A of Wild 2019 (https://www-sciencedirect-com.pva.uib.no/science/article/pii/S0168192318304118#sec0095) and https://www.tomst.com/tms/tacr/TMS3calibr1-11.xlsm

library(tidyverse)
library(lubridate)

soilmoist_correct <- function(rawsoilmoist, soil_temp, soilclass){
  # creating df with parameters for each soil type
  soilclass.df <- tibble(
    soil = c("sand", "loamy_sand_A", "loamy_sand_B", "sandy_loam_A", "sandy_loam_B", "loam", "silt_loam", "peat"),
    a = c(-3E-9, -1.9e-8, -2.3e-8, -3.8e-8, -9e-10, -5.1e-8, 1.7e-8, 1.23e-7),
    b = c(1.61192e-4, 2.6561e-4, 2.82473e-4, 3.39449e-4, 2.61847e-4, 3.97984e-4, 1.18119e-4, 1.44644e-4),
    c = c(-0.109956505, -0.154089291, -0.167211156, -0.214921782, -0.158618303, 0.291046437, -0.101168511, 0.202927906),
    AirCalib = rep(57.64530756, 8), # a constant across all soil types, don't know exactly what this does
    AirPuls = rep(56.88867311, 8), # a constant across all soil types, don't know exactly what this does
    DilVol = rep(-59.72975311, 8) # a constant across all soil types, don't know exactly what this does
  )
  
  #filtering soilclass.df based on which soilclass was entered in the function
  soilclass.df <- soilclass.df %>%
    filter(soil == soilclass)
  
  #calculating the volumetric soil moisture with the parameters corresponding to the soil class and the raw soil moisture from the logger
  volmoist = (soilclass.df$a * rawsoilmoist^2) + (soilclass.df$b * rawsoilmoist) + soilclass.df$c
  
  #temperature correction
  temp_ref <- 24
  delta_air <- 1.91132689118083
  delta_water <- 0.64108
  delta_dil <- -1.270246891 # this is delta-water - delta_air
  # we don't know what this does or what the variables do, but the result is the same as in excel
  temp_corr <- rawsoilmoist + ((temp_ref-soil_temp) * (delta_air + delta_dil * volmoist))
  # volumetric soil moisture with temperatue correction
  volmoistcorr <- with(soilclass.df,
                       ifelse(rawsoilmoist>AirCalib,
                              (temp_corr+AirPuls+DilVol*volmoist)^2*a+(temp_corr+AirPuls+DilVol*volmoist)*b+c,
                              NA))
  return(volmoistcorr)
}

TomstData_SoilMoistureCorrect<-TomstLoggerData%>%
  select(-V10)%>%
  mutate(Soilmoisture_Volumetric = soilmoist_correct(RawSoilmoisture, SoilTemperature, "peat"))

# check data 
TomstData_SoilMoistureCorrect %>% 
  filter(Treatment %in% c("C", "OTC"))%>%
  ggplot(aes(x = Date_Time, y = Soilmoisture_Volumetric, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstData_SoilMoistureCorrect %>% 
  filter(Treatment %in% c("C", "OTC"))%>%
  ggplot(aes(x = Date_Time, y = SoilTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstData_SoilMoistureCorrect %>%
  filter(Treatment %in% c("C", "OTC"))%>%
  ggplot(aes(x = Date_Time, y = GroundTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstData_SoilMoistureCorrect %>% 
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Date == "2022-03-31")%>%
  ggplot(aes(x = Date_Time, y = AirTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()


####################################################################################################################################

#Data Cleaning
TomstData_Clean<-TomstData_SoilMoistureCorrect%>%
  filter(SoilTemperature > -30)%>% # filter bad measurements from 3WGB_C logger
  filter(SoilTemperature < 80) # filter out bad measurements from 3WGB_C logger

# summary
summer_microclimate2020<-TomstData_Clean%>%
  filter(Treatment != "DEEP")%>%
  filter(PlotID != "2P_OTC_DISCONTINUED")%>%
  filter(Date > "2020-06-30" & Date < "2020-09-01")%>%
  select(PlotID, Habitat, Treatment, SoilTemperature, GroundTemperature, AirTemperature, Soilmoisture_Volumetric)%>%
  group_by(PlotID, Habitat, Treatment)%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)%>%
  gather(microclimate, value, SoilTemperature:Soilmoisture_Volumetric)

ggplot(summer_microclimate2020, aes(Habitat, value, fill= Treatment))+
  geom_boxplot()+
  facet_wrap(~microclimate, scales= "free")
  
summer_microclimate2021<-TomstData_Clean%>%
  filter(Treatment != "DEEP")%>%
  filter(PlotID != "2P_OTC_DISCONTINUED")%>%
  filter(Date > "2021-06-30" & Date < "2021-09-01" )%>%
  select(PlotID, Habitat, Treatment, SoilTemperature, GroundTemperature, AirTemperature, Soilmoisture_Volumetric)%>%
  group_by(PlotID, Habitat, Treatment)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>%
  gather(microclimate, value, SoilTemperature:Soilmoisture_Volumetric)

ggplot(summer_microclimate2021, aes(Habitat, value, fill= Treatment))+
  geom_boxplot()+
  facet_wrap(~microclimate, scales= "free")

winter_microclimate2020<-TomstData_Clean%>%
  filter(Treatment != "DEEP")%>%
  filter(PlotID != "2P_OTC_DISCONTINUED")%>%
  filter(Date > "2020-11-01" & Date < "2021-04-01" )%>%
  select(PlotID, Habitat, Treatment, SoilTemperature, GroundTemperature, AirTemperature, Soilmoisture_Volumetric)%>%
  group_by(PlotID, Habitat, Treatment)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>%
  gather(microclimate, value, SoilTemperature:Soilmoisture_Volumetric)

ggplot(winter_microclimate2020, aes(Habitat, value, fill= Treatment))+
  geom_boxplot()+
  facet_wrap(~microclimate, scales = "free")


# correct for high soilmoisture values? rescale saturated moisture to 1?

# Save file
#write_csv(TomstData_Clean, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\TOMSTdata_SMcalculated.csv")

