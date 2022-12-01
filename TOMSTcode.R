# TOMST logger processing
# load TOMSTloggerID information
library(tidyverse)
library(lubridate)

TomstID<-read.csv2("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\TOMST\\TOMSTloggerID.csv")%>%
  select(-X)

### Read in tomst logger files
files <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\TOMST\\TOMSTdata_04102022", 
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
  mutate(Date_Time = ymd_hm(Date_Time)) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(
    LoggerID = gsub(".*data_([^_]+)[_].*", "\\1", File), 
    LoggerID = as.integer(LoggerID)) 

# bind data to tomstID location
TomstLoggerData<- left_join(TomstID, data, by= "LoggerID")%>%
  mutate(Date = as.Date(Date_Time),
         LoggerID = as.factor(LoggerID))%>%
  filter(Date > "2020-06-25")%>% # remove all data from before installation of loggers in field
  filter(SoilTemperature > -30)%>% # filter 1 bad measurement from 4S_C logger
  filter(SoilTemperature < 80) # filter out bad measurements from 3WG_C logger


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

TomstLoggerData2<-TomstLoggerData%>%
  mutate(Soilmoisture = soilmoist_correct(RawSoilmoisture, SoilTemperature, "peat"))


# Save clean file
#write_csv(TomstLoggerData, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\TOMST\\TOMSTdata_processed.csv")





# check data 
TomstLoggerData %>% 
  filter(Treatment %in% c("C", "OTC"))%>%
  ggplot(aes(x = Date_Time, y = SoilTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstLoggerData %>%
  filter(Treatment %in% c("C", "OTC"))%>%
  ggplot(aes(x = Date_Time, y = GroundTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstLoggerData %>% 
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Date == "2022-03-31")%>%
  ggplot(aes(x = Date_Time, y = AirTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()


### calculate mean temp for habitat and treatments
TomstLoggerData_mean<-TomstLoggerData%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  gather(Climate_variable, value, SoilTemperature:RawSoilmoisture)%>%
  group_by(Habitat, Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))

ggplot(TomstLoggerData_mean, aes(Date, Mean, col= Treatment))+
  geom_line()+
  facet_grid(Climate_variable~Habitat, scales="free")



#######
#link TOMSTlogger data to flux data winter 2022
Cflux_metadata<-read.csv2("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2022\\Cfluxdata\\NEEmetadata_31032022.csv")%>%
  mutate(Time = as.integer(substr(Starttime,1,2)))%>%
  mutate(Habitat_TOMST = recode(Habitat, "W" = "WG" ))

Tomstdata_Cflux<- TomstLoggerData%>%
  filter(Date == "2022-03-31")%>%
  mutate(Time = hour(Date_Time))%>%
  group_by(Habitat, Treatment, Time)%>%
  summarise(AirTemperature = mean(AirTemperature))%>%
  ungroup()

Cflux_metadata<- left_join(Cflux_metadata, Tomstdata_Cflux, by= c("Habitat_TOMST"= "Habitat", "Treatment", "Time"))

write.csv(Cflux_metadata, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2022\\Cfluxdata\\NEEtempdataTOMST_31032022.csv")


######################################################################################################################################
### up to 12092021
### Read in tomst logger files
files <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\TOMST\\tomst15092021", 
             pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
temp1 <- map_df(set_names(files), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
}, .id = "File")

files2 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\TOMST\\tomst21092021", 
              pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

temp2 <- map_df(set_names(files2), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read_csv2(file = file, col_names = FALSE))
}, .id = "File")
temp2<- temp2%>%
  select(-X10)

data <- rbind(temp, temp2)

#All measurement runs in UTC, please use time zone parameter for recalculation to the local time. There is shake sensor placed on the device

data <- data %>% 
  # rename column names
  rename("ID" = "X1", "Date_Time" = "X2", "Time_zone" = "X3", "SoilTemperature" = "X4", "GroundTemperature" = "X5", "AirTemperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>% 
  mutate(Date_Time = ymd_hm(Date_Time)) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(
    LoggerID = substr(File, nchar(File)-13, nchar(File)-6),
    LoggerID = as.double(LoggerID)) 

# bind data to tomstID location
TomstLoggerData<- left_join(TomstID, data, by= "LoggerID")%>%
  mutate(Date = as.Date(Date_Time),
         LoggerID = as.factor(LoggerID))%>%
  filter(Date > "2020-06-25")%>%
  filter(SoilTemperature > -30) # filter 1 bad measurement from 4S_C logger



TomstLoggerData %>% 
  ggplot(aes(x = Date_Time, y = Soilmoisture, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstLoggerData %>% 
  ggplot(aes(x = Date_Time, y = SoilTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstLoggerData %>% 
  ggplot(aes(x = Date_Time, y = GroundTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

TomstLoggerData %>% 
  ggplot(aes(x = Date_Time, y = AirTemperature, colour = as.factor(Habitat))) +
  geom_line() +
  facet_grid(Transect~ Treatment, scales = "free") +
  theme_classic()

root_IGC<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\TOMST\\TOMSTdata_processed.csv")

root_IGC<-root_IGC%>%
  filter(PlotID %in% c("4P_OTC", "4P", "4M_OTC", "4M", "3P_OTC", "3P", "3M_OTC", "3M", "BP_OTC", "BP", "BM_OTC", "BM"))
  mutate(LoggerID = as.factor(LoggerID))

write.csv(root_IGC, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\TOMST\\root_IGC_TOMST_climatedata.csv")
