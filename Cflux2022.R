# combine Iskoras 2022 flux data
library(tidyverse)
library(lubridate)

# function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

## TOMST data 
TomstData<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\TOMSTdata_SMcalculated.csv")
TomstData<-TomstData%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  select(PlotID:LoggerID, Date, Date_Time, SoilTemperature:RawSoilmoisture, Soilmoisture_Volumetric)%>%
  mutate(Date = as.Date(Date),
         DateTime = as.POSIXct(strptime(Date_Time, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")),
         Hour = hour(DateTime))

# hourly climate data per plot
TomstData_HourlyPlotID<- TomstData%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date, Hour)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE), 
            GroundTemperature = mean(GroundTemperature, na.rm = TRUE),
            AirTemperature = mean(AirTemperature, na.rm = TRUE),
            Soilmoisture_Volumetric = mean(Soilmoisture_Volumetric, na.rm = TRUE))%>%
  ungroup()


# read in Metadata NEE
### Read in HMR output files
metafiles_NEE <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
                    pattern = "^NEEmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to combine metadata files into one dataframe
NEE_envdata2022 <- map_df(set_names(metafiles_NEE), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Transect = as.character(Transect)))
}, .id = "File")

# read in Metadata SR
metafiles_SR <-dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
                   pattern = "^SRmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to combine metadata files into one dataframe
SR_envdata2022 <- map_df(set_names(metafiles_SR), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Transect = as.character(Transect)))
}, .id = "File")


#### NEE data 2022 
### Read in HMR output files
CO2files_NEE <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
             pattern = "^HMR - HMRinput_NEE.*\\CO2.csv$", full.names = TRUE, recursive = TRUE)

CH4files_NEE <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
                pattern = "^HMR - HMRinput_NEE.*\\CH4.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
NEE_CO2data <- map_df(set_names(CO2files_NEE), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

NEE_CO2data<- NEE_CO2data%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "FluxID", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3),
         FluxID = as.integer(FluxID))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)

NEE_CH4data <- map_df(set_names(CH4files_NEE), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

NEE_CH4data<- NEE_CH4data%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "FluxID", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3),
         FluxID = as.integer(FluxID))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)


# combine NEE2022data with environmental data
NEE2022_CO2_env<- left_join(NEE_CO2data, NEE_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
    mutate(Hour = as.integer(substr(Starttime, 1,2)))

NEE2022_CH4_env<- left_join(NEE_CH4data, NEE_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
NEE2022_CO2_env_T<-left_join(NEE2022_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))
NEE2022_CH4_env_T<-left_join(NEE2022_CH4_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
NEE2022_CO2_env_T<- NEE2022_CO2_env_T%>%
  mutate(CO2flux = f0/(0.08205*(273.15+AirTemperature)),
         CO2flux.LR = LR.f0/(0.08205*(273.15+AirTemperature)))

NEE2022_CH4_env_T<- NEE2022_CH4_env_T%>%
  mutate(CH4flux = f0/(0.08205*(273.15+AirTemperature)),
         CH4flux.LR = LR.f0/(0.08205*(273.15+AirTemperature)))

##############################################################################################################################################
##### SR data 2022
CO2files_SR <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
                    pattern = "^HMR - HMRinput_SR.*\\CO2.csv$", full.names = TRUE, recursive = TRUE)

CH4files_SR <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
                    pattern = "^HMR - HMRinput_SR.*\\CH4.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
SR_CO2data <- map_df(set_names(CO2files_SR), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

SR_CO2data<- SR_CO2data%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "FluxID", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3),
         FluxID = as.integer(FluxID))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)

SR_CH4data <- map_df(set_names(CH4files_SR), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

SR_CH4data<- SR_CH4data%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "FluxID", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3),
         FluxID = as.integer(FluxID))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)

# combine NEE2022data with environmental data
SR2022_CO2_env<- left_join(SR_CO2data, SR_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

SR2022_CH4_env<- left_join(SR_CH4data, SR_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
SR2022_CO2_env_T<-left_join(SR2022_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))
SR2022_CH4_env_T<-left_join(SR2022_CH4_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
SR2022_CO2_env_T<- SR2022_CO2_env_T%>%
  mutate(CO2flux = f0/(0.08205*(273.15+AirTemperature)),
         CO2flux.LR = LR.f0/(0.08205*(273.15+AirTemperature)))

SR2022_CH4_env_T<- SR2022_CH4_env_T%>%
  mutate(CH4flux = f0/(0.08205*(273.15+AirTemperature)),
         CH4flux.LR = LR.f0/(0.08205*(273.15+AirTemperature)))




# basic plots
ggplot(NEE_CO2data, aes(Habitat, f0, fill = Treatment))+
  geom_boxplot()+
  facet_wrap(~Habitat)

WG_CH4data<- NEE_CH4data%>%
  filter(Habitat =="WG")%>%
  filter(Method != "No flux")%>%
  filter(f0<800)%>%
  filter(f0>0)

WG_CH4data%>%
  ggplot(aes(Treatment, f0, fill = Treatment))+
  geom_boxplot()+
  facet_wrap(~Habitat,scales = "free")

# ANOVA test
library(car)
library(emmeans)
CH4.rank<- aov( rank(f0)~ factor(Treatment), data = WG_CH4data,
                contrasts = list(Treatment = 'contr.sum' ))
Anova(CH4.rank, type = 'III')

res.CH4.rank = CH4.rank$resid
qqnorm(  res.CH4.rank, pch = 20, main = "Ranked CH4 Data",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.CH4.rank)
plot(CH4.rank, 1, main = "Ranked CH4 Data")
