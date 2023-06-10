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
NEE2022_CO2_env<- left_join(NEE_CO2data, NEE_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID", "Cover"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
    mutate(Hour = as.integer(substr(Starttime, 1,2)))

NEE2022_CH4_env<- left_join(NEE_CH4data, NEE_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID", "Cover"))%>%
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


####################### 2020-2021 NEEdata #################################

# NEE chamber  V = 2.5 L, A = 0.0625 m2, CH4 in ppb, C02 in ppm
NEE2020_CO2<- read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2020\\NEE2020_CO2_HMRoutput.csv", sep= ",")%>%
  rename(FluxID = X)%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))%>%
  mutate(PlotID = dplyr::recode(PlotID, "3WGA_OTC" = "3WGB_OTC"))

# load environmental metadata
metafiles_NEE2020 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2020\\LiCOR850\\", 
                     pattern = "^NEEmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to combine metadata files into one dataframe
NEE_envdata2020 <- map_df(set_names(metafiles_NEE2020), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Transect = as.character(Transect),
                    Date = as.Date(Date, "%d.%m.%Y")))
}, .id = "File")

# combine SR2021data with environmental data
NEE2020_CO2_env<- left_join(NEE2020_CO2, NEE_envdata2020, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "Cover"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)%>%
  mutate(CO2flux = f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = LR.f0/(0.08205*(273.15+SoilTemp1)))

# also add one month of li7810 data?


########## 2021 
NEE2021_CO2<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2021\\HMRoutput_NEE2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )

NEE2021_CH4<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2021\\HMRoutput_NEE2021_CH4.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )

## read in metadata
metafiles_NEE2021 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2021\\Cfluxdata\\", 
                        pattern = "^NEEmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

NEE_envdata2021 <- map_df(set_names(metafiles_NEE2021), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Transect = as.character(Transect),
                    FluxID = as.character(FluxID),
                    SoilTemp2 = dplyr::recode(SoilTemp2, '100.6' = 10.6L)))#correct typo on data
}, .id = "File")


# link Environmental data and CO2fluxdata
NEE2021_CO2_env<- left_join(NEE2021_CO2, NEE_envdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)%>%
  mutate(CO2flux = f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = LR.f0/(0.08205*(273.15+SoilTemp1)))

NEE2021_CH4_env<- left_join(NEE2021_CH4, NEE_envdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)%>%
  mutate(CH4flux = f0/(0.08205*(273.15+SoilTemp1)),
         CH4flux.LR = LR.f0/(0.08205*(273.15+SoilTemp1)))



#combine all NEE data
# CO2
NEE2020_CO2_env_combi<- NEE2020_CO2_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2021_CO2_env_combi<- NEE2021_CO2_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2022_CO2_env_combi<- NEE2022_CO2_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE_CO2_all<- rbind(NEE2020_CO2_env_combi, NEE2021_CO2_env_combi, NEE2022_CO2_env_combi)

# CH4
#NEE2020_CH4_env_combi<- NEE2020_CH4_env%>%
#  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
#         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2021_CH4_env_combi<- NEE2021_CH4_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2022_CH4_env_combi<- NEE2022_CH4_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE_CH4_all<- rbind(NEE2021_CH4_env_combi, NEE2022_CH4_env_combi)




#select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0. LR.f0.se, LR.f0.p, PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

######### 2019 data
# MAYBE REDO 2019 FLUX PROCESSSING?

library(readxl)
#import metadata
NEE_envdata2019 <- read_xlsx("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2019NEEdata\\Chamber_fluxes_metaIA.xlsx")
NEE_envdata2019 <- NEE_envdata2019 %>%
  mutate(PlotID = paste(PlotID, Treatment, sep = "_"))%>%
  filter(!NOTES == "delete" | is.na(NOTES)) # remove plots that were dropped

# import all flux data
# C in ppm = mg/L, V in m3, A in m2, t in s; micromol/mol to micromol/m3 is *1000
NEE2019_CO2 <- read_csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2019NEEdata\\HMRflux_PARcorrection.csv")%>%
  ungroup()%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, LR.f0, LR.f0.se, LR.f0.p, PAR.match)
  

# bind meta and flux data
# recalculate fluxes 
#It is critical that chamber volume (V) and area (A) were entered in L and m2, respectively, in the HMR csv file, prior to HMR analysis. If so, then you should just need this bit of code.  
# recalculate fluxes from uL/L to umol/m2
## essentially V = nRT / p, where n=1 and p=1, so that V=RT

NEE2019_CO2_env <- right_join(NEE2019_CO2, NEE_envdata2019 , by= c("Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  drop_na(f0)
# still some duplicates

  distinct(f0, .keep_all = TRUE) # remove duplicated rows


######### Combine 2019-2022 data


# bind together 2020 and 2021 NEE CO2 data
NEE20202021_CO2_env<- rbind(NEE2020_CO2_env, NEE2021_CO2_env)%>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date))%>%
  mutate(Habitat = dplyr::recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
NEE20202021_CO2_env$Habitat <- factor(NEE20202021_CO2_env$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))


##############

## Add airtemp based on TOMSTloggerData for measurement hour
#NEE2020_CO2_env<-left_join(NEE2020_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# NEED TO CORRECT FLUXES WITH IBUTTON TEMPERATURE TOMSTDATA not available for all dates !!!
#NEELi7810_notHMR<-read.csv("2020\\LiCOR7810\\NEEflux_2020_Li7810.csv")%>%
#  dplyr::select(Date, Transect, Habitat, Treatment, PlotID, Cover, Airtemp)%>%
#  mutate(Date = as.Date(Date, "%d.%m.%Y"))

#NEELi850_notHMR<-read.csv("2020\\LiCOR850\\NEEflux_2020_Li850.csv")%>%
#  dplyr::select(Date, Transect, Habitat, Treatment, PlotID, Cover, Airtemp)%>%
#  mutate(Date = as.Date(Date, "%d.%m.%Y"))

#NEE_ibuttonTemp<- rbind(NEELi7810_notHMR, NEELi850_notHMR)%>%
#  dplyr::rename(ChamberAirtemp= Airtemp)%>%
#  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))

# match chamber airtemp to NEE data
#NEE2020_CO2_env<- left_join(NEE2020_CO2_env, NEE_ibuttonTemp, by= c("Date", "Transect", "Habitat", "Treatment", "PlotID", "Cover"))%>%
#  distinct(FluxID, .keep_all = TRUE)

# match EC tower airtemp with fluxes
#ECtower<-read.csv("Climate\\Mobileflux1_level1_30min.csv")%>%
#  mutate(Date = as.Date(index, "%Y-%m-%d"),
#         Hour = as.integer(substr(index, 12,13)))%>%
#  group_by(Date, Hour)%>%
#  summarise(ECairtemp = mean(air_temperature))%>%
# dplyr::select(Date, Hour, ECairtemp)

#NEE2020_CO2_env<- left_join(NEE2020_CO2_env, ECtower, by= c("Date", "Hour"))
# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
# for now using soilTemp1 but better with either chamber ibutton data/ TOMST logger data or EC airtemp data
NEE20202021_CO2_env<-NEE20202021_CO2_env%>%
  mutate(CO2flux = f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = LR/(0.08205*(273.15+SoilTemp1)))

NEE2021_CH4_env<- NEE2021_CH4_env%>%
  mutate(CH4flux = f0/(0.08205*(273.15+SoilTemp1)),
         CH4flux.LR = LR/(0.08205*(273.15+SoilTemp1)))

NEE_CH4_all%>%
  mutate(Treatment = ifelse(Habitat == "R", "R", Treatment))%>%
  mutate(Treatment = ifelse(Habitat == "RZ", "R", Treatment))%>%
  mutate(Habitat = dplyr::recode(Habitat, "R" = "WG"),
         Habitat = dplyr::recode(Habitat, "RZ" = "WG"),
         Treatment = dplyr::recode(Treatment, "RZ" = "R"))%>%
  filter(Habitat =="WG")%>%
  mutate(Year = year(Date),
         Month = month(Date))%>%
  filter(f0>0)%>%
  filter(f0< 400)%>%
  filter(Month != "3")%>%
  ggplot(aes(as.factor(Month), f0, col=Treatment))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge())
