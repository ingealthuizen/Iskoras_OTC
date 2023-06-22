# combine Iskoras 2022 flux data
library(tidyverse)
library(lubridate)
library(readxl)

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

################################## NEEdata 2022 ############################################################################
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
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(PlotID =recode(PlotID, "2WG_OTC" = "2WGB_OTC")) 

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
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(PlotID =recode(PlotID, "2WG_OTC" = "2WGB_OTC")) 


# combine NEE2022data with environmental data
NEE2022_CO2_env<- left_join(NEE_CO2data, NEE_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID", "Cover"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
    mutate(Hour = as.integer(substr(Starttime, 1,2)))

NEE2022_CH4_env<- left_join(NEE_CH4data, NEE_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID", "Cover"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)))


####################### 2020 NEEdata #################################

# NEE chamber  V = 2.5 L, A = 0.0625 m2, CH4 in ppb, C02 in ppm
CO2files_NEE_2020 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2020", 
                    pattern = "^HMR - HMRinput.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
NEE_CO2data_2020 <- map_df(set_names(CO2files_NEE_2020), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

NEE2020_CO2<- NEE_CO2data_2020%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3),
         Date =ymd(Date))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)


# load environmental metadata
metafiles_NEE2020 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2020\\", 
                     pattern = "^NEEmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to combine metadata files into one dataframe
NEE_envdata2020 <- map_df(set_names(metafiles_NEE2020), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Transect = as.character(Transect),
                    Habitat = dplyr::recode(Habitat, WGA = "WG", WGB = "WG"), 
                    Date = as.Date(Date, "%d.%m.%Y")))
}, .id = "File")

# combine SR2021data with environmental data
NEE2020_CO2_env<- left_join(NEE2020_CO2, NEE_envdata2020, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "Cover"))%>%
  distinct(f0, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))

# count number of measurements
NEE2020_CO2_env%>%
  filter(Cover == "RECO")%>%
  filter(Method != "No flux")%>%
  count()

####################### 2021 NEEdata #################################

NEE2021_CO2<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2021\\HMR - HMRinput_NEE_2021_CO2.csv")

# data cleaning
# Only filtering for faulty Reco and NEE based on visual inspection
faulty_CO2_2021 <- c(
     "3S_C_RECO_20.07.2021_12",
     "3S_C_RECO_03.06.2021_15",
     "3S_C_RECO_03.06.2021_14",
     "3S_C_RECO_02.07.2021_25",
     "2WGB_C_RECO_21.07.2021_6",
     "2S_C_RECO_03.06.2021_46",
     "BM_C_RECO_23.07.2021_36",
     "AS_OTC_RECO_20.07.2021_14",
     "AS_OTC_RECO_17.08.2021_18",
     "AS_C_RECO_20.07.2021_15",
     "AS_C_RECO_03.06.2021_17",
     "AR_C_RECO_23.07.2021_26",
     "AR_C_RECO_12.09.2021_11",
     "4S_C_RECO_20.07.2021_21",
     "4S_C_RECO_03.06.2021_18",
     "4S_C_RECO_02.07.2021_17",
     "BP_C_RECO_23.07.2021_32",
     "BP_C_RECO_23.07.2021_31",
     "BWG_R_RECO_21.08.2021_34",
     "BS_C_RECO_23.07.2021_40",
     "BS_C_RECO_18.08.2021_24",
     "BS_C_RECO_11.09.2021_71",
     "BR_C_RECO_23.07.2021_34",
     "BR_C_RECO_12.09.2021_21",
     "BR_C_RECO_03.07.2021_50" , 
    # Bad NEE measurements 
     "BP_OTC_NEE_04.06.2021_11",    
     "4P_C_NEE1_03.06.2021_39",
     "4P_C_NEE2_03.06.2021_40",
     "1WG_C_NEE2_03.06.2021_52"
  )  

NEE2021_CO2<- NEE2021_CO2%>%
  filter(!Series %in% faulty_CO2_2021)%>%
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
             mutate(Habitat = recode(Habitat, "WGA" = "WG", "WGB"="WG"))%>%
             mutate(Transect = as.character(Transect),
                    FluxID = as.character(FluxID),
                    SoilTemp2 = dplyr::recode(SoilTemp2, '100.6' = 10.6L)))#correct typo on data
}, .id = "File")


# link Environmental data and CO2fluxdata
NEE2021_CO2_env<- left_join(NEE2021_CO2, NEE_envdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))


########### 2019 NEE data
### Read in HMR output files
CO2files_NEE_2019 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2019", 
                         pattern = "^HMR - HMRinput_.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
NEE_CO2data_2019 <- map_df(set_names(CO2files_NEE_2019), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

PlotID<- read.csv2("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2019NEEdata\\NEE_2019_IA.csv")%>%
  mutate(PlotID = paste(PlotID, "_", Treatment, sep = ""),
         Date = as.Date(Date, format="%d.%m.%Y"),
         Cover = recode(Cover, ER = "RECO"))%>%
  select(Series, PlotID, Transect, Habitat,  Treatment, Cover, Date )

NEE2019_CO2 <- left_join(NEE_CO2data_2019, PlotID, by= c("Series"))%>%
  distinct(f0, Series, .keep_all = TRUE)

NEE_envdata2019<-read_xlsx("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2019NEEdata\\Chamber_fluxes_metaIA.xlsx")%>%  
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(Date = as.Date(Date))%>%
  select(Date, hour, PlotID, Cover, PAR1, PAR2, PAR3, Soiltemp1, Soiltemp2, SoilMoist1, SoilMoist2, SoilMoist3, Airtemp, Redo)

NEE2019_CO2_env<- left_join(NEE2019_CO2, NEE_envdata2019, by= c("Date", "PlotID",  "Cover"))%>%
  distinct(f0, Series, .keep_all = TRUE)%>%
  dplyr::rename(SoilTemp1 = Soiltemp1, SoilTemp2= Soiltemp2, Hour = hour )



#NEE2019_CO2<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\2019\\HMR_NEEflux2019.csv")%>%
#  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
#  mutate(Cover = recode(Cover, ER = "RECO"))%>%
#  mutate(Date = as.Date(Date, "%d.%m.%Y"),
#         f0 = 10*f0,
#         LR.f0 = 10*f0)%>% # f0 calculated with Volume 2.5L instead of 25L, correcting here
#  select(Date, PlotID, Cover, Transect, Habitat, Treatment, f0:Method, Comment, LR.f0:LR.f0.up95)

# 2019 metadata
#NEE_envdata2019<-read_xlsx("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2019NEEdata\\Chamber_fluxes_metaIA.xlsx")%>%  
#  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
#  mutate(Cover = if_else(Series %in% c("a2", "d4"), 1, 2))
#  mutate(Date = as.Date(Date))%>%
#  select(Date, hour, PlotID, Cover, PAR1, PAR2, PAR3, Soiltemp1, Soiltemp2, SoilMoist1, SoilMoist2, SoilMoist3, Airtemp, Redo)

#NEE2019_CO2_env<- left_join(NEE2019_CO2, NEE_envdata2019, by= c("Date", "PlotID",  "Cover"))%>%
#  distinct(f0, .keep_all = TRUE)%>%
#  dplyr::rename(SoilTemp1 = Soiltemp1, SoilTemp2= Soiltemp2, Hour = hour )


#combine all NEE data
# CO2
NEE2019_CO2_env_combi<- NEE2019_CO2_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2020_CO2_env_combi<- NEE2020_CO2_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2021_CO2_env_combi<- NEE2021_CO2_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2022_CO2_env_combi<- NEE2022_CO2_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE_CO2_19_20_21_22<- rbind(NEE2019_CO2_env_combi, NEE2020_CO2_env_combi, NEE2021_CO2_env_combi, NEE2022_CO2_env_combi)


# calculate mean PAR, Soiltemp and SoilMoist of flux measurement based on point measurements accompanying flux measurements 
NEE_CO2_19_20_21_22_means<-NEE_CO2_19_20_21_22%>%
  group_by(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p)%>%
  gather(key = par, value = value, PAR1, PAR2, PAR3)%>%
  mutate(PAR_mean = mean(value, na.rm = TRUE))%>%
  gather(key = soilT, value = temp, SoilTemp1 , SoilTemp2)%>%
  mutate(SoilT_mean = mean(temp, na.rm = TRUE))%>%
  gather(key = soilM, value = moisture, SoilMoist1 , SoilMoist2, SoilMoist3)%>%
  mutate(SoilMoist_mean = mean(moisture, na.rm = TRUE))%>%
  distinct(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, .keep_all = TRUE)%>%
  select(-par, -value, -soilT, -temp, -soilM, -moisture)%>%
  ungroup()

# join NEEdata with TOMSTlogger data based on PlotID, date and hour of day
NEE_CO2_19_20_21_22_means_TOMST<-left_join(NEE_CO2_19_20_21_22_means, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))


# join NEEdata with ECtower data based on date and hour of day
#! Find reference for shortwave conversion to flux back
# read in ECtower data
#ref par calculation: https://www.sciencedirect.com/science/article/pii/0002157176900807

ECdata<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\Mobileflux1_level1_30min_forCasper_update2022.csv")%>%
  separate(index, into = c("Date", "Time"), sep = " " )%>%
  mutate(Hour = as.integer(substring(Time, 1, 2)),
         Date = as.Date(Date),
         PAR_EC = shortwave_incoming * 2.114)%>% # calculate PAR based on incoming shortwave radiation
  mutate(PAR_EC = ifelse(PAR_EC< 0, 0, PAR_EC))%>% # set PAR to zero if negative
  group_by(Date, Hour)%>%
  summarise_all(mean)%>%
  select(Date, Hour, PAR_EC, air_temperature, relative_humidity, air_pressure, shortwave_incoming)%>%
  ungroup()


NEE_CO2_19_20_21_22_means_TOMST_EC<- left_join(NEE_CO2_19_20_21_22_means_TOMST, ECdata, by= c("Date", "Hour"))%>%
  mutate(PAR_mean = ifelse(Cover == "RECO", 0, PAR_mean),
         PAR_ideal = ifelse(PAR_mean == "NaN", PAR_EC, PAR_mean))


### Recalculate PAR based on shading
NEE_CO2_19_20_21_22_means_TOMST_EC<- NEE_CO2_19_20_21_22_means_TOMST_EC%>%
  mutate(PAR_real = ifelse(Cover == "NEE1", PAR_ideal*0.67, 
                         ifelse(Cover == "NEE2", PAR_ideal*0.33, PAR_ideal)))

### fluxconversion based on airtemp 
# TOMST aitemp if possible, otherwise EC if available, otherwise soiltemp point measurements? 
NEE_CO2_19_20_21_22_means_TOMST_EC_new<- NEE_CO2_19_20_21_22_means_TOMST_EC%>%
  mutate(CO2flux = f0/(0.08205*(273.15+AirTemperature)),
         CO2flux.LR = LR.f0/(0.08205*(273.15+AirTemperature)),
         CO2flux_EC = f0/(0.08205*(273.15+air_temperature)),
         CO2flux.LR_EC = LR.f0/(0.08205*(273.15+air_temperature)))%>%
  mutate(CO2flux_final = ifelse(is.na(CO2flux) == TRUE, CO2flux_EC, CO2flux))%>%
  mutate(Cover = recode(Cover, Reco ="RECO"))

# count number of measurements
NEE_CO2_19_20_21_22_means_TOMST_EC_new%>%
  mutate(Date = ymd(Date),
         Year = year(Date))%>%
  #drop_na(CO2flux_final)%>%
  filter(Cover == "RECO")%>%
  filter(Method != "No flux")%>%
  filter(Habitat != "S")%>%
  filter(Treatment == "C")%>%
  group_by(Year)%>%
  count()

write.csv(NEE_CO2_19_20_21_22_means_TOMST_EC_new, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\Thawgradient\\NEE_2019-2022.csv")

#B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$B$ Calculate GPP B$B$B$B$B$B$B$B$B$B$
NEE_CO2_19_20_21_22_means_TOMST_EC_new<- read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\Thawgradient\\NEE_2019-2022.csv")

RECO_CO2 <- NEE_CO2_19_20_21_22_means_TOMST_EC_new%>%
  filter(Cover == "RECO")%>%
  filter(CO2flux_final>0)%>%
  select(X,Date, PlotID, Transect, Habitat, Treatment, RECOflux = CO2flux_final )

NEE_CO2 <- NEE_CO2_19_20_21_22_means_TOMST_EC_new%>%
  filter(Cover != "RECO")

GPP_CO2 <- left_join(NEE_CO2, RECO_CO2, by = c("Date", "PlotID", "Transect", "Habitat", "Treatment"))%>%
  distinct(X.x, .keep_all=TRUE)%>%
  rename( NEEflux = CO2flux_final )%>%
  mutate(GPPflux = (-1*NEEflux) + RECOflux) # GPP = NEE + RECO, multiple NEE by -1 to get positive numbers

write.csv(GPP_CO2, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\Thawgradient\\GPP_2019-2022.csv", row.names = FALSE)


#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
################### CH4

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

#### NEE data 2022 
CH4files_NEE <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
                    pattern = "^HMR - HMRinput_NEE.*\\CH4.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
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
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(PlotID =recode(PlotID, "2WG_OTC" = "2WGB_OTC")) 


# combine NEE2022data with environmental data
NEE2022_CH4_env<- left_join(NEE_CH4data, NEE_envdata2022, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "FluxID", "Cover"))%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)))


#CH4

NEE2021_CH4<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2021\\HMR - HMRinput_NEE_2021_CH4.csv")

## FAULTY CH4 visually inspected fluxes 
faulty_CH4_2021 <- c(
  "1M_C_NEE_11.09.2021_51" ,
  "1M_OTC_NEE_23.07.2021_11",
  "1R_C_RECO_23.07.2021_6",
  "1S_C_RECO_21.07.2021_11",
  "1WG_C_NEE2_03.06.2021_52",
  "1WG_C_RECO_18.08.2021_22",
  "1WG_R_NEE_21.08.2021_3",
  "2P_C_NEE_11.09.2021_73",
  "2P_C_NEE_23.07.2021_41",
  "2S_C_RECO_03.06.2021_46",
  "2S_C_RECO_18.08.2021_10",
  "2S_C_RECO_21.07.2021_1",
  "2S_OTC_RECO_11.09.2021_34",
  "2S_OTC_RECO_21.07.2021_2",
  "2WGB_C_RECO_18.08.2021_15",
  "2WGB_C_RECO_21.07.2021_6",
  "3M_C_NEE_20.07.2021_9",
  "3M_OTC_RECO_02.07.2021_31",
  "3P_C_NEE_02.07.2021_28",
  "3P_C_NEE_03.06.2021_5",
  "3P_C_NEE_20.07.2021_3",
  "3P_C_NEE1_03.06.2021_6",
  "3P_C_NEE2_03.06.2021_7",
  "3P_C_RECO_02.07.2021_29",
  "3P_C_RECO_20.07.2021_4",
  "3S_C_RECO_02.07.2021_25",
  "3S_C_RECO_03.06.2021_14",
  "4M_OTC_NEE1_03.06.2021_29",
  "4P_C_NEE_02.07.2021_1",
  "4P_C_NEE_03.06.2021_38",
  "4P_C_NEE_18.08.2021_3",
  "4P_C_NEE1_02.07.2021_2",
  "4P_C_NEE1_03.06.2021_39",
  "4P_C_NEE2_03.06.2021_40",
  "4P_C_RECO_03.06.2021_41",
  "4P_C_RECO_20.07.2021_32",
  "4P_OTC_NEE_03.06.2021_34",
  "4P_OTC_NEE1_03.06.2021_35",
  "4P_OTC_NEE2_03.06.2021_36",
  "4S_C_RECO_02.07.2021_17",
  "4S_C_RECO_03.06.2021_18",
  "4S_C_RECO_11.09.2021_14",
  "4S_C_RECO_20.07.2021_21",
  "4S_OTC_RECO_02.07.2021_16",
  "4S_OTC_RECO_03.06.2021_19",
  "AM_C_NEE_11.09.2021_61",
  "AP_C_NEE_03.07.2021_35",
  "AP_C_NEE2_03.06.2021_68",
  "AP_C_RECO_03.07.2021_36",
  "AR_C_NEE_12.09.2021_10",
  "AR_C_RECO_12.09.2021_11",
  "AS_C_RECO_02.07.2021_18",
  "AS_C_RECO_03.06.2021_17",
  "AS_C_RECO_11.09.2021_15",
  "AS_OTC_RECO_02.07.2021_19",
  "AS_OTC_RECO_20.07.2021_14",
  "AWG_R_NEE_21.08.2021_27",
  "BM_C_NEE_23.07.2021_35",
  "BM_C_RECO_23.07.2021_36",
  "BP_C_NEE_21.08.2021_31",
  "BP_C_NEE_23.07.2021_29",
  "BP_C_NEE_23.07.2021_30",
  "BP_C_RECO_21.08.2021_32",
  "BP_C_RECO_23.07.2021_31",
  "BP_C_RECO_23.07.2021_32",
  "BS_C_RECO_04.06.2021_5",
  "BS_C_RECO_11.09.2021_71",
  "BS_C_RECO_23.07.2021_40",
  "BS_OTC_RECO_11.09.2021_72",
  "BS_OTC_RECO_23.07.2021_39")

NEE2021_CH4 <- NEE2021_CH4 %>% 
  filter(!Series %in% faulty_CH4_2021)%>%
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
NEE2021_CH4_env<- left_join(NEE2021_CH4, NEE_envdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))

# combine 2021 and 2022 data
NEE2021_CH4_env_combi<- NEE2021_CH4_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE2022_CH4_env_combi<- NEE2022_CH4_env%>%
  select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, 
         PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)

NEE_CH4_21_22<- rbind(NEE2021_CH4_env_combi, NEE2022_CH4_env_combi)

NEE_CH4_21_22_means<-NEE_CH4_21_22%>%
  group_by(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p)%>%
  gather(key = par, value = value, PAR1, PAR2, PAR3)%>%
  mutate(PAR_mean = mean(value, na.rm = TRUE))%>%
  gather(key = soilT, value = temp, SoilTemp1 , SoilTemp2)%>%
  mutate(SoilT_mean = mean(temp, na.rm = TRUE))%>%
  gather(key = soilM, value = moisture, SoilMoist1 , SoilMoist2, SoilMoist3)%>%
  mutate(SoilMoist_mean = mean(moisture, na.rm = TRUE))%>%
  distinct(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0, LR.f0.se, LR.f0.p, .keep_all = TRUE)%>%
  select(-par, -value, -soilT, -temp, -soilM, -moisture)%>%
  ungroup()

# join NEEdata with TOMSTlogger data based on PlotID, date and hour of day

NEE_CH4_21_22_means_TOMST<-left_join(NEE_CH4_21_22_means, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))


# join NEEdata with ECtower data based on date and hour of day
#! Find reference for shortwave conversion to flux back
# read in ECtower data
#ref par calculation: https://www.sciencedirect.com/science/article/pii/0002157176900807

ECdata<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\Mobileflux1_level1_30min_forCasper_update2022.csv")%>%
  separate(index, into = c("Date", "Time"), sep = " " )%>%
  mutate(Hour = as.integer(substring(Time, 1, 2)),
         Date = as.Date(Date),
         PAR_EC = shortwave_incoming * 2.114)%>% # calculate PAR based on incoming shortwave radiation
  mutate(PAR_EC = ifelse(PAR_EC< 0, 0, PAR_EC))%>% # set PAR to zero if negative
  group_by(Date, Hour)%>%
  summarise_all(mean)%>%
  select(Date, Hour, PAR_EC, air_temperature, relative_humidity, air_pressure, shortwave_incoming)%>%
  ungroup()


NEE_CH4_21_22_means_TOMST_EC<- left_join(NEE_CH4_21_22_means_TOMST, ECdata, by= c("Date", "Hour"))%>%
  mutate(PAR_mean = ifelse(Cover == "RECO", 0, PAR_mean),
         PAR_ideal = ifelse(PAR_mean == "NaN", PAR_EC, PAR_mean))


### Recalculate PAR based on shading
NEE_CH4_21_22_means_TOMST_EC<- NEE_CH4_21_22_means_TOMST_EC%>%
  mutate(PAR_real = ifelse(Cover == "NEE1", PAR_ideal*0.67, 
                           ifelse(Cover == "NEE2", PAR_ideal*0.33, PAR_ideal)))

### fluxconversion based on airtemp 
# 
# TOMST aitemp if possible, otherwise EC if available, otherwise soiltemp point measurements? 
NEE_CH4_21_22_means_TOMST_EC_new<- NEE_CH4_21_22_means_TOMST_EC%>%
  mutate(CH4flux = f0/(0.08205*(273.15+AirTemperature)),
         CH4flux.LR = LR.f0/(0.08205*(273.15+AirTemperature)),
         CH4flux_EC = f0/(0.08205*(273.15+air_temperature)),
         CH4flux.LR_EC = LR.f0/(0.08205*(273.15+air_temperature)))%>%
  mutate(CH4flux_final = ifelse(is.na(CH4flux) == TRUE, CH4flux_EC, CH4flux))%>%
  mutate(Cover = recode(Cover, Reco ="RECO"))

# count number of measurements
NEE_CH4_21_22_means_TOMST_EC_new%>%
  mutate(Date = ymd(Date),
         Year = year(Date))%>%
  drop_na(CH4flux_final)%>%
  #filter(Cover == "RECO")%>%
  filter(Method != "No flux")%>%
  #filter(Habitat != "S")%>%
  filter(Treatment == "C")%>%
  group_by(Year)%>%
  count()


#write.csv(NEE_CH4_21_22_means_TOMST_EC_new, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\Thawgradient\\NEE_CH4_2021-2022.csv")

#select(Date, PlotID, Transect, Habitat, Treatment, Cover, f0, f0.se, f0.p, Method, LR.f0. LR.f0.se, LR.f0.p, PAR1, PAR2, PAR3, SoilTemp1, SoilTemp2, SoilMoist1, SoilMoist2, SoilMoist3, Hour)







# bind together 2020 and 2021 NEE CO2 data
NEE20202021_CO2_env<- rbind(NEE2020_CO2_env, NEE2021_CO2_env)%>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date))%>%
  mutate(Habitat = dplyr::recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
NEE20202021_CO2_env$Habitat <- factor(NEE20202021_CO2_env$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))


##############

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


##############################################################################################################################################
##### SR data 2022
# read in Metadata SR
metafiles_SR <-dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2022\\NewMetaData", 
                   pattern = "^SRmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to combine metadata files into one dataframe
SR_envdata2022 <- map_df(set_names(metafiles_SR), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Transect = as.character(Transect)))
}, .id = "File")


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


###Data cleaning SR data 2021
# SR CO2
sr_raw_hmr_CO2_prepped <- sr_raw_hmr_CO2 %>% 
   filter(
    # 
    # Unique_plot != "1M_C_2021-07-22_9848.13",
    # Unique_plot != "1M_C_2021-09-12_10863.95",
    # Unique_plot != "1P_C_2021-07-22_9768.14",
    # Unique_plot != "1S_OTC_2021-07-22_9668.225",
    # Unique_plot != "1WG_C_2021-06-30_11480.04",
    # Unique_plot != "1WG_OTC_2021-06-04_14068.55",
    # Unique_plot != "2S_C_2021-06-04_13808.4",
    # Unique_plot != "2WGA_C_2021-06-30_12471.75",
    # Unique_plot != "2WGA_C_2021-06-30_13364.25",
    # Unique_plot != "2WGA_C_2021-07-22_9548.44",
    # Unique_plot != "2WGB_OTC_2021-07-22_9702.385",
    # Unique_plot != "2WGB_OTC_2021-06-30_13753.85",
    # Unique_plot != "2WGB_C_2021-09-12_9375.075",
    # Unique_plot != "2WGB_C_2021-08-18_13768.35",
    # Unique_plot != "2WGB_C_2021-07-22_9784.835",
    # Unique_plot != "2WGB_C_2021-07-22_9731.99",
    # Unique_plot != "2WGB_C_2021-06-30_12820.2",
    # Unique_plot != "2WGA_C_2021-06-30_13364.25",
    # Unique_plot != "3M_C_2021-09-12_9108.77",
    # Unique_plot != "3W_C_2021-06-04_16948.05",
    # Unique_plot != "3WGA_C_2021-06-04_13586.85",
    # Unique_plot != "3WGA_C_2021-08-18_13845.5",
    # Unique_plot != "3WGA_C_2021-09-12_10306.955",
    # Unique_plot != "3WGB_C_2021-06-30_13709.85",
    # Unique_plot != "3WGB_C_2021-07-22_10023.39",
    # Unique_plot != "3WGB_C_2021-08-18_11112.2",
    # Unique_plot != "3WGB_C_2021-09-12_10567.135",
    # Unique_plot != "3WGB_OTC_2021-06-30_14682.25",
    # Unique_plot != "3WGB_OTC_2021-09-12_9377.785",
    # Unique_plot != "4P_C_2021-07-22_9499.22",
    # Unique_plot != "4WG_OTC_2021-06-30_12822.85",
    # Unique_plot != "4WG_OTC_2021-07-22_10238.31",
    # Unique_plot != "4WG_OTC_2021-08-18_10719.9",
    # Unique_plot != "4WG_OTC_2021-08-18_11005.15",
    # Unique_plot != "AS_C_2021-07-22_10303.75",
    # Unique_plot != "4WG_OTC_2021-09-12_8928.845",
    # Unique_plot != "AM_C_2021-06-30_14680.45",
    # Unique_plot != "AM_C_2021-07-22_9636.345",
    # Unique_plot != "AM_OTC_2021-06-04_17586.25",
    # Unique_plot != "BM_C_2021-09-12_10175.62"
  )


# SR CH4

## faulty CH4 SR fluxe
faulty_sr_CH4 <- c("1M_C_2021-09-12_10548.96" ,
                   "1S_C_2021-07-22_9432.465" ,
                   "1S_OTC_2021-07-22_9668.225" ,
                   "1WG_C_2021-07-22_9608.2" ,
                   "2S_C_2021-07-22_9479.05" ,
                   "2S_OTC_2021-07-22_9441.28" ,
                   "2WGA_C_2021-07-22_9548.44" ,
                   "2WGB_C_2021-07-22_9784.835" ,
                   "2WGB_OTC_2021-07-22_9455.935" ,
                   "3S_C_2021-07-22_9808.385" ,
                   "3WGB_C_2021-07-22_10023.39" ,
                   "4M_OTC_2021-06-30_12990.55" ,
                   "4M_OTC_2021-07-22_9482.91" ,
                   "4P_OTC_2021-06-30_12209.75" ,
                   "4S_C_2021-07-22_9866.48" ,
                   "4S_OTC_2021-07-22_9921.275" ,
                   "4WG_OTC_2021-06-30_12822.85" ,
                   "4WG_OTC_2021-07-22_10238.31" ,
                   "AM_C_2021-07-22_9636.345" ,
                   "AP_C_2021-06-04_17506.3" ,
                   "AS_C_2021-07-22_10303.75" ,
                   "BM_C_2021-08-18_12788.75" ,
                   "BM_C_2021-09-12_10175.62" ,
                   "BM_OTC_2021-07-22_9478.295" ,
                   "BP_C_2021-06-04_14151" ,
                   "BS_OTC_2021-07-22_9509.07"
)

sr_raw_hmr_CH4_prepped <- sr_raw_hmr_CH4 %>% 
  filter(!Series %in% faulty_sr_CH4)



