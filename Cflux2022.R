# combine Iskoras 2022 flux data
library(tidyverse)
library(lubridate)

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
         Habitat = str_sub(PlotID, 2,3))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))

NEE_CH4data <- map_df(set_names(CH4files_NEE), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

NEE_CH4data<- NEE_CH4data%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "FluxID", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))

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


## SR data
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
         Habitat = str_sub(PlotID, 2,3))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))

SR_CH4data <- map_df(set_names(CH4files_SR), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

SR_CH4data<- SR_CH4data%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "FluxID", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(Date = as.Date(Date, format="%d.%m.%Y"))


