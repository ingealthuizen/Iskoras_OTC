#Thaw Gradient Analysis
library(tidyverse)
library(ggplot2)
library(lubridate)
library(viridis)

# function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

# Ibutton data 
load("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\Thawgradient\\ER\\GAPFILLED_ALL_DATA_incl_2018_2019_groundtemps_to_2022.Rdata")


GAPFILLED_for_2018_2019_groundtemps_to_2022 %>% 
  mutate(Soil_temp_5cm_modelled = (Soil_temp_10cm + Gapfilled_2018_2019_Ground_temp) / 2)%>%
  filter(Treatment == "CONTROL")%>%
  #filter(Soil_depth_cm =="10")%>%
  #filter(Date > "2019-12-31" & Date <"2021-01-01")%>%
  ggplot(aes(Date, Soil_temp_5cm_modelled, col= Type))+
  geom_line()+
  facet_grid(~Data_source)+
  theme_bw()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


IbuttonData_17181920<- read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\4hour_iButton_2017-2021.csv")%>%
  mutate(Date = as.Date(Date))

# calculate mean over 2 and 10 cm Ibutton to fill gaps
IbuttonData_MeanDailyHabitat<- IbuttonData_17181920%>%
  filter(Soil_depth_cm !="20")%>%
  group_by(Habitat, Treatment, Date)%>%
  summarise_at(vars(Temperature), list(Min = min, Mean = mean, Max = max, sd = sd, se =se))%>%
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", W= "Thawpond", WG= "Vegetated Pond")) # recode Habitat
IbuttonData_MeanDailyHabitat$Habitat <- factor(IbuttonData_MeanDailyHabitat$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Thawpond", "Vegetated Pond"))


IbuttonData_MeanDailyHabitat%>%
  filter(Treatment == "C")%>%
  #filter(Soil_depth_cm =="10")%>%
  filter(Date > "2018-11-01" & Date <"2019-10-31")%>%
  ggplot(aes(Date, Mean, col= Habitat))+
  geom_line()+
  #geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  #scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
  #                   name = "Habitat")+
  #scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
  #                  name = "Habitat")+
  theme_bw()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


  ggplot(aes(Date, value, color= Habitat, linetype= Temp))+
  geom_line()+
  #stat_smooth(method="loess", span=0.01, se=FALSE, aes(fill=Habitat, linetype=Temp), alpha=0.3) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(x = "Date", y= "Temperature ?C")+
  theme( axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13), 
         panel.background = element_rect(fill = "white", colour = "black"))


#TOMST data mid june 2020- sept 2023
TomstData<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\TOMSTdata_SMcalculated2023.csv")

TomstData<-TomstData%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  select(PlotID:LoggerID, Date, Date_Time, SoilTemperature:RawSoilmoisture, Soilmoisture_Volumetric)%>%
  mutate(Date = as.Date(Date),
         DateTime_UTC = as.POSIXct(strptime(Date_Time, tz="UTC", "%Y-%m-%dT%H:%M:%SZ")),
         DateTime = format(DateTime_UTC, tz="Europe/Berlin"),
         Hour = hour(DateTime),
         Soilmoisture_Volumetric= Soilmoisture_Volumetric*100)

# Hourly climate data per plot
TomstData_HourlyPlotID<- TomstData%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date, Hour)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE), 
            GroundTemperature = mean(GroundTemperature, na.rm = TRUE),
            AirTemperature = mean(AirTemperature, na.rm = TRUE),
            Soilmoisture_Volumetric = mean(Soilmoisture_Volumetric, na.rm = TRUE))%>%
  ungroup()

##### DAILY
# Summary Daily Per Habitat and Treatment
TomstData_MeanDailyHabitat<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, sd = sd, se =se))%>%
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
TomstData_MeanDailyHabitat$Habitat <- factor(TomstData_MeanDailyHabitat$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))


# main figure 2 multiple year overview and zoom in to summer?
# missing thaw pond (W) in this figure!
TomstData_MeanDailyHabitat%>%
  filter(Treatment == "C")%>%
  filter(Climate_variable %in% c("SoilTemperature"))%>%
  #filter(Date > "2021-05-31" & Date <"2021-09-01")%>%
  ggplot(aes(Date, Mean, col= Habitat, linetype =Treatment))+
  geom_line()+
  #geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  #scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
  #                   name = "Habitat")+
  #scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
  #                  name = "Habitat")+
  #facet_grid(~Climate_variable, scales="free")+
  theme_bw()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )
