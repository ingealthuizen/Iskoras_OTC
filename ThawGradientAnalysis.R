#Thaw Gradient Analysis
library(tidyverse)
library(ggplot2)
library(lubridate)

# function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

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
