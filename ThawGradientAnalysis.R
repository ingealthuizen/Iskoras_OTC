#Thaw Gradient Analysis
library(tidyverse)
library(ggplot2)
library(lubridate)
library(viridis)
library(cowplot)
library(lme4)
library(emmeans)

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
  #facet_grid(~Soil_depth)+
  theme_bw()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

# RAW Ibutton data
IbuttonData_17181920<- read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\4hour_iButton_2017-2021.csv")%>%
  mutate(Date = as.Date(Date))%>%
  mutate(Soildepth=recode(Soil_depth_cm, '2'='2cm', '10'='10cm', '20'= '20cm'))

# test for difference between W and WG
testWvsWG_10<- IbuttonData_17181920%>%
  filter(Soildepth == "10cm")%>%
  filter(Treatment == "C")%>%
  group_by(Date, Habitat)%>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE))

testWvsWG_2<- IbuttonData_17181920%>%
  filter(Soildepth == "2cm")%>%
  filter(Treatment == "C")%>%
  group_by(Date, Habitat)%>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE))

testWvsWG_20<- IbuttonData_17181920%>%
  filter(Soildepth == "20cm")%>%
  filter(Treatment == "C")%>%
  group_by(Date, Habitat)%>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE))

testWvsWG_10<-testWvsWG_10%>%
  spread(Habitat, Temperature)

testWvsWG_2<-testWvsWG_2%>%
  spread(Habitat, Temperature)

testWvsWG_20<-testWvsWG_20%>%
  spread(Habitat, Temperature)

cor.test(testWvsWG_2$WG, testWvsWG_2$W) # 0.957 correlation
Plot_WvsWG_2<-ggplot(testWvsWG_2, aes(W, WG))+
  geom_point()+ 
  geom_smooth(method = "lm", se =F)+
  theme_bw()

lmW_WG_2<-lm(WG~W, data = testWvsWG_2)
coef_lmW_WG_2<-coef(lmW_WG_2)

cor.test(testWvsWG_10$WG, testWvsWG_10$W) # 0.998 correlation
Plot_WvsWG_10<-ggplot(testWvsWG_10, aes(W, WG))+
  geom_point()+ 
  geom_smooth(method = "lm", se =F)+
  theme_bw()

lmW_WG_10<-lm(WG~W, data = testWvsWG_10)
coef_lmW_WG_10<-coef(lmW_WG_10)

cor.test(testWvsWG_20$WG, testWvsWG_20$W) # 0.998 correlation
Plot_WvsWG_20<-ggplot(testWvsWG_20, aes(W, WG))+
  geom_point()+ 
  geom_smooth(method = "lm", se =F)+
  theme_bw()

lmW_WG_20<-lm(WG~W, data = testWvsWG_20)
coef_lmW_WG_20<-coef(lmW_WG_20)

# calculate mean over 2 and 10 cm Ibutton to fill gaps
IbuttonData_MeanDailyHabitat<- IbuttonData_17181920%>%
  filter(Treatment=="C")%>%
  group_by(Habitat, Treatment, Date, Soildepth, Soil_depth_cm)%>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE))%>%
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", W= "Thawpond", WG= "Vegetated Pond")) # recode Habitat
IbuttonData_MeanDailyHabitat$Habitat <- factor(IbuttonData_MeanDailyHabitat$Habitat, levels = c( "Bare Soil Palsa", "Vegetated Palsa", "Thaw slump", "Thawpond", "Vegetated Pond"))

IbuttonData_MeanDailyHabitat<-IbuttonData_MeanDailyHabitat%>%
  select(Habitat, Date, Soildepth, Temperature)%>%
  group_by(Date, Habitat)%>%
  spread(Soildepth, Temperature)%>%
  gather(key = Soildepth, value = "Temperature", "2cm", "10cm", "20cm")

Plot_Ibutton2<-IbuttonData_MeanDailyHabitat%>%
  filter(Soildepth =="2cm")%>%
  ggplot(aes(Date, Temperature, col= Habitat))+
  geom_line()+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_fill_viridis(discrete = TRUE, direction = -1)+
  geom_rect(aes(xmin=ymd('2020-06-15'), 
                xmax = ymd('2020-07-14'),
                ymin = -Inf,
                ymax = Inf), fill = 'white', color= "white") +
  facet_wrap(~Soildepth)+
  theme_classic()+
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

Plot_Ibutton10<-IbuttonData_MeanDailyHabitat%>%
  filter(Soildepth =="10cm")%>%
  ggplot(aes(Date, Temperature, col= Habitat))+
  geom_line()+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_fill_viridis(discrete = TRUE, direction = -1)+
  facet_wrap(~Soildepth)+
  theme_classic()+
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

Plot_Ibutton20<-
  
IbuttonData_MeanDailyHabitat%>%
  filter(Soildepth =="20cm")%>%
  ggplot(aes(Date, Temperature, col= Habitat))+
  geom_line()+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_fill_viridis(discrete = TRUE, direction = -1)+
  geom_rect(aes(xmin=ymd('2020-04-01'), 
                xmax = ymd('2020-08-01'),
                ymin = -Inf,
                ymax = Inf), fill = 'white', color= "white") +
  facet_wrap(~Soildepth)+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

plot_grid(Plot_Ibutton2, Plot_WvsWG_2,
          Plot_Ibutton10, Plot_WvsWG_10,
          Plot_Ibutton20, Plot_WvsWG_20,
          nrow=3, ncol=2, rel_widths = c(3, 1))




#TOMST data mid june 2020- sept 2023
TomstData<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\TOMSTdata_SMcalculated2023.csv")

TomstData_control<-TomstData%>%
  filter(Treatment %in% c("C"))%>%
  filter(PlotID != "2M_C")%>% # remove discontinued plot
  select(PlotID:LoggerID, Date, DateTime_UTC, DateTime_local, Hour, SoilTemperature:RawSoilmoisture, Soilmoisture_Volumetric)%>%
  mutate(Date = as.Date(Date))%>%
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
TomstData_control$Habitat <- factor(TomstData_control$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))

# Hourly climate data per plot
TomstData_HourlyPlotID<- TomstData_control%>%
  group_by(PlotID, Transect, Habitat, Treatment, DateTime_local, Date, Hour)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE), 
            GroundTemperature = mean(GroundTemperature, na.rm = TRUE),
            AirTemperature = mean(AirTemperature, na.rm = TRUE),
            Soilmoisture_Volumetric = mean(Soilmoisture_Volumetric, na.rm = TRUE))%>%
  ungroup()


##### DAILY
# Daily climate data per plot
TomstData_DailyPlotID<- TomstData_control%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE), 
            GroundTemperature = mean(GroundTemperature, na.rm = TRUE),
            AirTemperature = mean(AirTemperature, na.rm = TRUE),
            Soilmoisture_Volumetric = mean(Soilmoisture_Volumetric, na.rm = TRUE))%>%
  ungroup()


# Summary Daily Per Habitat and Treatment
TomstData_MeanDailyHabitat<-TomstData_control%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, sd = sd, se =se))#%>%
  #mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
#TomstData_MeanDailyHabitat$Habitat <- factor(TomstData_MeanDailyHabitat$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))


# main figure 2 multiple year overview and zoom in to summer?
# missing thaw pond (W) in this figure!

TOMSTairtemp<-TomstData_MeanDailyHabitat%>%
  #filter(Date > "2021-05-01" & Date <"2022-04-30")%>%
  filter(Treatment == "C")%>%
  filter(Climate_variable %in% c("AirTemperature"))%>%
  ggplot(aes(Date, Mean, col= Habitat))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_fill_manual(values= c("#fc8d62","#e5c494", "#66c2a5", "#8da0cb"), name = "Habitat")+ 
  geom_hline(yintercept = 0, linetype= "dashed")+
  facet_wrap(~Climate_variable, scales="free")+
  labs(y= expression('Air temperature ('*~degree*C*')'))+
  theme_classic()+
  theme(legend.position = c(0.9, 0.2), strip.text = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11))

Airtemp_annual<- TomstData_control%>%
  mutate(Month = month(Date),
         Year = year(Date))%>%
  filter(Year > 2020 & Year < 2023)%>%
  group_by(Year, PlotID, Habitat, Treatment)%>%
  summarise(AirTemperature = mean(AirTemperature, na.rm = TRUE))

Airtemp_annual_plot<- ggplot(Airtemp_annual, aes(Treatment, AirTemperature, fill=Habitat))+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, position = position_dodge(width = 0.75))+
  scale_fill_manual(values= c("#fc8d62","#e5c494", "#66c2a5", "#8da0cb"), name = "Habitat")+ 
  labs(x= "Annual")+
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_text(size =12))

Airtemp_summer<- TomstData_control%>%
  mutate(Month = month(Date),
         Year = year(Date))%>%
  filter(Year > 2020 & Year < 2024)%>%
  filter(Month > 5 & Month < 9)%>%
  filter(PlotID != "3WGA_C" | Year > 2022)%>% # data missing from 10.06.2023
  group_by(Year, PlotID, Habitat, Treatment)%>%
  summarise(AirTemperature = mean(AirTemperature, na.rm = TRUE))

Airtemp_summer_plot<-ggplot(Airtemp_summer, aes(Treatment, AirTemperature, fill=Habitat))+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, position = position_dodge(width = 0.75))+
  scale_fill_manual(values= c("#fc8d62","#e5c494", "#66c2a5", "#8da0cb"), name = "Habitat")+ 
  labs(x= "Summer")+
    theme_classic()+
  theme(legend.position = "none", axis.text.x = element_blank(),  axis.title.y = element_blank(), axis.title.x = element_text(size =12))

TOMSTsoiltemp<-TomstData_MeanDailyHabitat%>%
  filter(Climate_variable %in% c("SoilTemperature"))%>%
  ggplot(aes(Date, Mean, col= Habitat))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  geom_hline(yintercept = 0, linetype= "dashed")+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_fill_manual(values= c("#fc8d62","#e5c494", "#66c2a5", "#8da0cb"), name = "Habitat")+ 
  facet_wrap(~Climate_variable, scales="free")+
  labs(y= expression('Soil temperature ('*~degree*C*')'))+
  theme_classic()+
  theme(legend.position = "none", strip.text = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

Soiltemp_annual<- TomstData_control%>%
  mutate(Month = month(Date),
         Year = year(Date))%>%
  filter(Year > 2020 & Year < 2023)%>%
  group_by(Year, PlotID, Habitat, Treatment)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE))

Soiltemp_annual_plot<- ggplot(Soiltemp_annual, aes(Treatment, SoilTemperature, fill=Habitat))+
    geom_boxplot(outlier.shape = NA)+
    stat_summary(fun=mean, geom="point", shape=4, size=2, position = position_dodge(width = 0.75))+
    scale_fill_manual(values= c("#fc8d62","#e5c494", "#66c2a5", "#8da0cb"), name = "Habitat")+
    labs(x= "Annual")+
    theme_classic()+
    theme(legend.position = "none", axis.text.x = element_blank(), axis.title.y = element_blank(), axis.title.x = element_text(size =12))

Soiltemp_summer<- TomstData_control%>%
  mutate(Month = month(Date),
         Year = year(Date))%>%
  filter(Year > 2020 & Year < 2024)%>%
  filter(PlotID != "3WGA_C" | Year > 2022)%>% # data missing from 10.06.2023
  filter(Month > 5 & Month < 9)%>%
  group_by(Year, PlotID, Habitat, Treatment)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE))


Soiltemp_summer_plot<- ggplot(Soiltemp_summer, aes(Treatment, SoilTemperature, fill=Habitat))+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, position = position_dodge(width = 0.75))+
  scale_fill_manual(values= c("#fc8d62","#e5c494", "#66c2a5", "#8da0cb"), name = "Habitat")+ 
  labs(x= "Summer")+
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.title.y = element_blank(), axis.title.x = element_text(size =12))

library(cowplot)
plot_grid(TOMSTairtemp, Airtemp_annual_plot, Airtemp_summer_plot, TOMSTsoiltemp, Soiltemp_annual_plot, Soiltemp_summer_plot, nrow = 2, rel_widths = c(3,1,1))

# test for significant differences between habitat microclimate
                      
library(lme4)
AT_annual.lmer<-lmer(SoilTemperature ~ Habitat + (1|PlotID), data=Soiltemp_summer)
summary(AT_annual.lmer)


### Soilgas data
