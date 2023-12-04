library(tidyverse)
library(stringr)
library(readxl)
library(janitor)
library(lubridate)
library(ggplot2)

setwd("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\soilgas")
#### Read raw data files soilgas 2019 ####
col_names <- c("Vial_ID", "Transect", "Plot", "Soil_depth", "Treatment", "Time_SECS", "Type", "Date", "CH4_ppm", "CO2_ppm", "N2O_ppm")
raw_dataa <- read_xlsx("15 - 4a. jordgas.xlsx", skip = 12, range = "F4:P101", trim_ws = TRUE)
colnames(raw_dataa) <- col_names
raw_datab <- read_xlsx("15 - 4b. jordgas.xlsx", skip = 12, range = "F4:P101", trim_ws = TRUE)
colnames(raw_datab) <- col_names
raw_datah <- read_xlsx("15 - 4h. jordgas.xlsx", skip = 12, range = "F4:P89", trim_ws = TRUE)
colnames(raw_datah) <- col_names
raw_datai <- read_xlsx("15 - 4i. jordgas.xlsx", skip = 12, range = "F4:P88", trim_ws = TRUE) 
colnames(raw_datai) <- col_names
raw_dataj <- read_xlsx("15 - 4j. jordgas.xlsx", skip = 12, range = "F4:P88", trim_ws = TRUE)
colnames(raw_dataj) <- col_names
raw_dataq <- read_xlsx("15 - 4q. jordgas.xlsx", skip = 12, range = "F4:P66", trim_ws = TRUE)
colnames(raw_dataq) <- col_names

#### Merge data, modify columns, remove blank entries ####
soilgas <- bind_rows(raw_dataa, raw_datab, raw_datah, raw_datai, raw_dataj, raw_dataq)
soilgas <-soilgas %>%
  select(Transect, Plot, Soil_depth, CO2_ppm, CH4_ppm, N2O_ppm, Date) %>% 
  filter(Transect != "blank",
         Transect != "Blank",
         Transect!= "",
         !str_detect(Transect, "Std"))

# read soilgas data 2017-2018
soilgas_1718 <- read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\soilgas\\April_update_soilgas_2019.csv")%>%
  mutate(Transect = as.character(Transect),
         Soildepth = as.character(Soildepth),
         Habitat = recode(Plot, "Vegetated palsa" = "P", "Soil palsa" = "S", "Thaw slump" = "M", "Thaw pond" = "W"),
         Date = as.Date(Date, "%Y-%m-%d"))%>%
  select(-Plot)

# read soilgas data 2019
soilgas_19 <- read.csv2("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\soilgas\\Soilgas_2019.csv", fill = TRUE)%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))
         

# combine all soilgas data
soilgas_171819 <- bind_rows(soilgas_1718, soilgas_19)%>%
  mutate(Soildepth = as.integer(Soildepth),
         Transect = as.factor(Transect))

# soilgas data 2020
soilgas_2020 <- read.csv2("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2020\\Soilgas\\Soilgas_2020data.csv")%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"),
         Transect = as.factor(Transect),
         Soildepth = as.integer(Soildepth))%>%
  ungroup()

soilgas_17181920<- dplyr::bind_rows(soilgas_171819, soilgas_2020)%>%
  mutate(yday = as.factor(yday(Date)),
         month = as.factor(month(Date)),
         year = as.factor(year(Date)))%>%
  filter(!Habitat %in% c("AIR", "N/A"))%>% # remove ambient air samples
  filter(!is.na(year))%>%
  filter(!is.na(Habitat))%>%
  select(-Ar., -O2., -N2.)


#write.csv(soilgas_17181920, file = "C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Manuscripts\\Cbudgets_gradient\\Data\\Soilgas.csv")

# median soilgas concentratin per habitat
soilgas_17181920%>%
  group_by(Habitat)%>%
  summarize(median_co2 = median(CO2_ppm),
            median_ch4 = median(CH4_ppm),
            median_n2o = median(N2O_ppm))

# remove bad measurements from broken chambers and outliers.
soilgas_17181920<-soilgas_17181920%>%
  filter(!grepl("broken", Comment),
         !grepl("vacuum", Comment),
         !grepl("underpressure", Comment), 
         !grepl("water", Comment))%>% # remove bad sample from broken chambers
  filter(CO2_ppm > 0)%>% # faulty measurement
  mutate(outlier =  ifelse((Habitat %in% c("S","P") & CH4_ppm > 800), "outlier", ""))%>% # P & S with CH4_ppm > 800 as outlier
  filter(!grepl("outlier", outlier)) #! filter out CH4 measurements above 800 ppm for P and S. 


# Non-parametric test
library(FSA) # dunntest

#Overall comparison per soildepth across all years 
# could compare different years but number of samples differ quite a lot

Ktest_10<-soilgas_17181920%>%
  filter(Soildepth =="10")

kruskal.test(CO2_ppm ~ Habitat, data = Ktest_10)
dunnTest(CO2_ppm ~ Habitat, data = Ktest_10, method="bh")

kruskal.test(CH4_ppm ~ Habitat, data = Ktest_10)
dunnTest(CH4_ppm ~ Habitat, data = Ktest_10, method="bh")

kruskal.test(N2O_ppm ~ Habitat, data = Ktest_10)
dunnTest(N2O_ppm ~ Habitat, data = Ktest_10, method="bh")

#soildepth 20
Ktest_20<-soilgas_17181920%>%
  filter(Soildepth =="20")

kruskal.test(CO2_ppm ~ Habitat, data = Ktest_20)
dunnTest(CO2_ppm ~ Habitat, data = Ktest_20, method="bh")

kruskal.test(CH4_ppm ~ Habitat, data = Ktest_20)
dunnTest(CH4_ppm ~ Habitat, data = Ktest_20, method="bh")

kruskal.test(N2O_ppm ~ Habitat, data = Ktest_20)
dunnTest(N2O_ppm ~ Habitat, data = Ktest_20, method="bh")

#soildepth 40
Ktest_40<-soilgas_17181920%>%
  filter(Soildepth =="40")

kruskal.test(CO2_ppm ~ Habitat, data = Ktest_40)
dunnTest(CO2_ppm ~ Habitat, data = Ktest_40, method="bh")

kruskal.test(CH4_ppm ~ Habitat, data = Ktest_40)
dunnTest(CH4_ppm ~ Habitat, data = Ktest_40, method="bh")

kruskal.test(N2O_ppm ~ Habitat, data = Ktest_40)
dunnTest(N2O_ppm ~ Habitat, data = Ktest_40, method="bh")

#should probably split data by soildepth and year?



###### graphing
# plot data
ggplot(soilgas_17181920, aes(month, CO2_ppm, fill = year))+
  geom_boxplot()+
  facet_wrap(Soildepth~Habitat, scales = "free_y")

ggplot(soilgas_17181920, aes(month, CH4_ppm, fill = year))+
  geom_boxplot()+
  facet_grid(Habitat~Soildepth, scales = "free_y")
         
ggplot(soilgas_17181920, aes(month, N2O_ppm, fill = year))+
  geom_boxplot()+
  facet_grid(Habitat~Soildepth, scales = "free_y")

# some months multiple measurement days
soilgas_mean <- soilgas_17181920%>%
  group_by(Habitat, Soildepth, month)%>%
  summarise(CO2 = mean(CO2_ppm, na.rm = TRUE),
            CO2_sd = sd(CO2_ppm, na.rm = TRUE),
            CH4 = mean(CH4_ppm, na.rm = TRUE),
            CH4_sd = sd(CH4_ppm, na.rm = TRUE),
            N2O = mean(N2O_ppm, na.rm = TRUE),
            N2O_sd = sd(N2O_ppm, na.rm = TRUE),
            n_observations = n())

# interpolate concentration of different gasses at 30cm depth based on mean of 20 and 40cm data. 
#soilgas_30mean <- soilgas_mean%>%
#  filter(!Soildepth == "10")%>%
#  group_by(Habitat, month)%>%
#  summarise(CO2 = mean(CO2),
#            CH4 = mean(CH4),
  #          N2O = mean(N2O))%>%
#  mutate(Soildepth = as.integer(30))%>%
#  ungroup()

#soilgas_mean <- bind_rows(soilgas_mean, soilgas_30mean)%>%
#  mutate(Month = as.numeric(as.character(month)),
 #        Habitat = as.factor(Habitat))%>%
 # ungroup()

soilgas_mean$Habitat <- factor(soilgas_mean$Habitat, levels = c("P", "S", "M", "W"))

ggplot(soilgas_mean)+
  geom_point(aes(Soildepth, CO2, fill= Habitat, shape = Habitat), size = 2)+
  scale_shape_manual(values = c( 21, 22, 24, 25))+
  geom_line(aes(Soildepth, CO2, col=Habitat))+
  geom_errorbar(aes(x= Soildepth, y= CO2, ymin=CO2-CO2_sd, ymax=CO2+CO2_sd, color=Habitat), width=.1, position=position_dodge(0.05))+
  geom_hline(yintercept=0 , linetype= "dashed")+
  scale_x_continuous(trans = "reverse")+
  facet_grid(month~Habitat, scales = "free_x")+
  coord_flip()

ggplot(soilgas_mean)+
  geom_point(aes(Soildepth, CH4, fill= year, shape = year), size = 2)+
  scale_shape_manual(values = c( 21, 22, 24, 25))+
  geom_line(aes(Soildepth, CH4, col = year))+
  geom_errorbar(aes(x= Soildepth, y= CH4, ymin=CH4-CH4_sd, ymax=CH4+CH4_sd, col = year), width=.1, position=position_dodge(0.05))+
  geom_hline(yintercept=0 , linetype= "dashed")+
  scale_x_continuous(trans = "reverse")+
  facet_grid(month~Habitat, scales = "free_x")+
  coord_flip()

ggplot(soilgas_mean)+
  geom_point(aes(Soildepth, N2O, fill= year, shape = year), size = 2)+
  scale_shape_manual(values = c( 21, 22, 24, 25))+
  geom_line(aes(Soildepth, N2O, col = year))+
  geom_errorbar(aes(x= Soildepth, y= N2O, ymin=N2O-N2O_sd, ymax=N2O+N2O_sd, col = year), width=.1, position=position_dodge(0.05))+
  geom_hline(yintercept=0 , linetype= "dashed")+
  scale_x_continuous(trans = "reverse")+
  facet_grid(month~Habitat, scales = "free_x")+
  coord_flip()


# Thawdepth data
MetaData<-read.csv2("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2020\\MetaData_2017-2020.csv")
MetaData<-MetaData%>%
  mutate(Habitat = recode(Habitat, "WGA" = "WG"),
         Habitat = recode(Habitat, "WGB" = "WG"),
         Habitat = factor(Habitat, levels = c("S", "P", "M", "W", "WG")),
         Date = as.Date(Date, format = "%d.%m.%Y"),
         year = year(Date),
         Month = month(Date),
         jday = yday(Date))%>%
  filter(Treatment == "C")

Thawdepth<-MetaData%>%
  filter(year %in% c("2018", "2019", "2020"))%>%
  filter(!Habitat == "WG")%>%
  filter(Month < 10)%>%
  group_by(Habitat, Month, year)%>%
  summarise(Soildepth_sd = sd(Thawdepth, na.rm =TRUE),
            Soildepth = mean(Thawdepth, na.rm =TRUE))


# Soilprofile graphs over season
CO2<-soilgas_mean%>%
  #filter(year == "2019")%>%
  ggplot(aes( -Soildepth, Month, fill = CO2))+
  geom_raster(interpolate = TRUE)+ # Raster pixels are placed at uneven vertical intervals and will be shifted. Consider using geom_tile() instead. 
  coord_fixed(expand = FALSE)+
  geom_line(data = Thawdepth, col= "red")+
  scale_fill_viridis_c(trans= "log", breaks = c(500, 2500, 12500, 62500))+
  scale_y_continuous(name = "Month", breaks = c(3,4,5,6,7,8,9,10), limits = c(2.5,10.5))+
  facet_grid(Habitat~year)+
  theme( axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip()

CH4<-soilgas_mean%>%
  filter(year == "2019")%>%
  ggplot(aes(-Soildepth, Month, fill = CH4))+
  geom_raster(interpolate = TRUE)+ # Raster pixels are placed at uneven vertical intervals and will be shifted. Consider using geom_tile() instead.
  coord_fixed(expand = FALSE)+
  scale_fill_viridis_c(trans= "log", breaks = c(1, 10, 100, 1000, 10000, 100000))+
  scale_y_continuous(name = "Month", breaks = c(3,4,5,6,7,8,9,10), limits = c(2.5,10.5))+
  facet_grid(Habitat~year)+
  theme( axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 11),
         axis.text.y = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom",legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip()

N2O<-soilgas_mean%>%
  filter(year == "2019")%>%
  ggplot(aes(-Soildepth, Month, fill = N2O))+
  geom_raster(interpolate = TRUE)+ # Raster pixels are placed at uneven vertical intervals and will be shifted. Consider using geom_tile() instead.
  coord_fixed(expand = FALSE)+
  coord_fixed(expand = FALSE)+
  scale_fill_viridis_c()+
  scale_y_continuous(name = "Month", breaks = c(3,4,5,6,7,8,9,10), limits = c(2.5,10.5))+
  facet_grid(Habitat~year)+
  theme( axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip()


# With Thawdepth !!
CO2<- soilgas_mean%>%
  filter(year %in% c("2018", "2019", "2020"))%>%
  ggplot(aes(-Soildepth, Month, CO2))+
  geom_raster(aes(fill=CO2), interpolate = TRUE)+
  geom_line(data = Thawdepth, size = 1, col = "lightgrey") +
  scale_fill_viridis_c(trans= "log", breaks = c(500, 2500, 12500, 62500))+
  scale_y_continuous(name = "Month", breaks = c(3,4,5,6,7,8,9,10))+
  scale_x_continuous(breaks = c(0, -10, -20, -30, -40))+
  facet_grid(Habitat~year)+ 
  theme( axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.y = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip(xlim = c(-40,-5))



CH4<- soilgas_mean%>%
  filter(year %in% c("2018", "2019", "2020"))%>%
  ggplot(aes(-Soildepth, Month, CH4))+
  geom_raster(aes(fill=CH4), interpolate = TRUE)+
  coord_fixed(expand = FALSE)+
  geom_line(data = Thawdepth, size = 1, col = "lightgrey") +
  scale_fill_viridis_c(trans= "log", breaks = c(1, 10, 100, 1000, 10000, 100000))+
  scale_y_continuous(name = "Month", breaks = c(3,4,5,6,7,8,9,10), limits = c(2.5,10.5))+
  scale_x_continuous(breaks = c(0, -10, -20, -30, -40, -50, -60))+
  facet_grid(Habitat~year)+ 
  theme( axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  strip.text.y = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip(xlim = c(-40,-5))

N2O<- soilgas_mean%>%
  filter(year %in% c("2018", "2019", "2020"))%>%
  ggplot(aes(-Soildepth, Month, N2O))+
  geom_raster(aes(fill=N2O), interpolate = TRUE)+
  coord_fixed(expand = FALSE)+
  geom_line(data = Thawdepth, size = 1, col = "lightgrey") +
  scale_fill_viridis_c(breaks = c(3,6,9,12,15))+
  scale_y_continuous(name = "Month", breaks = c(3,4,5,6,7,8,9,10), limits = c(2.5,10.5))+
  scale_x_continuous(breaks = c(0, -10, -20, -30, -40, -50, -60))+
  facet_grid(Habitat~year)+ 
  theme( axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip(xlim = c(-40,-5))



# change color of specific  y x axis ticks that are based on interpolation
#a <- ifelse(data$category == 0, "red", "blue")
# ggplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = a))

library(cowplot)
plot_grid(CO2, CH4, N2O, ncol= 3, labels = c("A", "B", "C"))

soilgas_mean%>%
  filter(!Habitat %in% c("P", "S"))%>%
  ggplot(aes( -Soildepth, Month, fill = CO2))+
  geom_raster(interpolate = TRUE)+ # Raster pixels are placed at uneven vertical intervals and will be shifted. Consider using geom_tile() instead. 
  coord_fixed(expand = FALSE)+
  scale_fill_viridis_c()+
  scale_y_continuous(name = "Month", breaks = c(3,4,5,6,7,8,9,10), limits = c(2.5,10.5))+
  facet_grid(Habitat~year)+
  theme( axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip()


soilgas_mean%>%
  ggplot(aes( -Soildepth, Month, fill = n_observations))+
  geom_tile()+ # Raster pixels are placed at uneven vertical intervals and will be shifted. Consider using geom_tile() instead. 
  coord_fixed(expand = FALSE)+
  scale_fill_viridis_b(option = "magma")+
  facet_grid(Habitat~year)+
  theme( axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.key.width= unit(1.5, 'cm'), plot.margin = unit(c(0, 0, 0, 0), "cm"))+ 
  coord_flip()




################################## Gas flux through soil profile ############################################################
# bulk density data
BD_data<- read_xlsx("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\soilgas\\Bulk_density_peat_plateau.xlsx")%>%
  select(-Notes)

soilflux_17181920 <- left_join(soilgas_17181920, BD_data, by= "Soildepth")%>%
  mutate(Soildepth = as.factor(Soildepth))

# Environmental metadata 
# soil temperature/moisture Vegetronix in 3 transects; 2, 3 and 4
soiltemp <- read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\soilgas\\Vegetronix_soiltemp.csv")%>%
  mutate(Transect = as.factor(Transect),
         Date = as.Date(Date),
         Soildepth = as.factor(Soildepth))%>%
  group_by(Date, Habitat, Soildepth)%>%
  summarise(Soiltemp = mean(Soiltemp, na.rm = TRUE))%>% # calculate mean soiltemp per habitat type and soildepth
  ungroup()

soilmoisture<- read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\soilgas\\Vegetronix_soilmoist.csv")%>%
  mutate(Date = as.Date(Date),
         Soilmoist = Soilmoist/100,
         Transect = as.factor(Transect),
         Soildepth = as.factor(Soildepth))%>%
  mutate(Soilmoist = ifelse(Soilmoist > 1, 1, ifelse(Soilmoist < 0, 0, Soilmoist)))%>%
  group_by(Date, Habitat, Soildepth)%>%
  summarise(Soilmoist = mean(Soilmoist, na.rm = TRUE))%>%
  ungroup()

# Join soiltemp and soilmoisture data
soil_envdata <- right_join(soiltemp, soilmoisture, by =c("Date", "Habitat", "Soildepth"))

# replace NaN with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
soil_envdata [is.nan(soil_envdata )] <- NA

soilflux_17181920<- left_join(soilflux_17181920, soil_envdata, by= c("Date", "Habitat", "Soildepth"))%>%
  mutate(month = month(Date))

ggplot(soil_envdata)+ 
  geom_point(aes(Date, Soiltemp, col=Soildepth, alpha=0.3)) +
  facet_wrap(~Habitat)+
  theme_bw()

ggplot(soil_envdata)+ 
  geom_point(aes(Date, Soilmoist, col=Soildepth, alpha=0.3)) +
  facet_wrap(~Habitat)+
  theme_bw()

# calculate mean soiltemp and soilmoisture to gap fil NA
env_gapfil<- soil_envdata%>%
  mutate(month = month(Date),
         year = year(Date))%>%
  group_by(Habitat, Soildepth, month)%>%
  summarise(Soiltemp_month = mean(Soiltemp, na.rm = TRUE),
            Soilmoist_month = mean(Soilmoist, na.rm = TRUE))%>%
  ungroup()

soilflux_17181920<- left_join(soilflux_17181920, env_gapfil, by= c("month", "Habitat", "Soildepth"))


# total porosity & particle density https://www.arlis.org/docs/vol1/ICOP/55700698/Pdf/Chapter_230.pdf
#Porosity = recode(Plot, M = 0.892, W = 0.98, P = 0.899, S = 0.899) # Porosity based on literature; cm3/cm3 millington equation
#Deff = D0 * e^4/3(e/a)^2(T/273)^1.75
#a = (1- Bulk density/Particle density)
#e = (Total porosity-VWC)
#D0 is diffusivity in air, a is total porosity (1- Bulk density/Particle density), and e is air-filled porosity (Total porosity-VWC). The diffusion coefficient for CO2 in free air was 1.39  10^-5 m2 s1 [Gaudinski et al., 2000], and 1.7  10^-9 m2 s 1 in water [Jahne et al., 1987]. VWC not measured thus used a mean diffusion coefficient estimated from 2006 and 2007 at each gas well.

#############
# replace values of CH4 concentration higher than 2x atmospheric conc with NA for P and S plots, faulty GC measurements 
soilflux_17181920<- soilflux_17181920%>%
  mutate(CH4_ppm = ifelse(Habitat == "P" & CH4_ppm >= 4 | Habitat == "S" & CH4_ppm >= 4, NA, CH4_ppm))

soilflux_17181920<-soilflux_17181920%>%
  filter(!Habitat %in% c("AIR", "N/A"))%>%
  mutate(dz = ifelse(Soildepth == "40", 20, 10), # soil layer thickness; cm
         alpha = 1-BD_g.cm3/1.55, # total Porosity based on BD data and Partical Density value from literature: 1,55 g/cm3
         eta = alpha-Soilmoist_month)%>%# air filled porosity
  mutate(eta = ifelse(eta < 0, 0, eta))%>% # recode negative eta to 0
  gather(key = "soilgas", value = value, CO2_ppm, CH4_ppm, N2O_ppm)%>%
  group_by(Date)%>%
  arrange(desc(Soildepth))%>%
  group_by(Transect, Habitat, Date, soilgas)%>%
  mutate(dC = lead(value)- value)%>% # diff in soilgas conc between two adjacent layers, dC is negative when concentration is higher in lower layer
  mutate(dC = ifelse(Soildepth == 10 | soilgas == "CO2_pmm", 406.7-value, dC))%>% # atmospheric GHG content - conc at soildepth 10
  mutate(dC = ifelse(Soildepth == 10 | soilgas == "CH4_pmm", 1.85-value, dC))%>%
  mutate(dC = ifelse(Soildepth == 10 | soilgas == "N2O_pmm", 0.33-value, dC))%>%
  mutate(MolarMass = ifelse(soilgas == "CH4_pmm", 16.04, 44.01))%>%
  #filter(Plot == "P" | Plot == "S", soilgas == "CH4_ppm", value < 2)%>%   # only 34 values under 2
  ungroup()

# atmospheric content in 2017/2018 of CO2 = 407.6 ppm , CH4 = 1.850 ppm  and N2O = 0.33 ppm
#source: https://www.eea.europa.eu/data-and-maps/daviz/atmospheric-concentration-of-carbon-dioxide-5#tab-chart_5_filters=%7B%22rowFilters%22%3A%7B%7D%3B%22columnFilters%22%3A%7B%22pre_config_polutant%22%3A%5B%22CH4%20(ppb)%22%5D%7D%7D consulted feb 2020


# gas diffusivity in substrate in cm2/s
Diffusivity <- read_xlsx("Soilgas_diffusivity.xlsx")
soilflux_17181920 <- left_join(soilflux_17181920, Diffusivity, by = c("soilgas", "Habitat"))

#?! Should I add atmosphere layer for diffusion from 10cm layer to atmosphere?
#Deff = Diffusivity * Porosity^(4/3)* (alpha/Porosity)^2 * (Soiltemp/273)^1.75,
soilflux_17181920_layer<-soilflux_17181920 %>%
  mutate(Deff = Diffusivity * eta^(4/3)* (eta/alpha)^2 * ((Soiltemp_month+273.15)/273)^1.75)%>%
  mutate(Deff = ifelse(Habitat == "W", 0.00000000107, Deff))%>%
  mutate(Deff = ifelse(eta <= 0, 0.00000000107, Deff ), # diffusion in water
         Flux_ppm = -Deff*(dC/dz))
# need to recalculate to g/m2!

ggplot(soilflux_17181920_layer, aes(as.factor(month), Flux_ppm, fill = as.factor(Soildepth)))+
  geom_boxplot()+
  facet_wrap(soilgas~Habitat, scales = "free")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


soilflux_17181920_total<-soilflux_17181920_layer%>%
  group_by(month, Transect, Habitat, soilgas)%>%
  summarize(Soilflux = sum(Flux_ppm, na.rm = TRUE))%>%
  ungroup()

soilflux_17181920_total%>%
  filter(month > 5)%>%
  ggplot(aes(as.factor(month), Soilflux))+
  geom_boxplot()+
  facet_wrap(soilgas~Habitat, scales = "free")+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# cannot deal with negative eta or temperatures!! > frozen soil gives bad soil moisture measurement > leave out?
# sometimes positive Flux meaning uptake?

soilgas_17181920%>%
  #filter(Plot %in% c("S", "P"))%>%
  ggplot(aes(as.factor(Date), value, fill = as.factor(Soildepth)))+
  geom_boxplot()+
  facet_wrap(soilgas~Habitat, scales = "free")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

soilflux_17181920%>%
  filter(!Soilflux<0)%>%
  ggplot(aes(as.factor(Date), Soilflux, fill = Habitat))+
  geom_boxplot()+
  facet_wrap(soilgas~Habitat, scales = "free")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

soilflux_17181920%>%
  filter(soilgas=="CO2_ppm")%>%
  ggplot(aes(as.factor(Date), Soilflux, fill = Habitat))+
  geom_boxplot()+
  facet_wrap(~Habitat, scales = "free")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#NOTES
## filter out values based on extreme treshhold for GC values, but don't want to filter out values of other gasses when value of one gas is too high
#SOIL GAS CONCENTRATIONS:
#M and W plots:Use as is for all gases.
#P and S plots:CO2 should be fine.
#CH4 -> 10cm should be around ambient atmospheric CH4 conc. (around 2), and then for 20 and 40 cm, concentrations should decrease from ambient (<2).
#N2O: assume is fine.


# soil gas flux calculation, see Lee et al 2010
#CO2 Flux = Deff *dC/dz 
#Deff is gas diffusion coefficient in soil, dC is the difference of CO2 concentrations between two adjacent soil horizons, and dz is thickness of each soil horizon that was sampled. The gas diffusion coefficient (Deff) was estimated based on air-filled porosity in soil using Millington?s equation, as well as the CO2 gradient [Davidson and Trumbore, 1995; Gaudinski et al., 2000; Hirsch et al., 2002; Millington, 1959]. 

#Vogt Karolina, Christina Biasi GCB paper, citations of the paper to find methods
# Hirsh and Risk (complicated) good explanation of method

# find particle density

# porosity 
# particle density PD of 1,55 g/cm3 for the organic fraction (OM) and 2,65 g/cm3 for the mineral fraction (Carter 1993). 
# data on bulk density, VWC
# look up diffusivity of CH4 air and water

#Soil profile CO2 flux was estimated from the difference between CO2 concentrations at two different soil depths and the diffusion coefficient (equation (1)). The CO2 production for each layer was calculated as the difference between CO2 fluxes at two adjacent layers (equation (3)).
#CO2 production = Fi -Fi+1 Fi is flus one horizon and Fi+1 is flux in horizon below
#Soil CO2 production was estimated as a sum of CO2 production for each soil layer across the entire soil profile




#########################################################################################################################################################################
###### soiltemp modelling 
# read in Ibutton data
Temp_ibutton <- read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\iButton\\iButton_AllData_2017-2020.csv")%>%
  mutate(Date = as.Date(Date),
         Soildepth = as.factor(Soildepth))%>%
  filter(Treatment == "C")%>%
  filter(!Soildepth == 2)%>%
  group_by(Date, Habitat, Treatment, Soildepth)%>%
  summarise(T_ibutton = mean(Tmean, na.rm =TRUE))%>%
  ungroup()

# soiltemp vegetronix and Ibutton compare /model
Temp_match <- full_join(soiltemp, Temp_ibutton, by= c("Date", "Habitat", "Soildepth"))%>%
  select(-Treatment)%>%
  filter(!Habitat == "WG")%>%
  mutate(Soildepth = as.factor(Soildepth))
#cor(Temp_match$Soiltemp, Temp_match$T_ibutton, method = "pearson", use = "complete.obs") #0.97633

ggplot(Temp_match, aes(x= T_ibutton , y= Soiltemp, col= Soildepth))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Habitat)


######## Model relationship between Vegetronix and Ibutton data per habitat. Linear relationship between different depths of same habitat similar
test<- Temp_match%>%
  group_by(Date, Habitat, Soildepth)%>%
  mutate(P = ifelse(grepl("P", Habitat, fixed=TRUE), 1, 0),
         S = ifelse(grepl("S", Habitat, fixed=TRUE), 1, 0),
         M = ifelse(grepl("M", Habitat, fixed=TRUE), 1, 0),
         W = ifelse(grepl("W", Habitat, fixed=TRUE), 1, 0))%>%
  ungroup()

tempmodel<-lm(Soiltemp ~ T_ibutton + P + S + M + W, test)

TempModelled <-test%>% 
  mutate(modelTemp = ifelse( Habitat == "S", coef(tempmodel)[1] + T_ibutton * coef(tempmodel)[2] + S * coef(tempmodel)[4] ,
                             ifelse( Habitat == "P", coef(tempmodel)[1] +  T_ibutton * coef(tempmodel)[2] + P * coef(tempmodel)[3],
                                     ifelse( Habitat == "M",coef(tempmodel)[1] + T_ibutton * coef(tempmodel)[2] + M * coef(tempmodel)[5],
                                             ifelse( Habitat == "W", coef(tempmodel)[1] + T_ibutton * coef(tempmodel)[2] , NA)))))

TempModelled2<- TempModelled%>%
  mutate(Soildepth = recode(Soildepth, "10" = "ten", "40" = "forty", "20" = "twenty"))%>%
  group_by(Date, Habitat, Soildepth)%>%
  spread(Soildepth, Soiltemp, fill = NA)%>%
  ungroup()

TempModelled2<-left_join(TempModelled2, TempModelled2, by=c("Date", "Habitat", "P", "S", "M" , "W"))%>%
  select(Date, Habitat, P, S, M , W , ten.x, forty.x, forty.y)

tempmodel40<-lm(forty.y ~ ten.x + P + S + M + W, TempModelled2)

ggplot(TempModelled2, aes(x= ten.x , y= forty.y, col= Habitat))+
  geom_point()+
  geom_smooth(method = "lm")

TempModelled <-TempModelled%>% 
  mutate(modelTemp = ifelse( Habitat == "S", coef(tempmodel)[1] + T_ibutton * coef(tempmodel)[2] + S * coef(tempmodel)[4] ,
                             ifelse( Habitat == "P", coef(tempmodel)[1] +  T_ibutton * coef(tempmodel)[2] + P * coef(tempmodel)[3],
                                     ifelse( Habitat == "M",coef(tempmodel)[1] + T_ibutton * coef(tempmodel)[2] + M * coef(tempmodel)[5],
                                             ifelse( Habitat == "W", coef(tempmodel)[1] + T_ibutton * coef(tempmodel)[2] , NA)))))




# missing data for M 2018-06-21 to 2018-07-12; P,S,W 2018-07-01 to 2018-07-12
iButtonData_10cm<- iButtonData%>%
  filter(!Habitat == "WG",
         !Treatment == "OTC",
         Soildepth == 10 )%>%
  complete(Date = seq.Date(min(Date), max(Date), by= "day"), Habitat)%>%
  group_by(Habitat)%>%
  fill(Tmean, .direction = "up")%>% # fill missing values with Tmean of 2018-07-13
  mutate(P = ifelse(grepl("P", Habitat, fixed=TRUE), 1, 0),
         S = ifelse(grepl("S", Habitat, fixed=TRUE), 1, 0),
         M = ifelse(grepl("M", Habitat, fixed=TRUE), 1, 0),
         W = ifelse(grepl("W", Habitat, fixed=TRUE), 1, 0),
         WG = ifelse(grepl("WG", Habitat, fixed=TRUE), 1, 0))%>%
  ungroup()

# fill in with mean temp of date before and after?
#gapfil_Ibutton <- iButtonData%>%
#filter( Date > as.Date("2018-06-29") )%>%
#  filter(Date < as.Date("2018-07-14"))%>%
#  filter(Soildepth == 10,
#         Treatment != "OTC")%>%
#  group_by(Habitat, Treatment)%>%
#  summarise(Tmean = mean(Tmean, ))

# fill in NA's in Soiltemp with data from Ibutton
Temp_match$Soiltemp[is.na(Temp_match$Soiltemp)] <-Temp_match$T_ibutton[is.na(Temp_match$Soiltemp)]
#ggplot(Temp_match, aes(Soil_temp, T_ibutton, col=as.factor(Soil_depth)))+geom_point()+geom_smooth(method = "lm")+ facet_wrap(~Type)



################## MAP plots


#library(R.matlab) # cite: Henrik Bengtsson (2017). R.matlab: Read and Write MAT Files and Call MATLAB from Within R. R package version 3.6.1-9000. https://github.com/HenrikBengtsson/R.matlab
#Sebastian_Tloggers<-readMat("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\iButton\\Sebastian_logger_data\\Iskoras_logger_data.mat")

#locMat2016 <- t(apply(X = Sebasdata2, FUN = function(curMat) {
#  setNames(as.numeric(curMat), rownames(curMat))
#}, MARGIN = 3))

#restructureLoggersAsFrame <- function(inData) {
  # Firstly retrieve the logger positional data and ID
#  idFrame <- as.data.frame(t(apply(X = inData$loc, FUN = function(curMat) {
#    setNames(as.numeric(curMat), rownames(curMat))
#  }, MARGIN = 3)))
  # Secondly go over each of the yearly data
#  yearDataFrame <- do.call(rbind.fill, lapply(X = names(inData)[grepl("^y", names(inData), perl = TRUE)], FUN = function(yearText, inData) {
    # Retrieve the data in the current year
#    curYearData <- inData[[yearText]]
    # Retrieve the year
#    curYear <- as.integer(gsub("^y", "", yearText, perl = TRUE))
    # Restructure all information into a data frame
#    outFrame <- do.call(rbind, apply(X = curYearData, FUN = function(curLoggerData, curYear) {
      # Retrieve the number of records made by the current logger
#      numR <- nrow(curLoggerData["time", 1][[1]])
#      loggerData <- NULL
#      if(!is.null(numR) && numR > 0) {
        # Restructure the logger data (if there is any)
#        loggerData <- cbind(as.data.frame(setNames(apply(X = curLoggerData, FUN = function(curElement, numR) {
#          outVec <- as.vector(curElement[[1]])
#          outVec[(1:numR - 1) %% length(outVec) + 1]
#        }, MARGIN = 1, numR = numR), rownames(curLoggerData))), data.frame(
#          year = rep(curYear, numR)
#        ))
#      }
#      loggerData
#    }, MARGIN = 3, curYear = curYear))
#    outFrame
#  }, inData = inData))
  # Merged the logger position information and the records
#  merge(yearDataFrame, idFrame, by = "loggerID")
#}
#loggerData <- restructureLoggersAsFrame(Sebastian_data)

#write.csv(loggerData, "C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\iButton\\Sebastian_logger_data\\Sebastian_LoggerData.csv")

# Sebastian Tloggers
# coordinates in UTM, zone 35 Northern Hemisphere
Tloggers<- read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\iButton\\Sebastian_logger_data\\Sebastian_LoggerData.csv")

#Convert a numeric  MATLAB datenum (days since 0000-1-1 00:00) to seconds in 
#the Unix epoch (seconds since 1970-1-1 00:00). Specify a time zone if the 
#input datenum is anything other than the GMT/UTC time zone. 
matlab2POS = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  # This next string of functions is a complete disaster, but it works.
  # It tries to outsmart R by converting the secs value to a POSIXct value
  # in the UTC time zone, then converts that to a time/date string that 
  # should lose the time zone, and then it performs a second as.POSIXct()
  # conversion on the time/date string to get a POSIXct value in the user's 
  # specified timezone. Time zones are a goddamned nightmare.
  return(as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
                                        tz = 'UTC'), format = '%Y-%m-%d %H:%M', 
                             tz = 'UTC', usetz = FALSE), tz = timez))
}

Tloggers$time<-matlab2POS(Tloggers$time)
Tloggers<-Tloggers%>%
  mutate(loggerID = as.numeric(loggerID),
         palsa= fct_explicit_na(palsa, na_level = "NA"),
         status= fct_explicit_na(status, na_level = "NA"))

Habitat<- Tloggers%>%
  filter(loggerID>24 & loggerID< 29 | loggerID>43 & loggerID<57 | loggerID>62 & loggerID<74 | loggerID>78 & loggerID<80)%>%
  filter(year== "2017")%>%
  distinct(loggerID, .keep_all= TRUE)%>%
  mutate(habitat =  ifelse(grepl("sat", moisturestat, fixed=TRUE) & grepl("cottongras", vegetation, fixed = TRUE), "WG", 
                                 ifelse(grepl("fen", landform, fixed=TRUE), "WG",
                                  ifelse(grepl("lake", landform, fixed= TRUE), "W",
                                    ifelse(grepl("bare", vegetation, fixed = TRUE), "S" ,
                                      ifelse(grepl("dry", moisturestat, fixed=TRUE), "P" , "M")
                                    
                                            )
                                        )
                                    )
                                )
                            )%>%
  dplyr::select(loggerID, habitat)

Tlogger_habitat<-left_join(Tloggers, Habitat, by= "loggerID")%>%
  filter(loggerID>24 & loggerID< 29 | loggerID>43 & loggerID<57 | loggerID>62 & loggerID<74 | loggerID>78 & loggerID<86)%>%
  filter(!remark == "Logger was not burried")%>% # bad data
  filter(!remark=="Logger at water level in ground")%>% # bad data
  filter(!loggerID %in% c(53, 70))%>% # located in lake
  filter(!is.na(habitat))
#write.csv(Tlogger_habitat, "C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\iButton\\Sebastian_logger_data\\LoggerHabitat_Sebastian.csv")

ggplot(Tlogger_habitat, aes(time, temp, col = habitat))+
  geom_point()+
  facet_grid(~habitat, scales = "fixed")

### LOAD GEOTIF ####
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rgdal)
library('latticeExtra')
#GDALinfo("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Iskoras_2018_ortho_10cm.tif")
orthophoto<- raster(x="C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Iskoras_2018_ortho_10cm.tif")

# Create map of logger location and plots
# create SpatialPointDataframe of location of Sebastians Tloggers
# prepare the 3 components: coordinates, data, and proj4string
Tlogger_location <- Tloggers%>%
  #filter(!status == "forest")%>%
  filter(!grepl("tree", vegetation))%>%
  distinct(loggerID, .keep_all= TRUE)

Loggercoords <- Tlogger_location[ , c("X", "Y")]   # coordinates
Loggerdata   <- Tlogger_location[ , 1:31]          # data
Loggercrs    <- CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") # proj4string of coords

# make the spatial points data frame object
spdf_logger <- SpatialPointsDataFrame(coords = Loggercoords,
                               data = Loggerdata, 
                               proj4string = Loggercrs)

#Location of selected loggers in study area
Tlogger_habitat2 <- Tlogger_habitat%>%
  distinct(loggerID, .keep_all= TRUE)

coords_habitat <- Tlogger_habitat2[ , c("X", "Y")]   # coordinates
data_habitat   <- Tlogger_habitat2[ , c("loggerID", "habitat")]          # data
crs_habitat   <- CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") # proj4string of coords

# make the spatial points data frame object
spdf_loggers <- SpatialPointsDataFrame(coords = coords_habitat,
                                    data = data_habitat, 
                                    proj4string = crs_habitat)

##################################################################################################################################################################################################
##### Create map with Iskoras field location
library(maps)
Iskoras<- data.frame(long = 25.29, lat = 69.34 )
Norway<-map_data("world", region = c("Norway"))

NorwayMap<-ggplot(Norway, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group), fill = NA, color = "black")+
  coord_map(xlim = c(4,30), ylim = c(55,72))+
  geom_point(data = Iskoras, aes(x = long, y = lat), color = "black", size = 3) +
  geom_text(data = Iskoras, aes(x = long, y = lat+0.3), color = "black", fontface = "bold", label = "69.34 N, 25.29 E")+
  scale_fill_grey()+
  labs(x= "longitude", y="latitude")+
  theme_void()


### LOAD GEOTIF ####
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rgdal)
library('latticeExtra')
#GDALinfo("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Iskoras_2018_ortho_10cm.tif")
orthophoto<- raster(x="C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Iskoras_2018_ortho_10cm.tif")

# make the spatial points object for study area
Studyarea<- data.frame(X = c(432758, 433065), Y =  c(7693198 , 7693383))
coords <- Studyarea[ , c("X", "Y")]   # coordinates
crs    <- CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") # proj4string of coords

spdf_area <- SpatialPoints(coords = coords,
                           proj4string = crs)

# Plot Locations
Plots<-read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\GPScoordinates.csv", header = TRUE, sep = ";", dec = ",")%>%
  mutate(Xdegrees = as.numeric(substring(X, 2, 3)),
         Xminutes = as.numeric(substring(X, 5,6))/60,
         Xseconds = as.numeric(substring(X, 8,10))/36000, #60000, gives approximate right position
         Ydegrees = as.numeric(substring(Y, 2, 3)),
         Yminutes = as.numeric(substring(Y, 5,6))/60,
         Yseconds = as.numeric(substring(Y, 8,10))/36000, #60000, gives approximate right position
         X = Xdegrees + Xminutes + Xseconds,
         Y = Ydegrees + Yminutes + Yseconds)

#Plot_cord.dec = SpatialPoints(cbind(Plots$long,Plots$lat),proj4string=CRS("+proj=longlat"))
#Plot_UTM35<- spTransform(Plot_cord.dec, CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#spdf_plots <- SpatialPointsDataFrame(coords = coords_habitat,
#                                       data = data_habitat, 
 #                                      proj4string = crs_habitat)
#

# prepare coordinates, data, and proj4string
Plotcoords <- Plots[ , c("Y", "X")]   # coordinates
Plotdata   <- Plots[ , 1:4]          # data
Plotcrs    <- CRS("+proj=longlat +ellps=WGS84") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_plots <- SpatialPointsDataFrame(coords  = Plotcoords,
                                     data = Plotdata, 
                                     proj4string = Plotcrs)

spdf_plot_UTM35<- spTransform(spdf_plots, CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


#coordinates(Plots)<- c("long", "lat")
#proj4string(Plots)<- CRS('+proj=longlat +ellps=WGS84')
#res<-spTransform(Plots, CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

# plotting instructions https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
studysite<- crop(orthophoto, spdf_area) #crop orthophoto to studysite based on logger locations
plot(studysite, box=TRUE, axes=TRUE, col = grey(1:100/100), main = "ISKORAS FEEDBACK")
points(coordinates(spdf_plot_UTM35),  pch = 17, col = "red")


plot(coordinates(spdf_plot_UTM35))
plotcolor<- c("red", "yellow", "green", "darkorange", "lightblue")
points(coordinates(spdf_plot_UTM35), spdf_plot_UTM35$Treatment, col = plotcolor[as.numeric(spdf_plot_UTM35$Habitat)])


# Plot locations
PlotLocations<-read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\FeedBack_PlotLocations.txt", header = FALSE)
PlotLocations<- PlotLocations%>%
    separate(V1, c("Transect", "Plot"))

PlotLocations_UTM35<-read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\PlotLocations.csv", header = TRUE, sep = ";")
PlotLocations<-left_join(PlotLocations, PlotLocations_UTM35, by=c("Transect", "Plot"))
PlotLocations$Plot<- as.factor(PlotLocations$Plot)

# prepare the 3 components: coordinates, data, and proj4string
coords_plots <- PlotLocations[ , c("X", "Y")]   # coordinates
data_plots   <- PlotLocations[ , 1:2]          # data
crs_plots    <- CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") # proj4string of coords

# make the spatial points data frame object
spdf_plots <- SpatialPointsDataFrame(coords = coords_plots,
                                 data = data_plots, 
                                 proj4string = crs_plots)


# plotting instructions https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
studysite<- crop(orthophoto, spdf_area) #crop orthophoto to studysite based on logger locations
plotcolor<- c("green", "white", "white", "blue")
habitatcolor<- c("red", "yellow", "green", "blue", "white", "purple")
plot(studysite, box=FALSE, axes=FALSE, col = grey(1:100/100), main = "ISKORAS FEEDBACK")
points(coordinates(spdf_loggers), pch= 19, col = habitatcolor[as.factor(spdf_loggers$habitat)]) # , col = as.factor(spdf_loggers$habitat)
points(coordinates(spdf_plots), pch = 17, col = plotcolor[as.numeric(spdf_plots$Plot)])
text(coordinates(spdf_loggers), labels = spdf_loggers$loggerID, pos = 4, offset = 0.1, col=habitatcolor[as.factor(spdf_loggers$habitat)])
text(coordinates(spdf_plots), labels = spdf_plots$Plot, pos = 4, offset = 0.1, col=plotcolor[as.numeric(spdf_plots$Plot)])




Logger_location<-read.csv("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\FINNMARK2019_PlotLocations.txt", header = FALSE)
# prepare the 3 components: coordinates, data, and proj4string
xy<-Logger_location%>%
  mutate(V17 = as.character(V17))%>%
  mutate(Year = as.factor(substr(V17, nchar(V17)-4+1, nchar(V17))))%>%
  dplyr::select("V1", "V2", "V3", "Year")

coordinates(xy)<- c("V3", "V2")
proj4string(xy)<- CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
res<-spTransform(xy, CRS("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

# plotting instructions https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
studysite<- crop(orthophoto, spdf_logger) #crop orthophoto to studysite based on logger locations
plot(studysite, box=FALSE, axes=FALSE, col = grey(1:100/100), main = "ISKORAS FEEDBACK")
points(coordinates(res), pch= 19, col= res$Year) #, col = statuscolor[(spdf_logger$palsa)] 
plotcolor<- c("red", "yellow", "green", "darkorange", "lightblue", "white")
points(coordinates(spdf_plots), pch = 17, col = plotcolor[as.numeric(spdf_plots$Plot)])
text(coordinates(res), labels =res$Year, col = "lightgrey", pos = 2, offset = 0.1)
text(coordinates(spdf_plots), labels = spdf_plots$Plot, pos = 4, offset = 0.1, col=plotcolor[as.numeric(spdf_plots$Plot)])


plot(spdf, add=TRUE)
plot(spdf_2, add=TRUE)


