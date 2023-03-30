# C vs OTC Iskoras
setwd("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\")
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
#library(readxl)
library(vegan)

# Vegetation Composition Data
VegComposition<-read.csv2("VegetationData\\Vegetation_plot_visual.csv")

# Select 2021 vegetation data as this is most recent
VegComp2021<- VegComposition%>%
  filter(Year==2021)%>%
  filter(Habitat != "S") # Remove Soil palsas from vegetation analysis no vascular species

# Create a vegetation cover matrix
VegMatrix<- VegComp2021%>%
  select(PlotID:Sphagnum.sp)%>%
  column_to_rownames(var="PlotID")

VegMatrix[is.na(VegMatrix)] <- 0 # replace NA with 0 cover value
VegMatrix<-as.matrix(VegMatrix)

# NMDS analysis with Bray-Curtis distance which is not affected by number of null values between samples like Euclidean distance
Iskoras_NMDS<-metaMDS(VegMatrix, distance="bray")
stressplot(Iskoras_NMDS)

#extact NMDS scores for plots and species
data.scores <- scores(Iskoras_NMDS) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores.sites <- as.data.frame(data.scores$sites)
data.scores.sites$sitename <- rownames(data.scores.sites)  # create a column of site names, from the rownames of data.scores
data.scores.sites$Treatment <- VegComp2021$Treatment#  add the treatment variable 
data.scores.sites$Habitat <- VegComp2021$Habitat #  add Habitat variable
data.scores.sites<-data.scores.sites%>%
  mutate(Habitat =recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", WG= "Vegetated Pond")) # recode Habitat
data.scores.sites$Habitat <- factor(data.scores.sites$Habitat, levels = c("Vegetated Palsa", "Thawslump", "Vegetated Pond"))

species.scores <- as.data.frame(data.scores$species)
species.scores$species <- rownames(species.scores)

ggplot() + 
  geom_point(data=data.scores.sites, aes(x=NMDS1, y=NMDS2, shape=Treatment, fill= Habitat), size=4) + # add the point markers
  stat_ellipse(data=data.scores.sites, aes(x=NMDS1, y=NMDS2, linetype = Treatment, col = Habitat), linewidth = 1) +
  geom_text(data=species.scores, aes(x=NMDS1, y=NMDS2, label= species), alpha=0.9) +  # add the species labels
  scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat")+
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat")+ 
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  scale_linetype_manual(values= c("solid", "dashed"), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  labs(title = "NMDS")+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

Iskoras_env <-VegComp2021%>%
  select(Habitat, Treatment, Transect)

### analysis of dissimilarites a.k.a. non-parametric
### permutational anova
adonis2(VegMatrix ~ Habitat * Treatment, strata = Iskoras_env$Transect, data=Iskoras_env, perm=999 )
#Significant differences between Habitat, minor significance for Treatment  

###########################################################################################################################################################
#### TRAIT DATA
### !!! for relatively small leaves 3 leaf samples were taken, only 1st leave used in analysis

Traitdata_raw<-read.csv2("VegetationData\\LeafTraits\\LeafTraits_Iskoras_leaves.csv")
Traitdata<- Traitdata_raw%>%
  mutate(LDMC1 = Dry_weight1/Wetweight_1,
         LDMC2 = Dry_weight2/Wetweight_2,
         LDMC3 = Dry_weight3/Wetweight_3,
         SLA = LA/Dry_weight1)%>% # use Dryweight 1 as that is leaf used for Leaf area scan
  #gather(measurement, LDMC, LDMC1:LDMC3)%>%
  #group_by(SampleID, Species, Sample, Treatment, Habitat)%>%
  #summarise_if(is.numeric, mean, na.rm = TRUE)%>% # calculate average LDMC per SampleID
  #group_by(SampleID, Species, Sample, Treatment, Habitat)%>%
  #gather(measurement, LT, LT1:LT3)%>%
  #summarise_if(is.numeric, mean, na.rm = TRUE)%>% #calculate average leaf thickness per SampleID
  ungroup()

Traitdata%>%
  select(Species,Treatment, Habitat, VH, LA, LDMC1, SLA, LT1)%>%
  gather(Trait, value, VH:LT1)%>%
  ggplot(aes(Species, value, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Trait~Habitat, scales="free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

Traitdata%>%
  select(Species, Habitat, VH, LA, LDMC1, SLA, LT1)%>%
  gather(Trait, value, VH:LT1)%>%
  ggplot(aes(Habitat, value, fill=Habitat))+
  geom_boxplot()+
  facet_grid(Trait~Species, scales="free")


library(glmmTMB)
library(DHARMa)

# What distribution to use?
Traitmodel<- Traitdata%>%
  filter(!Species %in% c("Arc.alp", "Eri.vag"))%>%
  mutate(VHlog = log(VH),
         LAlog = log(SLA),
         LDMC1log = log(LDMC1),
         SLAlog = log(SLA),
         LTlog = log(LT))%>%
  filter(Habitat != "WG")


hist(log(Traitmodel$VH))
hist(log(Traitmodel$LA))
hist(log(Traitmodel$LDMC1))
hist(log(Traitmodel$SLA))
hist(log(Traitmodel$LT))

# Not sure how to parameterize model to test OTC treatment effect on Traits across 
m1 <- glmmTMB(VHlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m2 <- glmmTMB(VHlog ~ Treatment + Species:Treatment + (1|Habitat ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m2)
plotQQunif(m2)
plotResiduals(m2)

m1 <- glmmTMB(LAlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m1 <- glmmTMB(LDMC1log ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m1 <- glmmTMB(LTlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m1 <- glmmTMB(SLAlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)


#### Community weighted Trait Values
# Mean trait data per species, Habitat and  Treatment

# function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

# Plot mean + sd of trait values for each species in each habitat and treatment
Traitdata%>%
  select(SampleID, Species, Habitat, Treatment, VH, LA, LT1, LA, SLA, LDMC1)%>%
  gather(Trait, value, VH:LDMC1)%>%
  group_by(Species, Treatment, Habitat, Trait)%>%
  summarise(mean=mean(value), se=se(value))%>% #calculate mean, sd traits per species, treatment, habitat
  ggplot(aes(Habitat, mean, color=Treatment))+ 
  geom_point(position=position_dodge(width = 0.5), stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width = 0.5), width=.2)+
  facet_grid(Trait~Species, scales = "free")


# create traitmatrix for calculating community weighted trait values based on species, habitat and treatment specific values
SpeciesTrait<-  Traitdata%>%
  select(SampleID, Species, Habitat, Treatment, VH, LA, LT1, LA, SLA, LDMC1)%>%
  mutate(Habitat = recode(Habitat, WG ="M"))%>% #recode WG to M as trait values do not vary between these habitats
  group_by(Species, Treatment, Habitat)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

SpeciesCover<- VegComp2021%>%
  gather(Species, cover, And.pol:Eri.vag)%>%
  mutate(Trait_Habitat = recode(Habitat, WG = "M"))

CWMtraits <- left_join(SpeciesCover, SpeciesTrait, by= c("Species", "Treatment", "Trait_Habitat"="Habitat"))%>%
  drop_na(cover)%>%
  rename(LT = "LT1", LDMC = "LDMC1")%>%
  gather(Trait, value, VH:LDMC)%>%
  group_by(PlotID, Habitat, Treatment, Trait)%>%
  summarise(CWM = weighted.mean(value, cover, na.rm = TRUE))%>%
  mutate(Habitat =recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", WG= "Vegetated Pond")) # recode Habitat
CWMtraits$Habitat <- factor(CWMtraits$Habitat, levels = c("Vegetated Palsa", "Thawslump", "Vegetated Pond"))

CWMplot<- CWMtraits%>%
  group_by(Treatment, Habitat, Trait)%>%
  summarise(mean=mean(CWM), se=sd(CWM))%>% #calculate mean, se traits per species, treatment, habitat
  ggplot(aes(Habitat, mean, color=Treatment, shape= Treatment))+ 
  geom_point(position=position_dodge(width = 0.5), stat="identity", size= 4)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width = 0.5), width=.2)+
  scale_color_manual(values= c("grey", "red"), name = "Treatment", labels = c("C", "OTC"))+
  scale_shape_manual(values= c(19, 17), name = "Treatment", labels = c("C", "OTC"))+
  facet_grid(Trait~., scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")

#%>%
#  unite(Species_Habitat_Treatment, c("Species", "Habitat", "Treatment"))%>%
#  filter(!Species_Habitat_Treatment == "Bet.nan_P_C")%>% # remove Bet.nan control P values as not in vegetation data
#  column_to_rownames(var="Species_Habitat_Treatment")


VegComp2021_Traits<- left_join(VegComp2021, CWMtraits, by = c("PlotID", "Habitat", "Treatment"))%>%
  spread(Trait, CWM)%>%
  rename(LT = "LT1", LDMC = "LDMC1")%>%
  mutate(Habitat =recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", WG= "Vegetated Pond")) # recode Habitat
VegComp2021_Traits$Habitat <- factor(VegComp2021_Traits$Habitat, levels = c("Vegetated Palsa", "Thawslump", "Vegetated Pond"))

#PCA
library(ggfortify)
library(devtools)
library(factoextra)
library(cowplot)

# Not sure about LT measurments
TraitPCA<- VegComp2021_Traits%>%
  select(VH, LA, SLA, LDMC)
TraitPCA <- princomp(TraitPCA, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

PCAplot<- autoplot(TraitPCA, data = VegComp2021_Traits,  size = 4, fill= "Habitat", shape = "Treatment",
                   loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat", labels = c("Veg. Palsa", "Thawslump", "Veg. Pond"))+
  scale_shape_manual(values= c(21, 24), guide= "none")+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )
PCAplot

plot_grid(PCAplot, CWMplot, labels= c("A", "B" ))

# leaf thicknes in same direction as VH?
  

# NDVI data
se <- function(x) sd(x)/sqrt(length(x))
# group by month and year!
NDVIdata<-read.csv2("VegetationData\\NDVI_Greenseeker.csv")%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date))%>%
  filter(!grepl("water", Comment))%>% #filter out plots that were fully or partially under water
  gather(Measurement, NDVI, Value1:Value2)%>%
  group_by(Year, Month, Transect, Habitat, Treatment, PlotID)%>%
  summarise(NDVI = mean(NDVI, na.rm=TRUE))%>%
  ungroup()

NDVImean<- NDVIdata%>%
  group_by(Month, Habitat, Treatment)%>%
  summarise(NDVI.se = se(NDVI),
            NDVI.mean = mean(NDVI))%>%
  ungroup()%>%
  mutate(Habitat =recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
NDVImean$Habitat <- factor(NDVImean$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))


ggplot(NDVImean, aes(as.factor(Month), NDVI.mean, color= Habitat, shape= Treatment)) +
  geom_point(position = position_dodge(0.8), size=4) +
  geom_errorbar(aes(ymin=NDVI.mean-NDVI.se, ymax=NDVI.mean+NDVI.se), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  labs(x = "Month of the year", y= "NDVI")+
  theme_classic()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


####################################################################################################################################
####### Abiotic conditions (TOMST loggers)
#! NEED TO CHECK LOGGERID and LOCATION!!!
library(lubridate)
TomstData<-read.csv("AnalysisR\\TOMSTdata_SMcalculated.csv")
se <- function(x) sd(x)/sqrt(length(x))

### calculate mean temp for habitat and treatments
TomstData<-TomstData%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  select(PlotID:LoggerID, Date, Date_Time, SoilTemperature:RawSoilmoisture, Soilmoisture_Volumetric)%>%
  mutate(Date = as.Date(Date),
         DateTime = as.POSIXct(strptime(Date_Time, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")),
         Hour = hour(DateTime))

TomstData_HourlyPlotID<- TomstData%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date, Hour)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE), 
            GroundTemperature = mean(GroundTemperature, na.rm = TRUE),
            AirTemperature = mean(AirTemperature, na.rm = TRUE),
            Soilmoisture_Volumetric = mean(Soilmoisture_Volumetric, na.rm = TRUE))%>%
            ungroup()

# calculate min, max and mean for each Climatic variable measured by TOMST
ma <- function(x, n = 7){  stats::filter(x, rep(1 / n, n), sides = 2)} # moving average for 7 days 

DailyVariablity<- TomstData%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date)%>%
  gather(Climatevariable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  select(PlotID, Transect, Habitat, Treatment, Date, Climatevariable, value)%>%
  group_by(Habitat, Treatment, Date, Climatevariable)%>%
  summarise(min = min(value),
            max = max(value),
            mean = mean(value))%>%
  mutate(Treatment = factor(Treatment, c("OTC", "C"), ordered = T)) %>% 
  group_by(Habitat, Date, Climatevariable) %>% 
  mutate(diff_min = min - lag(min, 1),
         diff_max = max - lag(max, 1),
         diff_mean = mean - lag(mean, 1))%>%
  filter(Treatment != "C")

library(zoo)
DailyVariablity%>%
  gather(Difference, value, diff_min:diff_mean)%>%
  group_by(Habitat, Climatevariable)%>%
  mutate(moving_avg7 = rollmean(value, k=7, fill=NA, align='right'))%>%
  filter(Climatevariable != "RawSoilmoisture")%>%
  filter(Date > "2021-06-01" & Date <"2021-09-01" )%>%
  ggplot(aes(Date, value, col= Difference))+
  geom_point()+
  geom_line(aes(y=moving_avg7))+
  facet_grid(Climatevariable~Habitat, scales = "free")


# summary Hourly per Transect and Habitat
TomstData_MeanHourlyTransect<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Transect, Date, Hour, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se = se))

# Plot SoilTemperature 
TomstData_MeanHourlyTransect%>%
  filter(Date == "2021-07-01")%>%
  filter(Climate_variable %in% c("SoilTemperature"))%>%
  ggplot(aes(Hour, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

# Plot AirTemperature 
TomstData_MeanHourlyTransect%>%
  filter(Date == "2021-07-01")%>%
  filter(Climate_variable %in% c("AirTemperature"))%>%
  ggplot(aes(Hour, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

# Plot Soilmoisture
TomstData_MeanHourlyTransect%>%
  filter(Date == "2021-07-01")%>%
  filter(Climate_variable %in% c("Soilmoisture_Volumetric"))%>%
  ggplot(aes(Hour, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")


# summary Hourly per Habitat
TomstData_MeanHourlyHabitat<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  filter(Date > "2021-06-01" & Date <"2021-09-01")%>%
  group_by(Habitat, Treatment, Hour, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se =se))

# plot summer season hourly based on June-August data in 2021
TomstData_MeanHourlyHabitat%>%
  filter(Climate_variable %in% c("AirTemperature", "SoilTemperature"))%>%
  ggplot(aes(Hour, Mean, col= Habitat, linetype =Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  facet_grid(Climate_variable~Habitat, scales="free")

##### DAILY
# Summary Daily Per Transect and Habitat
TomstData_MeanDailyTransect<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se =se))

# plot summer period
TomstData_MeanDailyTransect%>%
  filter(Date > "2021-06-01" & Date <"2021-09-01")%>%
  filter(Climate_variable %in% c("SoilTemperature"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Treatment), alpha=0.3) +
  facet_grid(Climate_variable~Habitat, scales="free")

 TomstData_MeanDailyTransect%>%
  filter(Date > "2022-06-01" & Date <"2022-09-01")%>%
  filter(Climate_variable %in% c("AirTemperature"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

TomstData_MeanDailyTransect%>%
  filter(Date > "2022-06-01" & Date <"2022-09-01")%>%
  filter(Climate_variable %in% c("Soilmoisture_Volumetric"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Treatment), alpha=0.3) +
  facet_grid(~Habitat, scales="free")

# Summary per Habitat
TomstData_MeanDailyHabitat<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat,Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))

# plot summer period
TomstData_MeanDailyHabitat%>%
  #filter(Date > "2022-06-01" & Date <"2022-09-01")%>%
  filter(Climate_variable %in% c("AirTemperature", "SoilTemperature", "Soilmoisture_Volumetric"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  #geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Climate_variable~Habitat, scales="free")


#####################################################################################################################################
####### Cflux data
# SR 2021 data
library(here)
library(tidyverse)
library(ggplot2)


#Soil Respiration
# ### HMR output based on V  in L, A in m2, CH4 in ppb, C02 in ppm
# load HMR output 2020; collar Area and volume taken into account
SR2020_CO2<-read.csv("Cflux\\SR2020_CO2_HMRoutput.csv")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "comment"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3),
         Date = as.Date(Date, "%Y-%m-%d"))%>%
  unite(PlotID, Plot:Treatment, remove = FALSE)%>%
  #select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  rowid_to_column(var='FluxID')%>%
  select(-comment)

## Environmental data 2020 and 2021
SRenvdata04102020<-read.csv2("Cflux\\2020\\SRmetadata_04102020.csv")
SRenvdata08092020<-read.csv2("Cflux\\2020\\SRmetadata_08092020.csv")
SRenvdata11082020<-read.csv2("Cflux\\2020\\SRmetadata_11082020.csv")
SRenvdata1507020<-read.csv2("Cflux\\2020\\SRmetadata_15072020.csv")
SRenvdata17072020<-read.csv2("Cflux\\2020\\SRmetadata_17072020.csv")
SRenvdata2020<-rbind(SRenvdata1507020, SRenvdata17072020, SRenvdata11082020, SRenvdata08092020, SRenvdata04102020 )%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat = dplyr::recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))

SR2020_CO2_env<- left_join(SR2020_CO2, SRenvdata2020, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
SR2020_CO2_env<-left_join(SR2020_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
#!! for now recalculate flux with soiltemp as loggerID missing for some TOMSTloggers
SR2020_CO2_env<- SR2020_CO2_env%>%
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+SoilTemp1)))

# load HMR output, collar volume taken into account 
SR2021_CO2<-read.csv("Cflux\\2021\\HMRoutput_SR2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "H2O"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3),
         Date = as.Date(Date, "%Y-%m-%d"))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  rowid_to_column(var='FluxID')%>%
  select(-H2O)

# SR cH4 data
SR2021_CH4<-read.csv2("Cflux\\2021\\HMRoutput_SR2021_CH4.csv", dec = ".")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "H2O"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3),
         Date = as.Date(Date, "%Y-%m-%d"))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)%>%
  rowid_to_column(var='FluxID')%>%
  select(-H2O)
  

## Environmental data 2021
SRenvdata04062021<-read.csv2("Cflux\\2021\\SRmetadata_04062021.csv")
SRenvdata19082021<-read.csv2("Cflux\\2021\\SRmetadata_19082021.csv")
SRenvdata19082021_2<-read.csv2("Cflux\\2021\\SRmetadata_19082021_2.csv")
SRenvdata12092021<-read.csv2("Cflux\\2021\\SRmetadata_12092021.csv")
SRenvdata21082021<-read.csv2("Cflux\\2021\\SRmetadata_21082021.csv")
SRenvdata22072021<-read.csv2("Cflux\\2021\\SRmetadata_22072021.csv")
SRenvdata30062021<-read.csv2("Cflux\\2021\\SRmetadata_30062021.csv")
SRenvdata2021<- rbind(SRenvdata04062021, SRenvdata30062021, SRenvdata22072021, SRenvdata19082021, SRenvdata19082021_2, SRenvdata21082021, SRenvdata12092021)%>%
  mutate(Date = dplyr::recode(Date, "19.08.2021" = "18.08.2021", "21.08.2021" = "18.08.2021"))%>% #match envdata to flux data
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat = dplyr::recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))

# combine SR2021data with environmental data
SR2021_CO2_env<- left_join(SR2021_CO2, SRenvdata2021, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
SR2021_CO2_env<-left_join(SR2021_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
SR2021_CO2_env<- SR2021_CO2_env%>%
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+SoilTemp1)))

# bind together 2020 and 2021 CO2 flux data
SR20202021_CO2_env<- rbind(SR2020_CO2_env, SR2021_CO2_env)

# combine SR2021data CH4 with environmental data
SR2021_CH4_env<- left_join(SR2021_CH4, SRenvdata2021, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
SR2021_CH4_env<-left_join(SR2021_CH4_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
SR2021_CH4_env<- SR2021_CH4_env%>%
  filter(Method.CH4 != "No flux")%>%
  mutate(CH4flux = CH4.f0/(0.08205*(273.15+SoilTemp1)),
         CH4flux.LR = CH4.LR/(0.08205*(273.15+SoilTemp1)))


########################################################################################################################################
### DATA CLEANING & visualitions
SR20202021_CO2_env_clean<-SR20202021_CO2_env%>%
  mutate(Month = as.factor(month(Date)),
         Year = as.factor(year(Date)))%>%
  filter(Comment != "redo")%>%
  filter(Habitat != "W")%>%
  filter(CO2flux > 0)%>% # remove negative values
  #filter(CO2flux< 2.1)%>% #remove 3 outliers
  mutate(Habitat =dplyr::recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
SR20202021_CO2_env_clean$Habitat <- factor(SR20202021_CO2_env_clean$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))

# compute summary statistics
SR20202021_CO2_env_clean%>%
  group_by(Habitat, Treatment) %>%
  summarise(
    count = n(),
    mean = round(mean(CO2flux, na.rm = TRUE), 2),
    median = round(median(CO2flux, na.rm = TRUE), 2),
    sd = round(sd(CO2flux, na.rm = TRUE), 2),
    cv = round(sd/mean, 2),
  ) %>%
  ungroup()

# ANOVA to test Treatment and Habitat effect 
aov.org <- aov( CO2flux ~ Habitat * Treatment, data = SR20202021_CO2_env_clean,
  contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.org, type = 'III')

aov.log <- aov( log(CO2flux) ~ Habitat * Treatment, data = SR20202021_CO2_env_clean,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.log, type = 'III')

aov.rank <- aov( rank(CO2flux) ~ Habitat * Treatment, data = SR20202021_CO2_env_clean,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.rank, type = 'III')

res.org = aov.org$resid
res.log = aov.log$resid
res.rnk = aov.rank$resid

qqnorm(  res.org, pch = 20, main = "Original Data",
  cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.org)
plot(aov.org, 1, main = "Original Data")

qqnorm(  res.log, pch = 20, main = "Log-Transformed",
  cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.log)
plot(aov.log, 1, main = "Log-Transformed")

qqnorm(  res.rnk, pch = 20, main = "Rank-Transformed",
  cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.rnk)
plot(aov.rank, 1, main = "Rank-Transformed")

library(emmeans)
emmeans(aov.log, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(aov.log,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)

# SR for each habitat and treatment over summer seasons 2020 and 2021 
SR20202021_CO2_env_clean%>%
  ggplot(aes(Habitat, CO2flux, fill=Treatment))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=TRUE)+
  theme_classic()

SR20202021_CO2_env_clean%>%
  group_by(Habitat, Treatment)%>%
  summarise(CO2flux.se = se(CO2flux),
            CO2flux.sd = sd(CO2flux, na.rm=TRUE),
            CO2flux.mean= mean(CO2flux, na.rm=TRUE))%>%
  ggplot(aes(Habitat, CO2flux.mean, color= Habitat, shape= Treatment))+
  geom_point(position = position_dodge(0.8), size=4 )+
  geom_errorbar(aes(ymin=CO2flux.mean-CO2flux.sd, ymax=CO2flux.mean+CO2flux.sd), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

# supplement include seasonal pattern
SR20202021_CO2_env_clean%>%
  ggplot(aes(Month, CO2flux, fill=Treatment))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  facet_grid(~Habitat)+
  theme_classic()

SR20202021_CO2_env_clean%>%
  group_by(Habitat, Treatment, Month)%>%
  summarise(CO2flux.se = se(CO2flux),
            CO2flux.sd = sd(CO2flux, na.rm=TRUE),
            CO2flux.mean= mean(CO2flux, na.rm=TRUE))%>%
  ggplot(aes(Month, CO2flux.mean, color= Habitat, shape= Treatment))+
  geom_point(position = position_dodge(0.8), size=4 )+
  geom_errorbar(aes(ymin=CO2flux.mean-CO2flux.sd, ymax=CO2flux.mean+CO2flux.sd), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  facet_grid(~Habitat)+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


SR2021_CH4_env_clean<-SR2021_CH4_env%>%
  mutate(Month = as.factor(month(Date)),
         Year = as.factor(year(Date)))%>%
  filter(Habitat != "W")%>%
  filter(CH4flux < 45)%>%# remove extreme values
  mutate(Habitat = dplyr::recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
SR2021_CH4_env_clean$Habitat <- factor(SR2021_CH4_env_clean$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))

SR2021_CH4_env_clean%>%
  group_by(Habitat, Treatment)%>%
  summarise(CH4flux.se = se(CH4flux),
            CH4flux.sd = sd(CH4flux, na.rm=TRUE),
            CH4flux.mean= mean(CH4flux, na.rm=TRUE))%>%
  ggplot(aes(Habitat, CH4flux.mean, color= Habitat, shape= Treatment))+
  geom_point(position = position_dodge(0.8), size=4 )+
  geom_errorbar(aes(ymin=CH4flux.mean-CH4flux.sd, ymax=CH4flux.mean+CH4flux.sd), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  geom_hline(yintercept =0, color= "grey")+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

SR2021_CH4_env_clean%>%
  ggplot(aes(Habitat, CH4flux, fill=Treatment))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=TRUE)

# ANOVA to test Treatment and Habitat effect 
aov.org <- aov( CH4flux ~ Habitat * Treatment, data = SR2021_CH4_env_clean,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.org, type = 'III')

aov.rank <- aov( rank(CH4flux) ~ Habitat * Treatment, data = SR2021_CH4_env_clean,
                 contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.rank, type = 'III')

res.org = aov.org$resid
res.rnk = aov.rank$resid

qqnorm(  res.org, pch = 20, main = "Original Data",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.org)
plot(aov.org, 1, main = "Original Data")

qqnorm(  res.rnk, pch = 20, main = "Rank-Transformed",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.rnk)
plot(aov.rank, 1, main = "Rank-Transformed")

emmeans(aov.rank, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(aov.rank,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)



######################################################################################################################################################
##### NET ECOSYSTEM EXCHANGE 
# NEE chamber  V = 2.5 L, A = 0.0625 m2, CH4 in ppb, C02 in ppm
NEE2020_CO2<- read.csv("Cflux\\2020\\NEE2020_CO2_HMRoutput.csv", sep= ",")%>%
  rename(FluxID = X, CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))%>%
  mutate(PlotID = dplyr::recode(PlotID, "3WGA_OTC" = "3WGB_OTC"))

# load environmental metadata
meta.data1<- read.csv2("2020\\LiCOR850\\NEEmetadata_14072020.csv")
meta.data2<- read.csv2("2020\\LiCOR850\\NEEmetadata2_14072020.csv")
meta.data3<- read.csv2("2020\\LiCOR850\\NEEmetadata_15072020.csv")
meta.data4<- read.csv2("2020\\LiCOR850\\NEEmetadata2_15072020.csv")
meta.data5<- read.csv2("2020\\LiCOR850\\NEEmetadata_14082020.csv")
meta.data6<- read.csv2("2020\\LiCOR850\\NEEmetadata_15082020.csv")
meta.data7<- read.csv2("2020\\LiCOR850\\NEEmetadata_07092020.csv")
meta.data8<- read.csv2("2020\\LiCOR850\\NEEmetadata2_07092020.csv")
meta.data9<- read.csv2("2020\\LiCOR850\\NEEmetadata_08092020.csv")
meta.data10<- read.csv2("2020\\LiCOR850\\NEEmetadata_09092020.csv")
meta.data11<- read.csv2("2020\\LiCOR850\\NEEmetadata2_09092020.csv")
meta.data12<- read.csv2("2020\\LiCOR850\\NEEmetadata_04102020.csv")
meta.data13<- read.csv2("2020\\LiCOR850\\NEEmetadata_05102020.csv")
#Li7810
meta.data14<- read.csv2("2020\\LiCOR7810\\NEEmetadata_20062020.csv")
meta.data15<- read.csv2("2020\\LiCOR7810\\NEEmetadata_21062020.csv")

NEEenvdata2020<- rbind(meta.data1, meta.data2,meta.data3, meta.data4, meta.data5, meta.data6, meta.data7, meta.data8, meta.data9,  meta.data10, meta.data11, meta.data12, meta.data13, meta.data14, meta.data15)%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))

# combine SR2021data with environmental data
NEE2020_CO2_env<- left_join(NEE2020_CO2, NEEenvdata2020, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "Cover"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)


########## 2021 
NEE2021_CO2<-read.csv("2021\\Cfluxdata\\HMRoutput_NEE2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  #dplyr::select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)

NEE2021_CH4<-read.csv("2021\\Cfluxdata\\HMRoutput_NEE2021_CH4.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  #dplyr::select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)

## read in metadata
NEEenvdata03062021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_03062021.csv")
NEEenvdata04062021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_04062021.csv")
NEEenvdata02072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_02072021.csv")%>%
  dplyr::select(-X)# Check PAR ECtower
NEEenvdata03072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_03072021.csv")
NEEenvdata20072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_20072021.csv")
NEEenvdata21072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_21072021.csv")
NEEenvdata23072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_23072021.csv")
NEEenvdata17082021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_17082021.csv")
NEEenvdata18082021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_18082021.csv")
NEEenvdata21082021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_21082021.csv")
NEEenvdata11092021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_11092021.csv")
NEEenvdata12092021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_12092021.csv")

NEEenvdata2021<- rbind(NEEenvdata03062021, NEEenvdata04062021, NEEenvdata02072021,NEEenvdata03072021, NEEenvdata20072021, NEEenvdata21072021, NEEenvdata23072021, NEEenvdata17082021, NEEenvdata18082021, NEEenvdata21082021, NEEenvdata11092021, NEEenvdata12092021)%>%
  mutate(FluxID = as.character(FluxID))

# link Environmental data and CO2fluxdata
NEE2021_CO2_env<- left_join(NEE2021_CO2, NEEenvdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)

# bind together 2020 and 2021 NEE CO2 data
NEE20202021_CO2_env<- rbind(NEE2020_CO2_env, NEE2021_CO2_env)%>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date))

NEE2021_CH4_env<- left_join(NEE2021_CH4, NEEenvdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)

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



##############
# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
# for now using soilTemp1 but better with either chamber ibutton data/ TOMST logger data or EC airtemp data
NEE20202021_CO2_env<-NEE20202021_CO2_env%>%
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+SoilTemp1)))

NEE2021_CH4_env<- NEE2021_CH4_env%>%
  mutate(CH4flux = CH4.f0/(0.08205*(273.15+SoilTemp1)),
         CH4flux.LR = CH4.LR/(0.08205*(273.15+SoilTemp1)))

# RECO
NEE20202021_CO2_env%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  filter(Cover == "RECO")%>%
  filter(Comment != "redo")%>%
  filter(Method.CO2 != "No flux")%>%
  filter(CO2flux > 0)%>%
  ggplot(aes(as.factor(Month), CO2flux, fill=Treatment))+ 
  geom_boxplot()+
  facet_grid(~Habitat)

NEE20202021_CO2_env%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  filter(Cover == "RECO")%>%
  filter(Comment != "redo")%>%
  filter(Method.CO2 != "No flux")%>%
  filter(CO2flux > 0)%>%
  ggplot(aes(x=SoilTemp1+273.15, y=CO2flux, col=Habitat, shape= Treatment, linetype=Treatment))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~A*exp(-308.56/I(x-227.13)), method.args = list(start=c(A=0)), se=FALSE, na.rm= TRUE)+
  facet_grid(~Habitat)+
  theme_bw()

NEE2021_CH4_env%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  #filter(Cover == "RECO")%>%
  filter(CH4flux < 25)%>%
  filter(CH4flux >-30)%>%
  filter(Method.CH4 != "No flux")%>%
  ggplot(aes(Habitat, CH4flux, col=Treatment))+ 
  geom_boxplot()+
  geom_hline(yintercept = 0)+
  facet_wrap(~Habitat, scales="free")

# calculate GPP 
RECO_CO2<-NEE20202021_CO2_env%>%
  filter(!grepl("R", PlotID))%>%
  filter(Cover== "RECO")%>%
  filter(Comment != "redo")%>%
  filter(CO2flux > 0)%>%
  rename( CO2flux_RECO = CO2flux,  CO2flux.LR_RECO = CO2flux.LR )%>%
  dplyr::select(PlotID, Date, Year, Month, CO2flux_RECO, CO2flux.LR_RECO)

NEE_CO2<-NEE20202021_CO2_env%>%
  filter(!grepl("R", PlotID))%>%
  filter(Treatment %in% c("C",  "OTC"))%>%
  filter(Cover != "RECO")%>%
  filter(Method.CO2 != "No flux") # 11 measurements dropped

# Recalculate PAR based on shading cover, 
# need to check shading effect of covers
GPP_CO2<-left_join(NEE_CO2, RECO_CO2, by=c("PlotID", "Date", "Year", "Month"))%>%
  mutate(GPPflux = (CO2flux- CO2flux_RECO)*(-1))%>%
  drop_na(GPPflux)%>%
  group_by(PlotID, Transect, Habitat, Treatment, Cover, Date, Hour, Month, Year)%>%
  gather(PAR, value, PAR1:PAR3)%>%
  mutate(PAR.mean = mean(value, na.rm=TRUE))%>%
  distinct(GPPflux, .keep_all = TRUE)%>%
  mutate(PAR.mean = ifelse(Cover == 'NEE2', PAR.mean*0.33,
                         ifelse(Cover == 'NEE1', PAR.mean*0.67, PAR.mean)))%>%
  filter(GPPflux > 0)%>%
  ungroup()

group_by(PlotID, Transect, Habitat, Treatment, Cover, Date, Hour, Month, Year)%>%
  gather(PAR, value, PAR1:PAR3)%>%
  mutate(PAR.mean = mean(value, na.rm=TRUE))%>%
  unique()

#!!!! CHECK PAR for NEE measurements 02072021, PAR sensor wrong 
### process fluxes with fluxcalc function to get airtemp from chamber!

#### MODELLING GPP
# Link NDVI data to GPP 
GPP_NDVI<- left_join(GPP_CO2, NDVIdata, by=c("PlotID", "Treatment", "Habitat", "Transect",  "Year", "Month"))

CWMtraits_wide<- CWMtraits%>%
  spread(Trait, CWM)%>%
  ungroup()%>%
  select(-Habitat)

GPP_NDVI_CWM<-left_join(GPP_NDVI, CWMtraits_wide, by= c("PlotID", "Treatment"))

ggplot(GPP_NDVI_CWM, aes(NDVI, GPPflux))+
  geom_point(aes(col=Habitat))+
  geom_smooth(method = "lm")

library(glmmTMB)
library(lme4)
library(lmerTest)
library(DHARMa)

hist(log(GPP_NDVI_CWM$GPPflux))

fitGPP <- lmer(log(GPPflux) ~ scale(PAR.mean) + SoilTemp1 + SoilMoist1 + NDVI + VH + LA + SLA +LDMC + (1|PlotID) ,  data = GPP_NDVI_CWM)
summary(fitGPP)
anova(fitGPP)

hist(log(GPP_NDVI_CWM$CO2flux_RECO))
fitReco <- lmer(log(CO2flux_RECO) ~ SoilTemp1 + SoilMoist1 + NDVI + VH + LA + SLA +LDMC + (1|PlotID) ,  data = GPP_NDVI_CWM)
summary(fitReco)
anova(fitReco)



# https://www.nature.com/articles/s42003-023-04626-3 
# Bürkner, P.-C. Advanced bayesian multilevel modeling with the R Package brms. R J. 10, 395–411 (2018).
# Mac Nally, R. & Walsh, C. J. Hierarchical partitioning public-domain software. Biodivers. Conserv. 13, 659 (2004).
# Murray, K. & Conner, M. M. Methods to quantify variable importance: implications for the analysis of noisy ecological data. Ecology 90, 348–355 (2009).

# Bayesion PAR model
# These packages need to be loaded to run the model
#install.packages("nimble")
library(nimble)
library(DHARMa)
library(coda)
library(parallel)  # Adds the library that allows to detect the number of cores you have
library(future)    # Adds the library that allows asynchronous calling of R code
plan(multisession) # Allows the future library to do parallel processing


# Import your data (replace this line to whereever you store your data)
#CO2_mass_traits <- readRDS(paste(Sys.getenv("WORKSPACE_BASELOC"), "Work", "FUNCAB", "CO2_traits13022020.rds", sep = "/"))

# The source files have been removed from the FUNCAB directory (instead moved to the public PaGAn repository) so you can
# import the files directly from their GitHub links.  This means that if I update PaGAn then you automatically have the
# updated files
source("C:\\Users\\ialt\\OneDrive - NORCE\\FunCab\\Data\\FunCaB2\\TraitCO2\\mcmcInternals.R")
source("C:\\Users\\ialt\\OneDrive - NORCE\\FunCab\\Data\\FunCaB2\\TraitCO2\\glmNIMBLE.R")
source("C:\\Users\\ialt\\OneDrive - NORCE\\FunCab\\Data\\FunCaB2\\TraitCO2\\gppLightCurveCorrection.R")

GPP_NDVI_CWM_noNA<-GPP_NDVI_CWM%>%
  drop_na(NDVI)
  
GPPmodel <- gppLightCurveCorrection(
  # Set the input data
  inputData = GPP_NDVI_CWM_noNA,
  # Tell the model which column represents the light values (for the curve correction)
  lightValues = "PAR.mean",
  # Set the three sub-models for the model components.  You probably want to keep the x-assymtote model as an intercept-only model
  # and turn off the multiplier model (by setting the multiplier to 1.0).  This means only the y-asymptote will change with the
  # environmental covariates + VegetationHeight + Richness + Evenness + Diversity + CWM_N + CWM_C + CWM_CN + CWM_LDMC + CWM_LT + CWM_LA + CWM_SLA + CWM_VH + FDis_Traits
  yAsymModel = GPPflux ~ NDVI + VH + LA + SLA +LDMC, 
  xAsymModel = ~ 1,
  multiplierModel = 1,
  # Tell the model that you want to do LASSO regularisation
  regCoeffs = "lasso" , 
  # Define the set of indirect models that you also want to fit (check how the sub-models are defined)
  indirectComponents = list(
    list(modelFormula = CO2flux_RECO ~ SoilTemp1 + SoilMoist1 + VH + LA + SLA +LDMC, errorFamily = Gamma(link = "log"),
         regCoeffs = "lasso", 
         suffix = "_Reco_Model")
  ),
  # A vector of different PAR values you want to get predictions for (so that you can do PAR standardisation of the values)
  lightStandards = c(800),
  mcmcParams = list(numRuns = 100000, thinDensity = 400, predictThinDensity = 400),
  numCores = 0)

# Get convergence plots models
plot(GPPmodel$parameterSamples) 
plot(GPPmodel$parameterSamples) 

# Retrieve the DHARMa plot of the model
plot(GPPmodel$DHARMaResiduals) # GPP
plot(GPPmodel$DHARMaResiduals) # GPP

#Paramater values GPP models
GPPsummary<-GPPmodel$parameterSummary #summary of GPP model
GPPsummary<- as.data.frame(GPPsummary)%>% 
  mutate_if(is.numeric, round, 3)
GPPmodel$WAIC
GPPmodel$rSquared




# for bootstrapping
library(viridis)
library(broom)
library(investr)
library(lubridate)
library(patchwork)

#### separate datasets per habitat and Treatment
GPP_P_C <- GPP_CO2 %>%
  filter(Month %in% 7:8 )%>%
  filter(Habitat == "P", Treatment == "C") %>% 
  dplyr::select(GPPflux, PAR) %>% 
  na.omit() 

GPP_P_OTC <- GPP_CO2 %>% 
  filter(Month %in% 7:8 )%>%
  filter(Habitat == "P", Treatment == "OTC") %>% 
  dplyr::select(GPPflux, PAR) %>% 
  na.omit() 

GPP_M_C <- GPP_CO2 %>% 
  filter(Month %in% 7:8 )%>%
  filter(Habitat == "M", Treatment == "C") %>% 
  dplyr::select(GPPflux, PAR) %>% 
  na.omit() 

GPP_M_OTC <- GPP_CO2 %>% 
  filter(Month %in% 7:8 )%>%
  filter(Habitat == "M", Treatment == "OTC") %>% 
  dplyr::select(GPPflux, PAR) %>% 
  na.omit() 

GPP_WG_C <- GPP_CO2 %>% 
  filter(Month %in% 7:8 )%>%
  filter(Habitat == "WG", Treatment == "C") %>% 
  dplyr::select(GPPflux, PAR) %>% 
  na.omit() 

GPP_WG_OTC <- GPP_CO2 %>% 
  filter(Month %in% 7:8 )%>%
  filter(Habitat == "WG", Treatment == "OTC") %>% 
  dplyr::select(GPPflux, PAR) %>% 
  na.omit() 


## get coefficients for each landscape unit model
# model P
fit_P_C <- nls( GPPflux ~  (aGPP*GPPmax*PAR)/(aGPP*PAR+GPPmax), data = GPP_P_C, start = list(aGPP=0.01, GPPmax=2))
aGPP_P_C <- coef(summary(fit_P_C))[1]
GPPmax_P_C <- coef(summary(fit_P_C))[2]

new.data <- data.frame(PAR=seq(1, 2000, by = 10))
fitted_P_C <- as_tibble(predFit(fit_P_C, newdata = new.data, interval = "confidence", level =0.9 ))%>%
  mutate(PAR = new.data$PAR,
         Habitat = "P",
         Treatment = "C")

fit_P_OTC <- nls( GPPflux ~  (aGPP*GPPmax*PAR)/(aGPP*PAR+GPPmax), data = GPP_P_OTC, start = list(aGPP=0.01, GPPmax=2))
aGPP_P_OTC <- coef(summary(fit_P_OTC))[1]
GPPmax_P_OTC <- coef(summary(fit_P_OTC))[2]

fitted_P_OTC <- as_tibble(predFit(fit_P_OTC, newdata = new.data, interval = "confidence", level =0.9 ))%>%
  mutate(PAR = new.data$PAR,
         Habitat = "P",
         Treatment = "OTC")

# model M
fit_M_C <- nls( GPPflux ~  (aGPP*GPPmax*PAR)/(aGPP*PAR+GPPmax), data = GPP_M_C, start = list(aGPP=0.01, GPPmax=2)) 
aGPP_M_C <- coef(summary(fit_M_C))[1]
GPPmax_M_C <- coef(summary(fit_M_C))[2]

fitted_M_C <- as_tibble(predFit(fit_M_C, newdata = new.data, interval = "confidence", level =0.9 ))%>%
  mutate(PAR = new.data$PAR,
         Habitat = "M",
         Treatment = "C")

fit_M_OTC <- nls( GPPflux ~  (aGPP*GPPmax*PAR)/(aGPP*PAR+GPPmax), data = GPP_M_OTC, start = list(aGPP=0.01, GPPmax=2))
aGPP_M_OTC <- coef(summary(fit_M_OTC))[1]
GPPmax_M_OTC <- coef(summary(fit_M_OTC))[2]

fitted_M_OTC <- as_tibble(predFit(fit_M_OTC, newdata = new.data, interval = "confidence", level =0.9 ))%>%
  mutate(PAR = new.data$PAR,
         Habitat = "M",
         Treatment = "OTC")


# model WG
fit_WG_C <- nls( GPPflux ~  (aGPP*GPPmax*PAR)/(aGPP*PAR+GPPmax), data = GPP_WG_C, start = list(aGPP=0.01, GPPmax=2))
aGPP_WG_C <- coef(summary(fit_WG_C))[1]
GPPmax_WG_C <- coef(summary(fit_WG_C))[2]

fitted_WG_C <- as_tibble(predFit(fit_WG_C, newdata = new.data, interval = "confidence", level =0.9 ))%>%
  mutate(PAR = new.data$PAR,
         Habitat = "WG",
         Treatment = "C")

fit_WG_OTC <- nls( GPPflux ~  (aGPP*GPPmax*PAR)/(aGPP*PAR+GPPmax), data = GPP_WG_OTC, start = list(aGPP=0.01, GPPmax=2))
aGPP_WG_OTC <- coef(summary(fit_WG_OTC))[1]
GPPmax_WG_OTC <- coef(summary(fit_WG_OTC))[2]

fitted_WG_OTC <- as_tibble(predFit(fit_WG_OTC, newdata = new.data, interval = "confidence", level =0.9 ))%>%
  mutate(PAR = new.data$PAR,
         Habitat = "WG",
         Treatment = "OTC")


fitted_Habitat <- rbind(fitted_P_C, fitted_M_C, fitted_WG_C, fitted_P_OTC, fitted_M_OTC, fitted_WG_OTC)

plotGPP<-GPP_CO2 %>%
  filter(Month %in% 7:8 )%>%
  ggplot() +  
  geom_point(aes(x=PAR, y=GPPflux, col = Habitat, shape=Treatment)) + 
  xlab("PAR") + ylab("GPP")

plotGPP +  geom_line(data=fitted_Habitat, aes(x = PAR, y = fit, color = Habitat, linetype=Treatment ))+
  geom_ribbon(data=fitted_Habitat, aes(x=PAR, ymin=lwr, ymax=upr, fill= Treatment), alpha=0.3, inherit.aes=F)+
  scale_fill_viridis(discrete = TRUE)+
  theme( axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  facet_grid(~Habitat)

#fit.Reco_ALL<-nls((Reco~A*exp(-308.56/I(tempK-227.13))), start=c(A=0 ), data=CO2_RECO_1516Trait) #

#fit.GPP_ALL<-nls((GPP~ (A*B*PAR.x)/(A*PAR.x+B)), start=c(A=0.01, B=2), data=CO2_GPP_1516Trait)
















