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
  select(PlotID:moss)%>%
  column_to_rownames(var="PlotID")

VegMatrix[is.na(VegMatrix)] <- 0 # replace NA with 0 cover value
VegMatrix<-as.matrix(VegMatrix)

# NMDS analysis with Bray-Curtis distance which is not affected by number of null values between samples like Euclidean distance
Iskoras_NMDS<-metaMDS(VegMatrix, distance="bray")
stressplot(Iskoras_NMDS)

#plot NMDS
plot_NMDS <- scores(Iskoras_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  mutate(PlotID = site)%>%
  select(-site)%>%
  full_join(VegComp2021, by = "PlotID")%>%
  mutate(Habitat = as.factor(Habitat))

plot_nmds <- ggplot(plot_NMDS, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill= Habitat, shape = Treatment), size = 3, alpha = 0.8) +
  stat_ellipse(aes(linetype = Treatment, col = Habitat), size = 1) +
  scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat", labels = c("Palsa", "Thawslump", "Vegetated Pond"))+
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat", labels = c("Palsa", "Thawslump", "Vegetated Pond"))+ 
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  scale_linetype_manual(values= c("solid", "dashed"), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  labs(title = "NMDS")+
  theme_classic()
plot_nmds

Iskoras_env <-VegComp2021%>%
  select(Habitat, Treatment, Transect)

### analysis of dissimilarites a.k.a. non-parametric
### permutational anova
adonis2(VegMatrix ~ Habitat * Treatment, strata = Iskoras_env$Transect, data=Iskoras_env, perm=999 )

#Significant differences between Habitat, minor significance for Treatment and Habitat:treatment interaction 


#### TRAIT DATA
### !!! for relatively small leaves 3 leaf samples were taken

Traitdata_raw<-read.csv2("VegetationData\\LeafTraits\\LeafTraits_Iskoras_leaves.csv")
Traitdata<- Traitdata_raw%>%
  mutate(LDMC1 = Dry_weight1/Wetweight_1,
         LDMC2 = Dry_weight2/Wetweight_2,
         LDMC3 = Dry_weight3/Wetweight_3,
         SLA = LA/Dry_weight1)%>% # use Dryweight 1 as that is leaf used for Leaf area scan
  #gather(measurement, LDMC, LDMC1:LDMC3)%>%
  #group_by(SampleID, Species, Sample, Treatment, Habitat)%>%
  #summarise_if(is.numeric, mean, na.rm = TRUE)%>% # calculate average LDMC per SampleID
  group_by(SampleID, Species, Sample, Treatment, Habitat)%>%
  gather(measurement, LT, LT1:LT3)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>% #calculate average leaf thickness per SampleID
  ungroup()

Traitdata%>%
  select(Species,Treatment, Habitat, VH, LA, LDMC1, SLA, LT)%>%
  gather(Trait, value, VH:LT)%>%
  ggplot(aes(Species, value, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Trait~Habitat, scales="free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

Traitdata%>%
  select(Species, Habitat, VH, LA, LDMC1, SLA, LT)%>%
  gather(Trait, value, VH:LT)%>%
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


# Mean trait data per species
SpeciesTraits<- Traitdata%>%
  group_by(Species, Treatment)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>% #calculate average traits per species, treatment, habitat
  select(Species, Treatment, VH, LA, LDMC, SLA, LT)%>%
  ungroup()

TraitMatrix<- SpeciesTraits%>%
  unite(Species_Treatment, c("Species", "Treatment"))%>%
  filter(!Species_Treatment == "Bet.nan_P_C")%>% # remove Bet.nan control P values as not in vegetation data
  column_to_rownames(var="Species_Treatment")

TraitMatrix[is.na(TraitMatrix)] <- 0 # replace NA with 0
TraitMatrix<-as.matrix(TraitMatrix)

SpeciesTreatmentMatrix<- VegComp2021%>%
  gather(Species, cover, And.pol:Eri.vag)%>%
  unite(Species_Treatment, c("Species", "Treatment"))%>%
  select(PlotID, Species_Treatment, cover)%>%
  spread(Species_Treatment, cover)%>%
  column_to_rownames(var="PlotID")

SpeciesTreatmentMatrix[is.na(SpeciesTreatmentMatrix)] <- 0 # replace NA with 0
SpeciesTreatmentMatrix<-as.matrix(SpeciesTreatmentMatrix[, colSums(SpeciesTreatmentMatrix) != 0])

# calculate CWM traits 
library(FD)
FD_Traits<-dbFD(TraitMatrix, SpeciesTreatmentMatrix, w.abun= TRUE, calc.CWM = TRUE)
Iskoras_CWM<-FD_Traits$CWM%>%
  rownames_to_column(var = "PlotID_Treatment")%>%
  mutate(PlotID = PlotID_Treatment)%>%
  mutate(Treatment = str_extract(PlotID, "[^_]+$"),
         Transect = substring(PlotID_Treatment, 1,1),
         Habitat = substring(PlotID_Treatment, 2,3),
         Habitat = sub("_", "", Habitat))

Iskoras_CWM%>%
  gather(Trait, value, VH:LT)%>%
  ggplot(aes(Habitat, value, fill = Treatment))+
  geom_boxplot()+
  facet_wrap(~Trait, scales = "free")

VegComp2021_Traits<- left_join(VegComp2021, Iskoras_CWM, by = c("PlotID", "Habitat", "Treatment"))

#PCA
library(ggfortify)
library(devtools)
library(factoextra)

TraitPCA<- VegComp2021_Traits%>%
  select(VH, LA, SLA, LT, LDMC)
TraitPCA <- princomp(TraitPCA, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

PCAplot<- autoplot(TraitPCA, data = VegComp2021_Traits,  size = 4, fill= "Habitat", shape = "Treatment",
                   loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat", labels = c("Palsa", "Thawslump", "Vegetated Pond"))+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()
PCAplot

Vegetation_P<- VegComp2021_Traits%>%
  filter(Habitat == "P")

Trait_P<- VegComp2021_Traits%>%
  filter(Habitat == "P")%>%
  select(VH, LA, SLA, LT, LDMC)

TraitPCA_P <- princomp(Trait_P, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

palsaPCA<- autoplot(TraitPCA_P, data = Vegetation_P,  size = 4, fill= "Habitat", shape = "Treatment",
                   loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#fc8d62"), name = "Habitat", labels = c("Palsa"))+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()
palsaPCA


Vegetation_M<- VegComp2021_Traits%>%
  filter(Habitat == "M")

Trait_M<- VegComp2021_Traits%>%
  filter(Habitat == "M")%>%
  select(VH, LA, SLA, LT, LDMC)

TraitPCA_M <- princomp(Trait_M, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

thawslumpPCA<- autoplot(TraitPCA_M, data = Vegetation_M,  size = 4, fill= "Habitat", shape = "Treatment",
                    loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                    loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#66c2a5"), name = "Habitat", labels = c("Thawslump"))+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()
thawslumpPCA


Vegetation_WG<- VegComp2021_Traits%>%
  filter(Habitat == "WG")

Trait_WG<- VegComp2021_Traits%>%
  filter(Habitat == "WG")%>%
  select(VH, LA, SLA, LT, LDMC)

TraitPCA_WG <- princomp(Trait_WG, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

VegetatedpondPCA<- autoplot(TraitPCA_WG, data = Vegetation_WG,  size = 4, fill= "Habitat", shape = "Treatment",
                        loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                        loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#8da0cb"), name = "Habitat", labels = c("Vegetated Pond"))+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()
VegetatedpondPCA

library(cowplot)
plot_grid(PCAplot, palsaPCA, thawslumpPCA, VegetatedpondPCA, labels= c("A", "B", "C", "D"))

# leaf thicknes in same direction as VH?
  
# NDVI data
# group by month and year!
NDVIdata<-read.csv2("VegetationData\\NDVI_Greenseeker.csv")%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"),
         Month = format(as.Date(Date, format="%d/%m/%Y"),"%m"),
         Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y"))%>%
  filter(!grepl("water", Comment))%>% #filter out plots that were fully or partially under water
  gather(Measurement, NDVI, Value1:Value2)%>%
  group_by(Year, Month, Transect, Habitat, Treatment, PlotID)%>%
  summarise(NDVI = mean(NDVI, na.rm=TRUE))%>%
  ungroup()

NDVImean<- NDVIdata%>%
  group_by(Month, Habitat, Treatment)%>%
  summarise(NDVI.sd = sd(NDVI, na.rm = TRUE),
            NDVI.mean = mean(NDVI, na.rm=TRUE))%>%
  ungroup()

ggplot(NDVImean, aes(as.factor(Month), NDVI.mean, color= Habitat, shape= Treatment)) +
  geom_point(position = position_dodge(0.8), size=2) +
  geom_errorbar(aes(ymin=NDVI.mean-NDVI.sd, ymax=NDVI.mean+NDVI.sd), position = position_dodge(0.8), width=.2)+
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat", labels = c("Palsa", "Thawslump", "Vegetated Pond"))+
  scale_shape_manual(values= c(17,19), name = "Treatment", labels = c("Control", "OTC"))+
  facet_grid()+
  theme_bw()

####################################################################################################################################
####### Abiotic conditions (TOMST loggers)
TomstData<-read.csv("AnalysisR\\TOMSTdata_SMcalculated.csv")

#
### calculate mean temp for habitat and treatments
TomstData<-TomstData%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  select(PlotID:LoggerID, Date, Date_Time, SoilTemperature:RawSoilmoisture, Soilmoisture_Volumetric)%>%
  mutate(Date = as.Date(Date),
         DateTime = as.POSIXct(strptime(Date_Time, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")),
         Hour = hour(DateTime))

# summary Hourly per Transect and Habitat
TomstData_MeanHourlyTransect<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Transect, Date, Hour, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))

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
  group_by(Habitat, Treatment, Date, Hour, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))

# plot summerdays hourly
TomstData_MeanHourlyHabitat%>%
  filter(Date == "2021-07-01")%>%
  filter(Climate_variable %in% c("AirTemperature", "GroundTemperature", "SoilTemperature", "Soilmoisture_Volumetric"))%>%
  ggplot(aes(Hour, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Climate_variable~Habitat, scales="free")

##### DAILY
# Summary Daily Per Transect and Habitat
TomstData_MeanDailyTransect<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat,Treatment, Transect, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))

# plot summer period
TomstData_MeanDailyTransect%>%
  filter(Date > "2022-06-01" & Date <"2022-09-01")%>%
  filter(Climate_variable %in% c("SoilTemperature"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

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
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

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

## NEED TO CHECK whether I need to do conversion still
# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp)) 
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
# think about using FluxCalR of gasfluxes for processing data

#Soil Respiration
# ### HMR output based on V  in L, A in m2, CH4 in ppb, C02 in ppm

# load HMR output 2020; collar Area () volume taken into account
SR2020_CO2<-read.csv("SR2020_CO2_HMRoutput.csv")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "comment"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3))%>%
  unite(PlotID, Plot:Treatment, remove = FALSE)%>%
  select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  rowid_to_column(var='FluxID')

# load HMR output, collar volume taken into account 
SR2021_CO2<-read.csv("SR2021_CO2_HMRoutput.csv")%>%
  separate(PlotID, sep = "_", into = c("Plot", "Treatment"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  rename(FluxID = X)%>%
  select(-Plot)

# bind together all SR CO2 flux data
SR20202021_CO2<- rbind(SR2020_CO2, SR2021_CO2)%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

# SR cH4 data
SR2021_CH4<-read.csv("SR2021_CH4_HMRoutput.csv")%>%
  separate(PlotID, sep = "_", into = c("Plot", "Treatment"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE)%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))%>%
  rename(FluxID = X)%>%
  select(-Plot)
  

## Environmental data 2020 and 2021
SRenvdata04102020<-read.csv2("2020\\SRmetadata_04102020.csv")
SRenvdata08092020<-read.csv2("2020\\SRmetadata_08092020.csv")
SRenvdata11082020<-read.csv2("2020\\SRmetadata_11082020.csv")
SRenvdata1507020<-read.csv2("2020\\SRmetadata_15072020.csv")
SRenvdata17072020<-read.csv2("2020\\SRmetadata_17072020.csv")
SRenvdata2020<-rbind(SRenvdata1507020, SRenvdata17072020, SRenvdata11082020, SRenvdata08092020, SRenvdata04102020 )%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat = recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))

SRenvdata04062021<-read.csv2("2021\\SRmetadata_04062021.csv")
SRenvdata19082021<-read.csv2("2021\\SRmetadata_19082021.csv")
SRenvdata19082021_2<-read.csv2("2021\\SRmetadata_19082021_2.csv")
SRenvdata12092021<-read.csv2("2021\\SRmetadata_12092021.csv")
SRenvdata21082021<-read.csv2("2021\\SRmetadata_21082021.csv")
SRenvdata22072021<-read.csv2("2021\\SRmetadata_22072021.csv")
SRenvdata30062021<-read.csv2("2021\\SRmetadata_30062021.csv")
SRenvdata2021<- rbind(SRenvdata04062021, SRenvdata30062021, SRenvdata22072021, SRenvdata19082021, SRenvdata19082021_2, SRenvdata21082021, SRenvdata12092021)%>%
  mutate(Date = recode(Date, "19.08.2021" = "18.08.2021", "21.08.2021" = "18.08.2021"))%>% #match envdata to flux data
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat = recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))

SRenvdata_20202021<- rbind(SRenvdata2020, SRenvdata2021)
  
# match CO2 fluxdata and envdata
SR_CO2_FluxEnv<- left_join(SRenvdata_20202021, SR20202021_CO2, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  mutate(Month = format(as.Date(Date, format="%d/%m/%Y"),"%m"),
         Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y"))
# TOO MANY ENTRIES, double matching

SR_CO2_FluxEnv_clean<-SR_CO2_FluxEnv%>%
  filter(Comment != "redo")%>%
  filter(Habitat != "W")%>%
  filter(CO2.f0 > 0) # remove negative values

ggplot(SR_CO2_FluxEnv_clean, aes(as.factor(Date), CO2.f0, fill=Treatment))+
  geom_boxplot()+
  geom_vline(xintercept = 4.5)+
  facet_grid(~Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# match CH4 fluxdata and envdata
SR_CH4_FluxEnv<- left_join(SRenvdata2021, SR2021_CH4, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"),
         Month = format(as.Date(Date, format="%d/%m/%Y"),"%m"),
         Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y"),
         Habitat = recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))
## TOO MANY ENTRIES!

SR_CH4_FluxEnv_clean<-SR_CH4_FluxEnv%>%
  filter(Habitat != "W")%>%
  filter(CH4.f0 < 1000) # remove extreme values

ggplot(SR_CH4_FluxEnv_clean, aes(as.factor(Date), CH4.f0, fill=Treatment))+
  geom_boxplot()+
  facet_grid(~Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### ? Use environmental data to predict fluxes, see Konsta paper on tundra
ggplot(SR_CO2_FluxEnv_clean, aes(SoilTemp1, CO2.f0, col=Treatment, shape =Treatment))+
  geom_point(size =1)+
  facet_grid(~Habitat)

ggplot(SR_CH4_FluxEnv_clean, aes(SoilTemp1, CH4.f0, col=Treatment, shape =Treatment))+
  geom_point(size =1)+
  facet_grid(~Habitat)

##### NET ECOSYSTEM EXCHANGE 
# NEE chamber  V = 2.5 L, A = 0.0625 m2, CH4 in ppb, C02 in ppm

NEE2021_CO2<-read.csv("2021\\HMRoutput_NEE2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)

NEE2021_CH4<-read.csv("2021\\HMRoutput_NEE2021_CH4.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)


NEE2021_CO2CH4<- inner_join(NEE2021_CO2, NEE2021_CH4, by= c("Date","PlotID","Transect", "Habitat", "Treatment", "FluxID", "Cover"))

## read in metadata
NEEenvdata03062021<-read.csv2("2021\\NEEmetadata_03062021.csv")
NEEenvdata04062021<-read.csv2("2021\\NEEmetadata_04062021.csv")
NEEenvdata02072021<-read.csv2("2021\\NEEmetadata_02072021.csv")%>%
  select(-X)# Check PAR ECtower
NEEenvdata03072021<-read.csv2("2021\\NEEmetadata_03072021.csv")
NEEenvdata20072021<-read.csv2("2021\\NEEmetadata_20072021.csv")
NEEenvdata21072021<-read.csv2("2021\\NEEmetadata_21072021.csv")
NEEenvdata23072021<-read.csv2("2021\\NEEmetadata_23072021.csv")
NEEenvdata17082021<-read.csv2("2021\\NEEmetadata_17082021.csv")
NEEenvdata18082021<-read.csv2("2021\\NEEmetadata_18082021.csv")
NEEenvdata21082021<-read.csv2("2021\\NEEmetadata_21082021.csv")
NEEenvdata11092021<-read.csv2("2021\\NEEmetadata_11092021.csv")
NEEenvdata12092021<-read.csv2("2021\\NEEmetadata_12092021.csv")

NEEenvdata2021<- rbind(NEEenvdata03062021, NEEenvdata04062021, NEEenvdata02072021,NEEenvdata03072021, NEEenvdata20072021, NEEenvdata21072021, NEEenvdata23072021, NEEenvdata17082021, NEEenvdata18082021, NEEenvdata21082021, NEEenvdata11092021, NEEenvdata12092021)%>%
  mutate(FluxID = as.character(FluxID))

#!!!! CHECK PAR for NEE measurements 02072021, PAR sensor wrong 

# link Environmental data and fluxdata
NEE_FluxEnv2021<- left_join(NEEenvdata2021, NEE2021_CO2CH4, 
                            by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(plotDate = recode(Date, "03.06.2021" = "04.06.2021", 
                           "02.07.2021" = "03.07.2021", 
                           "20.07.2021" = "23.07.2021", "21.07.2021" = "23.07.2021", 
                           "17.08.2021" = "21.08.2021", "18.08.2021" = "21.08.2021", 
                           "11.09.2021" = "12.09.2021"))%>%
  mutate(plotDate = as.Date(plotDate, "%d.%m.%Y"),
         Date = as.Date(Date, "%d.%m.%Y"),
         Month = format(as.Date(Date, format="%d/%m/%Y"),"%m"))%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  filter(CH4.f0< 1000)%>%
  filter(CH4.f0>-1000)

# RECO measurements
NEE_FluxEnv2021%>%
  filter(Cover == "RECO")%>%
  ggplot(aes(as.factor(plotDate), CO2.f0, fill=Treatment))+
  geom_boxplot()+
  facet_grid(~Habitat, scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

NEE_FluxEnv2021%>%
  filter(Cover == "RECO")%>%
  ggplot(aes(as.factor(plotDate), CH4.f0, fill=Treatment))+
  geom_boxplot()+
  facet_grid(~Habitat, scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

NEE_FluxEnv2021%>%
  filter(Cover == "RECO")%>%
  ggplot(aes(SoilTemp1, CO2.f0, col=Treatment, shape =Treatment))+
  geom_point(size =1)+
  facet_grid(~Habitat, scales = "free")

NEE_FluxEnv2021%>%
  filter(Cover == "RECO")%>%
  ggplot(aes(SoilTemp1, CH4.f0, col=Treatment, shape =Treatment))+
  geom_point(size =1)+
  facet_grid(~Habitat, scales = "free")



# NEE measurements
NEE_FluxEnv2021%>%
  filter(Cover != "RECO")%>%
  ggplot(aes(as.factor(plotDate), CO2.f0, fill=Treatment))+
  geom_boxplot()+
  facet_grid(~Habitat, scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

NEE_FluxEnv2021%>%
  filter(Cover != "RECO")%>%
  ggplot(aes(as.factor(plotDate), CH4.f0, fill=Treatment))+
  geom_boxplot()+
  facet_grid(~Habitat, scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


