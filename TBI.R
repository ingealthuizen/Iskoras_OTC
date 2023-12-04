# TBI 2023
TBI<-read.csv2("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Decomposition\\TBI2023_long.csv")
TBI<- TBI%>%
  mutate(Teatype = substring(TeaID, 1,1),
         Habitat = substring(PlotID, 2,2),
         Treatment = sub("^[^_]*_", "", PlotID),
         massloss = Startweight-Endweight)

# clean data
# remove data with broken tea bags and lost material
# add weight of string and label to measurements without 



ggplot(TBI, aes(Habitat, massloss, fill= Treatment) )+
  geom_boxplot()+
  facet_grid(~Teatype)
