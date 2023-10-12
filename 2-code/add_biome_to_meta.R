######written August 9, 2022 to map metadata to biomes from Olson 2001######
rm(list=ls());graphics.off()
setwd("~/Documents/GitHub/ML_Genes")
library(maptools);library(sf)
#require(spatialEco)

######read in data#####
shp = readShapePoly('terrestrial_ecosystems_teow/wwf_terr_ecos.shp')
meta.mg = readxl::read_excel("data/1000Soils_Metadata_Site_Mastersheet_v1.xlsx")

long.lat = 
  as.data.frame(cbind(meta.mg$Site_Code,meta.mg$Long,meta.mg$Lat)) 
names(long.lat) = c("name",'longitude','latitude')

long.lat  = 
  long.lat %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

#long.lat.no.na = long.lat[-which(is.na(long.lat$longitude)==T),]
#long.lat.no.na.data.frame = as.data.frame(long.lat[-which(is.na(long.lat$longitude)==T),])


#https://stackoverflow.com/questions/44170102/find-in-which-polygon-the-points-in-a-map-are
coordinates(long.lat) <- ~ longitude + latitude
# # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(long.lat) <- proj4string(shp)
poly = over(long.lat, shp)

eco.meta = cbind(long.lat,poly,rep(NA, nrow(poly)));names(eco.meta)[ncol(eco.meta)] = 'biome_name'
eco.meta = as.data.frame(eco.meta)
#set biome names, from readme file
#1, Tropical & Subtropical Moist Broadleaf Forests
#2, Tropical & Subtropical Dry Broadleaf Forests
#3, Tropical & Subtropical Coniferous Forests
#4, Temperate Broadleaf & Mixed Forests
#5, Temperate Conifer Forests
#6, Boreal Forests/Taiga
#7, Tropical & Subtropical Grasslands, Savannas & Shrublands
#8, Temperate Grasslands, Savannas & Shrublands
#9, Flooded Grasslands & Savannas
#10, Montane Grasslands & Shrublands
#11, Tundra
#12, Mediterranean Forests, Woodlands & Scrub
#13, Deserts & Xeric Shrublands
#14, Mangroves

eco.meta$biome_name[which(eco.meta$BIOME == 1)] = "Tropical & Subtropical Moist Broadleaf Forests"
eco.meta$biome_name[which(eco.meta$BIOME == 2)] = "Tropical & Subtropical Dry Broadleaf Forests"
eco.meta$biome_name[which(eco.meta$BIOME == 3)] = "Tropical & Subtropical Coniferous Forests"
eco.meta$biome_name[which(eco.meta$BIOME == 4)] = "Temperate Broadleaf & Mixed Forests"
eco.meta$biome_name[which(eco.meta$BIOME == 5)] = "Temperate Conifer Forests"
eco.meta$biome_name[which(eco.meta$BIOME == 6)] = "Boreal Forests/Taigas"
eco.meta$biome_name[which(eco.meta$BIOME == 7)] = "Tropical & Subtropical Grasslands, Savannas & Shrublands"
eco.meta$biome_name[which(eco.meta$BIOME == 8)] = "Temperate Grasslands, Savannas & Shrublands"
eco.meta$biome_name[which(eco.meta$BIOME == 9)] = "Flooded Grasslands & Savannas"
eco.meta$biome_name[which(eco.meta$BIOME == 10)] = "Montane Grasslands & Shrublands"
eco.meta$biome_name[which(eco.meta$BIOME == 11)] = "Tundra"
eco.meta$biome_name[which(eco.meta$BIOME == 12)] = "Mediterranean Forests, Woodlands & Scrub"
eco.meta$biome_name[which(eco.meta$BIOME == 13)] = "Deserts & Xeric Shrublands"
eco.meta$biome_name[which(eco.meta$BIOME == 14)] = "Mangroves"


biome = 
  eco.meta %>% 
  dplyr::select(name, biome_name) %>% 
  rename(Site_Code = name)

# write.csv(eco.meta,'eco.meta.csv',row.names = F)

