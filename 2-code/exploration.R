## EMSL MONET 2023 WORKSHOP
## EXPLORATION OF BGC DATA
## KFP, 2023-10-10

################################# #
################################# #

library(tidyverse)
library(maptools); library(sf) # for biomes
library(ggthemes) # for theme_map

options(scipen = 999) # to prevent scientific notation numbers
theme_set(theme_bw(base_size = 12))


# load files --------------------------------------------------------------

data = readxl::read_excel("1-data/1000S_Dataset_Biogeochem_Biomass_Tomography_WEOM_2023_06_12.xlsx")
metadata = readxl::read_excel("1-data/1000Soils_Metadata_Site_Mastersheet_v1.xlsx")
analysis_key = read.csv("1-data/analyses_list.csv", na.strings = "") %>% dplyr::select(column, abbreviated, analysis_type)


#
# process metadata --------------------------------------------------------

# assign biomes
assign_biomes_to_meta = function(metadata){
  # code adapted from EBG, 2023
  
  shp = readShapePoly('1b-terrestrial_ecosystems_teow/wwf_terr_ecos.shp')

  long.lat = 
    metadata %>% 
    dplyr::select(Site_Code, Long, Lat) %>% 
    rename(name = Site_Code,
           longitude = Long,
           latitude = Lat) %>% 
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude))
  

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
  
  biome = 
    eco.meta %>% 
    mutate(biome_name = case_when(
      BIOME == 1 ~ "Tropical & Subtropical Moist Broadleaf Forests",
      BIOME == 2 ~ "Tropical & Subtropical Dry Broadleaf Forests",
      BIOME == 3 ~ "Tropical & Subtropical Coniferous Forests",
      BIOME == 4 ~ "Temperate Broadleaf & Mixed Forests",
      BIOME == 5 ~ "Temperate Conifer Forests",
      BIOME == 6 ~ "Boreal Forests/Taigas",
      BIOME == 7 ~ "Tropical & Subtropical Grasslands, Savannas & Shrublands",
      BIOME == 8 ~ "Temperate Grasslands, Savannas & Shrublands",
      BIOME == 9 ~ "Flooded Grasslands & Savannas",
      BIOME == 10 ~ "Montane Grasslands & Shrublands",
      BIOME == 11 ~ "Tundra",
      BIOME == 12 ~ "Mediterranean Forests, Woodlands & Scrub",
      BIOME == 13 ~ "Deserts & Xeric Shrublands",
      BIOME == 14 ~ "Mangroves"
    )) %>% 
    dplyr::select(name, biome_name) %>% 
    rename(Site_Code = name)
  
  metadata_biome = 
    metadata %>% 
    left_join(biome)
  
  metadata_biome
}
# metadata_biome = assign_biomes_to_meta(metadata) %>% dplyr::select(Site_Code, Site_Name_Long, Lat, Long, Vegetation, biome_name)
# metadata_biome %>% write.csv("1-data/metadata_biome.csv", row.names = F, na = "")

metadata_biome = read.csv("1-data/metadata_biome.csv")

#
# process data ------------------------------------------------------------

process_bgc_data = function(data, analysis_key, metadata_biome){
  # there are a LOT of columns, we don't need all
  # match with the `analysis_key` to get abbreviated names and subset the columns
  
  long = 
    data %>% 
    rename(Site_Code = Sample_ID) %>% 
    pivot_longer(cols = -c(Site_Code, Location)) %>% 
    mutate(value = as.numeric(value)) %>% 
    left_join(analysis_key, by = c("name" = "column")) %>% 
    drop_na() %>% 
    mutate(Site_Code = str_remove(Site_Code, "1000S_")) %>% 
    left_join(metadata_biome %>% dplyr::select(Site_Code, Lat, Long, biome_name))
  
#  long = 
#    data %>% 
#    rename(Site_Code = Sample_ID) %>% 
#    pivot_longer(cols = -c(Site_Code, Location)) %>% 
#    mutate(value = as.numeric(value)) %>% 
#    left_join(analysis_key %>% dplyr::select(column, abbreviated), by = c("name" = "column")) %>% 
#    drop_na() %>% 
#    mutate(Site_Code = str_remove(Site_Code, "1000S_")) %>% 
#    dplyr::select(-abbreviated) %>% 
#    pivot_wider()
#  long %>% write.csv("1-data/bgc_data.csv", row.names = F, na = "")
  
  
  
  wide = 
    long %>% 
    dplyr::select(-c(name, analysis_type)) %>% 
    pivot_wider(names_from = "abbreviated")

  list(long = long,
       wide = wide)  
}

bgc_long = process_bgc_data(data, analysis_key, metadata_biome)$long
bgc_wide = process_bgc_data(data, analysis_key, metadata_biome)$wide

#
# PCAs --------------------------------------------------------------------

compute_pca = function(bgc_wide){
  library(ggbiplot)
  

  fit_pca_function = function(dat){
    
    dat %>% 
      drop_na()
    
    num = 
      dat %>%       
      dplyr::select(where(is.numeric)) %>%
      dplyr::mutate(row = row_number()) %>% 
      drop_na()
    
    num_row_numbers = num %>% dplyr::select(row)
    
    grp = 
      dat %>% 
      dplyr::select(where(is.character)) %>% 
      dplyr::mutate(row = row_number()) %>% 
      right_join(num_row_numbers)
    
    
    num = num %>% dplyr::select(-row)
    pca_int = prcomp(num, scale. = T)
    
    list(num = num,
         grp = grp,
         pca_int = pca_int)
  }
  
  ## PCA input files ----
  pca_overall = fit_pca_function(bgc_wide %>% dplyr::select(-Lat, -Long, -GWC)) 
  
  ## PCA plots overall ----
  gg_pca_overall = 
    ggbiplot(pca_overall$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_overall$grp$biome_name), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size = 4, stroke = 1.5, alpha = 0.8,
               aes(shape = pca_overall$grp$Location,
                   color = groups))+ 
    scale_shape_manual(values = c(1, 16))+
    labs(shape="",
         color = "",
         title = "Overall PCA",
         subtitle = "")+
    NULL
  
  gg_pca_overall
  
}

#
# correlations ------------------------------------------------------------

compute_correlations = function(data_combined){
  #library(corrplot)
  

  fit_correlations_function = function(dat, TITLE){
    num = 
      dat %>%       
      dplyr::select(where(is.numeric)) %>%
      drop_na()
    
    num_clean = 
      num %>% 
      rownames_to_column("row") %>% 
      pivot_longer(-row) %>% 
      separate(name, sep = "_", into = c("name")) %>% 
      pivot_wider() %>% 
      dplyr::select(-row)
    
    
    m = cor(num_clean)
    p.mat <- ggcorrplot::cor_pmat(num_clean)
    
    ggcorrplot::ggcorrplot(m, type = "lower",
                           p.mat = p.mat,
                           outline.color = "black",
                           #   lab = TRUE, 
                           insig = "blank",
                           colors = c("#E46726", "white", "#6D9EC1"),
                           title = TITLE)
    
  }
  
  corr_all = fit_correlations_function(bgc_wide %>% dplyr::select(-Lat, -Long, -GWC), TITLE = "Only significant correlations (p <= 0.05) shown here")
  corr_all
}

#
# maps --------------------------------------------------------------------

make_map = function(bgc_wide, VAR, TITLE = ""){
  
  ## Set CRS
  common_crs <- 4326
  
  ## Set map size and point size
  point_size <- 2
  map_width = 9
  map_height = 6
  
  # Set up map layers for plotting
  
  ## Make US states map cropped to contiguous region
  us <- read_sf("1c-cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
    st_transform(., crs = common_crs) 
  
  us_bbox <- c(xmin = -125, xmax = -60, ymin = 20, ymax = 50)
  region <- st_crop(us, y = us_bbox)
  
  # make a dataset merging metadata with site lat-longs
  df_map <- 
    bgc_wide %>% 
    filter(!is.na(Lat) & !is.na(Long)) %>% 
    st_as_sf(., coords = c("Long", "Lat"), crs = common_crs)
  
  # Make the base map with all sites
  ## base_plot <- 
  ggplot() + 
    geom_sf(data = region, fill = "white", color = "grey70") + 
    geom_sf(data = df_map, aes_string(fill = VAR), size = 7, shape = 21, color = "black", stroke = 1) + 
    labs(title = TITLE)+
    ggthemes::theme_map() + 
    theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
          legend.key = element_rect(fill = "transparent"), 
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, face = "bold", vjust = 0.7),
          legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          plot.background = element_rect(color = "black", fill = NA, linewidth = 1)) + 
#    scale_color_viridis_c(option = "plasma") +
    scale_fill_gradientn(colors = soilpalettes::soil_palette("redox2", 5))
  
}


