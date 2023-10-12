library(tidyverse)


data = readxl::read_excel("data/1000S_Dataset_Biogeochem_Biomass_Tomography_WEOM_2023_06_12.xlsx")
bgc_data = 
  data %>% 
  dplyr::select(
                "Sample_ID", "Location","mean_GWC", "K_mg_per_kg", "SO4-S_mg_per_kg", "B_mg_per_kg",                                           
                "Zn_mg_per_kg",                                          
                "Mn_mg_per_kg",                                          
                "Cu_mg_per_kg",                                          
                "Fe_mg_per_kg",                                          
                "Ca_Meq_per_100_g",                                      
                "Mg_Meq_per_100_g",                                      
                "Na_Meq_per_100_g",                                      
                "Total_Bases_Meq_per_100_g",                             
                "CEC_Meq_per_100_g",                                     
                "TKN_pct",                                               
                "Total_Sulfur_pct",                                      
                "Total_Nitrogen_pct",                                    
                "Total_Carbon_pct",                                      
                "C_to_N_ratio",                                          
                "NO3-N_mg_per_g_of_soil_mean",
                "NH4-N_mg_per_g_of_soil_mean",                           
                "P_Bicarb_mg_per_g_of_soil_mean",                        
                "pH",                                                    
                "P_Extract",                                             
                "Sand_pct",                                              
                "Silt_pct",                                              
                "Clay_pct",                                              
                "WEOC_mean",                                             
                "WETN_mean",                                             
                "MBC_mean",                                              
                "MBN_mean"
                ) %>% 
  rename(Site_Code = Sample_ID) %>% 
  mutate(Site_Code = str_remove(Site_Code, "1000S_")) %>% 
  mutate_at(vars(-Site_Code, -Location), as.numeric)



metadata = readxl::read_excel("data/1000Soils_Metadata_Site_Mastersheet_v1.xlsx")
metadata_biome = 
  metadata %>% 
  left_join(biome)

bgc_data_long = 
  bgc_data %>% 
  pivot_longer(cols = -c(Site_Code, Location)) %>% 
  mutate(value = as.numeric(value)) %>% 
  left_join(biome)

bgc_data_long %>% 
  drop_na() %>% 
  ggplot(aes(x = biome_name, y = value))+
  geom_jitter(width = 0.3)+
  facet_wrap(~name, scales = "free")+
  theme(axis.text.x = element_text(angle = 45, size = 2, hjust = 1))

plot(bgc_data)

metadata %>% 
  ggplot(aes(x = Long, y = Lat))+
  geom_point()
