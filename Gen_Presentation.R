library(magrittr)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(cowplot)
library(sf)       # newer, better version of sp
library(tigris)   # for shapefiles
library(sjPlot);  # for lm plots
# library(qwraps2)  # gives nice wrapper to calculate mean/CI with inline R code
library(texreg)   # Pretty regression tables
library(tidyverse)
library(gt)
library(mapdeck)

# settings for tigris
options(tigris_class = "sf")
options(tigris_use_cache = T) 



#-######################-#
####   READ IN DATA   ####
#------------------------#
# get shapefiles for US counties & states
US_counties <- tigris::counties(state = c(state.abb, "DC"), cb=T, resolution = "5m") %>%
  filter(STATEFP!="02", STATEFP!="15")
US_states <- tigris::states(cb=T, resolution = "5m") %>%
  filter(STATEFP %in% unique(US_counties$STATEFP))

# Distances for each county (from review_comment_v1.Rmd)
# distances <- read_csv("Data/County_Distances-WEIGHTED.csv")
distances <- paste0("https://raw.githubusercontent.com/HunterRatliff1/",
                    "NSG_geography/master/public_data/County_Distances-WEIGHTED.csv") %>%
  read_csv()


# Our main dataset
df_full <- paste0("https://raw.githubusercontent.com/HunterRatliff1/",
                  "NSG_geography/master/public_data/regression.csv") %>%
  read_csv() %>%
  
  select(-region, -division) %>%
  mutate(male   = male * 100,
         Over65 = Over65 *100) %>%
  mutate(Urbanization = forcats::fct_relevel(Urbanization, "Metro")) %>%
  
  # Drop old distance, add new distance
  select(-starts_with("dist_")) %>%
  inner_join(distances)

df <- df_full %>%
  filter(!is.na(Deaths)) # drop counties with missing rates

rm(distances)

#-####################-#
####   Base model   ####
#----------------------#
base_mod <- MASS::glm.nb(Deaths ~ offset(log(StdPop)) + male + Over65 + Urbanization +
                           dist_nsg_cent + dist_trm_cent, 
                         data = mutate_at(df, vars(starts_with("dist_")), ~.x/25))



#-#############################-#
####   Get point estimates   ####
#-------------------------------#

provs <- read_csv("Google_results/geo_google_raw.csv") %>%
  filter(geoState!="hi", geoState!="ak") %>%
  st_as_sf(coords = c("lon", "lat"), crs=4269)

POS <- read_csv("POS_2017/geocoded/POS_2017_tracts.csv", 
                col_types = cols(Census.Tracts.GEOID = col_character(), 
                                 accuracy = col_double(), type = col_character())) %>%
  filter(HospType!="Long term") %>%
  st_as_sf(coords = c("long", "lat"), crs=4269) %>%
  filter(Trauma)


#-##########################-#
####   Get imputed data   ####
#----------------------------#
TBI_Overall_state <- read_csv("TBI_data/State/TBI_Overall_state.csv") %>%
  select(ST:Deaths_ST)


df_imputed <- df_full %>%
  mutate(imputed = is.na(Deaths)) %>%
  # filter(ST=="MA") %>%
  
  ## For each state, find the number of deaths accounted for, and 
  ## total population of those counties with rates
  group_by(ST, imputed) %>%
  summarise(Deaths = sum(Deaths, na.rm = T),
            Pop    = sum(StdPop, na.rm=T)) %>%
  left_join(TBI_Overall_state) %>%
  
  
  ## Limiting to counties with existing rates, find the
  ## number of deaths and population unaccounted for
  filter(!imputed) %>%
  mutate(Pop_miss    = StdPop_ST - Pop,
         Deaths_miss = Deaths_ST - Deaths) %>%
  
  
  ## Determine the hypothetical mortality rate in
  ## counties with missing data. Join to original
  ## data.frame
  mutate(rate_miss = Deaths_miss/Pop_miss) %>%  # This is the raw rate (not the rate per 100,000)
  select(ST, rate_miss, starts_with("n_"), Deaths_miss) %>%
  left_join(df_full, .) %>%
  mutate(imputed = is.na(Deaths)) %>%
  
  # filter(ST=="MA") %>%
  
  ## Impute the missing rates, by calculating the projected number of
  ## deaths. If that's over 20, round down (since we know that can't
  ## be the case). Then, calculate the crude mortality rate
  mutate(Deaths_imp = if_else(imputed, round(StdPop*rate_miss), Deaths),
         OverEst    = if_else(imputed & Deaths_imp > 20, TRUE, FALSE),
         Deaths_imp = if_else(imputed & Deaths_imp > 20, 20, Deaths_imp),
         Crude_imp  = 100000*Deaths_imp/StdPop) %>%
         
  
  ## Add checks and benchmarks 
  group_by(ST) %>%
  mutate(n_missing    = sum(imputed),
         prop_missing = sum(imputed)/n()) %>%
  
  relocate(Deaths_imp, .after=Deaths) %>%
  relocate(Crude_imp, .after=Crude) 

rm(TBI_Overall_state) # remove state totals


#-#########################-#
####   Limit to states   ####
#---------------------------#
dat <- US_counties %>%
  # filter(STATEFP %in% c("48", "22", "40", "05","28")) %>%
  geo_join(df_full, by="GEOID") 

#-##################-#
####   Figure 1   ####
#--------------------#
p0 <- dat %>% ggplot() +
  
  # Tiles for distance to NSG
  geom_sf(aes(fill=dist_nsg_cent), color=NA) +
  
  # # Plot NSG (provs) or Trauma (POS)
  # geom_sf(data=st_intersection(provs, dat), alpha=.2) +
  
  # Add in the state borders
  geom_sf(data = filter(US_states, STATEFP %in% unique(dat$STATEFP)), 
          fill=NA, color="black") +
  
  coord_sf(crs = 4269) + 
  ggthemes::theme_map() +
  # scale_fill_distiller(palette="RdYlGn", guide = 'legend',
  #                      na.value = "grey50",
  #                      breaks = c(10,20,40,60),
  #                      values=scales::rescale(quantile(df$Crude)),
  #                      # values=scales::rescale(quantile(df_imputed$Crude_imp)),
  #                      name="TBI Mortality Rate\n(per 100,000)", direction=-1) +
  scale_fill_distiller(palette="Blues", guide = 'legend',
                       na.value = "grey50",
                       trans="sqrt",
                       name="Distance to NSG (miles)", direction=1) +
  
  labs(title = "Distance from county weighted centroid") +
  theme(legend.direction="horizontal")

p1 <- dat %>% ggplot() +
  
  # Tiles for distance to NSG
  geom_sf(aes(fill=dist_nsg_cent), color=NA) +
  
  # Plot NSG (provs) or Trauma (POS)
  geom_sf(data=st_intersection(provs, dat), alpha=.2) +
  
  # Add in the state borders
  geom_sf(data = filter(US_states, STATEFP %in% unique(dat$STATEFP)), 
          fill=NA, color="black") +
  
  coord_sf(crs = 4269) + 
  ggthemes::theme_map() +
  # scale_fill_distiller(palette="RdYlGn", guide = 'legend',
  #                      na.value = "grey50",
  #                      breaks = c(10,20,40,60),
  #                      values=scales::rescale(quantile(df$Crude)),
  #                      # values=scales::rescale(quantile(df_imputed$Crude_imp)),
  #                      name="TBI Mortality Rate\n(per 100,000)", direction=-1) +
  scale_fill_distiller(palette="Blues", guide = 'legend',
                       na.value = "grey50",
                       trans="sqrt",
                       name="Distance to NSG (miles)", direction=1) +
  
  labs(title = "Locations of neurosurgeons") +
  theme(legend.direction="horizontal")

p2 <- dat %>% ggplot() +
  
  # Tiles for distance to NSG
  geom_sf(aes(fill=dist_nsg_cent), color=NA) +
  
  # Plot NSG (provs) or Trauma (POS)
  # geom_sf(data=st_intersection(provs, dat), alpha=.2) +
  geom_sf(data=st_intersection(POS, dat), alpha=.5, shape=4) +
  
  # Add in the state borders
  geom_sf(data = filter(US_states, STATEFP %in% unique(dat$STATEFP)), 
          fill=NA, color="black") +
  
  coord_sf(crs = 4269) + 
  ggthemes::theme_map() +
  # scale_fill_distiller(palette="RdYlGn", guide = 'legend',
  #                      na.value = "grey50",
  #                      breaks = c(10,20,40,60),
  #                      values=scales::rescale(quantile(df$Crude)),
  #                      # values=scales::rescale(quantile(df_imputed$Crude_imp)),
  #                      name="TBI Mortality Rate\n(per 100,000)", direction=-1) +
  scale_fill_distiller(palette="Blues", guide = 'legend',
                       na.value = "grey50",
                       trans="sqrt",
                       # breaks = c(10,25,50,100,150),
                       name="Distance to NSG (miles)", direction=1) +
  
  labs(title = "Locations of trauma facility") +
  theme(legend.direction="horizontal")

p0
p1
p2
cowplot::plot_grid(p1, p2)  # export: 700 x 450

p1 + labs(title = "Distance from county weighted centroid",
          subtitle = "With locations of neurosurgeons (dots)")


p3 <- dat %>% ggplot() +
  
  # Tiles for mortality
  geom_sf(aes(fill=Crude), color=NA) +
  
  # Plot NSG (provs) or Trauma (POS)
  geom_sf(data=st_intersection(provs, dat), alpha=.2) +
  # geom_sf(data=st_intersection(POS, dat), alpha=.5, shape=4) +
  
  # Add in the state borders
  geom_sf(data = filter(US_states, STATEFP %in% unique(dat$STATEFP)), 
          fill=NA, color="black") +
  
  coord_sf(crs = 4269) + 
  ggthemes::theme_map() +
  scale_fill_distiller(palette="RdYlGn", guide = 'legend',
                       na.value = "grey50",
                       breaks = c(10,20,40,60),
                       values=scales::rescale(quantile(df$Crude)),
                       # values=scales::rescale(quantile(df_imputed$Crude_imp)),
                       name="TBI Mortality Rate\n(per 100,000)", direction=-1) +
  # scale_fill_distiller(palette="Blues", guide = 'legend',
  #                      na.value = "grey50",
  #                      trans="sqrt",
  #                      # breaks = c(10,25,50,100,150),
  #                      name="Distance to NSG (miles)", direction=1) +
  
  # labs(title = "Locations of trauma facility") +
  theme(legend.direction="horizontal")

p3 # export: 700 x 450


#-##################-#
####   Figure 1   ####
#--------------------#
Fig_rates <- dat %>%
  # geo_join(df_imputed, by="GEOID") %>%
  ggplot() +
  
  
  geom_sf(aes(fill=Crude), color=NA) +
  # geom_sf(aes(fill=Crude_imp), color=NA) +
  
  # NSG & Trauma
  # geom_sf(data=st_intersection(provs, dat), alpha=.2) +
  geom_sf(data=st_intersection(POS, dat), alpha=.2) +
  
  geom_sf(data = filter(US_states, STATEFP %in% unique(dat$STATEFP)), 
          fill=NA, color="black") +
  
  coord_sf(crs = 4269) + # RdYlGn
  scale_fill_distiller(palette="RdYlGn", guide = 'legend',
                       na.value = "grey50",
                       breaks = c(10,20,40,60),
                       values=scales::rescale(quantile(df$Crude)),
                       # values=scales::rescale(quantile(df_imputed$Crude_imp)),
                       name="TBI Mortality Rate\n(per 100,000)", direction=-1) +
  # scale_fill_distiller(palette="Blues", guide = 'legend',
  #                      na.value = "grey50",
  #                      trans="sqrt",
  #                      # values=scales::rescale(quantile(df$dist_cent)),
  #                      name="Distance (miles)", direction=1) +
  
  ggthemes::theme_map() +
  # labs(title = "[Fig 1] TBI Mortality Rates") +
  theme(legend.direction="horizontal")

Fig_rates # export: 700 x 450

Fig_rates + scale_fill_distiller(palette="Greys", guide = 'legend',
                                 na.value = "black",
                                 breaks = c(10,20,40,60),
                                 values=scales::rescale(quantile(df$Crude)),
                                 name="TBI Mortality Rate\n(per 100,000)", direction=1) 


#-##################-#
####   Figure 2   ####
#--------------------#
Fig_dist <- US_counties %>%
  select(-NAME) %>%
  # filter(STATEFP=="48") %>%
  geo_join(df_full, by="GEOID") %>%
  ggplot() +
  
  geom_sf(data = US_states, fill=NA, color="black") +
  geom_sf(aes(fill=dist_cent), color=NA) + 
  
  coord_sf(crs = 4269) +
  scale_fill_distiller(palette="Blues", guide = 'legend',
                       na.value = "grey50",
                       trans="sqrt",
                       # values=scales::rescale(quantile(df$dist_cent)),
                       name="Distance (miles)", direction=1) +
  # theme_dark()
  ggthemes::theme_map() +
  # labs(title = "[Fig 2] Distance to nearest neurosurgeon") +
  theme(legend.direction="horizontal") 

Fig_dist # export: 700 x 450

Fig_dist + scale_fill_distiller(palette="Greys", guide = 'legend',
                                na.value = "grey50",
                                trans="sqrt",
                                # values=scales::rescale(quantile(df$dist_cent)),
                                name="Distance (miles)", direction=1) 



mapdeck(token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
        # style = mapdeck_style('dark'), 
        pitch = 0 ) %>%
  add_sf(data=dat,
         fill_colour = "Crude",
         # fill_colour = "dist_nsg_cent",
         # elevation = "dist_nsg_cent",
         # elevation = "Crude",
         elevation_scale = 1000,
         
         fill_opacity = .75,
         legend = T,
         palette = "reds") %>%
  
  # add_hexagon(data = st_intersection(POS, dat),
  #             layer_id = "Trauma",
  #             radius = 5000,
  #             elevation_scale = 100)
  add_grid(data=st_intersection(POS, dat),
           cell_size = 5000,
           layer_id = "Trauma")
  # add_screengrid(data = st_intersection(POS, dat),
  #                opacity = 0.3,
  #                cell_size = 10,
  #                layer_id = "Trauma")
  add_grid(data = mutate(x, lat=as.numeric(lat), lon=as.numeric(lon))
           , lat = "lat"
           , lon = "lon"
           # , weight = "weight"
           , layer_id = "grid_layer"
           , cell_size = 1000
           # , opacity = 0.3
           # , colour_range = colourvalues::colour_values(1:6, palette = "plasma")
  )

