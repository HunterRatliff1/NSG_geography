library(rvest)
library(httr)
library(mapboxapi)
library(mapdeck)
library(leaflet)
library(sf)
library(tidyverse)

x <- paste0("https://fortress.maptive.com/ver4/data.php?operation=get_map_markers&data_id=13417&map_id=13398&",
            "bounds=3.357907,-179.291017,75.935743,-54.837892&zoom=3&shared_map=1&lat_col=12&lng_col=13&name_col=4",
            "&pie_chart=0&aggregation=-1&group=all&group_col=7") %>%
  
  jsonlite::fromJSON() %>%
  .$markers %>%
  as_tibble() 
names(x) <- c("id", "lat", "lon", "Name", "level")

# x %>% write_csv("ATS data/scraped_locations.csv")

# Write & upload to mapbox
# https://walker-data.com/mapboxapi/articles/creating-tiles.html
walk(1:5, ~{
  x %>%
    st_as_sf(coords = c("lon", "lat"), crs=4269) %>%
    filter(level==.x) %>%
    tippecanoe(input = .,
               output = paste0("level", .x, ".mbtiles"), 
               layer_name = paste0("level", .x),
               overwrite = T)
})


read_csv("POS_2017/geocoded/POS_2017_tracts.csv", 
                col_types = cols(Census.Tracts.GEOID = col_character(), 
                                 accuracy = col_double(), type = col_character())) %>%
  filter(HospType!="Long term") %>%
  filter(Trauma) %>% View()
  st_as_sf(coords = c("long", "lat"), crs=4269) %>%
  tippecanoe(input = .,
             output = "POS.mbtiles", 
             layer_name = "TraumaPOS",
             overwrite = T)

read_csv("Google_results/geo_google_raw.csv") %>%
  filter(geoState!="hi", geoState!="ak") %>%
  st_as_sf(coords = c("lon", "lat"), crs=4269) %>%
  tippecanoe(input = ., 
             output = "NSG.mbtiles", 
             # layer_name = "NSG",
             overwrite = T)

options(tigris_class = "sf")
tigris::counties(state = c(state.abb, "DC"), cb=T, resolution = "5m") %>%
  filter(STATEFP!="02", STATEFP!="15") %>%
  # str()
  upload_tiles(username = "hunterratliff1", tileset_id = "US_Counties")





POS <- read_csv("POS_2017/geocoded/POS_2017_tracts.csv", 
         col_types = cols(Census.Tracts.GEOID = col_character(), 
                          accuracy = col_double(), type = col_character())) %>%
  filter(HospType!="Long term") %>%
  filter(Trauma) 
# st_as_sf(coords = c("long", "lat"), crs=4269) %>%

mapdeck(token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
        style = "mapbox://styles/hunterratliff1/ckddr29pn2rva1hpblsw1jibp") %>%
  add_scatterplot(data=POS, lat="lat", lon="long",
                  radius_min_pixels = 5, radius = 5, tooltip = "FAC_NAME")
x %>%
  filter(level!=0) %>%
  # mutate(lat = as.numeric(lat),
         # lon = as.numeric(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs=4269) %>%
  mutate(Name = str_glue("[{level}] {Name}")) %>%

  mapdeck(token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
          style = "mapbox://styles/hunterratliff1/ckddr29pn2rva1hpblsw1jibp",
          zoom = 3,
          location = c(-96.065, 38.116)) %>%
  add_scatterplot(update_view = F,
                  fill_colour="level",
                  # fill_opacity=0,
                  # auto_highlight=T,
                  radius_max_pixels = 5,
                  radius = 5000, 
                  palette = "heat_hcl",
                  legend = T,
                  tooltip = "Name")

# [1] "viridis"        "cividis"        "magma"          "inferno"        "plasma"         "ylorrd"        
# [7] "ylorbr"         "ylgnbu"         "ylgn"           "reds"           "rdpu"           "purples"       
# [13] "purd"           "pubugn"         "pubu"           "orrd"           "oranges"        "greys"         
# [19] "greens"         "gnbu"           "bupu"           "bugn"           "blues"          "spectral"      
# [25] "rdylgn"         "rdylbu"         "rdgy"           "rdbu"           "puor"           "prgn"          
# [31] "piyg"           "brbg"           "terrain"        "topo"           "heat"           "cm"            
# [37] "rainbow"        "terrain_hcl"    "heat_hcl"       "sequential_hcl" "rainbow_hcl"    "diverge_hcl"   
# [43] "diverge_hsv"    "ygobb"          "matlab_like2"   "matlab_like"    "magenta2green"  "cyan2yellow"   
# [49] "blue2yellow"    "green2red"      "blue2green"     "blue2red"      


# tigris::counties(state = c(state.abb, "DC"), cb=T, resolution = "5m") %>%
#   filter(STATEFP!="02", STATEFP!="15") %>%

list_styles("hunterratliff1") %>% str()
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # addMapboxTiles()
  mapboxapi::addMapboxTiles(username = "hunterratliff1",layerId = "traumapos",
                            style_id = "ckddr29pn2rva1hpblsw1jibp", group = "test") %>%
  addLayersControl(
    # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("test"),
    options = layersControlOptions(collapsed = FALSE)
  )



# rstudioapi::selectFile()

# Go to the page below, select the levels desired,
# and the addresses will appear at the bottom
# save the webpage as a single HTML file in Chrome
# then scrape away!
# > http://www.maptive.com/ver3/traumacenters

# Or use this JSON 
# https://fortress.maptive.com/ver4/data.php?operation=get_map_markers&data_id=13417&map_id=13398&bounds=3.357907,-179.291017,75.935743,-54.837892&zoom=3&shared_map=1&lat_col=12&lng_col=13&name_col=4&pie_chart=0&aggregation=-1&group=all&group_col=7

pg <- "~/Downloads/U.S. Trauma Centers.html" %>%
  read_html() %>%
  # html_nodes(".tabRow , .tabA") %>%
  html_nodes(".tabRow")

pg %>% html_nodes(".tabT") %>% html_text()

pg %>% html_nodes(".tabA") %>% 
  as.character() %>%
  str_replace("<br>", " ~ ") %>%
  str_remove('</span>') %>%
  str_remove('<span .+>')

pg %>% html_nodes(".tabD div:nth-child(2)") %>% 
  .[1:1] %>%
  html_text()





  
  # html_structure()
  # html_text()

# Name: .tabT
# Address: .tabA
# Trauma level: .tabD div:nth-child(2)

  
  
  
