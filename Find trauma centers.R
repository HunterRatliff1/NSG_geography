library(rvest)
library(tidyverse)

############################       This data was originally downloaded
###   Call the results   ###       on Jan 20th 2020
############################
trauma_pg1 <- read_html("https://www.facs.org/search/trauma-centers?country=united%20states&n=250") %>%
  html_nodes(".searchResults li , #content_element_0_acsCol9MainColumn h3")
trauma_pg2 <- read_html("https://www.facs.org/search/trauma-centers?country=united%20states&n=250&page=2") %>%
  html_nodes(".searchResults li , #content_element_0_acsCol9MainColumn h3")
trauma_pg3 <- read_html("https://www.facs.org/search/trauma-centers?country=united%20states&n=250&page=3") %>%
  html_nodes(".searchResults li , #content_element_0_acsCol9MainColumn h3")



############################
##   Make a single list   ##
############################
x <- c(html_text(trauma_pg1), html_text(trauma_pg2), html_text(trauma_pg3))

#################################
##   Make in to a data.frame   ##
#################################
df <- x %>%
  matrix(byrow=T,
         nrow=length(x)/5, 
         ncol=5) %>%
  as.data.frame() %>%
  select(-V1) %>%
  as_tibble()

names(df) <- c("CenterName", "Address", "Country", "Level")

df <- df %>% 
  mutate(
    Level = str_replace(Level, "Level/Details ", ""),
    Level = str_replace(Level, " Trauma Center", "")
    ) %>%
  
  mutate(
    LevelFull = Level,
    Level = case_when(
      Level == "Level I Adult, Level I Pediatric"   ~ "Level I",
      Level == "Level II Adult, Level II Pediatric" ~ "Level II",
      Level == "Level I Adult, Level II Pediatric"  ~ "Level I",
      Level == "Level I Pediatric"                  ~ "Level I",
      Level == "Level II Pediatric"                 ~ "Level II",
      T                                             ~ Level
    )
  ) 

rm(trauma_pg1, trauma_pg2, trauma_pg3, x)

#####################
##  Write to CSV   ##
#####################
df %>% write_csv("Data/TraumaCenters.csv")  

traumaCenters <- read_csv("Data/TraumaCenters.csv")


gc <- tibble(address="301 University Blvd., Galveston, TX 77555") %>%
  mutate_geocode(address,output = "all", nameType="short")

gc %>%
  group_by(address) %>%
  mutate(results = getResults(results))

getResults <- function(gc){
  i <- gc %>%
    # pluck("results", 1) %>%
    pluck("address_components") %>%
    map(pluck, "types") %>%
    detect_index(has_element, "administrative_area_level_2")
  
  # pluck(gc, "results", 1, "address_components", i, "long_name")
  
  tibble(
    lon = pluck(gc, "geometry", "location", "lng"),
    lat = pluck(gc, "geometry", "location", "lat"),
    type = pluck(gc, "types", 1),
    loctype = pluck(gc,  "geometry", "location_type"),
    address = pluck(gc,  "formatted_address"),
    county = pluck(gc, "address_components", i, "long_name"),
    details = pluck(gc, "address_components") %>%
      map_df(function(x){
        tibble(level = x$types[[1]], result = x$"short_name")
      })
  )
}

