library(tidyverse)
library(highcharter)
library(maps)
library(countrycode)
theme_set(theme_bw())

# load("patient_data_orig.rda")
# load("registrations_data_orig.rda")

reg_summary = registrations_orig %>% 
  mutate(country = case_when(str_detect(data_collection_dag, "_gaza_") ~ "Gaza",
                             country == "West Bank and Gaza" ~ "West Bank",
                             .default = country)) %>% 
  count(country, iso2, region, wb, name = "n_registrations")

         
country_summary = patient_data_orig %>% 
  mutate(country = case_when(str_detect(redcap_data_access_group, "_gaza_") ~ "Gaza",
                             country == "West Bank and Gaza" ~ "West Bank",
                             .default = country)) %>% 
  count(country, iso2, region, wb, name = "n_patients") %>% 
  full_join(reg_summary) %>% 
  rename(continent = region, mapname = country) %>% 
  # mapnames no longer used in matching
  mutate(mapname = fct_recode(mapname,
                             "USA"         = "United States",
                             "UK"          = "United Kingdom",
                             "Egypt"       = "Egypt, Arab Rep.",
                             "Russia"      = "Russian Federation",
                             "Slovakia"    = "Slovak Republic",
                             "Trinidad"    = "Trinidad and Tobago",
                             "Iran"        = "Iran, Islamic Rep.",
                             "Syria"       = "Syrian Arab Republic",
                             "Venezuela"   = "Venezuela, RB",
                             "Yemen"       = "Yemen, Rep.",
                             "Gambia"      = "Gambia, The",
                             "South Korea" = "Korea, Rep.",
                             "Taiwan"      = "Taiwan, China")) %>% 
  mutate(`iso-a2` = case_match(mapname,
                           "Kosovo" ~"KV",
                           "West Bank" ~ "WE",
                           "Gaza" ~ "GZ",
                           .default = iso2
                           )) %>% 
  mutate(n_patients = if_else(is.na(n_patients), 0, n_patients),
         n_registrations = if_else(is.na(n_registrations), 0, n_registrations))


map_patients = country_summary %>%
  mutate(has_data = if_else(n_patients > 0, 1, 0)) %>% 
  hcmap(
    map = "custom/world-palestine-highres", # high resolution world map
    # data = int_map_data, # name of dataset
    joinBy = "iso-a2",
    value = "has_data",
    #showInLegend = FALSE, # hide legend
    nullColor = "white",
    download_map_data = TRUE,
    name = "",
    borderColor = "black", borderWidth = 0.1,
    tooltip = list(pointFormat = "{point.name}: {point.n_patients} patients")
  ) %>%
  hc_mapView(projection = "EqualEarth") %>% 
  hc_mapNavigation(enabled = FALSE) %>%
  hc_legend("none") %>%
  #hc_title(text = "Gecko patient data entry") %>%
  hc_colorAxis(minColor = "white", maxColor = "#74a73e") %>%
  hc_chart(zoomType = "xy")


map_registrations = country_summary %>%
  mutate(has_data = if_else(n_registrations > 0, 1, 0)) %>% 
  hcmap(
    map = "custom/world-palestine-highres", # high resolution world map
    # data = int_map_data, # name of dataset
    joinBy = "iso-a2",
    value = "has_data",
    #showInLegend = FALSE, # hide legend
    nullColor = "white",
    download_map_data = TRUE,
    name = "",
    borderColor = "black", borderWidth = 0.1,
    tooltip = list(pointFormat = "{point.name}: {point.n_registrations} hospitals registered")
  ) %>%
  hc_mapView(projection = "EqualEarth") %>% 
  hc_mapNavigation(enabled = FALSE) %>%
  hc_legend("none") %>%
  #hc_title(text = "Gecko patient data entry") %>%
  hc_colorAxis(minColor = "#de6a35", maxColor = "#de6a35") %>%
  hc_chart(zoomType = "xy")

