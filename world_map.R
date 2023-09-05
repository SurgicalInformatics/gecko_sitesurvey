library(tidyverse)
library(highcharter)
library(maps)
theme_set(theme_bw())

# load("patient_data_orig.rda")
# load("registrations_data_orig.rda")

reg_summary = registrations_orig %>% 
  count(country, iso2, region, wb, name = "n_registrations")

country_summary = patient_data_orig %>% 
  count(country, iso2, region, wb, name = "n_patients") %>% 
  full_join(reg_summary) %>% 
  rename(continent = region, mapname = country) %>% 
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
                             "Palestine"   = "West Bank and Gaza",
                             "Gambia"      = "Gambia, The",
                             "South Korea" = "Korea, Rep.",
                             "Taiwan"      = "Taiwan, China"))

#rm(patient_data_orig, registrations_orig)


world_map = iso3166 %>% 
  mutate(mapname = fct_recode(mapname,
                             "Czechia"   = "Czech Republic",
                             "Türkiye"   = "Turkey",
                             "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                             "Macao SAR, China" = "China:Macao",
                             "Hong Kong SAR, China" = "China:Hong Kong")) %>% 
  mutate(mapname = recode(mapname,
                          "UK(?!r)" = "UK", "Norway(?!:Bouvet|:Svalbard|:Jan Mayen)" = "Norway", "Finland(?!:Aland)" = "Finland"
  ))

countries = world_map %>%
  distinct(mapname)

countries_all = left_join(countries, country_summary) %>% 
  mutate(n_patients = if_else(is.na(n_patients), 0, n_patients),
         n_registrations = if_else(is.na(n_registrations), 0, n_registrations))

countries_mismatch = anti_join(country_summary, countries)
stopifnot(nrow(countries_mismatch) == 0)

world_map_data = iso3166

world_map_data = rename(world_map_data, "iso-a3" = a3)

world_map_data = world_map_data %>%
  mutate(mapname = recode(mapname,
                          "UK(?!r)" = "UK", "Norway(?!:Bouvet|:Svalbard|:Jan Mayen)" = "Norway", "Finland(?!:Aland)" = "Finland"
  ))


int_map_data = merge(world_map_data, countries_all, by = "mapname")

map_patients = int_map_data %>%
  mutate(has_data = if_else(n_patients > 0, 1, 0)) %>% 
  hcmap(
    map = "custom/world-robinson-highres", # high resolution world map
    # data = int_map_data, # name of dataset
    joinBy = "iso-a3",
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


map_registrations = int_map_data %>%
  mutate(has_data = if_else(n_registrations > 0, 1, 0)) %>% 
  hcmap(
    map = "custom/world-robinson-highres", # high resolution world map
    # data = int_map_data, # name of dataset
    joinBy = "iso-a3",
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
  hc_colorAxis(minColor = "white", maxColor = "#de6a35") %>%
  hc_chart(zoomType = "xy")

