library(tidyverse)
library(countrycode)

wb_orig = read_csv("data_raw/world_bank_income_2024.csv", skip = 1) %>% 
  janitor::remove_empty()

wb = wb_orig %>% 
  mutate(iso2 = countrycode(iso3, origin = "iso3c", destination = "iso2c")) %>% 
  mutate(iso2 = case_when(country == "Kosovo"          ~ "XK",
                          country == "Channel Islands" ~ "CX",
                          TRUE ~ iso2)) %>% 
  mutate(wb = if_else(country == "Venezuela, RB", "Upper middle income", wb))


wb %>% 
  count(iso2, sort = TRUE)

wb %>% 
  count(wb)

wb %>% write_csv("data_processed/world_bank_classifications2024.csv")
