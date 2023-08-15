library(tidyverse)

patient_data_orig <- list("token"=Sys.getenv("gecko_data_collection_token"),
                         content='report',
                         action='export',
                         format='csv',
                         type='flat',
                         csvDelimiter='',
                         report_id= 499,
                         rawOrLabel='label',
                         rawOrLabelHeaders='raw',
                         exportCheckboxLabel='false',
                         exportSurveyFields='false',
                         exportDataAccessGroups='false',
                         returnFormat='json') %>% 
  httr::POST(Sys.getenv("redcap_globalsurg_uri"), body = ., encode = "form") %>% 
  httr::content()

wb = read_csv("data_processed/world_bank_classifications2024.csv", na = "") %>% 
  mutate(wb = fct_relevel(wb, "High income", "Upper middle income", "Lower middle income"))

n_records0 = n_distinct(patient_data_orig$record_id)

patient_data_orig = patient_data_orig %>% 
  mutate(iso2 = toupper(str_sub(redcap_data_access_group, 1, 2))) %>% 
  left_join(wb)

n_records1 = n_distinct(patient_data_orig$record_id)
stopifnot(n_records0 == n_records1)
stopifnot(nrow(drop_na(wb)) == nrow(wb))

# site_survey %>% 
#   filter(is.na(wb))
