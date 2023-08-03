library(tidyverse)
# site_survey_orig = REDCapR::redcap_read(redcap_uri = Sys.getenv("redcap_globalsurg_uri"),
#                                         token      = Sys.getenv("gecko_teamreg_token"),
#                                         forms      = 'site_survey',
#                                         records    = 'ae_1385'
# )$data

site_survey_orig <- list("token"=Sys.getenv("gecko_teamreg_token"),
                         content='report',
                         action='export',
                         format='csv',
                         type='flat',
                         csvDelimiter='',
                         report_id= 495,
                         rawOrLabel='label',
                         rawOrLabelHeaders='raw',
                         exportCheckboxLabel='false',
                         exportSurveyFields='false',
                         exportDataAccessGroups='false',
                         returnFormat='json') %>% 
  httr::POST(Sys.getenv("redcap_globalsurg_uri"), body = ., encode = "form") %>% 
  httr::content()

save(site_survey_orig, file = "site_survey_orig.rda")

