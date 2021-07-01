library(readxl)
library(janitor)

## analyze and grab info
metabolic_equivalents <- read_xlsx(
  path = here::here("data", "provided", "client_energy_calc.xlsx"),
  sheet = 5, skip = 8
) %>% 
  clean_names() %>% 
  select(activity, activity_specificity, mets) %>% 
  mutate(activity = factor(activity))

saveRDS(metabolic_equivalents, file  = here::here("data", "prepped", "met_equi.RDS"))
