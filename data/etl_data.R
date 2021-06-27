library(dplyr, quietly = T, warn.conflicts = F)
library(tidyr, quietly = T, warn.conflicts = F)
library(tibble, quietly = T, warn.conflicts = F)
library(here, quietly = T, warn.conflicts = F)
library(readxl, quietly = T, warn.conflicts = F)
library(janitor, quietly = T, warn.conflicts = F)
library(forcats, quietly = T, warn.conflicts = F)
library(rvest, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(gender, quietly = T, warn.conflicts = F)

city_url <- "https://citymonitor.ai/environment/where-are-largest-cities-britain-1404"
uk_cities <- session(url = city_url) %>%
  html_elements(css = "ol:nth-child(16) li") %>%
  html_text2() %>%
  as_tibble() %>%
  separate(col = value, into = c("city", "population"), sep = " – ") %>%
  mutate(population = parse_number(population)) %>%
  slice_sample(weight_by = population, replace = T, n = 100) %>%
  pull(city)


clients_info <- read_excel(here("data", "provided", "fake_clients_info_data.xlsx")) %>%
  add_column(city = uk_cities) %>%
  separate(col = phone_number_prefix, into = c("phone_number_prefix", "other"), sep = " ") %>%
  select(-c(sex, other, age)) %>%
  mutate(
    email = tolower(email),
    birthday = dmy(birthday),
    age = interval(start = birthday, end = today()) / duration(n = 1, unit = "years"),
    age = floor(age),
    phone_number_prefix = parse_number(phone_number_prefix),
    phone_number = parse_number(phone_number),
    years = year(birthday),
    sex = ifelse(str_sub(client_name, -1) %in% c("a", "e", "y", "e"), "F", "M"),
    sex = as_factor(sex)
  ) %>% 
  select(-years)

# do(results = gender(.$client_names, years = .$years[1], method = "ssa")) %>%
# do(bind_rows(.$results))

# 
# client_measurament = read_csv(here("data", "provided", "fake_bmi_data.csv")) %>% 
#   clean_names() %>% 
#   rename(height = height_inches, weight = weight_pounds) %>% 
#   drop_na(bmi) %>% 
#   mutate(height = height*2.54,
#          weight = 0.4535*weight,
#          ) %>% 
#   sample_n(100)
#

saveRDS(clients_info, file = "data/prepped/clients_info.RDS")

