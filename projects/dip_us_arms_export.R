library(tidyverse)
library(openxlsx)
library(janitor)

usdef <- openxlsx::read.xlsx("https://www.dsca.mil/sites/default/files/EDA_Public_Report_2020-06-15.xlsx") |> 
  as_tibble()

df <- usdef |> 
  clean_names() |> 
  select(fiscal_year_of_request,
         country_transfer_to,
         total_delivered_qty,
         total_current_value) |> 
  mutate(total_delivered_qty = as.numeric(total_delivered_qty)) |> 
  filter(!is.na(total_delivered_qty)) |> 
  filter(total_delivered_qty >= 1)

countries <- df |> 
  group_by(country_transfer_to) |> 
  summarise(total_current_value = sum(total_current_value)) |> 
  arrange(desc(total_current_value))

select_con <- countries |> 
  slice_head(n = 10) |> 
  pull(country_transfer_to)

df1 <- df |> 
  mutate(country = if_else(
    country_transfer_to %in% select_con,
    country_transfer_to,
    "Others"
  )) |> 
  group_by(fiscal_year_of_request, country) |> 
  summarize(total = sum(total_current_value))


df1 |> 
  mutate(label_con = if_else(
    total >= 50000000,
    country,
    NA
  )) |> 
  ggplot(aes(y = fiscal_year_of_request, 
             x = total,
             fill = country,
             label = label_con)) +
  geom_col(colour = "white",
           orientation = "y")
