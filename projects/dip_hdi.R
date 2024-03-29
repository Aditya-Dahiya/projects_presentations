# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# Source URL: https://hdr.undp.org/data-center/documentation-and-downloads
# Credits: UNDP Human Development Reports
# Human development, quantified. One of the most widely recognized measures, 
# the United Nations' Human Development Index amalgamates data on life 
# expectancy, per capita income, and educational attainment into a singular 
# value for each country-year. The UN offers downloadable files and an API 
# encompassing all yearly HDI rankings and sub-indicators spanning from 1990 to
# 2022. These resources also encompass information from correlated indices like 
# the Inequality-adjusted Human Development Index, Gender Development Index, and 
# Gender Inequality Index.

# =============================================================================#
# Findings ---------------------------------------------------------------------
# =============================================================================#


# =============================================================================#
# Library Load-in---------------------------------------------------------------
# =============================================================================#

# Data Wrangling Tools
library(tidyverse)
library(janitor)
library(here)
library(sf)

# Final plot (ggplot2) tools
library(scales)
library(fontawesome)
library(ggtext)
library(showtext)
library(colorspace)


# =============================================================================#
# Data Load-in, EDA & Data Wrangling--------------------------------------------
# =============================================================================#

url <- "https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv"

hdi <- read_csv(url)

names(hdi)

dflong <- hdi |> 
  select(iso3, 
         country, 
         region, 
         (contains("hdi_") & !(contains("_m_")) & !(contains("_f_")) & !(contains("ihdi")) & !(contains("phdi")) & !(contains("rank"))
          )
         )

df1 <- dflong |> 
  pivot_longer(
    cols = contains("hdi"),
    names_to = "year",
    values_to = "value"
  ) |> 
  mutate(year = parse_number(year))

df_imp <- dflong |> 
  mutate(improvement = hdi_2022 - hdi_2020) |> 
  select(country, improvement)

df_imp |> 
  arrange(improvement)

# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Road Rage",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Bubbler One",
  family = "body_font"
) 

showtext_auto()

# Define colours
mypal <- paletteer::paletteer_d("lisa::FridaKahlo")

bg_col <- mypal[3] # Background Colour
text_col <- mypal[1] # Colour for the text
text_hil <- mypal[5] |> darken(0.3) # Colour for highlighted text

# Define Text Size
ts <- unit(20, units = "cm") # Text Size

# Caption stuff
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_hil}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_hil}'>{xtwitter_username}</span>")


# Add text to plot--------------------------------------------------------------
plot_title <- ""
plot_caption <- paste0("**Data:** ", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- ""
plot_subtitle <- subtitle_text

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

df1 |> 
  ggplot(
    aes(
      x = year,
      y = value,
      group = country
    )
  ) +
  geom_line() +
  facet_wrap(~ region)

df1
# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_hdi.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = "white"
)
