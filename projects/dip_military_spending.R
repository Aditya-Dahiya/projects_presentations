# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DHMZOW
# Credits:
#   Harvard Dataverse
#   Miriam Barnum; Christopher Fariss; Jonathan Markowitz; Gaea Morales, 2022, 
#   Global Military Spending Dataset, https://doi.org/10.7910/DVN/DHMZOW, 
#   Harvard Dataverse, V3, UNF:6:SpAlzGWegLfksS5ID/EbfQ== [fileUNF]
#   
# How much money has each country allocated to its military annually? Various 
# datasets offer differing insights, spanning distinct periods and employing 
# diverse methodologies. Miriam Barnum and colleagues have compiled the Global 
# Military Spending Dataset, amalgamating data from 76 variables across 9 
# collection projects. They assert that this dataset offers the most 
# comprehensive compilation of published military spending data to date. Each 
# variable corresponds to a specific data source or methodology, with each 
# observation representing a country-year pair. The authors note that discrepancies 
# in reported expenditure for a given country-year are widespread, even among 
# datasets from the same project. 

# Credits: {dataverse} package by Shiro Kuriwaki ORCID iD [aut, cre], 
# Will Beasley ORCID iD [aut], Thomas J. Leeper ORCID iD [aut], 
# Philip Durbin ORCID iD [aut], Sebastian Karcher ORCID iD [aut], 
# Jan Kanis [ctb], Edward Jee [ctb]. Maintainer:	Shiro Kuriwaki

# =============================================================================#
# Findings ---------------------------------------------------------------------
# =============================================================================#


# =============================================================================#
# Library Load-in---------------------------------------------------------------
# =============================================================================#
# Data loading tools
library(dataverse) # download public data files directly in R

# Data Wrangling Tools
library(tidyverse)
library(openxlsx)
library(janitor)

# Final plot (ggplot2) tools
library(scales)
library(fontawesome)
library(ggtext)
library(showtext)
library(colorspace)

# =============================================================================#
# Data Load-in & Data Wrangling-------------------------------------------------
# =============================================================================#

# Credits: R package dataverse vignettes
# Technique URL: https://cran.r-project.org/web/packages/dataverse/vignettes/C-download.html


df2 <- get_dataframe_by_name(
  filename = "estimates_milex_cur_20220729.tab",
  dataset = "10.7910/DVN/DHMZOW", 
  server = "dataverse.harvard.edu",
  .f = readr::read_tsv)

df2 |> 
  filter(year == 2019) |> 
  select(gwno, year, indicator, mean) |> 
  count(indicator, sort = TRUE) |> 
  View()


# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Fjalla One",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Anton",
  family = "body_font"
) 

showtext_auto()

# Define colours
bg_col <- "white"   # Background Colour
text_col <- "#009270FF" |> darken(0.5) # Colour for the text
text_hil <- "#009270FF" # Colour for higlighted text

# Add text to plot--------------------------------------------------------------
plot_title <- ""
plot_caption <- paste0("**Data:** CRS Report for Congress. Excess Defense Articles: Grants and Sales to Allies and Friendly Countries", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- ""
plot_subtitle <- str_wrap(subtitle_text, width = 100)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#



  theme(
    legend.position = "none",
    strip.text.x = element_text(
      colour = text_col,
      family = "body_font",
      size = 18,
      margin = margin(5, 0, 5, 0, unit = "mm")
    ),
    plot.title = element_text(
      hjust = 0.5,
      family = "title_font",
      size = 32,
      colour = text_hil, 
      margin = margin(15, 0, 5, 0, unit = "mm")
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "caption_font",
      size = 14,
      colour = text_col,
      lineheight = 1.1,
      margin = margin(5, 0, 5, 0, unit = "mm")
    ),
    plot.caption = element_text(
      hjust = 0.5,
      colour = text_hil,
      size = 9,
      family = "caption_font",
      margin = margin(5, 0, 10, 0, unit = "mm")
    )
  )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_military_spending.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = bg_col
)
