# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# URL: https://unosat.org/products/3793
# The UNOSAT Gaza Strip Comprehensive Damage Assessment for January 2024 
# presents a thorough evaluation of structural damage and destruction based 
# on satellite imagery. The assessment compares images from January 6 and 7, 
# 2024, with those taken on May 1, 2023, May 10, 2023, September 18, 2023, 
# October 15, 2023, November 7, 2023, and November 26, 2023. According to the 
# analysis, 22,130 structures were destroyed, 14,066 severely damaged, and 
# 32,950 moderately damaged, totaling 69,146 structures. This represents 
# approximately 30% of all structures in the Gaza Strip, estimating 93,800 
# damaged housing units. Notably, Gaza and Khan Yunis governorates experienced 
# significant damage increases, with 10,319 and 11,893 structures newly damaged,
# respectively. Gaza City saw the highest number of newly destroyed structures, 
# totaling 8,926. It's important to note that this analysis is preliminary 
# and awaits field validation.


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
library(usmap)
library(usmapdata)
library(ggthemes)
library(patchwork)

# =============================================================================#
# Data Load-in, EDA & Data Wrangling--------------------------------------------
# =============================================================================#

gaza1 <- read_sf(here("data", "UNOSAT_GazaStrip_Agriculture_DA_GDB_January2024.zip"))


gaza2 <- read_sf(here("data", "UNOSAT_GazaStrip_CDA_January2024_GDB_V2.zip")) 

gaza2 <- gaza2 |> 
  filter(Damage_Status_4 == 3 | Damage_Status_4 == 0)


gaza_b <- read_sf(here("data", "gazastrip_municipalboundaries", "GazaStrip_MunicipalBoundaries.shp"))

# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Faster One",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Iceberg",
  family = "body_font"
) 

showtext_auto()

# Define colours
bg_col <- "white" # Background Colour
text_col <- "#012169FF" # Colour for the text
text_hil <- "#C8102EFF" # Colour for highlighted text

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
plot_title <- "Deadly police chases"
plot_caption <- paste0("**Data:** The San Francisco Chronicle", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- "Geographical distribution, race, age and role of persons killed in Police Chases in the United States (2017 - 2022)"
plot_subtitle <- str_wrap(subtitle_text, width = 70)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- ggplot() +
  geom_sf(
    data = gaza1,
    aes(fill = Classname),
    colour = "#dfffd4"
  ) +
  scale_fill_manual(values = c("blue", "#dfffd4")) +
  geom_sf(
    data = gaza2,
    aes(colour = factor(Damage_Status_4)),
    alpha = 0.5,
    size = 0.5
  ) +
  scale_colour_manual(
    values = c("#2e5c00", "red")
  ) +
  geom_sf(
    data = gaza_b,
    colour = "black",
    fill = "transparent"
  ) +
  theme_map() +
  theme(
    legend.position.inside = c(0.8, 0.2)
  )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_gaza_agri.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = "white"
)
