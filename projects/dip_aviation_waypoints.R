# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# Aviation navigation points. Jon Keegan from Beautiful Public Data recently 
# delved into the FAA’s aviation charts, transforming the agency’s extensive 
# list of over 67,000 navigation points into a dataset available for download. 
# Keegan notes, “Frequently, these navigation points are named after aspects 
# of the local culture, cuisine, or sports teams.” 
# Boston’s rich sports heritage is evident in names like BOSOX, BRUWN, CELTS, 
# PATSS, FENWY, ORRRR, and BORQE. Salem features WITCH, while Plymouth boasts 
# PLGRM.


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

aviation <- read_csv("https://raw.githubusercontent.com/jonkeegan/faa-navigation-waypoints/main/faa_navigation_waypoints.csv")


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


# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_aviation_waypoints.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = mypal[3]
)
