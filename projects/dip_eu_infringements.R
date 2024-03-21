# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#


# =============================================================================#
# Findings ---------------------------------------------------------------------
# =============================================================================#

setwd("C:/Users/dradi/Downloads/mapping-indias-elections")
hy1 <- read_sf("haryana/haryana.assembly.shp")

plotdf <- hy1 |> 
  mutate(area = st_area(geometry),
         area = as.numeric(area))
  
ggplot(plotdf) + 
  geom_sf(
    aes(fill = area)
  ) +
  scale_fill_binned_sequential(
    palette = "Mint",
    labels = scales::label_number(
      scale = 1e-6,
      suffix = "sq.km."
    )
  ) +
  theme_map()
# =============================================================================#
# Library Load-in---------------------------------------------------------------
# =============================================================================#

# Data Wrangling Tools
library(tidyverse)
library(janitor)
library(here)
library(sf)
library(readxl)
library(rnaturalearth)

# Final plot (ggplot2) tools
library(scales)
library(fontawesome)
library(ggtext)
library(showtext)
library(colorspace)
library(ggthemes)
library(patchwork)

# =============================================================================#
# Data Load-in, EDA & Data Wrangling--------------------------------------------
# =============================================================================#

eudf <- read_excel(here("data", "Infringement_export.xlsx"))

eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", 
  "Croatia", "Republic of Cyprus", "Czechia", 
  "Denmark", "Estonia", "Finland", "France", "Germany", 
  "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
  "Luxembourg", "Malta", "Netherlands", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", 
  "Spain", "Sweden"
)

eumap <- ne_countries(
  scale = "medium",
  continent = "Europe",
  returnclass = "sf"
) |> 
  filter((sovereignt %in% eu_countries))


plotdf <- eumap |>
  left_join(
    eudf |> count(Country, sort = T),
    by = join_by(sovereignt == Country)
  )

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
mypal2 <- c("#121510FF", "#6D8325FF", "#D6CFB7FF", "#E5AD4FFF", "#BD5630FF")

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
plot_title <- "Gaza's Shattered Spaces"
plot_caption <- paste0("**Data:** UNOSAT by United Nations Institute for Training and Research (UNITAR)", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- "Utilizing R, ggplot2, and sf packages, we depict\nextensive building damage in Gaza Strip\nusing satellite data from UNOSAT and\nUNITAR. Significant agricultural field\ndevastation,especially in\nnorthern Gaza Strip,\nis also highlighted."
plot_subtitle <- subtitle_text

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

ggplot(plotdf) +
  geom_sf(
    aes(
      fill = n
    ),
    colour = "white"
  ) +
  geom_sf_text(
    aes(label = sovereignt),
    colour = "darkgrey"
  ) +
  scale_fill_binned_sequential(
    palette = "RdPu",
    breaks = seq(0, 40, 5)
  ) +
  coord_sf(
    xlim = c(-25, 40),
    ylim = c(35, 72)
  ) +
  theme_map()

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_gaza_agri.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = mypal[3]
)
