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

# Most damage is concentrated around two major cities: Gaza City and Khan 
# Younis; and in the agricultural fields surrounding it.

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

g <- ggplot() +
  geom_sf(
    data = gaza1,
    aes(fill = Classname),
    colour = bg_col
  ) +
  scale_fill_manual(
    values = c(mypal[1], mypal[2]),
    labels = c(
      "Damaged agricultural land",
      "Unaffected agricultural land"
    )
    ) +
  geom_sf(
    data = gaza2,
    aes(colour = factor(Damage_Status_4)),
    alpha = 0.5,
    size = 0.9,
    pch = 20
  ) +
  scale_colour_manual(
    values = c(mypal[4], mypal[5]),
    labels = c("Partial Building Damage", "Severe Building Damage")
  ) +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(0)) +
  labs(
    x = NULL, 
    y = NULL,
    fill = "Agricultural land status",
    colour = "Buildings' damage status",
    caption = plot_caption) +
  geom_sf(
    data = gaza_b,
    colour = mypal[1],
    fill = "transparent",
    linewidth = 0.5
  ) +
  geom_sf_label(
    data = gaza_b,
    aes(label = NAME),
    size = ts,
    colour = text_col,
    hjust = 0.5,
    family = "body_font",
    fill = bg_col,
    alpha = 0.7,
    na.rm = TRUE,
    label.padding = unit(0.1, "lines")
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 10
      )
    ),
    colour = guide_legend(
      override.aes = list(
        size = 20,
        alpha = 1
      )
    )
  ) +
  annotate(
    geom = "text",
    x = 34.20, y = 31.57,
    label = plot_title,
    family = "title_font",
    size = 6 * ts,
    colour = text_hil,
    hjust = 0,
    vjust = 1
  ) +
  annotate(
    geom = "text",
    x = 34.20, y = 31.53,
    label = plot_subtitle,
    family = "body_font",
    size = 2 * ts,
    colour = text_col,
    hjust = 0,
    vjust = 1,
    lineheight = 0.4
  ) +
  theme_map() +
  theme(
    legend.position.inside = c(0.6, 0.2),
    plot.background = element_rect(
      fill = mypal[3],
      colour = mypal[3]
    ),
    legend.title = element_text(
      family = "body_font",
      size = 7 * ts,
      hjust = 0,
      colour = text_col,
      margin = margin(5, 0, 5, 0, unit = "mm")
    ),
    legend.text = element_text(
      family = "body_font",
      size = 5 * ts,
      hjust = 0,
      vjust = 0.5,
      colour = text_col,
      margin = margin(5, 0, 5, 5, unit = "mm")
    ),
    legend.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    legend.box.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    legend.box.spacing = unit(4, "cm"),
    plot.caption = element_textbox(
      family = "caption_font",
      size = 3 * ts,
      hjust = 0.5,
      colour = text_hil
    )
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
  bg = mypal[3]
)
