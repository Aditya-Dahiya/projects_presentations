# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# Harvard Dataverse
# Replication data for: Border Orientation in a Globalizing World
# 
# Dataset URL: https://dataverse.harvard.edu/file.xhtml?fileId=4556042&version=1.1


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
library(dataverse)

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

border <- get_dataframe_by_id(
  fileid = 4556042,
  .f = readr::read_tsv,
  server = "dataverse.harvard.edu"
) |> 
  as_tibble()


# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Stint Ultra Condensed",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Advent Pro",
  family = "body_font"
) 

# Font for plot text 2
font_add_google("UnifrakturMaguntia",
  family = "country_font"
)

showtext_auto()

bg_col <- "white" # Background Colour
text_col <- "#364052" # Colour for the text
text_hil <- "#4a5a78" # Colour for highlighted text

# Define Text Size
ts <- 80 # Text Size

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
plot_subtitle <- str_wrap("", width = 70)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- ggplot()


# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "....png"),
  plot = g,
  width = 40,
  height = 59,
  units = "cm",
  bg = "white"
)
