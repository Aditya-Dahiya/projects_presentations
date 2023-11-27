# Loading required libraries----------------------------------------------------
library(tidyverse)      # tidy tools data wrangling
library(ggtext)         # text into ggplot2
library(sf)             # maps and plotting
library(here)           # files location and loading
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(summarytools)   # Exploratory Data Analysis
library(figpatch)       # Images in patchwork
library(patchwork)      # For compiling plots

# Loading the data--------------------------------------------------------------


# Exploratory Data Analysis-----------------------------------------------------


# Getting the data into a tidy form for plotting


# Visualization parameters------------------------------------------------------

# Load fonts
font_add_google("Fjalla One", "title_font")       # Font for titles
font_add_google("Pragati Narrow", "caption_font") # Font for the caption
font_add_google("Fjalla One", "body_font")        # Font for plot text
showtext_auto()

# Define colours
low_col <- "#f593d7"               # Heat map: low colour
hi_col <- "#c7028c"                # Heat map: high colour
bg_col <- "#f7c8e9"                # Background Colour
text_col <- "#750153"              # Colour for the text
text_hil <- "#e30039"              # Colour for highlighted text

# Define Text Size
ts = 24                            # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
mastodon <- "&#xf4f6"
mastodon_username <- "@adityadahiya@mastodon.social"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: #000000'>{github_username}  </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: #000000'>{xtwitter_username}</span>"
)

# Add text to plot--------------------------------------------------------------
plot_title <- "..."

plot_subtitle <- "..."

plot_caption <- paste0("...", social_caption)

# Actual Plots------------------------------------------------------------------


# Compiling Plots: Patchwork----------------------------------------------------



# Saving the final image--------------------------------------------------------
ggsave(
  filename = here::here("docs", "....png"),
  plot = g,
  device = "png", dpi = "retina", 
  width = 10, height = 7, units = "cm",
  bg = bg_col)
)


