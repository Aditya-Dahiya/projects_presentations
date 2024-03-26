#================= #TidyTuesday : NCAA Men's March MAdness ====================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(janitor)        # Cleaning names etc. of messy dataset
library(glue)           # To apste together text for ggtext

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(scales)         # Scale Labels in ggplot2


library(patchwork)      # For compiling plots
library(figpatch)       # Images in patchwork
library(magick)         # Work with Images and Logos
library(colorspace)     # Lighten and darken Colours
library(cropcircles)    # Circular cropping of the image
library(cowplot)        # Adding images to x-axis

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load('2024-03-26')
results <- tuesdata$`team-results` |> 
  clean_names()
public_picks <- tuesdata$`public-picks`
rm(tuesdata)

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

# library(summarytools)
# dfSummary(results) |> view()

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

col1 <- "darkred"
col2 <- "blue"
col3 <- "darkgreen"

df <- results |> 
  filter(games > 20) |> 
  mutate(
    sublabel = glue("<i style='color:{col1}'>C {champ}</i> | <i style='color:{col2}'>F {f2}</i> | <i style='color:{col3}'>SF {f4}</i>")
  )




ggplot(
  data = df,
  aes(
    x = games, 
    y = winpercent,
    label = team
  )
  ) +
  geom_text(
    size = 6
  ) +
  geom_richtext(
    aes(
      y = winpercent - 0.05,
      label = sublabel
    ),
    size = 3
  )

  
#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Trade Winds", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Zen Dots", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("MoMAColors::Dali")
mypal

# Define colours
text_col <- "#DC3B34FF"               # Colour for the text
text_hil <- "#DC3B34FF"               # Colour for highlighted text
bg_col <- "black"                     # Background Colour

# Define Text Size
ts = unit(40, units = "cm")           # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "X-Men Mutant Moneyball"
subtitle_text <- "Comparison of the main characters in X-Men Mutant comic-books from 1960s to 1990s"
plot_subtitle <- str_wrap(subtitle_text, 100)

plot_caption <- paste0("**Data & Inspiration:** X-Men Mutant Moneyball by Anderson Evans |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#


#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_xmen_mutant.png"),
  plot = g,
  width = 40, 
  height = 55, 
  units = "cm",
  bg = bg_col
)