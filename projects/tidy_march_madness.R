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
library(magick)         # Work with Images and Logos

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
# Exploratory Data Analysis & Data Wrangling -----------------------------------
#==============================================================================#

# library(summarytools)
# dfSummary(results) |> view()

img1 <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/2/28/March_Madness_logo.svg/1200px-March_Madness_logo.svg.png")

img1
#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Racing Sans One", 
                family = "title_font")       # Font for titles
font_add_google("Bowlby One SC",
                family = "subtext_font")     # Font for subtext
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Changa", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nbapalettes::hornets")
mypal

# Define colours
text_col <- "#0c2d56"               # Colour for the text
text_hil <- "#005eb9"               # Colour for highlighted text
bg_col <- "white"                   # Background Colour
lines_col <- "#717b85"              # Colour for lines and axes etc.

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
plot_title <- "NCAA Men's March Madness"
subtitle_text <- "Comparison of major teams' performance in NCAA Men Division-I basketball single-elimination tournament for the national championship."
plot_subtitle <- str_wrap(subtitle_text, 55)
plot_caption <- paste0("**Data & Inspiration:** Nishaan Amin (Bracketology: predicting March Madness) |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

col1 <- mypal[1]
col2 <- mypal[2]
col3 <- mypal[3]

remove_names <- c("Kentucky", "Gonzaga", "Florida", "Butler", "Baylor", "Syracuse", "Tennessee", "West Virginia")
df <- results |> 
  filter(games > 20) |> 
  filter(!(team %in% remove_names)) |> 
  mutate(
    sublabel = glue("<b style='color:{col1}'>C: {champ}</b>  |  <b style='color:{col2}'>F: {f2}</b>  |  <b style='color:{col3}'>SF: {f4}</b>"),
    size_var = games * winpercent
  )


g1 <- ggplot(
  data = df,
  aes(
    x = games, 
    y = winpercent,
    label = team,
    size = size_var
    )
  ) +
  geom_text(
    vjust = 0,
    family = "title_font", 
    colour = text_col
  ) +
  scale_size_continuous(
    range = c(30, 55)
  ) +
  ggnewscale::new_scale("size") +
  geom_richtext(
    aes(
      y = winpercent - 0.001,
      label = sublabel,
      size = size_var
    ),
    vjust = 1,
    fill = "transparent",
    color = "transparent",
    fontface = "bold",
    family = "subtext_font"
  ) +
  scale_size_continuous(
    range = c(9, 18)
  ) +
  annotate(
    geom = "label",
    label = "Name of the Team",
    x = 45,
    y = 0.5,
    family = "title_font", 
    colour = text_col,
    size = 55,
    label.padding = unit(0.5, "cm")
  ) +
  annotate(
    geom = "richtext",
    label = glue("Number of times a team has been: <br><b style='color:{col1}'>Champions</b>    <b style='color:{col2}'>Finalists</b>    <b style='color:{col3}'>Semi-Finalists</b>"),
    x = 45,
    y = 0.485,
    vjust = 1,
    fill = "white",
    color = lines_col,
    fontface = "bold",
    family = "subtext_font",
    size = 24,
    lineheight = 0.4,
    label.padding = unit(0.5, "cm"),
    label.r = unit(1.5, "lines")
  ) +
  scale_x_continuous(
    limits = c(18, 57),
    breaks = seq(20, 50, 10)
  ) +
  scale_y_continuous(
    limits = c(0.44, 0.81),
    labels = label_percent(),
    breaks = seq(0.5, 0.8, 0.1)
  ) +
  labs(
    title = NULL,
    subtitle = plot_subtitle,
    caption = plot_caption,
    y = "Percentage matches won by the team",
    x = "Total games played by the team"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 6 * ts,
      colour = text_hil,
      family = "title_font",
      hjust = 0.5,
      margin = margin(2, 0, 0, 0, "cm")
    ),
    plot.subtitle = element_text(
      size = 3 * ts,
      color = text_hil,
      hjust = 0.5,
      family = "title_font",
      lineheight = 0.3,
      margin = margin(10, 0, 1, 0, "cm")
    ),
    plot.caption = element_textbox(
      family = "caption_font",
      hjust = 0.5,
      vjust = 0,
      size = 1.5 * ts,
      colour = text_col,
      margin = margin(0.75, 0, 0.5, 0, "cm")
    ),
    axis.line = element_line(
      colour = lines_col,
      arrow = arrow(length = unit(1, "cm")),
      linetype = 1,
      linewidth = 1
    ),
    panel.grid = element_line(
      colour = lines_col,
      linetype = 2,
      linewidth = 0.3
    ),
    axis.text = element_text(
      colour = lines_col,
      family = "subtext_font",
      size = 2 * ts
    ),
    axis.title = element_text(
      colour = lines_col,
      family = "body_font",
      size = 3 * ts,
      hjust = 1, 
      margin = margin(0.4, 0.4, 0.4, 0.4, "cm")
    ),
    plot.title.position = "plot"
  )

gtop <- ggplot() +
  theme_void() +
  annotation_custom(
    grob = grid::rasterGrob(img1),
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf
  )

g <- g1 +
  inset_element(
    p = gtop,
    left = 0.1,
    right = 0.9,
    top = 0.99,
    bottom = 0.82,
    align_to = "full"
  )

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_march_madness.png"),
  plot = g,
  width = 40, 
  height = 55, 
  units = "cm",
  bg = bg_col
)
  